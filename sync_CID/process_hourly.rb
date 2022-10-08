require 'aws-sdk-s3'
require 'open-uri'
require 'cgi'
require 'json'
require 'csv'
require 'date'

# Syncs the acoustic data from the CIDataSolutions server to an S3 bucket,
# converting high resolution measurements in CID's binary files to usable 
# readings in daily CSV files.
#
# This code is designed to run as an AWS Lambda function, but can also be run locally.
# In either scenerio, the following environment variables must be set: 
#
#   SSND_USER, SSND_SERIAL, SSND_ISN, SSND_CID, SSND_S3_NAME, SSND_S3_REGION
#
# The user must have access to the destination S3 bucket, and the credentials above
# must be valid for accessing the CIDataSolutions API.

S3_BUCKET = Aws::S3::Resource.new(region: ENV['SSND_S3_REGION']).bucket(ENV['SSND_S3_NAME'])

# Assume: 1 s sampling interval, 10 min buckets, daily report files, woken at least hourly
SAMP_IN_REPT = 24*60*60
BUCK_IN_REPT = 24*6
SAMP_IN_BUCK = 10*60
SEC_IN_DAY   = 24*60*60
HALF_BUCK    = 5*60


def lambda_handler(event:, context:)
    now = Time.now.utc
    today = DateTime.new(now.year, now.month, now.day, 0, 0, 0, Rational(0/24)).to_time.utc
    etags = []

    # try twice to complete yesterday's data
    if 1 <= now.hour && now.hour <= 3
        yesterday = today - SEC_IN_DAY
        if (res1 = sync_day(yesterday))
            etags << res1.etag
        end
    end

    # always update today's data
    if (res2 = sync_day(today))
        etags << res2.etag
    end

    retval = { statusCode: 200, body: JSON.generate(etags) }
    puts retval
    retval
end


def sync_day(day_time)
    name, csv_old = get_s3_file_by_day(day_time) 
    res = nil

    if !csv_old || csv_old.split("\n").length-1 < BUCK_IN_REPT
        csv_new = generate_csv_for_day(day_time)

        if !csv_old || csv_new.split("\n").length > csv_old.split("\n").length 
            if csv_new.split("\n").length > 1
                res = S3_BUCKET.object(name).put(body: csv_new, content_type: "text/csv; charset=utf-8")
            end
        end
    end
    res
end

def get_s3_file_by_day(day_time)
    name = sprintf("SSND_%s_%d_%02d_%02d.csv", ENV['SSND_CID'], day_time.year, day_time.month, day_time.day)
    res = nil

    begin
        res = S3_BUCKET.object(name).get
    rescue Aws::S3::Errors::NoSuchKey
    end

    csv_data = res.body.string if res 

    [name, csv_data]
end

def generate_csv_for_day(day_time)
    # offset by half a bucket, so that our averages will be properly centered
    start_time = day_time - HALF_BUCK

    ranges = get_range_data_from_cidata(start_time)

    buckets = align_and_group_ranges(ranges, start_time)
    format_csv_file(buckets, day_time)
end

def get_range_data_from_cidata(start_time)
    latest_src_name = get_recent_source_filename(1)
    update_source_file(latest_src_name)

    end_time = start_time + SEC_IN_DAY

    raw = get_hourly_source_data(latest_src_name, start_time, end_time)
    ranges = extract_wls_ranges(raw)

    if ranges.length > 0
        first_range_start_time = abstime2local(ranges.map {|x| x[:header][0]}.sort.first)

        if first_range_start_time > start_time
            # The latest file doesn't cover the day, so get the previous one too.
            # We assume the last two wlg files cover the time period we care about,
            # because we don't trust the filename timestamp, and we're not cracking all
            # the files open to hunt for more.
            if second_latest_src_name = get_recent_source_filename(2)
                update_source_file(second_latest_src_name)

                raw2 = get_hourly_source_data(second_latest_src_name, start_time, end_time)
                ranges2 = extract_wls_ranges(raw2)

                ranges += ranges2
            end
        end
    end
    ranges
end

def align_and_group_ranges(ranges, start_time_ref)

    # First, every sample needs a time offset so that we can make sense of the coverage.
    buckets_for_ranges = ranges.map do |rng|

        # Subtract start_time_ref so that time offset at beginning of desired range is zero.
        start_offset = (abstime2local(rng[:header][0]) - start_time_ref).to_i

        lmax = rng[:data][:lmax][:samples]
        leq = rng[:data][:leq][:samples]
        lmin = rng[:data][:lmin][:samples]
        num = rng[:data][:leq][:header][1]

        (start_offset...(start_offset+num)).zip(lmax).zip(leq).zip(lmin).map {|x| x.flatten}.group_by {|x| x[0] / SAMP_IN_BUCK}
    end

    # Next, ranges may collide on buckets but not samples, so merge down to single bucket.
    master_buckets = {}

    buckets_for_ranges.each do |bucket| 
        master_buckets = bucket.inject(master_buckets) do |car, (key,val)| 
            if existing = car[key]
                car[key] = existing + val
            else
                car[key] = val
            end
            car
        end
    end

    # Ignore buckets with a very small number of samples.
    master_buckets.select {|key,val| val.length > 3}
end

def format_csv_file(buckets, day_time)
    results = buckets.map do |idx, val| 
        num = val.length

        [idx, {
            :Time   => (day_time + idx*10*60).utc.to_s,
            :LAeq   => (10*Math.log10(val.map {|x| 10**(x[2]/10.0)}.reduce(:+)/num)).round(2).to_s,
            :LAFmax => val.map {|x| x[1]}.max.round(2).to_s,
            :LAFmin => val.map {|x| x[3]}.min.round(2).to_s,
            :LAF01  => val.map {|x| x[2]}.sort[((1-0.01)*num).floor].round(2).to_s,
            :LAF10  => val.map {|x| x[2]}.sort[((1-0.10)*num).floor].round(2).to_s,
            :LAF50  => val.map {|x| x[2]}.sort[((1-0.50)*num).floor].round(2).to_s,
            :LAF90  => val.map {|x| x[2]}.sort[((1-0.90)*num).floor].round(2).to_s,
            :LAF99  => val.map {|x| x[2]}.sort[((1-0.99)*num).floor].round(2).to_s,
        }]
    end

    csv_string = CSV.generate do |csv|
        csv << ["Time", "LAeq", "LAFmax", "LAFmin", "LAF01", "LAF10", "LAF50", "LAF90", "LAF99"]

        results.sort_by {|x| x[0]}.each do |rec|
            r = rec[1]
            csv << [r[:Time], r[:LAeq], r[:LAFmax], r[:LAFmin], r[:LAF01], r[:LAF10], r[:LAF50], r[:LAF90], r[:LAF99]]
        end
    end
end

def cfc_api(method, params=nil)
    url_base = "https://www.cidatasolutions.com/cfc/api.cfc?method=#{method}&user_name=#{ENV['SSND_USER']}&SN=#{CGI.escape(ENV['SSND_SERIAL'])}&ISN=#{ENV['SSND_ISN']}"

    case method
    when "showInstrumentList"
        url = url_base 
    when "updateWLGfile"
        url = url_base + "&FileName=#{params['FileName']}&FileExt=wlg"
    when "createPartialFile"
        url = url_base + "&FileName=#{params['FileName']}&FileExt=wlg&StartUTC=#{params['StartUTC']}&EndUTC=#{params['EndUTC']}"
    end

    URI.parse(url).read
end

def get_recent_source_filename(far_back)
    res = cfc_api("showInstrumentList")
    files = JSON.parse(res).first["List"].split(',')

    timestamps_by_device = files.map do |name| 
        # ex name: CID_1453_2020_03_10__09h44m22s

        if m = name.match(/^CID_(\d+)_(\d\d\d\d)_(\d\d)_(\d\d)__(\d\d)h(\d\d)m(\d\d)s$/)
            ts = DateTime.new(m[2].to_i, m[3].to_i, m[4].to_i, m[5].to_i, m[6].to_i, m[7].to_i)
            {:cid => m[1], :ts => ts, :file => name}
        end
    end

    ordered = timestamps_by_device.select {|x| x[:cid] == ENV['SSND_CID']}.sort_by {|x| x[:ts]}

    if far_back <= ordered.length
        return ordered.last(far_back).first[:file]
    else
        return nil
    end
end

def update_source_file(name) 
    # no idea what this does.  bypass caching?
    # regardless, "make sure we are receiving the most recent data" sounds important, so do it
    cfc_api("updateWLGfile", {"FileName" => name})
end

def get_hourly_source_data(name, start_time, end_time)
    t1 = local2abstime(start_time)
    t2 = local2abstime(end_time)

    cfc_api("createPartialFile", {"FileName" => name, "StartUTC" => t1, "EndUTC" => t2})
end


# == A note on time references == 
# 64 bit timestamps are sec since 1/1/1904, 0 dawn, UTC
# (there are other places where the reference is 12 noon instead, but not here)

def abstime2local(abstime)
    DateTime.new(1904,1,1,0,0,0, Rational(0,24)).to_time + abstime
end

def local2abstime(local)
    (local - DateTime.new(1904,1,1,0,0,0, Rational(0,24)).to_time).round
end


def extract_wls_ranges(raw)
    # do a full parse of the wls file, even though we only keep the ranges
    idx = 0
    header = raw[idx,86].unpack("a3CH6CA12CA25CA7CA11Q>Q>N")
    version = header[1]
    num_sync = header[13]
    idx += 86

    sync_details = [] 
    num_sync.times do
        if 1 == version
            sync_details << raw[idx,16].unpack("Q>G")
            idx += 16
        else
            sync_details << raw[idx,20].unpack("Q>Gg")
            idx += 20
        end
    end

    num_ranges = raw[idx,4].unpack("N").first
    idx += 4

    ranges = [] 
    num_ranges.times do 
        range_header = raw[idx,23].unpack("Q>H22l>")
        idx += 23
        range_data = {}

        [:lmax, :leq, :lmin].each do |metric|
            subhead = raw[idx,16].unpack("H24N")
            idx += 16

            samples = raw[idx,4*subhead[1]].unpack("g*")
            idx += 4*subhead[1]

            range_data[metric] = {:header => subhead, :samples => samples}
        end

        ranges << {:header => range_header, :data => range_data}
    end

    num_samps = ranges.map{|x| x[:data].map {|k,v| v[:samples].length}.inject(:+)}.inject(:+) || 0

    # can make invalid file detection a lot better, but if these 3 values scattered across all layers
    # in the file add up to the total file length, then we have confidence we got it right

    unless raw.length == (90 + 16*num_sync + (23+3*16)*ranges.length + 4*num_samps)
        raise "parsing failed; something is missing #{raw.length} #{num_sync} #{ranges.length} #{num_samps}"
    end

    ranges
end
