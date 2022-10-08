# This is not production code.  It can be used for debugging, or
# more practically as a reference for the WLS and WLG file formats.


# WLS file format
#
# 86 + 16*M + 4 + (23 + 3*16)*R + 4*N total filesize
#    86 byte fixed header
#     M sync details, each 16 bytes (roughly 1/hour)
#     4 byte range count
#     R ranges, each having 23 byte range header and 3*16 byte subheaders for Lmax,Leq,Lmin
#     N total samples across 3 flavors Lmax,Leq,Lmin; each 4 bytes
# 
# 86 byte main header    
#      0-2    WLS signature, char(3), a3
#      3      version, uint(1), C
#      4-6    always 0, unit(3), H6
#      7      model str len, uint(1), C
#      8-19   model, char(12), A12
#      20     serial no len, uint(1), C 
#      21-45  serial no, char(25), A25
#      46     firmware len, uint(1), C
#      47-53  firmware revision, char(7), A7
#      54     user id len, uint(1), C
#      55-65  user id, char(11), A11
#      66-73  time1 very close to date of birth, uint(8), Q>
#      74-81  time2 close to last calibration, uint(8), Q>
#      82-85  number of sync details M, uint(4), N
# 16*M sync details 
#      0-7    server time of sync, uint(8), Q>
#      8-15   drift in ppm, float(8), G
#      16-19  RSSI (only in v2), float(4), g
#  4 bytes num ranges, uint(4), N
# 23 byte range header
#      0-7   start time of file range, uint(8), Q>
#      8-18  ???, H22
#     19-22  instrument timezone, int(4), l>
#  16 byte subheader for Lmax
#      0-11   ???, H24
#      12-16  number of samples N, uint(4), N
# 4*N byte data buffer Lmax
#      0-3    Lmax, float(4), g
#  16 byte subheader for Leq
#      0-11   ???, H24
#      12-16  number of samples N, uint(4), N
# 4*N byte data buffer Leq
#      0-3    Leq, float(4), g
#  16 byte subheader for Lmin
#      0-11   ???, H24
#      12-16  number of samples N, uint(4), N
# 4*N byte data buffer Lmin
#      0-3    Lmin, float(4), g
def parse_wls(name)
    raw = File.open(name, "rb").read
    idx = 0

    header = raw[idx,86].unpack("a3CH6CA12CA25CA7CA11Q>Q>N")
    num_sync = header[13]
    idx += 86

    sync_details = [] 
    num_sync.times do
        sync_details << raw[idx,16].unpack("Q>G")
        idx += 16
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

    unless raw.length == (90 + 16*num_sync + (23+3*16)*ranges.length + 4*num_samps)
        raise "something is missing #{raw.length} #{num_sync} #{ranges.length} #{num_samps}"
    end

    dump_wls_header(header)
    dump_wls_sync_details(sync_details)
    dump_wls_ranges(ranges)

    {:header => header, :sync_details => sync_details, :ranges => ranges}
end

def dump_wls_header(vals) 
    puts "====== HEADER ======="
    puts "sig    = #{vals[0]}"
    puts "ver    = #{vals[1]}"
    puts "zero   = #{vals[2]}"
    puts "model  = #{vals[4]}"
    puts "serial = #{vals[6]}"
    puts "firm   = #{vals[8]}"
    puts "user   = #{vals[10]}"
    puts "birth  = #{vals[11]} (#{abstime2local(vals[11])})"
    puts "config = #{vals[12]} (#{abstime2local(vals[12])})"
    puts "n sync = #{vals[13]}"
end

def dump_wls_sync_details(vals) 
    puts "====== SYNC DETAILS =======" unless vals.length == 0

    for idx in 0..(vals.length-1) do
        idx_str = sprintf("%02d", idx)
        puts "#{idx_str}, serv time = #{vals[idx][0]} (#{abstime2local(vals[idx][0])}), drift ppm #{vals[idx][1]}"
    end
end

def dump_wls_ranges(ranges) 
    for idx in 0..(ranges.length-1) do
        idx_str = sprintf("%03d", idx)
        puts
        puts "====== RANGE #{idx_str} ======="

        dump_wls_range_header(ranges[idx][:header])

        ranges[idx][:data].each do |key,val|
            dump_wls_sub_header(key, val[:header])
        end

#        dump_wls_all_samples(ranges[idx][:data])
    end
end

def dump_wls_range_header(vals)
    puts "--- RANGE HEADER ---"
    puts "start  = #{vals[0]} (#{abstime2local(vals[0])})"
    puts "???      #{vals[1]}"
    puts "instTZ = #{vals[2]}"
end

def dump_wls_sub_header(name, vals)
    puts "--- #{name} SUB HEADER ---"
    puts "??? #{vals[0]}, num samps = #{vals[1]}"
end

def dump_wls_all_samples(valh) 
    puts "--- SAMPLES ---"
    lmax, leq, lmin = valh[:lmax][:samples], valh[:leq][:samples], valh[:lmin][:samples]

    for idx in 0..(lmax.length-1) do
        puts "#{lmax[idx]}, #{leq[idx]}, #{lmin[idx]}"
    end
end



#  WLG file format
#
#  with M frames and N samples, size = 16 + 196*M + 2*N
#
#  4  byte frame length
#  16 byte file header (only first frame)
#  2*N byte frame samples
#  192 byte trailer
#
#   4 bytes frame length, uint(4), L>
#
#  16 byte file header (only first frame)
#      0-15     ????
#
#  2*N samples, samples grouped in triples
#      0-1      Lmax, int(2), s
#      2-3      Leq, int(2), s
#      4-5      Lmin, int(2), s
#      ...      ...
#
#  192 byte trailer
#      0-7      double?, float(8), G
#      8-15     double?, float(8), G
#      16-18    zero, uint(3), H6
#      19       model str len, uint(1), C
#      20-31    model, char(12), A12
#      32       firmware len, uint(1), C
#      33-39    firmware revision, char(7), A7
#      40       serial no len, uint(1), C
#      41-65    serial no, char(25), A25
#      66       time1 str len, uint(1), C
#      67-96    time1 str very close to date of birth, char(30), A30
#      97       time2 str len, uint(1), C
#      98-127   time2 str close to last calibration, char(30), A30
#.     128      user id len, uint(1), C
#      129-139  user id, char(11), A11
#      140-155  ???? H32
#      156-159  instrument timezone, int(4), l>
#      160-167  start date time, uint(8), Q>
#      168-175  server time, uint(8), Q>
#      176-183  meter time, uint(8), Q>
#      184-187  float?, float(4), g
#      187-191  zero, uint(4), H8

def abstime2local(abstime)
    # 64 bit timestamps are in seconds since 1/1/1904 at 0 dawn in UTC
    DateTime.new(1904,1,1,0,0,0, Rational(0,24)).to_time + abstime
end

def local2abstime(local)
    (local - DateTime.new(1904,1,1,0,0,0, Rational(0,24)).to_time).round
end

def parse_wlg(name)
    raw = File.open(name, "rb").read

    idx = 0
    frames = []
    framesize = 192

    while idx < raw.length do
        file_header = nil
        samples = nil
        trailer = nil

        num_bytes = raw[idx,4].unpack("L>").first
        idx += 4

        if 4 == idx
            file_header = raw[idx,16].unpack("H32")
            idx += 16
            num_bytes -= 16
        end        

        samples = raw[idx,num_bytes].unpack("s*")
        idx += num_bytes

        if raw.length != 362
            trailer = raw[idx,192].unpack("GGH6CA12CA7CA25CA30CA30CA11H32l>Q>Q>Q>gH8")
            framesize = 192
        else
            # user id is missing from small sample file
            trailer = raw[idx,192].unpack("GGH6CA12CA7CA25CA30CA30H32l>Q>Q>Q>gH8")
            framesize = 180
        end
        idx += framesize

        frames << {:header => file_header, :samples => samples, :trailer => trailer}
    end

    num_frames = frames.length
    num_samps = frames.map {|x| x[:samples].length}.inject(:+)

    raise "something missing #{raw.length}, #{framesize}, #{num_frames}, #{num_samps}" unless raw.length == 16 + (4+framesize)*num_frames + 2*num_samps

    dump_wlg_frames(frames, framesize)

    frames
end


def dump_wlg_frames(frames, framesize)
    for idx in 0..(frames.length-1) do
        idx_str = sprintf("%03d", idx)
        puts
        puts "====== FRAME #{idx_str} ======="

        dump_wlg_header(frames[idx][:header])
        dump_wlg_trailer(frames[idx][:trailer], framesize)
#        dump_wlg_all_samples(frames[idx][:samples])
    end
end

def dump_wlg_header(vals)
    puts "--- HEADER : #{vals.first}" if vals
end

def dump_wlg_trailer(vals, framesize)
    puts "--- TRAILER ---"
    puts "float? = #{vals[0]}"
    puts "float? = #{vals[1]}"
    puts "zero   = #{vals[2]}"
    puts "mod    = #{vals[4]}"
    puts "firm   = #{vals[6]}"
    puts "ser    = #{vals[8]}"
    puts "t1     = #{vals[10]}"
    puts "t2     = #{vals[12]}"

    if framesize == 192
        puts "user   = #{vals[14]}"
        idx = 15
    else
        idx = 13
    end

    puts "????   = #{vals[idx]}"
    puts "instTZ = #{vals[idx+1]}"
    puts "start  = #{vals[idx+2]} (#{abstime2local(vals[idx+2])})"
    puts "serv t = #{vals[idx+3]} (#{abstime2local(vals[idx+3])})"
    puts "metr t = #{vals[idx+4]} (#{abstime2local(vals[idx+4])})"
    puts "float? = #{vals[idx+5]}"
    puts "zero   = #{vals[idx+6]}"
end


def dump_wlg_all_samples(vals)
    puts "--- SAMPLES ---"

    db_vals = vals.map {|x| (124.7847 + x / 20.0).round(4)}

    for idx in 0..(db_vals.length/3-1) do
        puts "#{db_vals[3*idx]}, #{db_vals[3*idx+1]}, #{db_vals[3*idx+2]}"  
    end
end
