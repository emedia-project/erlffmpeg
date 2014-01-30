-define(FFPROBE_PATH, "ffprobe").
-define(FFMPEG_PATH, "ffmpeg").

-define(FFPROBE_OPTIONS, "-v quiet -print_format json -show_format -show_streams").

-record(ffmpeg, {
  ffmpeg_path = ?FFMPEG_PATH,
  ffprob_path = ?FFPROBE_PATH,
  transcoding = false
}).

-record(ffmpeg_stream_info, {
}).

-record(ffmpeg_movie_info, {
  filename,
  streams,
  format,
  long_format,
  start_time,
  duration,
  size,
  bit_rate
}).
