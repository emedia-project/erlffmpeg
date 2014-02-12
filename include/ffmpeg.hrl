-define(FFPROBE_PATH, "ffprobe").
-define(FFMPEG_PATH, "ffmpeg").

-define(FFPROBE_OPTIONS, "-v quiet -print_format json -show_format -show_streams").

-record(ffmpeg, {
  ffmpeg_path  = ?FFMPEG_PATH,
  ffprobe_path = ?FFPROBE_PATH,
  transcoding  = false
}).

-record(ffmpeg_stream_disposition_info, {
  default          :: pos_integer(),
  dub              :: pos_integer(),
  original         :: pos_integer(),
  comment          :: pos_integer(),
  lyrics           :: pos_integer(),
  karaoke          :: pos_integer(),
  forced           :: pos_integer(),
  hearing_impaired :: pos_integer(),
  visual_impaired  :: pos_integer(),
  clean_effects    :: pos_integer(),
  attached_pic     :: pos_integer()
}).

-record(ffmpeg_stream_info, {
  index                :: pos_integer(),
  codec_name           :: binary(),
  codec_long_name      :: binary(),
  codec_type           :: binary(),
  codec_time_base      :: binary(),
  codec_tag_string     :: binary(),
  codec_tag            :: binary(),
  profile              :: binary(),
  width                :: pos_integer(),
  height               :: pos_integer(),
  has_b_frames         :: pos_integer(),
  sample_aspect_ratio  :: binary(),
  display_aspect_ratio :: binary(),
  pix_fmt              :: binary(),
  level                :: pos_integer(),
  sample_fmt           :: binary(),
  sample_rate          :: binary(),
  channels             :: pos_integer(),
  channel_layout       :: binary(),
  bits_per_sample      :: pos_integer(),
  r_frame_rate         :: binary(),
  avg_frame_rate       :: binary(),
  time_base            :: binary(),
  start_pts            :: pos_integer(),
  start_time           :: binary(),
  duration_ts          :: binary(),
  duration             :: binary(),
  bit_rate             :: binary(),
  nb_frames            :: binary(),
  disposition          :: #ffmpeg_stream_disposition_info{},
  tags                 :: [any()]
}).

-record(ffmpeg_format_info, {
  filename         :: binary(),
  nb_streams       :: pos_integer(),
  nb_programs      :: pos_integer(),
  format_name      :: binary(),
  format_long_name :: binary(),
  start_time       :: binary(),
  duration         :: binary(),
  size             :: binary(),
  bit_rate         :: binary(),
  probe_score      :: pos_integer(),
  tags             :: [any()]
}).

-record(ffmpeg_movie_info, {
  streams :: [#ffmpeg_stream_info{}],
  format  :: #ffmpeg_format_info{}
}).

