-module(ffmpeg).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/ffmpeg.hrl").
-define(DEC(X), $0 + X div 10, $0 + X rem 10).
-define(FFPROBE_OPTIONS, "-v quiet -print_format json -show_format -show_streams").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([
  set/2,
  infos/1,
  transcode/2,
  transcode/3,
  transcoding/0,
  screenshot/2,
  screenshot/3,
  to_html5_webm/2,
  to_html5_mp4/2,
  to_html5_ogg/2
  ]).
-export([start_trancode/4, stop_transcode/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set(ffmpeg, Path) ->
  gen_server:call(?SERVER, {ffmpeg, Path});
set(ffprobe, Path) ->
  gen_server:call(?SERVER, {ffprobe, Path}).

infos(Movie) ->
  gen_server:call(?SERVER, {infos, Movie}).

transcode(Movie, Output) ->
  transcode(Movie, Output, []).
transcode(Movie, Output, Options) ->
  gen_server:cast(?SERVER, {transcode, Movie, Output, Options}).

transcoding() ->
  gen_server:call(?SERVER, {transcoding}).

screenshot(Movie, Output) ->
  screenshot(Movie, Output, []).
screenshot(Movie, Output, Options) ->
  gen_server:call(?SERVER, {screenshot, Movie, Output, Options}).

to_html5_webm(Input, Output) ->
  transcode(Input, Output, [{output_format, "webm"}, {vcodec, "libvpx"}, {output_acodec, "libvorbis"}, {output_frame_size, "640x360"}]).

to_html5_mp4(Input, Output) ->
  transcode(Input, Output, [{output_format, "mp4"}, {vcodec, "libx264"}, {output_acodec, "libfaac"}, {output_frame_size, "640x360"}]).

to_html5_ogg(Input, Output) ->
  transcode(Input, Output, [{vcodec, "libtheora"}, {output_acodec, "libvorbis"}, {output_frame_size, "640x360"}]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, #ffmpeg{}}.

handle_call({ffmpeg, Path}, _From, State) ->
    {reply, ok, State#ffmpeg{ffmpeg_path = Path}};
handle_call({ffprobe, Path}, _From, State) ->
    {reply, ok, State#ffmpeg{ffprobe_path = Path}};

handle_call({infos, Movie}, _From, #ffmpeg{ffprobe_path = FFProbe} = State) ->
  {reply, get_movie_info(Movie, FFProbe), State};

handle_call({transcoding}, _From, #ffmpeg{transcoding = Transcoding} = State) ->
  {reply, Transcoding, State};

handle_call({stop_transcode}, _From, State) ->
  {reply, ok, State#ffmpeg{transcoding = false}};

handle_call({screenshot, Movie, Output, Options}, _From, #ffmpeg{ffmpeg_path = FFMpeg, ffprobe_path = FFProbe} = State) ->
  #ffmpeg_movie_info{format = #ffmpeg_format_info{duration = Duration}} = get_movie_info(Movie, FFProbe),
  SS = trunc(list_to_float(binary_to_list(Duration))/2),
  HH = trunc(SS / 3600),
  SS1 = SS - (HH * 3600),
  MM = trunc(SS1 / 60),
  SS2 = SS1 - (MM * 60),
  InputPosition = [?DEC(HH), $:, ?DEC(MM), $:, ?DEC(SS2)],
  OptionsStr = gen_options(Movie, Output, Options, [{yes, true}, {duration, 1}, {output_format, "mjpeg"}], [{input_position, InputPosition}]),
  ScanCommand = FFMpeg ++ OptionsStr,
  Result = case ffmpeg_cmd:execute(ScanCommand) of
    {0, _} -> ok;
    {1, Stderr} -> 
      error_logger:error_msg("COMMAND : ~p~nERROR : ~p", [list_to_bitstring(ScanCommand), list_to_bitstring(Stderr)]),
      error
  end,
  {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({transcode, Movie, Output, Options}, #ffmpeg{ffmpeg_path = FFMpeg} = State) ->
  do_transcoding(FFMpeg, Movie, Output, Options),
  {noreply, State#ffmpeg{transcoding = true}};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_transcoding(FFMpeg, Movie, Output, Options) ->
  spawn_link(ffmpeg, start_trancode, [FFMpeg, Movie, Output, Options]).

get_movie_info(Movie, FFProbe) ->
  ScanCommand = case ucp:detect(Movie) of
    utf8 -> FFProbe ++ " " ++ ?FFPROBE_OPTIONS ++ " \"" ++ Movie ++ "\"";
    _ -> FFProbe ++ " " ++ ?FFPROBE_OPTIONS ++ " \"" ++ ucp:to_utf8(Movie) ++ "\""
  end,
  {_RCod, FileInfo} = ffmpeg_cmd:execute(ScanCommand), % TODO
  J = jsx:decode(list_to_binary(FileInfo)),
  ffmpeg_rec:to_rec(J).

%% -------------------------------------------------------------------
%% Public hidden fonctions
%% -------------------------------------------------------------------

%% @hidden
start_trancode(FFMpeg, Movie, Output, Options) ->
  OptionsStr = gen_options(Movie, Output, Options, [{yes, true}], []),
  ScanCommand = FFMpeg ++ OptionsStr,
  error_logger:info_msg("COMMAND : ~p", [list_to_bitstring(ScanCommand)]),
  _Result = case ffmpeg_cmd:execute(ScanCommand) of
    {0, _} -> ok;
    {1, Stderr} -> 
      error_logger:error_msg("COMMAND : ~p~nERROR : ~p", [list_to_bitstring(ScanCommand), list_to_bitstring(Stderr)]),
      error
  end,
  % TODO
  ffmpeg:stop_transcode().

%% @hidden
stop_transcode() ->
  gen_server:call(?SERVER, {stop_transcode}).

gen_options(Input, Output, Options, OverwriteOptions, MissingOptions) ->
  Options1 = kv_merge(Options, OverwriteOptions),
  Options2 = kv_merge(MissingOptions, Options1),
  gen_options(Input, Output, Options2).
gen_options(Input, Output, Options) ->
  [
    {input, InputOptions}, 
    {output, OutputOptions}, 
    {global, GlobalOptions}
  ] = ffmpeg_options:options(Options),
  GlobalOptions ++
  InputOptions ++
  input(Input) ++
  OutputOptions ++
  output(Output).

kv_merge(KV1, KV2) ->
  lists:foldl(fun({Key, _} = E, KV) ->
      case lists:keysearch(Key, 1, KV) of
        {value, _} -> lists:keyreplace(Key, 1, KV, E);
        false -> KV ++ [E]
      end
    end, KV1, KV2).

input(File) ->
  " -i " ++ encode_filename(File).

output(File) ->
  " " ++ encode_filename(File).

encode_filename(File) ->
  case ucp:detect(File) of
    utf8 -> "\"" ++ File ++ "\"";
    _ -> "\"" ++ ucp:to_utf8(File) ++ "\""
  end.

