-module(ffmpeg).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("../include/ffmpeg.hrl").

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
  screenshot/3
  ]).
-export([start_trancode/1, stop_transcode/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, #ffmpeg{}}.

handle_call({ffmpeg, Path}, _From, State) ->
    {reply, ok, State#ffmpeg{ffmpeg_path = Path}};
handle_call({ffprobe, Path}, _From, State) ->
    {reply, ok, State#ffmpeg{ffprob_path = Path}};

handle_call({infos, Movie}, _From, #ffmpeg{ffprob_path = FFProbe} = State) ->
  ScanCommand = case ucp:detect(Movie) of
    utf8 -> FFProbe ++ " " ++ ?FFPROBE_OPTIONS ++ " \"" ++ Movie ++ "\"";
    _ -> FFProbe ++ " " ++ ?FFPROBE_OPTIONS ++ " \"" ++ ucp:to_utf8(Movie) ++ "\""
  end,
  {_RCod, FileInfo} = ffmpeg_cmd:execute(ScanCommand), % TODO
  _J = jsx:decode(FileInfo),
  {reply, ok, State};

handle_call({transcoding}, _From, #ffmpeg{transcoding = Transcoding} = State) ->
  {reply, Transcoding, State};

handle_call({stop_transcode}, _From, State) ->
  {reply, ok, State#ffmpeg{transcoding = false}};

handle_call({screenshot, Movie, Output, _Options}, _From, #ffmpeg{ffmpeg_path = FFMpeg} = State) ->
  ScanCommand = case ucp:detect(Movie) of
    utf8 -> FFMpeg ++ " -ss 00:10:00 -y -i " ++ " \"" ++ Movie ++ "\" -t 1 -f mjpeg";
    _ -> FFMpeg ++ " -ss 00:10:00 -y -i " ++ " \"" ++ ucp:to_utf8(Movie) ++ "\" -t 1 -f mjpeg "
  end,
  ScanCommand1 = case ucp:detect(Output) of
    utf8 -> ScanCommand ++ "\"" ++ Output ++ "\"";
    _ -> ScanCommand ++ "\"" ++ ucp:to_utf8(Output) ++ "\""
  end,
  % TODO _Options
  Result = case ffmpeg_cmd:execute(ScanCommand1) of
    {0, _} -> ok;
    {1, Output} -> 
      lager:info("COMMAND : ~p~nERROR : ~p", [list_to_bitstring(ScanCommand1), list_to_bitstring(Output)]),
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

%% -------------------------------------------------------------------
%% Public hidden fonctions
%% -------------------------------------------------------------------

start_trancode([_FFMpeg, _Movie, _Output, _Options]) ->
  % TODO
  ffmpeg:stop_transcode().

stop_transcode() ->
  gen_server:call(?SERVER, {stop_transcode}).

