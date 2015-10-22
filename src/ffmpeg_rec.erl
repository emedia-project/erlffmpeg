-module(ffmpeg_rec).

-export([set/4, to_rec/1]).

-include("../include/ffmpeg.hrl").

to_rec(Data) ->
  set_ffmpeg_movie_info(Data).

set_ffmpeg_movie_info(Data) ->
  set_rec(ffmpeg_movie_info, #ffmpeg_movie_info{}, Data).

set_ffmpeg_format_info(Data) ->
  set_rec(ffmpeg_format_info, #ffmpeg_format_info{}, Data).

set_ffmpeg_stream_info(Data) ->
  set_rec(ffmpeg_stream_info, #ffmpeg_stream_info{}, Data).

set_ffmpeg_stream_disposition_info(Data) ->
  set_rec(ffmpeg_stream_disposition_info, #ffmpeg_stream_disposition_info{}, Data).

set_rec(_, Record, []) -> Record;
set_rec(Rec, Record, [{Field, Data}|Rest]) ->
  set_rec(Rec, set(Rec, Record, binary_to_atom(Field, utf8), Data), Rest).

set(ffmpeg_movie_info, Record, streams, Value) ->
  Streams = lists:map(fun(E) ->
          set_ffmpeg_stream_info(E)
      end, Value),
  Record#ffmpeg_movie_info{streams = Streams};
set(ffmpeg_movie_info, Record, format, Value) ->
  Record#ffmpeg_movie_info{format = set_ffmpeg_format_info(Value)};
set(ffmpeg_stream_info, Record, disposition, Value) ->
  Record#ffmpeg_stream_info{disposition = set_ffmpeg_stream_disposition_info(Value)};
set(RecordName, Record, Field, Value) ->
  case field_num(RecordName, Field) of
    {ok, Length} -> setelement(Length, Record, Value);
    {error, _} -> error_logger:info_msg("Ignore field ~p.~p", [RecordName, Field]), Record
  end.

field_num(RecordName, Field) ->
  Fields = case RecordName of
    ffmpeg_stream_disposition_info -> record_info(fields, ffmpeg_stream_disposition_info);
    ffmpeg_stream_info -> record_info(fields, ffmpeg_stream_info);
    ffmpeg_format_info -> record_info(fields, ffmpeg_format_info);
    ffmpeg_movie_info -> record_info(fields, ffmpeg_movie_info)
  end,
  DifField = fun (FieldName) -> Field /= FieldName end,
  case length(lists:takewhile(DifField, Fields)) of
    Length when Length =:= length(Fields) ->
      {error, not_found};
    Length ->
      {ok, Length + 2}
  end.

