# erlFFMpeg

ffmpeg for Erlang

## APIs

`ffmpeg:set/2`
: set `ffmpeg` or `ffprobe` path.

`ffmpeg:info/1`
: Return informations on the given file.

`ffmpeg:transcode/2`
`ffmpeg:transcode/3`
: Trancode the given file

`ffmpeg:screenshot/2`
`ffmpeg:screenshot/3`
: Take a screenshot

## Options

### Main options

`{output_format, Format}`
`{input_format, Format}`
: Force input or output file format. The format is normally auto detected for input files and guessed from the file extension for output files, so this option is not needed in most cases.

`{yes, true}`
: Overwrite output files without asking.

`{encoder, [Stream, Codec]}` | `{encoder, Codec}`
: Select and encoder for one or more output streams.

`{decoder, [Stream, Codec]}` | `{decoder, Codec}`
: Select a decoder for one or more input streams.

`{duration, Duration}`
: Stop writing the output after its duration reaches duration. Duration may be a number in seconds, or in "hh:mm:ss[.xxx]" form.

`{limit_size, Limit}`
: Set the file size limit. Limit expressed in bytes.

`{input_position, Position}`
: Seeks in the input file to position. Position may be either in seconds or in "hh:mm:ss[.xxx]" form.

`{output_position, Position}`
: Decodes but discards input until the timestamps reach position. Position may be either in seconds or in "hh:mm:ss[.xxx]" form.

`{itoffset, Offset}`
: Set the input time offset in seconds. "[-]hh:mm:ss[.xxx]" syntax is also supported. The offset is added to the timestamps of the input files.

`{timestamp, Time}`
: Set the recording timestamp in the container.  The syntax for time is: `now|([(YYYY-MM-DD|YYYYMMDD)[T|t| ]]((HH:MM:SS[.m...])|(HHMMSS[.m...]))[Z|z])`

`{target, Type}`
: Specify target file type ("vcd", "svcd", "dvd", "dv", "dv50"). Type may be prefixed with "pal-", "ntsc-" or "film-" to use the corresponding standard. 

`{dframes, Number}`
: Set the number of data frames to record.

`{frames, [Stream, Framecount]}` | `{frames, Framecount}`
: Stop writing to the Stream after Framecount frames.

`{qscale, [Stream, Quality]}` | `{qscale, Quality}`
: Use fixed quality scale (VBR).

`{filter, [Stream, Filtergraph]}` | `{filter, Filtergraph}`
: Create the filtergraph specified by Siltergraph and use it to filter the Stream. (see ffmpeg-filters manual for more information about the filtergraph syntax.)

`{filter_script, [Stream, Filename]}` | `{filter_script, Filename}`
: This option is similar to filter, the only difference is that its argument is the name of the file from which a filtergraph description is to be read.

`{pre, [Stream, PresetName]}` | `{pre, PresetName}`
: Specify the preset for matching stream(s).

### Video Options

`{vframes, Number}`
: Set the number of video frames to record. This is an alias for "-frames:v".

`{input_frame_rate, [Stream, Fps]}` | `{input_frame_rate, Fps}`
`{output_frame_rate, [Stream, Fps]}` | `{output_frame_rate, Fps}`
: Set frame rate (Hz value, fraction or abbreviation).

`{input_frame_size, [Stream, Size]}` | `{input_frame_size, Size}`
`{output_frame_size, [Stream, Size]}` | `{output_frame_size, Size}`
: Set frame size.

`{aspect, [Stream, Aspect]}` | `{aspect, Aspect}`
: Set the video display aspect ratio specified by aspect. Aspect can be a floating point number string, or a string of the form num:den, where num and den are the numerator and denominator of the aspect ratio. For example "4:3", "16:9", "1.3333", and "1.7777" are valid argument values.

`{no_video_recording, true}`
: Disable video recording.

`{vcodev, Codec}`
: Set the video codec.

`{pass, [Stream, N]}` | `{pass, N}`
: Select the pass number (1 or 2).

`{vlang, Code}`
: Set the ISO 639 language code (3 letters) of the current video stream.

`{video_filtergraph, Filter}`
: Create the filtergraph specified by filtergraph and use it to filter the stream.

### Advanced Video Options

`{input_pixel_format, [Stream, Format]}` | `{input_pixel_format, Format}`
`{output_pixel_format, [Stream, Format]}` | `{output_pixel_format, Format}`
: Set pixel format. 

`{input_sws_flags, Flags}`
`{output_sws_flags, Flags}`
: Set SwScaler flags.

`{rc_override, [Stream, Override]}` | `{rc_override, Override}`
: Rate control override for specific intervals

`{top, [Stream, N]}` | `{top, N}`
: top=1/bottom=0/auto=-1 field first

`‘force_key_frames, [Stream, KeyFrame]}` | `{force_key_frames, KeyFrame}`
: Force key frames at the specified timestamps, more precisely at the first frames after each specified time.

`{copyinkf, [Stream, true]}` | `{copyinkf, true}`
: When doing stream copy, copy also non-key frames found at the beginning.

### Audio Options

`{aframes, N}`
: Set the number of audio frames to record. 

`{input_audio_frequency, [Stream, Frequency]}` | `{input_audio_frequency, Frequency}`
`{output_audio_frequency, [Stream, Frequency]}` | `{output_audio_frequency, Frequency}`
: Set the audio sampling frequency.

`{audio_quality, Quality}`
: Set the audio quality (codec-specific, VBR).

`{input_audio_channels, [Stream, N]}` | `{input_audio_channels, N}`
`{output_audio_channels, [Stream, N]}` | `{output_audio_channels, N}`
: Set the number of audio channels.

`{no_audio_recording, true}`
: Disable audio recording.

`{input_acodec, Codec}`
`{output_acodec, Codec}`
Set the audio codec.

`{sample_fmt, [Stream, AudioSampleFormat]}` | `{sample_fmt, AudioSampleFormat}`
Set the audio sample format.

`{audio_filtergraph, Filter}`
Create the filtergraph specified by filtergraph and use it to filter the stream.

### Advanced Audio options:

`{guess_layout_max, Channels}`
: If some input channel layout is not known, try to guess only if it corresponds to at most the specified number of channels. 

### Subtitle options:

`{input_scodec, Codec}`
`{output_scodec, Codec}`
: Set the subtitle codec.

`{no_subtitle_recording, true}`
: Disable subtitle recording.

### Advanced Subtitle options:

`{fix_sub_duration, true}`
: Fix subtitles durations.

`{canvas_size, Size}`
: Set the size of the canvas used to render subtitles.

### Advanced options

`{map, Map}`
: Designate one or more input streams as a source for the output file.

`{map_channel, MapChannel}`
: Map an audio channel from a given input to an output.

`{map_chapters, InputFileIndex}`
: Copy chapters from input file with index input_file_index to the next output file.

`{vsync, Parameter}`
: Video sync method.

`{async, SamplePerSec}`
: Audio sync method.

`{copytb, Mode}`
: Specify how to set the encoder timebase when stream copying.

`{shortest, true}`
: Finish encoding when the shortest input stream ends.

`{dts_delta_threshold, true}`
: Timestamp discontinuity delta threshold.

`{muxdelay, Second}`
: Set the maximum demux-decode delay.

`{muxpreload, Second}`
: Set the initial demux-decode delay.

`{streamid, OSI_NV}`
: Assign a new stream-id value to an output stream.

`{bitstream_filters, [Stream, Filters]}` | `{bitstream_filters, Filters}`
: Set bitstream filters for matching streams.

`{timecode, Timecode}`
: Specify Timecode for writing.

`{filter_complex, Filtergraph}`
: Define a complex filtergraph, i.e. one with arbitrary number of inputs and/or outputs. 

`{filter_complex_script, Filename}`
: This option is similar to filter_complex, the only difference is that its argument is the name of the file from which a complex filtergraph description is to be read.

`{accurate_seek, true}`
: This option enables or disables accurate seeking in input files with the input_position option.

## Licence

erlFFMpeg is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014 Grégoire Lejeune <<gregoire.lejeune@free.fr>>

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

