# ScriptMidi

![Beethoven](http://i.imgur.com/gWi4GOj.png)

Run scripts from your Midi device.


## Video

<https://www.youtube.com/watch?v=g356ANC2zp4&list=UUiGSEVP_W5pJXMymVzG09zA>


## Why?

I use this for all kinds of reasons.

* A convenient way to trigger system functions during a music performance.
* A test-case laucher during software developement so that you don't need to change windows to trigger a curl script, or something along those lines.
* A ghetto sampler - triggering sounds with `afplay` or "vocals" with the `say` command.

More to the point, if you already have a Midi device sitting on your desk unused, why not use it for something?


## How?

You can start ScriptMidi with the `scriptmidi` command.

This will expect

1) A number indicating which midi-device you wish to use
2) Pairs of input, indicating which note, and what command
3) Lines starting with a # are ignored

The note can be indicated with either the note-number on STDIN, or by playing
the note itself on the MIDI device.

All input can optionally be written to a log-file simply by redirecting STDOUT.
This allows you to play back a session in case you wish to reuse the bindings
you created.

In order to replay a session you just redirect STDIN into `scriptmidi`.

If you wish to be able to play-back a session, but also keep using the device
after the commands are read, you will need to enlist the help of your shell.

For example:

		(cat history.txt ; cat) | scriptmidi


## Source

The source is available [on github](https://github.com/sordina/ScriptMidi) and can be built with [Cabal.](https://www.haskell.org/cabal/)


## Binaries

Here are some pre-build binaries available for various systems below:

<http://sordina.binaries.s3.amazonaws.com/scriptmidi-0.1.0.0-MacOSX-10.9.5-13F34.zip>
