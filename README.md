# Clojure and the Immutable Emulator

*Clones* is an
[NES](https://en.wikipedia.org/wiki/Nintendo_Entertainment_System) emulator for
the gentlman (or woman) of distinguished immutable character. A core design
goal is to build an entirely functional emulator.

Is this madness? **Yes**.

But how many cools would it be to be able to rewind and fast forward a game
through states? Eleventy.

### Why would you do such a thing?

> Mallory is famously quoted as having replied to the question **"Why do you
> want to climb Mount Everest?"** with the retort **"Because it's there"**
>
> [- quoteth the 'pedes](https://en.wikipedia.org/wiki/George_Mallory)

### So... you want to join the immutability cult?

*[Graphics and GUI are coming soon]*

### How do I test this monstrosity?

The specs test *clones'* internal API, state and functioning:

    $ lein spec

There are also [test ROMs](http://wiki.nesdev.com/w/index.php/Emulator_tests)
that exercise the entire machine.

The nestest binary can be run and verified against the known-good
[Nintendulator](http://www.qmtpro.com/~nes/nintendulator/) debug log:

    $ lein with-profile nestest run

[Blargg's](http://blargg.8bitalley.com/nes-tests/) CPU tests can be run
headlessly and report results:

    $ lein with-profile blargg run [rom file...]

