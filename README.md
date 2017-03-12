Mushu [![Build Status](https://travis-ci.org/elaye/mushu.svg?branch=master)](https://travis-ci.org/elaye/mushu)
=====

Mushu is a minimalist MPD client with a Text User Interface (TUI) focused on ease of use.
Its main feature is an incremental fuzzy finder for the music library.
The interface is largely inspired by [ncmpcpp](https://github.com/arybczak/ncmpcpp).

![Screenshot](screenshot.png?raw=true "Mushu screenshot")

This client was made to fit my needs so it may not fit yours.
A lot of MPD features are not implemented.

Dependencies
--------------

`libncurses5-dev`

Install
-------

Mushu is available on stack.

  `stack install mushu`

If you don't have `stack` please install it following instructions [here](https://docs.haskellstack.org/en/stable/README/).

Why another MPD client?
-----------------------

I used [ncmpcpp](https://github.com/arybczak/ncmpcpp) for quite some time and realised that I was always doing the same thing with it: search for music -> add to playlist and play.
I was not entirely satisfied with the search in ncmpcpp so I decided to make my own.
Also, ncmpcpp as way too many features that I don't use.

The aim is to keep the number of features to a minimum without being unusable.

I also wanted to develop a real world haskell project and this seemed like a good opportunity.

TODO
----

 - [ ] Add config file (mainly to be able to choose a different server than `localhost:6600` for MPD)
 - [ ] Add MPD error handling and show error in a notification widget
 - [ ] Add MPD playback modes handling
 - UI
   - [ ] Improve help screen
   - [ ] Add top / bottom list keybindings (`G` and `g`)
   - [ ] Add delete from playlist keybinding (`d` or `x`?)
 - [ ] Fix all the bugs and refactor!

Contributing
------------

Contributions are very welcome. However please open an issue before developing anything too big so we can discuss it, I'd hate to have wasted your time if the Pull Request you made can't be merged.
Please read [CONTRIBUTING.md](CONTRIBUTING.md) before contributing.
If you want an overview of the app, check out [DEVELOPER.md](DEVELOPER.md).

