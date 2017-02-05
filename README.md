Mushu
=====

Mushu is a minimalist MPD client with a Text User Interface (TUI) focused on ease of use.
Its main feature is an incremental fuzzy finder for the music library.
The interface is largely inspired by [ncmpcpp](https://github.com/arybczak/ncmpcpp).

![Screenshot](screenshot.png?raw=true "Mushu screenshot")

Dependencies
--------------

libncurses5-dev

Install
-------

Mushu is available on stack.

  `stack install mushu`

If you don't have `stack` please install it following instructions [here](https://docs.haskellstack.org/en/stable/README/).

Why another MPD client?
-----------------------

I used [ncmpcpp](https://github.com/arybczak/ncmpcpp) for quite some time and realised that I was always doing the same thing with it: search for music -> add to playlist and play.

The aim is to keep the number of features to a minimum without being unusable.

I also wanted to develop a real world haskell project and this seemed like a good opportunity.

Contributing
------------

Contributions are very welcome. However please open an issue before developing anything too big so we can discuss it, I'd hate to have wasted your time if the Pull Request you made can't be merged.

