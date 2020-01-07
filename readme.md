# How to create a custom keymap.
_(For use on Wayland.)_

## A variety of approaches.

There are 2 chief kinds of keymaps on Linux.

* `kbd`.
    - Works in _"virtual terminal"_.
    - See `man keymaps`.
    - Tools: `loadkeys`, `dumpkeys`.
    - Files live in `/usr/share/kbd/keymaps`, one file per layout.
* `xkb`.
    - Created for X Windows, but is also used in Wayland.
    - See `xkeyboard-config`.
    - A variety of crufty tools. Wayland makes use of the `xkbcommon` library, but the tools
      mostly won't work.
    - Files live in `/usr/share/X11/xkb`, one file per language. Inside a file, a bunch of
      _"variants"_ will be found that are actually individual layouts. A certain style of
      reference is used, so, for example, `fr(bepo)` would point to the file
      `/usr/share/X11/xkb/symbols/fr` and the section `bepo` within it.

## The way forward.

A Wayland compositor would normally be able to consult an `xkb` keymap, and even allow for
switching between several, automatically locating the keymaps corresponding to (layout, variant)
pairs in the file system. To simplify and modularize things, we will not touch those files, but
rather create our own monolithic file with all the keymaps we would need.

## Tips and tricks.

### Create a PDF picture of the keyboard layout:

    setxkbmap -print [layout] [variant] > setxkbmap.out
    xkbcomp -xkm setxkbmap.out
    xkbprint -label symbols setxkbmap.xkm xkbprint.ps
    ps2pdf xkbprint.{ps,pdf}
