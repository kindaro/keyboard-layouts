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
    - See `man xkeyboard-config`.
    - A variety of crufty tools. Wayland makes use of the `xkbcommon` library, but the tools
      mostly won't work.
    - Files live in `/usr/share/X11/xkb`, one file per language. Inside a file, a bunch of
      _"variants"_ will be found that are actually individual layouts. A certain style of
      reference is used, so, for example, `fr(bepo)` would point to the file
      `/usr/share/X11/xkb/symbols/fr` and the section `bepo` within it.

## The way forward.

A Wayland compositor would normally be able to consult an `xkb` keymap, and even allow for
switching between several, automatically locating the keymaps corresponding to (layout, variant)
pairs in the file system. To simplify and modularize things, I will not touch those files, but
rather create our own monolithic file with all the keymaps I would need.

I am going to use `klfc`.

1. Extract keymaps from `xkb`'s database to `klfc`'s format.
1. Edit them as desired.
1. Rename them to their own namespace.
1. The arrow `xkbcomp . setxkbmap` creates a _"compiled layout"_ that I will be able to load in
   Sway with `input ... xkb_file ...`, as described in `man sway-input`. Then I could bind
   several independent layouts to a range of F keys, as usual.

### Project workflow.

I must establish a directory structure of some sort, and put it under version control. Some parts
should be auto-generated, so evidently I must have scripts.

The workflow is thus:

1. Extract keymaps. Artifact: `klfc` `json` files.
2. Hack on them. Evidently `json` is my source code.
3. Compile with `xkbcomp . setxkbmap`, as noted previously.
   - I should put in place a compiler script.

## Tips and tricks.

### Create a PDF picture of the keyboard layout:

    setxkbmap -model [model] -print [layout] [variant] > setxkbmap.out
    xkbcomp -xkm setxkbmap.out
    xkbprint -label symbols setxkbmap.xkm xkbprint.ps
    ps2pdf xkbprint.{ps,pdf}

For example, a model of the keyboard used on HP laptops may be called `hpmini110`. A list may be
found in `/usr/share/X11/xkb/rules/evdev`.

Also, you can use `-I` switch to `setxkbmap` to _"include"_ additional directories with custom
layout files.
