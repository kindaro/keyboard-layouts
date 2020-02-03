# How to create a custom keymap.
_(For use on Wayland/Sway.)_


1. Install [`klfc`][klfc].
1. Get some layouts: 

        klfc --from-xkb '/usr/share/X11/xkb/symbols/fr(bepo)' > src/fr-bepo.json

1. Rename.

    ```diff
    --- src/fr-bepo.json	2020-01-12 08:49:33.083220324 +0300
    +++ src/my-fr-bepo.json	2020-01-12 08:48:22.913221278 +0300
    @@ -1,6 +1,6 @@
     {
       "fullName": "French (Bepo, ergonomic, Dvorak way)",
    -  "name": "fr-bepo",
    +  "name": "my-fr-bepo",
       "singletonKeys": [
         [ "Alt_R", "AltGr" ]
       ],
    ```

1. Edit as desired.
1. Run `./build.hs` to compile your new layout. The new keymaps will be located under `./xkm`.
1. Find a way to tell your compositor about the new keymaps. For example, with Sway, add the
   following to `~/.config/sway/config`:

       bindsym $mod+f1 input * xkb_file /home/me/keymap/xkm/me-fr-bepo.xkb

    Now, pressing `mod + F1` will switch you to the Bépo layout _(without a way back)_.

1. Enjoy!


## Theory of keyboard layouts.

Kernel knows the keyboard by key codes, but we humans like to have an action associated with a
key, such as a symbol appearing at cursor, consistently across programs. So, the desktop
environment must associate meaningful events with arbitrary koy codes. A _«key map»_, or
_«keyboard layout»_, is this mapping.

### Wayland and the X Keyboard Extension format.

There is any number of formats used to describe keyboard layouts, but the most likely to be used
in Wayland, and the one we study here, is `xkb` — the X Keyboard Extension format. While made for
the X window system, it is also ordinarily understood by Wayland compositors, by the means of the
`xkbkommon` library, and moves towards universal adoption.

### A collection of layout data.

An `xkb` configuration is not small and not exhausted by a mere mapping of the keys. It may vary
along the dimensions of keyboard model, layout, variant and miscellaneous options, and runs to
some thousands of lines in a _nominally_ human readable format, possibly distributed across a few
files. Modifying an existing layout may be challenging, and creating one from scratch is quite
insurmountable. Thanks to the hard work of unknown heroes, a sizable collection of keymaps ships
with a usual Linux; it is called `xkeyboard-config` and the corresponding `man` page can be
consulted for a complete enumeration. The files would be located under `/usr/share/X11/xkb`, and
may be examined with a text viewer, though the structure is non-trivial: for example, all the
various layouts for a given language would be found within one file. A specific layout is referred
to as, say, `fr(bepo)`, pointing to the file `/usr/share/X11/xkb/symbols/fr` and the section
`bepo` within it.

## Editing a layout.

A Wayland compositor would normally be able to consult an `xkb` keymap, and even allow for
switching between several, automatically locating the keymaps corresponding to a layout-variant
pair in the file system. For the sake of simplicity, I will not touch any existent files, but
rather create my own copies, tweak them to my heart's content, then figure out a way to make the
compositor recognize the newly defined layouts.

[klfc]: https://github.com/39aldo39/klfc

* * *

I am going to use `klfc`.

1. Extract keymaps from `xkb`'s database to `klfc`'s format. Example:

       klfc --from-xkb '/usr/share/X11/xkb/symbols/fr(bepo)' > src/fr-bepo.json

1. Edit them as desired.
1. Rename them to their own namespace.
1. The arrow `xkbcomp . setxkbmap` creates a _"compiled layout"_ that I will be able to load in
   Sway with `input ... xkb_file ...`, as described in `man sway-input`. Then I could bind
   several independent layouts to a range of F keys, as usual.

### Project workflow.

I must establish a directory structure of some sort, and put it under version control. Some parts
should be auto-generated, so evidently I must have scripts.

The workflow is thus:

1. Extract keymaps. Artifact: `klfc` `json` files.1. Hack on them. Evidently `json` is my source code.
1. Compile with `xkbcomp . setxkbmap`, as noted previously.
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

TODO:
* Print a picture of a keymap.
* Set some model.
* Find or compose a basic Haskell layout.
    - Build a list of the most frequent 1, 2 & 3 letter long sequences on Hackage not made of
      letters. _(That would be operators.)_
* Rename output directory _(and adjust the docs)_.
