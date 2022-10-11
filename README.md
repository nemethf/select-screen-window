# Incremental search for window selection of GNU Screen

This program implements an alternative window selection functionality
to [GNU Screen][screen].  It provides incremental search by relying on
the `completing-read` functions of [Emacs].

# Installation

You can enable `ssw` by creating a shortcut in your `~/.screenrc`.
For example:

```
    bind b screen $HOME/src/ssw
```

`ssw` starts Emacs as a sub-process, but it should start reasonably
fast because it does not read the user's init file.  If that's not
fast enough and there's a running Emacs daemon, which has already
loaded ssw.el, then this might work as well:

```
    bind b screen $HOME/src/ssw-emacsclient
```

# Demo

  ![demo](https://raw.githubusercontent.com/wiki/nemethf/select-screen-window/demo.gif)

# License

[GPLv3+][gpl]

[gpl]: COPYING
[screen]: https://www.gnu.org/software/screen/
[Emacs]: https://www.gnu.org/software/emacs/
