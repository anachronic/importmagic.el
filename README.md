# importmagic.el [![Build Status](https://travis-ci.org/anachronic/importmagic.el.svg?branch=master)](https://travis-ci.org/anachronic/importmagic.el)

`importmagic.el` is an Emacs package inspired on
@alecthomas's [importmagic](https://github.com/alecthomas/importmagic)
library. It resolves unimported symbols in your Python buffers. A
Sublime Text example can be seen here:

![Example of Import Magic at work](importmagic.gif)

## Installation

This package is not on MELPA yet. You'll have to install it manually.

### Get Python dependencies
You'll need two Python packages for this to work:

``` shell
$ pip install importmagic epc
```

### Get the files
Download both `importmagic.el` and `importmagicserver.py`. Place them
on a load-path of your emacs directory. For instance:
`~/.emacs.d/site-lisp`

### Place them in a path that can be loaded
If you haven't already, tell emacs you want to load files from that
directory:
``` emacs-lisp
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "site-lisp/")))
```
The `site-lisp` there
should match the same string in the **Get the files** section.

### Use it
Somewhere in your `init.el` or `.emacs` put these lines:

``` emacs-lisp
(require 'importmagic)
(add-hook 'python-mode-hook 'importmagic-mode)
```

## Usage
The default behavior sets only one key binding: `C-c i`. It solves
imports for the symbol at point, which works *great* when using a cool
syntax checker like [Flycheck](http://www.flycheck.org/), because you
can have visual aid for symbols that you need to import.

By default, `importmagic.el` will recursively index every symbol from
the current buffer's directory, which means you should get fairly
accurate suggestions for imports you might need.

### Key bindings
Every key binding is under the `importmagic-mode-map`. If you don't
like the `C-c i` keybinding or want to add extra keys to your
configuration, just set them like so:

``` emacs-lisp
(define-key importmagic-mode-map (kbd "C-c C-f") 'importmagic-fix-symbol-at-point)
```

As you can imagine, the above code sets the key binding `C-c C-f` to
fix imports for the symbol at point. importmagic.el provides more
functions than just these, read on if you're interested

## Provided functions

A list of every provided function, in case you either want to bind
them to a key or just want to `M-x` for it.

### `importmagic-mode`
Turn on/off importmagic-mode in the current buffer. As usual, a
positive argument sets it on, negative argument sets it off. With just
`M-x`, it will toggle the mode.

### `importmagic-fix-imports`
Query for **every** unimported symbol in the current buffer. This can
be useful if you've written a lot of code and didn't bother to import
anything.

### `importmagic-fix-symbol-at-point`
Query for imports for the symbol at point. Note that this will query
the database even if the symbol at point is already
imported. [Flycheck](http://www.flycheck.org/) can be helpful for this
one.

### `importmagic-fix-symbol`
Prompts for a symbol to import. This function is the base for the two
above since it doesn't make any assumption on what you want to
import. It will find suitable candidates for the given symbol. Note
that it can import symbols that you're not currently using.

### `importmagic-update-index`
Updates index for the current file. This can be useful if something
changed in the current directory outside the current buffer and you
need to import symbols from those modified files.

## Contributing
Any kind of contribution is absolutely welcome.

If you haven't already,
install [Cask](https://github.com/cask/cask). Run the tests with:

``` shell
$ cask exec ert-runner
```

Note that you'll
need [ert-runner](https://github.com/rejeep/ert-runner.el) for that.

Send me pull-requests!
