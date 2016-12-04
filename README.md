# importmagic.el [![Build Status](https://travis-ci.org/anachronic/importmagic.el.svg?branch=master)](https://travis-ci.org/anachronic/importmagic.el)

`importmagic.el` is an Emacs package inspired on
@alecthomas's [importmagic](https://github.com/alecthomas/importmagic)
library. It resolves unimported symbols in your Python buffers. This
is what it looks like on my Emacs:

![Example of Import Magic at work](importmagic.gif)

## Usage
The default behavior sets only one key binding: `C-c i`. It solves
imports for the symbol at point, which works *great* when using a cool
syntax checker like [Flycheck](http://www.flycheck.org/), because you
can have visual aid for symbols that you need to import.

By default, `importmagic.el` will recursively index every symbol from
the current buffer's directory, which means you should get fairly
accurate suggestions for imports you might need.

### Use it
Somewhere in your `init.el` or `.emacs` put these lines:

``` emacs-lisp
(require 'importmagic)
(add-hook 'python-mode-hook 'importmagic-mode)
```

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

### Annoyances

Every package has its own annoyances, and this one is no
exception. I'll try to describe here how to get rid of annoyances this
package may produce.

#### Mode line

Yes, all our mode lines are always cluttered, `importmagic.el`'s mode
lighter is `import`. If you don't like its name or feel like it takes
up too much space, you can
use [this](https://github.com/myrjola/diminish.el) package to get rid
of it. Put this line somewhere in your init file:

``` emacs-lisp
(diminish 'importmagic-mode)
```

That should get rid of the `import` in your mode line. If you want to
change the name in the mode line, just add a second argument as a
string with the name you want.

#### Buffers

`importmagic.el` uses an [EPC](https://github.com/kiwanami/emacs-epc)
server to query for symbols. While it's very convenient for the
developer, it's not so good for the user, because it generates one
buffer with the EPC connection for every Python buffer you open. While
that's ok (at least for me), It can be troublesome for some people.

`Helm` users can get rid of these buffers (as in not see them)
evaluating the following expression:

``` emacs-lisp
(add-to-list 'helm-boring-buffer-regexp-list "\\*epc con")
```

Likewise, `ivy` users can get rid of it with the following:

``` emacs-lisp
(add-to-list 'ivy-ignore-buffers "\\*epc con")
```

For `ido` users, no idea, Sorry!

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
