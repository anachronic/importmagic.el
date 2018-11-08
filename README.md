# importmagic.el [![Build Status](https://travis-ci.org/anachronic/importmagic.el.svg?branch=master)](https://travis-ci.org/anachronic/importmagic.el) [![MELPA](https://melpa.org/packages/importmagic-badge.svg)](https://melpa.org/#/importmagic)


`importmagic.el` is an Emacs package inspired on
@alecthomas's [importmagic](https://github.com/alecthomas/importmagic)
library and PyCharm's ability to suggest imports for unresolved
symbols.

![Example of Import Magic at work](importmagic.gif)

## WARNING!

**PLEASE** use wrappers for virtual environments as `importmagic.el`
does not care at all about the environment on which it is
run. `importmagic.el` is known to work with
[virtualenvwrapper.el](https://github.com/porterjamesj/virtualenvwrapper.el)
and [pyvenv](https://github.com/jorgenschaefer/pyvenv).

If you still don't want to do that, check out the Usage section.

## Installation

To install this package, MELPA is the way to go, but you also need
some extra dependencies.

### Python dependencies

This package relies heavily on importmagic and EPC. You can get them
from pip. These packages should be installed in the same environment
you're working on.

``` shell
$ pip install importmagic epc
```

### Installing the Emacs package

It is recommended that you install this package
from [MELPA](https://melpa.org/). There's still a way if you don't use
it though.

#### From MELPA

A simple way would be to just:

``` emacs-lisp
M-x package-install RET importmagic RET
```

You can also
try [use-package](https://github.com/jwiegley/use-package):

``` emacs-lisp
(use-package importmagic
    :ensure t
    :config
    (add-hook 'python-mode-hook 'importmagic-mode))
```

The above example is the minimal configuration in order to get started
with importmagic.

Whichever way you choose remember to add the hook to python mode:

``` emacs-lisp
(add-hook 'python-mode-hook 'importmagic-mode)
```

#### Without MELPA

Download both `importmagic.el` and `importmagicserver.py`. Place them
on a load-path of your emacs directory. For instance:
`~/.emacs.d/site-lisp`

If you haven't already, tell emacs you want to load files from that
directory:

``` emacs-lisp
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "site-lisp/")))
```

Of course, you can choose to change the name of the `site-lisp`
portion of the code. Don't forget to add the mode to Python buffers,
put this line anywhere in your `.emacs` or `init.el`

``` emacs-lisp
(add-hook 'python-mode-hook 'importmagic-mode)
```

## Usage
The default behavior sets only one key binding: <kbd>C-c C-l</kbd>. It
solves imports for every unresolved symbol in the buffer, prompting
for one import at a time. If there are no imports found for a given
symbol, importmagic will let you know at the end of the process.

By default, `importmagic.el` will recursively index every symbol from
the current buffer's directory, which means you should get fairly
accurate suggestions for imports you might need.


### Key bindings
Every key binding is under the `importmagic-mode-map`. If you don't
like the `C-c C-l` keybinding or want to add extra keys to your
configuration, just set them like so:

``` emacs-lisp
(define-key importmagic-mode-map (kbd "C-c C-f") 'importmagic-fix-symbol-at-point)
```

Note that the example above will override a defined key binding in the
`python-mode-map`. You can do that as long as you feel the need to (as
I did). This package is not really intended to interfer with the
default bindings, though.

### Imports style

Importmagic
supports
[configuration](https://github.com/alecthomas/importmagic#configuration) on
the styles it has. As of July 2, 2017, so does `importmagic.el`.

When an import statement gets too long, you have the choice to group
it using either parentheses or backslashes. Both of these options
(maximum length of line and import group styles) can be set with
the variable `importmagic-configuration-style-alist`, which is an
alist (duh) of your preferences.

The default value is

``` emacs-lisp
'((multiline . parentheses)
  (max_columns . 79))
```

### Customizing

`M-x customize-group RET importmagic RET`.

### Python Interpreter

As we talked about in the first section of this document,
`importmagic.el` won't care about the environment an will thus use the
default system python interpreter. If you do not want to do this and
want to use a different python interpreter you can set
`importmagic-python-interpreter` like so:

``` emacs-lisp
(setq importmagic-python-interpreter "/path/to/my/virtualenv/bin/python")
```

### Annoyances

Every package has its own annoyances, and this one is no
exception. I'll try to describe here how to get rid of annoyances this
package may produce.

#### Non-error messages

`importmagic.el` can be very verbose when you develop Python for
several hours. In fact, it
was [suggested](https://github.com/anachronic/importmagic.el/issues/5)
that I gave the possibility to supress these messages. You can do so
by setting the variable `importmagic-be-quiet` to `t` like so:

``` emacs-lisp
(setq importmagic-be-quiet t)
```

This, however, will **not** supress error messages.

#### Mode line

If importmagic gets your mode line too cluttered,
try [diminish](https://github.com/myrjola/diminish.el). Something like
this would be fine:

``` emacs-lisp
(diminish 'importmagic-mode)
```

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

## Virtual environments

`importmagic.el` is known to work
with [pyvenv](https://github.com/jorgenschaefer/pyvenv)
and
[virtualenvwrapper.el](https://github.com/porterjamesj/virtualenvwrapper.el). Other
packages have not been tested.

Note that the above will mean that if either `importmagic` or `epc`
are not in your virtual env, it will fail. Don't worry too much
though. If importmagic fails, it will give you a warning, but it will
not get in your way.

## Known issues

There seems to be an issue going on with Gtk symbols. It doesn't only
affect importmagic, but it also affects
Jedi. See
[this issue](https://github.com/davidhalter/jedi/issues/531).

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
