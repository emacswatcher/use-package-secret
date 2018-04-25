* use-package-load-setq

** Extend use-package to set non-customizeable variables from external files

This extension covers a configuration corner case wherein a package
has non-customizeable variables which need to be set per-user, but
which you don’t want to specify in your dot emacs.

In my personal case, I have a dot emacs which is shared by many users.
Many packages don’t think of themselves as multi-user so they don’t
make certain variables customizeable. YMMV and your motivations for
using this also MV. I merely offer an idiomatic wrapper which puts
some sane sugared guardrails around a hack. (hashtag time-honored
rationalization)

** Design

This package maps the contents of certain files, by default in
’user-emacs-dir’, to variables, with some error-checking. The
original, inline version of this code loaded a file which contained a
single ’setq’ form, but after some consideration this interface was
deemed unsatisfactory because having the whole ’setq’ in the external
file obscured the name of the variable being set, thus removing one of
the joys of ’use-package’: all of the configuration is visible and
searchable in one place.

** Installation

This package is available from [[https://melpa.org/#/][MELPA]] and extends [[https://github.com/jwiegley/use-package][use-package]]. There is
documentation at those links which explains how to set your emacs up
to use this package.

Once those work, you can install ’use-package-load-setq’ and its dependencies
using:

#+BEGIN_EXAMPLE
  M-x package-install RET use-package RET
#+END_EXAMPLE

Now see [[*Post-Installation Tasks]].

** Installing from the Git Repository

First, use Git to clone the use-package repository:

#+BEGIN_SRC shell-script
  $ git clone https://github.com/emacswatcher/use-package-load-setq.git /your/desired/source/path
  $ cd /your/desired/source/path/use-package-load-setq
#+END_SRC

Then compile the libraries and generate the info manuals:

#+BEGIN_SRC shell-script
  $ make
#+END_SRC

And then, from a privileged account, to install:

#+BEGIN_SRC shell-script
  $ make install
#+END_SRC

** Usage



** license

