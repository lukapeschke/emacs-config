==============
 Emacs config
==============

This is my Emacs configuration. It supports various config formats and adds
advanced support for several languages through `lsp-mode`_ . PR's are welcome!

.. _lsp-mode: https://github.com/emacs-lsp/lsp-mode

**DISCLAIMER**: This is very opinionated and made to suit my needs, so it may
not fit yours. Most options are commented though, so you should be able to adapt
the config to what suits you best.

Installation
============

Make sure you've access to the internet the first time you start Emacs after
installing this config, as on startup emacs will ensure that all required
packages are installed.

.. code-block:: shell

   $ git clone https://github.com/lukapeschke/emacs-config ~/.emacs.d

Running
=======

I highly recommend to run emacs in daemon mode (I'm running it with a systemd
user service, but it is not mandatory) in order to benefit from instant start
times and shared clipboards, among others.


Updating
========

.. code-block:: shell

   $ cd ~/.emacs.d
   $ git pull

Supported languages
===================

* Python. Depends on pylint_, mypy_ and ms-pyls_ . You can optionally install black_ ,
  which will format your python code on save.

* Rust. Depends on rls_ .

* Go. Depends on gopls_ and ``goimports``.

* Elixir. Depends on ElixirLS_ .

* Scala. Depends on MetaLS_ .

* JavaScript/TypeScript. Depends either on deno_ or any other JS/TS language server.

* Kotlin. Depends on kotlin-language-server_.


.. _pylint: https://www.pylint.org/
.. _mypy: https://mypy.readthedocs.io/
.. _ms-pyls: https://github.com/microsoft/python-language-server
.. _black: https://github.com/psf/black
.. _rls: https://github.com/rust-lang/rls
.. _gopls: https://github.com/golang/tools/blob/master/gopls/doc/user.md
.. _ElixirLS: https://github.com/JakeBecker/elixir-ls
.. _MetaLS: https://scalameta.org/metals/docs/editors/emacs.html
.. _deno: https://deno.land/
.. _kotlin-language-server: https://github.com/fwcd/kotlin-language-server

Notes
=====

Jump to definiton
+++++++++++++++++

The config includes `dumb-jump`_ , which allows to jump to function definitions
and is bound to ``M-.`` (alt key plus dot key) by default.

For better performance, it is recommended to use **The silver searcher**
(``ag``) or **ripgrep** (``rg``).

.. _dumb-jump: https://github.com/jacktasia/dumb-jump

String cases
++++++++++++

The `string-inflection`_ package is also included. It allows to cycle between string cases for the current term.
By default, it is enabled for JS/TS and Scala (java cycle), Python and Rust (python cycle). The cycle command is
bound to ``C-c C-u``.

.. _string-inflection: https://github.com/akicho8/string-inflection

Multiple cursors
++++++++++++++++

Of course, the config includes `multiple-cursors`_. Following bindings are set:

* ``C-c C-c`` -> ``mc/edit-lines``
* ``C-c >`` -> ``mc/mark-next-like-this``
* ``C-c <`` -> ``mc/mark-previous-like-this``
* ``C-c a`` -> ``mc/mark-all-like-this``

.. _multipole-cursors: https://github.com/magnars/multiple-cursors.el
