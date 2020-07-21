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

* Python. Depends on pylint_ and pyls_ . You can optionally install black_ ,
  which will format your python code on save.

* Rust. Depends on rls_ .

* Go. Depends on gopls_ and ``goimports``.

* Elixir. Depends on ElixirLS_ .

.. _pylint: https://www.pylint.org/
.. _pyls: https://github.com/palantir/python-language-server
.. _black: https://github.com/psf/black
.. _rls: https://github.com/rust-lang/rls
.. _gopls: https://github.com/golang/tools/blob/master/gopls/doc/user.md
.. _ElixirLS: https://github.com/JakeBecker/elixir-ls


Notes
=====

Python virtualenvs
++++++++++++++++++

In case you're using vanilla virtualenvs like me, I use the following hack. If
you're using pyenv, I believe there's native support in pyls for that.

1. Run ``M-x pyvenv-activate`` (or ``M-x pyvenv-workon`` if you use
   virtualenvwrapper) in emacs and activate the desired venv.

2. In a shell, run ``kill pyls`` (you can add your favourite signal).

3. Emacs should prompt you to restart pyls. Restart it and your venv should be
   taken into account. You're done :-)

Jump to definiton
+++++++++++++++++

The config includes `dumb-jump`_ , which allows to jump to function definitions
and is bound to ``M-.`` (alt key plus dot key) by default.

For better performance, it is recommended to use **The silver searcher**
(``ag``) or **ripgrep** (``rg``).

.. _dumb-jump: https://github.com/jacktasia/dumb-jump
