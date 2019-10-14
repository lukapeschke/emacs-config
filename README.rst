==============
 Emacs config
==============

This is my Emacs configuration. It supports various config formats and adds
advanced support for several languages through `lsp-mode`_ . PR's are welcome!

.. _lsp-mode: https://github.com/emacs-lsp/lsp-mode

Installation
============

Make sure you've access to the internet the first time you start Emacs after
installing this config, as on startup emacs will ensure that all required
packages are installed.

.. code-block:: shell

   $ git clone https://github.com/lukapeschke/emacs-config ~/.emacs.d

Updating
========

.. code-block:: shell

   $ cd ~/.emacs.d
   $ git pull

Supported languages
===================

* Python. Depends on pylint_ and pyls_ .

* Rust. Depends on rls_ .

* Go. Depends on gopls_ .

.. _pylint: https://www.pylint.org/
.. _pyls: https://github.com/palantir/python-language-server
.. _rls: https://github.com/rust-lang/rls
.. _gopls: https://github.com/golang/tools/blob/master/gopls/doc/user.md
