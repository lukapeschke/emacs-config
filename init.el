;; Copyright 2019-2021 Luka Peschke

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;; http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.


;; package installation
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; forward compat for old emacs versions
(require 'cl-lib)

;; checking that use-package is installed:

(defvar my-packages
  '(blacken company counsel deno-fmt dockerfile-mode dockerfile-mode drag-stuff dumb-jump
            elixir-mode flycheck flycheck-kotlin flycheck-pycheckers gnu-elpa-keyring-update
            go-mode groovy-mode highlight-indentation ivy jinja2-mode kotlin-mode lsp-metals lsp-mode
            lsp-pyright lsp-ui multiple-cursors protobuf-mode py-isort pyvenv rainbow-delimiters
            rainbow-mode rust-mode sbt-mode scala-mode string-inflection swiper terraform-mode
            tangotango-theme typescript-mode use-package web-mode whitespace yaml-mode)
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))
;; end of package installation

;; VARIOUS

;; nice completion & search
(use-package ivy
  :config
  (ivy-mode 1)
  ;; ido-mode behaviour for file navigation
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (global-set-key "\C-s" 'swiper)
  )

(use-package counsel
  :config
  (counsel-mode 1)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  )

;; matching {} [] () in the same color
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; automatically make pairs for (), [] and {}, and automatically
;; delete pairs
(electric-pair-mode 1)
;; Highlight matching (), {}, and []
(setq show-paren-delay 0)
(show-paren-mode 1)
;; display line AND column in bottom bar
(setq column-number-mode t)
;; Make CamelCase bevahe as two words: Camel and Case
(add-hook 'prog-mode-hook 'subword-mode)
;; Delegate cursor behaviour handling to the terminal
(setq visible-cursor nil)

;; automatically goes to a new line when a certain length is reached (useful for
;; comments).
(add-hook 'prog-mode-hook #'auto-fill-mode)
;; Break at 80th char
(setq fill-column 80
      ;; only enable auto-fill for comments
      comment-auto-fill-only-comments t)

;; Highlight indentation
(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#333333"))

;; Uncomment this line to get a spaces-only indentation
(setq-default indent-tabs-mode nil)

;; Make characters after column 80 purple and trailing spaces red
;; This also shows tabulations, delete 'tab-mark' from the lines below to
;; cancel this
(use-package whitespace
  :config
  (setq whitespace-style '(face trailing tab-mark lines-tail))
  ;; Allowing lines of up to 100 chars
  (setq whitespace-line-column 101))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; scroll only one line at once
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; disable menu bar, set to 1 to enable it
(menu-bar-mode -1)

;; keybindings to toggle/wrap functions, use CTRL-c and arrow keys
(add-hook 'prog-mode-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;; Bind C-x C-a to eXecute Action
(add-hook 'prog-mode-hook
  (lambda()
    (local-set-key (kbd "C-x C-a") 'lsp-execute-code-action)))

;; List flycheck errors on C-x C-e
(add-hook 'prog-mode-hook
  (lambda()
    (local-set-key (kbd "C-x C-e") 'flycheck-list-errors)))


;; remove all trailing whitespaces when saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; keybindings to move stuff around, use Meta (alt) and arrow keys
(use-package drag-stuff
  :config
  (global-set-key (kbd "<M-up>")    'drag-stuff-up)
  (global-set-key (kbd "<M-down>")  'drag-stuff-down)
  (global-set-key (kbd "<M-left>")  'drag-stuff-left)
  (global-set-key (kbd "<M-right>")  'drag-stuff-right))

;; nice theme
(load-theme 'tangotango t)
;; keep the original background color, ie. transparent terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)


;; Enable rst-mode for .rst files
(use-package rst
  :config
  (add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode)))

;; Enable web-mode  for .html and .vue files
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(setq web-mode-enable-auto-closing t
      web-mode-enable-auto-pairing t
      web-mode-enable-current-column-highlight t
      web-mode-enable-current-element-highlight t
      web-mode-markup-indent-offset 2)

;; disable lock files
(setq create-lockfiles nil)
;; put backup files in .emacs/backups
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; put autosave files in .emacs/auto-saves
(setq auto-save-file-name-transforms
  `((".*" ,(concat user-emacs-directory "auto-saves/") t)))

;; Allow to cycle between string cases on C-c C-u
(defun java-style-string-cycle ()
  (local-set-key (kbd "C-c C-u") 'string-inflection-java-style-cycle))

(defun python-style-string-cycle ()
  (local-set-key (kbd "C-c C-u") 'string-inflection-python-style-cycle))

(use-package string-inflection
  :config
  (add-hook 'python-mode-hook 'python-style-string-cycle)
  (add-hook 'rust-mode-hook 'python-style-string-cycle)
  (add-hook 'scala-mode-hook 'java-style-string-cycle)
  (add-hook 'js-mode-hook 'java-style-string-cycle)
  (add-hook 'typescript-mode-hook 'java-style-string-cycle))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-c C-c") 'mc/edit-lines)
  (global-set-key (kbd "C-c >") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-c <") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c a") 'mc/mark-all-like-this))

;; END OF VARIOUS

;; SETTING PATH
;; golang
(add-to-list 'exec-path "~/go/bin/")
;; python
(add-to-list 'exec-path "~/.local/bin/")
;; rust
(add-to-list 'exec-path "~/.cargo/bin/")
;; custom built language servers
(add-to-list 'exec-path "~/bin/")
;; deno
(add-to-list 'exec-path "~/.deno/bin")
;; yarn
(add-to-list 'exec-path "~/.yarn/bin")
;; elixir
(add-to-list 'exec-path "~/bin/elixirls/")
;; END OF SETTING PATH

;; PER-LANGUAGE CONFIG & ON SAVE HOOKS
;; golang
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; JS& TS
(use-package deno-fmt
  :config
  (add-hook 'typescript-mode-hook 'deno-fmt-mode)
  (add-hook 'js-mode-hook 'deno-fmt-mode)
  (setq js-indent-level 2
        typescript-indent-level 2
        lsp-clients-deno-server "~/.deno/bin/deno"))

;; rust
(setq rust-format-on-save t
      lsp-rust-server 'rust-analyzer
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-analyzer-display-chaining-hints t)

;; python
(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save) ;; sorting imports on save
;; Enable indentation highlighting in python
(add-hook 'python-mode-hook 'highlight-indentation-mode)
;; Enable black formatting in python
(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

;; scala
(defun scala-mode-format-on-save-hook ()
  (when (eq major-mode 'scala-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook 'scala-mode-format-on-save-hook) ;; sorting imports on save
;; END OF ON SAVE HOOKS


;; START -- Support for Language Server Protocol (LSP)
(use-package lsp-mode
  :config

  (setq
   ;; use company-capf
   lsp-prefer-capf t
   ;; Disable Yasnippet
   lsp-enable-snippet nil
   gc-cons-threshold 100000000
   read-process-output-max (* 1024 1024)
   ;; disable breadcrumbs on top of window
   lsp-headerline-breadcrumb-enable nil)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.direnv\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.git\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\__pycache__\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\_build\\'")

  ;; add your languages here
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  ;; "language_server.sh" is assumed to be available in $PATH
  (add-hook 'elixir-mode-hook #'lsp)
  ;; "metals" is assumed to be available in $PATH
  (add-hook 'scala-mode-hook #'lsp)
  ;; "kotlin-language-server" is assumed to be available in $PATH
  (add-hook 'kotlin-mode-hook #'lsp)
  )

;; Scala

(use-package lsp-metals
  :config
  (setq lsp-metals-show-inferred-type t))

;; python-language-server

;; We use pyright as it is faster than palantir's
;; python-language-server and more stable than microsoft's language
;; server. However, it requires node to run. You can use microsoft's
;; LS instead by uncommenting the block below and commenting the
;; lsp-pyright block. You'll also have to updater the required packages

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init
;;   ;; Will install the ms lsp automatically, set to nil to disable
;;   (setq lsp-python-ms-auto-install-server t)
;;   :config
;;   :hook
;;   ((python-mode . (lambda ()
;;                     (require 'lsp-python-ms)
;;                     (lsp-deferred)
;;                 ))))


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))


;; lsp-ui allows (between other things) to display linter errors inline
(use-package lsp-ui
  :requires lsp-mode flycheck
  :config

  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; using company for auto-completion
(use-package company
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-capt t)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))


;; For python mode only, explicitly add python-pylint (which also adds mypy) to
;; the list of flycheck checkers. lsp-ui override's flycheck's checker
;; with "lsp" only
(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'lsp-after-initialize-hook
                      (lambda()(flycheck-add-next-checker 'lsp 'python-pylint)))))

;; END -- Support for Language Server Protocol (LSP)



;; CUSTOMIZATION
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("713f898dd8c881c139b62cf05b7ac476d05735825d49006255c0a31f9a4f46ab" default))
 '(groovy-indent-offset 2)
 '(package-selected-packages
   '(counsel swiper ivy kotlin-mode multiple-cursors string-inflection lsp-metals py-isort dumb-jump pyvenv rainbow-mode sbt-mode scala-mode blacken groovy-mode protobuf-mode elixir-mode yaml-mode use-package tangotango-theme terraform-mode rust-mode rainbow-delimiters lsp-ui jinja2-mode highlight-indentation gnu-elpa-keyring-update go-mode flycheck drag-stuff dockerfile-mode company-lsp company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "unspecified-bg" :foreground "color-252" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(rst-level-1 ((t (:background "color-239"))))
 '(rst-level-2 ((t (:background "color-239"))))
 '(rst-level-3 ((t (:background "color-239"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "brightblack"))))
 '(web-mode-html-tag-face ((t (:foreground "brightyellow" :weight bold)))))
(put 'upcase-region 'disabled nil)
