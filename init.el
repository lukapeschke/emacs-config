;; Copyright 2019-2020 Luka Peschke

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
  '(blacken company dockerfile-mode dockerfile-mode drag-stuff dumb-jump
            elixir-mode flycheck flycheck-pycheckers gnu-elpa-keyring-update go-mode groovy-mode
            highlight-indentation jinja2-mode js2-mode json-mode lsp-metals lsp-mode lsp-ui
            protobuf-mode py-isort pyvenv rainbow-delimiters rainbow-mode rust-mode
            sbt-mode scala-mode terraform-mode tangotango-theme use-package
            web-mode whitespace yaml-mode)
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

;; matching {} [] () in the same color
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; automatically make pairs for (), [] and {}, and automatically
;; delete pairs
(electric-pair-mode 1)

;; Highlight matching (), {}, and []
(show-paren-mode 1)

;; Highlight indentation
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#333333")

;; Uncomment this lines to get a spaces-only indentation
(setq-default indent-tabs-mode nil)

;; Make characters after column 80 purple and trailing spaces red
;; This also shows tabulations, delete 'tab-mark' from the lines below to
;; cancel this
(require 'whitespace)
(setq whitespace-style
      (quote (face trailing tab-mark lines-tail)))

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


;; remove all trailing whitespaces when saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; allows to drag a selected area around
(require 'drag-stuff)

;; keybindings to move stuff around, use Meta (alt) and arrow keys
(global-set-key (kbd "<M-up>")    'drag-stuff-up)
(global-set-key (kbd "<M-down>")  'drag-stuff-down)
(global-set-key (kbd "<M-left>")  'drag-stuff-left)
(global-set-key (kbd "<M-right>")  'drag-stuff-right)


;; nice theme
(load-theme 'tangotango t)
;; keep the original background color, ie. transparent terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; using js2-minor-mode for linting but js-mode for syntax highlighting
;; (js2-mode is slow af for highlighting, guess it's doing a lot of stuff
;; I don't need behind the scenes)
(require 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)

;; Enable rst-mode for .rst files
(require 'rst)
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))

;; Enable web-mode  for .html files
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-enable-auto-closing t
      web-mode-enable-auto-pairing t
      web-mode-enable-current-column-highlight t
      web-mode-enable-current-element-highlight t)


;; ido mode, fuzzy finding for files & buffers
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; SETTING PATH
;; golang
(add-to-list 'exec-path "~/go/bin/")
;; python
(add-to-list 'exec-path "~/.local/bin/")
;; rust
(add-to-list 'exec-path "~/.cargo/bin/")
;; custom built language servers
(add-to-list 'exec-path "~/bin/")
;; END OF SETTING PATH

;; PER-LANGUAGE CONFIG & ON SAVE HOOKS
;; golang
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;; set JSON and JavaScript indentation to 2
(setq js-indent-level 2)

;; rust
(setq rust-format-on-save t)
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-display-chaining-hints t)

;; python
(require 'py-isort)
(add-hook 'before-save-hook 'py-isort-before-save) ;; sorting imports on save
;; Enable indentation highlighting in python
(add-hook 'python-mode-hook 'highlight-indentation-mode)
;; Enable black formatting in python
(add-hook 'python-mode-hook 'blacken-mode)
(setq blacken-line-length 80)

;; scala
(defun scala-mode-format-on-save-hook ()
  (when (eq major-mode 'scala-mode)
    (lsp-format-buffer)))

(add-hook 'before-save-hook 'scala-mode-format-on-save-hook) ;; sorting imports on save


;; END OF ON SAVE HOOKS

;; START -- Support for Language Server Protocol (LSP)
(use-package lsp-mode
  :config

  (setq lsp-prefer-capf t ;; use company-capf
        lsp-enable-snippet nil ;; Disable Yasnippet
        gc-cons-threshold 100000000
        read-process-output-max (* 1024 1024)
        )

  ;; add your languages here
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  ;; "language_server.sh" is assumed to be available in $PATH
  (add-hook 'elixir-mode-hook #'lsp)
  ;; "metals" is assumed to be available in $PATH
  (add-hook 'scala-mode-hook #'lsp)
  )


;; We use microsoft's language server as it is MUCH faster than palantir's
;; python-language-server
(use-package lsp-python-ms
  :ensure t
  :init
  ;; Will install the ms lsp automatically, set to nil to disable
  (setq lsp-python-ms-auto-install-server t)
  :config
  :hook
  ((python-mode . (lambda ()
                    (require 'lsp-python-ms)
                    (lsp-deferred)
                ))))


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
   '(lsp-metals py-isort js2-mode json-mode dumb-jump pyvenv rainbow-mode sbt-mode scala-mode blacken groovy-mode protobuf-mode elixir-mode yaml-mode use-package tangotango-theme terraform-mode rust-mode rainbow-delimiters lsp-ui jinja2-mode highlight-indentation gnu-elpa-keyring-update go-mode flycheck drag-stuff dockerfile-mode company-lsp company)))
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
