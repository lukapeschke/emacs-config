;; package installation

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(require 'cl-lib)

;; checking that use-package is installed:

(defvar my-packages
  '(company company-lsp dockerfile-mode dockerfile-mode drag-stuff flycheck
            go-mode gnu-elpa-keyring-update highlight-indentation jinja2-mode
            lsp-mode lsp-ui rainbow-delimiters rust-mode
            terraform-mode tangotango-theme use-package whitespace yaml-mode)
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
(add-hook 'find-file-hook 'whitespace-mode)

;; scroll only one line at once
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; keybindings to toggle/wrap functions, use CTRL-c and arrow keys
(add-hook 'prog-mode-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

;; remove all trailing whitespaces when saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; set JSON and JavaScript indentation to 2
(setq js-indent-level 2)

;; allows to drag a selected area around
(require 'drag-stuff)

;; keybindings to toggle functions, use CTRL and arrow keys
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

;; Enable rst-mode for .rst files
(require 'rst)
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))


;; START -- Support for Language Server Protocol (LSP)
(use-package lsp-mode
  :config

  (setq lsp-prefer-flymake nil ;; Prefer using lsp-ui (flycheck) over flymake.
        lsp-enable-snippet nil ;; Disable Yasnippet
        )

  ;; add your languages here
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  )

;; lsp-ui allows (between other things) to display linter errors inline
(use-package lsp-ui
  :requires lsp-mode flycheck
  :config

  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
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
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

;; integration of company with lsp
(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

;; END -- Support for Language Server Protocol (LSP)
