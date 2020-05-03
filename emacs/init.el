(require 'package)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)

(package-initialize)

(unless (file-exists-p package-user-dir)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'cl-lib)
(require 'use-package)

(server-start)

(use-package s
  :ensure t)

(use-package auto-package-update
  :ensure t)

(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (setq ido-everywhere t)
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-prospects 10)
  (setq ido-case-fold t))

(use-package ido-grid-mode
  :ensure t
  :config
  (ido-grid-mode 1))

(use-package lsp-mode
  :ensure t
  :commands (lsp))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode))

(use-package company-lsp
  :ensure t
  :after (lsp-mode))

(use-package dap-mode
  :ensure t
  :after (lsp-mode))

(use-package company
  :ensure t
  :commands (company-mode)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.1))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :bind
  (:map markdown-mode-map
        ("M-n" . nil)
        ("M-p" . nil)
        ("C-c t 0" . markdown-remove-header)
        ("C-c t 1" . markdown-insert-header-atx-1)
        ("C-c t 2" . markdown-insert-header-atx-2)
        ("C-c t 3" . markdown-insert-header-atx-3)
        ("C-c t 4" . markdown-insert-header-atx-4)
        ("C-c t 5" . markdown-insert-header-atx-5)
        ("C-c t 6" . markdown-insert-header-atx-6)
        ("C-c t h" . markdown-insert-header-dwim)
        ("C-c t s" . markdown-insert-header-setext-2)
        ("C-c t t" . markdown-insert-header-setext-1))
  :mode
  (("\\.markdown$" . markdown-mode)
   ("\\.md$" . markdown-mode)))

(defun compile-immediate ()
  (interactive)
  (custom-set-variables
   '(compilation-read-command nil))
  (call-interactively 'compile))

(defun my-c-mode-common-hook ()
  (c-set-offset 'inextern-lang 0)
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'brace-list-intro '+)
  (setq-local c-default-style "linux")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 2)
  (setq-local c-basic-offset 2))

(defun my-java-mode-hook ()
  (setq-local c-basic-offset 4))

(use-package cc-mode
  :commands (cc-mode)
  :config
  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
  (add-hook 'java-mode-hook 'my-java-mode-hook))

(use-package highlight-parentheses
  :config
  (add-hook 'find-file-hook 'highlight-parentheses-mode))

(use-package emacs-lisp-mode
  :commands (emacs-lisp-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("M-." . find-function-at-point)))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;; (use-package auto-async-byte-compile
;;   :ensure t
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
;;   (setq auto-async-byte-compile-suppress-warnings t))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  :mode
  (("\\.asdf?$" . lisp-mode)))

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1)
  (push '(rspec-compilation-mode :noselect t :stick t) popwin:special-display-config)
  (push '(xref--xref-buffer-mode :noselect t) popwin:special-display-config)
  (push '(grep-mode :noselect t :stick t) popwin:special-display-config)
  (push '("*ruby*" :noselect t :stick t) popwin:special-display-config))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-vibrant t))

;; (use-package esup)

(defun eslint-fix-enable-p (file-name)
  (member (file-name-extension file-name) '("js" "ts" "jsx" "tsx")))

(defun eslint-fix-web-mode ()
  (when (and (buffer-file-name) (eslint-fix-enable-p (buffer-file-name)))
    (eslint-fix-on-save-mode)))

(use-package web-mode
  :ensure t
  :commands (web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset nil
        web-mode-script-padding 2
        web-mode-style-padding 2
        web-mode-content-types-alist '(("jsx"  . ".*\\.js[x]?\\'")))
  (add-hook 'web-mode-hook 'eslint-fix-web-mode)
  (add-hook 'web-mode-hook 'add-node-modules-path)
  :mode
  (("\\.html?$" . web-mode)
   ("\\.jsx?$" . web-mode)
   ("\\.json$" . web-mode)
   ("\\.erb$" . web-mode)
   ("\\.tsx?$" . web-mode)))

(use-package ws-butler
  :ensure t
  :config
  (mapc (lambda (hook)
          (add-hook hook 'ws-butler-mode))
        '(c-mode-common-hook
          enh-ruby-mode-hook
          python-mode-hook
          haml-mode-hook
          yaml-mode-hook
          lisp-mode-hook
          emacs-lisp-mode-hook)))

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
  (flycheck-add-mode 'javascript-eslint 'ssass-mode))

(use-package yaml-mode
  :ensure t
  :commands (yaml-mode))

(defun set-enh-ruby-mode-face ()
  (set-face-attribute 'enh-ruby-op-face nil :foreground nil :inherit 'default))

(defun enable-ruby-flycheck-if-rubocop-yml-exists ()
  (enable-flycheck-if-parent-file-exists ".rubocop.yml" 'ruby-rubocop))

(use-package enh-ruby-mode
  :ensure t
  :commands (enh-ruby-mode)
  :config
  (setq enh-ruby-deep-indent-paren nil
        enh-ruby-add-encoding-comment-on-save nil)
  (add-hook 'enh-ruby-mode-hook 'set-enh-ruby-mode-face t)
  (add-hook 'enh-ruby-mode-hook 'enable-ruby-flycheck-if-rubocop-yml-exists)
  :mode
  (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|jb\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package ruby-electric
  :ensure t
  :commands (ruby-electric-mode)
  :init (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode))

(defun notify-rspec-finish ()
  (let ((urgency (if rspec-last-failed-specs "critical" "normal"))
        (message (if rspec-last-failed-specs "Failed" "Succeeded")))
    (shell-command (format "notify-send --urgency %s 'RSpec completed' '%s'" urgency message))))

(use-package rspec-mode
  :ensure t
  :commands (rspec-mode)
  :init
  (add-hook 'enh-ruby-mode-hook 'rspec-mode)
  (add-hook 'dired-mode-hook 'rspec-dired-mode)
  :config
  (setq rspec-use-spring-when-possible nil
        rspec-use-bundler-when-possible t)
  (add-hook 'rspec-after-verification-hook 'notify-rspec-finish)
  (with-eval-after-load 'rspec-mode
    (rspec-install-snippets)))

(use-package inf-ruby
  :ensure t
  :commands (inf-ruby-minor-mode)
  :init
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

(defun enable-haml-flycheck-if-haml-lint-yml-exists ()
  (enable-flycheck-if-parent-file-exists ".haml-lint.yml" 'haml-lint))

(use-package haml-mode
  :ensure t
  :commands (haml-mode)
  :config
  (add-hook 'haml-mode-hook 'enable-haml-flycheck-if-haml-lint-yml-exists))

(use-package magit
  :ensure t
  :commands (magit-status))

(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "gray20")
  (set-face-background 'highlight-indentation-current-column-face "gray35")

  (defun my-highlight-indentation-guess-offset (orig-fn &rest args)
    (cond ((and (eq major-mode 'enh-ruby-mode) (boundp 'enh-ruby-indent-level))
           (setq ad-return-value enh-ruby-indent-level))
          ((and (eq major-mode 'haml-mode) (boundp 'haml-indent-offset))
           (setq ad-return-value haml-indent-offset))
          ((and (eq major-mode 'sass-mode) (boundp 'sass-indent-offset))
           (setq ad-return-value sass-indent-offset))
          ((and (eq major-mode 'ssass-mode) (boundp 'ssass-tab-width))
           (setq ad-return-value ssass-tab-width))
          (t
           (apply orig-fn args))))

  (advice-add 'highlight-indentation-guess-offset :around 'my-highlight-indentation-guess-offset)

  (let ((hooks '(enh-ruby-mode-hook
                 python-mode-hook haml-mode-hook
                 coffee-mode-hook sass-mode-hook
                 yaml-mode-hook org-mode-hook
                 vue-mode-hook)))
    (dolist (hook hooks)
      (add-hook hook 'highlight-indentation-mode)
      (add-hook hook 'highlight-indentation-current-column-mode))))

(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))

(use-package coffee-mode
  :ensure t
  :commands (coffee-mode)
  :config
  (custom-set-variables
   '(coffee-tab-width 2)))

(use-package sass-mode
  :ensure t
  :commands (sass-mode))

(use-package haskell-mode
  :ensure t
  :commands (haskell-mode))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package add-node-modules-path
  :ensure t)

(use-package js
  :config
  (setq js-indent-level 2))

(use-package vue-mode
  :ensure t
  :commands (vue-mode)
  :config
  (add-hook 'vue-mode-hook 'add-node-modules-path)
  (add-hook 'vue-mode-hook 'flycheck-mode)
  (add-hook 'vue-mode-hook 'eslint-fix-on-save-mode)
  ;; https://github.com/AdamNiederer/vue-mode/issues/74#issuecomment-528560608
  (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
  (custom-set-variables
   '(vue-html-extra-indent 2)))

(use-package ssass-mode
  :ensure t
  :commands (ssass-mode)
  :bind
  (:map ssass-mode-map
        ("\177" . my/ssass-dedent)  ; [backspace]
        ("C-i" . my/ssass-indent-cyclic))
  :config
  (defun ssass-indent ()
    "Indent the current line."
    (interactive)
    (indent-line-to
     (cond
      ((and (not ssass-indent-blanks) (ssass--whitespace-p 0)) 0)
      ((ssass--whitespace-p -1) 0)
      ((ssass--no-anchor-line-p) 0)
      ((ssass--comma-before-p) (ssass--last-anchor-line-indent-level))
      ;; ここが増えた。セレクタ行は自動インデントしない
      ((ssass--selector-p (buffer-substring (point-at-bol) (point-at-eol)))
       (current-indentation))
      (t
       (+ ssass-tab-width (ssass--last-anchor-line-indent-level))))))

  (defun my/ssass-dedent (n)
    (interactive "p")
    (if (= (point) (+ (point-at-bol) (current-indentation)))
        (ssass-dedent)
      (delete-backward-char n)))

  (defun my/ssass-indent-cyclic ()
    (interactive)
    (let* ((curr (current-indentation))
           (curr (- curr (mod curr ssass-tab-width)))
           (next (+ curr ssass-tab-width))
           (max-indent (+ ssass-tab-width (ssass--last-anchor-line-indent-level))))
      (indent-line-to (if (< max-indent next)
                          0
                        next)))))

(use-package pug-mode
  :ensure t
  :commands (pug-mode)
  :config
  (setq pug-tab-width 2))

(use-package typescript-mode
  :ensure t
  :commands (typescript-mode typescript-tsx-mode)
  :config
  (setq typescript-indent-level 2))

(use-package css-mode
  :commands (css-mode)
  :config
  (setq css-indent-offset 2))

(use-package sqlformat
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

(use-package reformatter
  :ensure t)

(use-package hydra
  :ensure t
  :commands
  (hydra-error/body hydra-window/body hydra-main/body)
  :bind
  (("M-o" . hydra-window/body)
   ("M-g" . hydra-error/body)
   ("C-z" . hydra-main/body))
  :config
  (defhydra hydra-error ()
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("g" goto-line "line" :exit t)
    ("q" nil "quit"))

  (defhydra hydra-window ()
    "window"
    ("h" windmove-left nil)
    ("j" windmove-down nil)
    ("k" windmove-up nil)
    ("l" windmove-right nil)
    ("|" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right))
     "vert")
    ("-" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down))
     "horz")
    ("1" delete-other-windows "one")
    ("0" delete-window "del")
    ("b" ido-switch-buffer "buf")
    ("p" ghq-cd "ghq")
    ("f" find-file-in-git-ls-files "git ls")
    ("q" nil "quit"))

  (defhydra hydra-main (:hint nil :exit t)
    "
^Main^                      ^Git^
^^^^^^^^-------------------------------------------------------
_r_: replace-string         _g_: magit-status
_R_: replace-regexp         _b_: magit-blame
_X_: query-replace-regexp   _d_: git-grep
_c_: grep                   _p_: ghq-cd
^  ^                        _f_: find-file-in-git-ls-files

_q_: quit
"
    ("r" replace-string)
    ("R" replace-regexp)
    ("X" query-replace-regexp)
    ("c" grep)
    ("g" magit-status)
    ("b" magit-blame)
    ("d" git-grep)
    ("p" ghq-cd)
    ("f" find-file-in-git-ls-files)
    ("q" nil)))


;;;; Load local files

(defun load-x (file &rest args)
  (let* ((dir (file-name-directory load-file-name))
         (f (expand-file-name file dir)))
    (apply 'load f args)))

(load-x "misc")
(load-x "defuns")
(load-x "site" t)
(load-x "flycheck-checker")
(load-x "format")

;;;; Global Bindings
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-r" 'git-grep-symbol-at-point)
;; (bind-key "M-]" 'bs-cycle-next)
;; (bind-key "M-[" 'bs-cycle-previous)

(bind-key "C-c g" 'revert-buffer)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["dark gray" "orange red" "green3" "yellow3" "dodger blue" "magenta3" "cyan3" "white smoke"])
 '(dired-dwim-target t)
 '(dired-isearch-filenames t)
 '(dired-recursive-copis (quote always))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(inferior-lisp-program "sbcl" t)
 '(select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
