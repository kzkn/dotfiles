(require 'cask "~/.cask/cask.el")
(cask-initialize)

(server-start)

;;;; Packages
(require 'use-package)

(use-package ido
  :config
  (ido-mode 1)
  (setq ido-everywhere t)
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-create-new-buffer 'always)
  (setq ido-max-prospects 10)
  (setq ido-case-fold t))

(use-package ido-grid-mode
  :config
  (ido-grid-mode 1))

(use-package markdown-mode
  :commands (markdown-mode)
  :config
  (bind-key "M-n" nil markdown-mode-map)
  (bind-key "M-p" nil markdown-mode-map)
  (unless window-system
    ;; `C-t' confilict to tmux's escape key, so avoid it
    (bind-key "C-c t 0" 'markdown-remove-header markdown-mode-map)
    (bind-key "C-c t 1" 'markdown-insert-header-atx-1 markdown-mode-map)
    (bind-key "C-c t 2" 'markdown-insert-header-atx-2 markdown-mode-map)
    (bind-key "C-c t 3" 'markdown-insert-header-atx-3 markdown-mode-map)
    (bind-key "C-c t 4" 'markdown-insert-header-atx-4 markdown-mode-map)
    (bind-key "C-c t 5" 'markdown-insert-header-atx-5 markdown-mode-map)
    (bind-key "C-c t 6" 'markdown-insert-header-atx-6 markdown-mode-map)
    (bind-key "C-c t h" 'markdown-insert-header-dwim markdown-mode-map)
    (bind-key "C-c t s" 'markdown-insert-header-setext-2 markdown-mode-map)
    (bind-key "C-c t t" 'markdown-insert-header-setext-1 markdown-mode-map))
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

(use-package elisp-mode
  :init
  (use-package eldoc
    :config
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
  (use-package auto-async-byte-compile
    :config
    (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
    (setq auto-async-byte-compile-suppress-warnings t))
  :config
  (bind-key "M-." 'find-function-at-point emacs-lisp-mode-map)
  :interpreter
  (("emacs" . emacs-lisp-mode))
  :mode
  (("Cask" . emacs-lisp-mode)))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"
        slime-contribs '(slime-fancy))
  :mode
  (("\\.asdf?$" . lisp-mode)))

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package popwin
  :config
  (popwin-mode 1)
  (push '(rspec-compilation-mode :noselect t :stick t) popwin:special-display-config)
  (push '(xref--xref-buffer-mode :noselect t) popwin:special-display-config)
  (push '(grep-mode :noselect t :stick t) popwin:special-display-config)
  (push '("*ruby*" :noselect t :stick t) popwin:special-display-config))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package inkpot-theme
  :if window-system)

;; (use-package esup)

(use-package web-mode
  :commands (web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset nil
        web-mode-script-padding 2
        web-mode-style-padding 2
        web-mode-content-types-alist '(("jsx"  . ".*\\.js[x]?\\'")))
  :mode
  (("\\.html?$" . web-mode)
   ("\\.jsx?$" . web-mode)
   ("\\.json$" . web-mode)
   ("\\.erb$" . web-mode)))

(use-package ws-butler
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
  :config
  (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
  (flycheck-add-mode 'javascript-eslint 'ssass-mode))

(use-package yaml-mode
  :commands (yaml-mode))

(defun set-enh-ruby-mode-face ()
  (set-face-attribute 'enh-ruby-op-face nil :foreground nil :inherit 'default))

(defun enable-ruby-flycheck-if-rubocop-yml-exists ()
  (enable-flycheck-if-parent-file-exists ".rubocop.yml" 'ruby-rubocop))

(use-package enh-ruby-mode
  :commands (enh-ruby-mode)
  :config
  (setq enh-ruby-deep-indent-paren nil
        enh-ruby-add-encoding-comment-on-save nil)
  (add-hook 'enh-ruby-mode-hook 'set-enh-ruby-mode-face t)
  (add-hook 'enh-ruby-mode-hook 'enable-ruby-flycheck-if-rubocop-yml-exists)
  :mode
  (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|jb\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package ruby-electric
  :commands (ruby-electric-mode)
  :init (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode))

(defun notify-rspec-finish ()
  (let ((urgency (if rspec-last-failed-specs "critical" "normal"))
        (message (if rspec-last-failed-specs "Failed" "Succeeded")))
    (shell-command (format "notify-send --urgency %s 'RSpec completed' '%s'" urgency message))))

(use-package rspec-mode
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
  :commands (inf-ruby-minor-mode)
  :init
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

(defun enable-haml-flycheck-if-haml-lint-yml-exists ()
  (enable-flycheck-if-parent-file-exists ".haml-lint.yml" 'haml-lint))

(use-package haml-mode
  :commands (haml-mode)
  :config
  (add-hook 'haml-mode-hook 'enable-haml-flycheck-if-haml-lint-yml-exists))

(use-package magit
  :commands (magit-status)
  :init (bind-key "C-x g" 'magit-status))

(use-package highlight-indentation
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
  :config
  (setq wgrep-auto-save-buffer t))

(use-package coffee-mode
  :commands (coffee-mode)
  :config
  (custom-set-variables
   '(coffee-tab-width 2)))

(use-package sass-mode
  :commands (sass-mode))

(use-package haskell-mode
  :commands (haskell-mode))

(use-package yasnippet
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package add-node-modules-path)

(use-package js
  :config
  (setq js-indent-level 2))

(use-package vue-mode
  :commands (vue-mode)
  :config
  (add-hook 'vue-mode-hook 'add-node-modules-path)
  (add-hook 'vue-mode-hook 'flycheck-mode)
  (custom-set-variables
   '(vue-html-extra-indent 2)))

(use-package ssass-mode
  :commands (ssass-mode)
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
                        next))))

  ;; [backspace] としたいところだけど効かなかった
  (bind-key "\177" 'my/ssass-dedent ssass-mode-map)
  (bind-key "C-i" 'my/ssass-indent-cyclic ssass-mode-map))

(use-package sqlformat
  :config
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode))

;;;; Load local files

(defun load-x (file &rest args)
  (let* ((dir (file-name-directory load-file-name))
         (f (expand-file-name file dir)))
    (apply 'load f args)))

(load-x "misc")
(load-x "defuns")
(load-x "site" t)
(load-x "flycheck-checker")
(load-x "rspec-result-mode")


;;;; Global Bindings
;; m-a, m-c, m-e, m-h, m-i, m-n, m-p
;; C-q
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-o" 'other-window)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-below)
(bind-key "M-3" 'split-window-right)
(bind-key "M-0" 'delete-window)
(bind-key "M-]" 'next-error)
(bind-key "M-[" 'previous-error)
(bind-key "M-g" 'goto-line)
(bind-key "M-r" 'git-grep-symbol-at-point)

(bind-key "C-c a" 'org-agenda)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c f" 'find-file-in-git-ls-files)
(bind-key "C-c g" 'ghq-cd)
(bind-key "C-c n" 'cleanup-buffer)

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
