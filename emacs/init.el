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

(use-package cc-mode
  :commands (cc-mode)
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-offset 'inextern-lang 0)
              (setq-local c-default-style "K&R")
              (setq-local indent-tabs-mode nil)
              (setq-local tab-width 2)
              (setq-local c-basic-offset 2)))
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local c-basic-offset 4)))
  (mapc (lambda (map)
          (bind-key "C-c c" 'compile-immediate map)
          (bind-key "C-c n" 'next-error map)
          (bind-key "C-c p" 'previous-error map))
        (list c-mode-map
              c++-mode-map)))

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
        web-mode-engines-alist '(("php" . "\\.ctp$")))
  :mode
  (("\\.ctp$" . web-mode)
   ("\\.html?$" . web-mode)
   ("\\.jsx?$" . web-mode)
   ("\\.json$" . web-mode)
   ("\\.ftl$" . web-mode)))

(use-package php-mode
  :commands (php-mode)
  :config
  (setq php-manual-path "/opt/phpdoc")
  (add-hook 'php-mode-hook
            (lambda ()
              (setq-local c-basic-offset 4)))
  (bind-key "C-c C-m" 'php-search-documentation php-mode-map)
  :mode
  (("\\.php$" . php-mode)))

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

(use-package flycheck)
(use-package flycheck-pyflakes)

(use-package yaml-mode
  :commands (yaml-mode))

(use-package editorconfig
  :init
  (add-hook 'prog-mode-hook (lambda () (editorconfig-mode 1)))
  (add-hook 'text-mode-hook (lambda () (editorconfig-mode 1))))

(use-package adoc-mode
  :commands (adoc-mode)
  :mode
  (("\\.adoc$" . adoc-mode)))

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
  (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
  :mode
  (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package ruby-electric
  :commands (ruby-electric-mode)
  :init (add-hook 'enh-ruby-mode-hook 'ruby-electric-mode))

(use-package inf-ruby
  :commands (inf-ruby-minor-mode)
  :config
  (eval-after-load 'inf-ruby
    '(define-key inf-ruby-minor-mode-map
       (kbd "C-c C-s") 'inf-ruby-console-auto)))

(use-package rspec-mode
  :commands (rspec-mode)
  :init
  (add-hook 'enh-ruby-mode-hook 'rspec-mode)
  :config
  (setq rspec-use-spring-when-possible nil
        rspec-use-bundler-when-possible t))

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

  (defadvice highlight-indentation-guess-offset (around my-highlight-indentation-guess-offset)
    (cond ((and (eq major-mode 'enh-ruby-mode) (boundp 'enh-ruby-indent-level))
           (setq ad-return-value enh-ruby-indent-level))
          ((and (eq major-mode 'haml-mode) (boundp 'haml-indent-offset))
           (setq ad-return-value haml-indent-offset))
          ((and (eq major-mode 'sass-mode) (boundp 'sass-indent-offset))
           (setq ad-return-value sass-indent-offset))
          (t
           ad-do-it)))

  (ad-activate 'highlight-indentation-guess-offset)

  (let ((hooks '(enh-ruby-mode-hook
                 python-mode-hook haml-mode-hook
                 coffee-mode-hook sass-mode-hook)))
    (dolist (hook hooks)
      (add-hook hook 'highlight-indentation-mode)
      (add-hook hook 'highlight-indentation-current-column-mode))))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package coffee-mode
  :config
  (custom-set-variables
   '(coffee-tab-width 2)))

(use-package sass-mode)

(use-package inflections)


;;;; Load local files

(defun load-x (file &rest args)
  (let* ((dir (file-name-directory load-file-name))
         (f (expand-file-name file dir)))
    (apply 'load f args)))

(load-x "misc")
(load-x "defuns")
(load-x "site" t)
(load-x "flycheck-checker")


;;;; Global Bindings

(bind-key "M-g" 'goto-line)
(bind-key "M-+" 'text-scale-increase)
(bind-key "M-_" 'text-scale-decrease)
(bind-key "M-k" 'kill-this-buffer)
(bind-key "M-o" 'other-window)
(bind-key "M-1" 'delete-other-windows)
(bind-key "M-2" 'split-window-below)
(bind-key "M-3" 'split-window-right)
(bind-key "M-0" 'delete-window)
(bind-key "M-}" 'next-buffer)
(bind-key "M-{" 'previous-buffer)
(bind-key "M-]" 'next-error)
(bind-key "M-[" 'previous-error)

(bind-key "C-c s" 'swap-windows)
(bind-key "C-c w" 'whitespace-mode)
(bind-key "C-c f" 'find-file-in-git-ls-files)

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
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (slime zenburn-theme yaml-mode ws-butler wgrep web-mode use-package ruby-electric rspec-mode popwin php-mode markdown-mode magit inkpot-theme inf-ruby highlight-parentheses highlight-indentation helm-gtags helm-git-grep helm-ag haml-mode flycheck-pyflakes enh-ruby-mode editorconfig dumb-jump creamsody-theme auto-async-byte-compile adoc-mode)))
 '(select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
