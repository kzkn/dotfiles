(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))

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

(use-package eglot
  :ensure t
  :commands (eglot)
  :config
  (add-to-list 'eglot-server-programs
               '(web-mode . ("typescript-language-server" "--stdio")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(ruby-mode . ("ruby-lsp")))
  )

;; (use-package lsp-mode
;;   :ensure t
;;   :commands (lsp))

;; (use-package lsp-ui
;;   :ensure t
;;   :after (lsp-mode)
;;   :config
;;   (custom-set-variables
;;    '(lsp-ui-sideline-actions-icon nil)))

;; (use-package company-lsp
;;   :ensure t
;;   :after (lsp-mode))

;; (use-package dap-mode
;;   :ensure t
;;   :after (lsp-mode))

(use-package company
  :ensure t
  :commands (company-mode)
  :config
  ;; (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.1))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode
  (("\\.markdown$" . markdown-mode)
   ("\\.md$" . markdown-mode)))

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

;; (use-package highlight-parentheses
;;   :config
;;   (add-hook 'find-file-hook 'highlight-parentheses-mode))

(use-package emacs-lisp-mode
  :commands (emacs-lisp-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("M-." . find-function-at-point)))

(use-package eldoc
  :config
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

(use-package sh-script
  :config
  (setq sh-basic-offset 2))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          grep-mode
          compilation-mode
          rspec-compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

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
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-hook 'web-mode-hook 'lsp-web-mode)
  :mode
  (("\\.html?$" . web-mode)
   ("\\.jsx?$" . web-mode)
   ("\\.tsx$" . web-mode)
   ("\\.json$" . web-mode)
   ("\\.erb$" . web-mode)))
   ;; ("\\.tsx?$" . web-mode)))

(use-package ws-butler
  :ensure t
  :config
  (mapc (lambda (hook)
          (add-hook hook 'ws-butler-mode))
        '(c-mode-common-hook
          ruby-ts-mode-hook
          python-mode-hook
          haml-mode-hook
          yaml-mode-hook
          lisp-mode-hook
          emacs-lisp-mode-hook)))

(use-package flycheck
  :ensure t
  :config
  (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
  (flycheck-add-mode 'javascript-eslint 'ssass-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq flycheck-command-wrapper-function 'my/flycheck-command-wrapper-function)
  ;; NOTE: プロジェクトローカルの eslint を使ってくれず devDependencies のパッケージを読めずにエラーとなるのでスキップしてる
  ;; add-node-modules-path が効いて動きそうな気もするが
  (advice-add 'flycheck-eslint-config-exists-p :around #'flycheck-skip-eslint-config-verification))

(use-package yaml-mode
  :ensure t
  :commands (yaml-mode))

(defun my/ruby-beginning-of-block ()
  (interactive)
  (goto-char (sp-get (sp-get-thing t) :beg)))
(defun my/ruby-end-of-block ()
  (interactive)
  (goto-char (sp-get (sp-get-thing) :end)))

(use-package ruby-ts-mode
  :ensure t
  :commands (ruby-ts-mode)
  :config
  (add-hook 'ruby-ts-mode-hook 'enable-ruby-flycheck-if-rubocop-yml-exists)
  ;; (setq ruby-insert-encoding-magic-comment nil)
  :bind
  (:map ruby-ts-mode-map
        ("M-C-p" . my/ruby-beginning-of-thing)
        ("M-C-n" . my/ruby-end-of-thing))
  :mode
  (("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|jb\\|gemspec\\|podspec\\|csb\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-ts-mode)))

(use-package rspec-mode
  :ensure t
  :commands (rspec-mode)
  :init
  (add-hook 'ruby-ts-mode-hook 'rspec-mode)
  (add-hook 'dired-mode-hook 'rspec-dired-mode)
  :config
  (setq rspec-use-spring-when-possible nil
        rspec-use-bundler-when-possible t
        rspec-factory-gem 'factory-bot)
  (add-hook 'rspec-after-verification-hook 'notify-rspec-finish)
  (add-hook 'rspec-mode-hook 'yas-minor-mode)
  (advice-add 'rspec-runner :around #'rspec-runner--lang-ja-jp)
  (with-eval-after-load 'rspec-mode
    (rspec-install-snippets)
    (yas-reload-all))) ; load the personal defined snippets

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
  (advice-add 'highlight-indentation-guess-offset :around 'my-highlight-indentation-guess-offset)
  (let ((hooks '(ruby-ts-mode-hook
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

(use-package yasnippet
  :ensure t
  :init
  (yas-reload-all)
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
  ;; 関数を上書き
  (defun ssass-indent ()
    "Indent the current line."
    (interactive)
    (my/ssass-indent)))

(use-package typescript-mode
  :ensure t
  :commands (typescript-mode typescript-tsx-mode)
  :config
  (add-hook 'typescript-mode-hook 'eslint-fix-web-mode)
  (add-hook 'typescript-mode-hook 'add-node-modules-path)
  (add-hook 'typescript-mode-hook 'lsp-deferred)
  (setq typescript-indent-level 2))

(use-package css-mode
  :commands (css-mode)
  :config
  (setq css-indent-offset 2))

(use-package sqlformat
  :ensure t
  :config
  (add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
  (custom-set-variables
   '(sqlformat-command 'pgformatter)))

(use-package reformatter
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode))

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
_e_: replace-regexp         _g_: magit-status
_r_: rg                     _b_: magit-blame
^  ^                        _d_: git-grep
^  ^                        _p_: ghq-cd
^  ^                        _f_: find-file-in-git-ls-files

_q_: quit
"
    ("e" replace-regexp)
    ("r" rg)
    ("g" magit-status)
    ("b" magit-blame)
    ("d" git-grep)
    ("p" ghq-cd)
    ("f" find-file-in-git-ls-files)
    ("q" nil)))

(use-package ruled-switch-buffer
  :ensure t
  :commands (ruled-switch-buffer)
  :config
  (ruled-switch-buffer-define view-component-rb
    :matcher (lambda (fn) (string-match "_component.rb$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.rb$" ".html.haml" fn))
              (lambda (fn) (replace-regexp-in-string "\\.rb$" ".html.erb" fn))))

  (ruled-switch-buffer-define view-component-haml
    :matcher (lambda (fn) (string-match "_component.html.haml$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.html.haml$" ".rb" fn)))

  (ruled-switch-buffer-define view-component-erb
    :matcher (lambda (fn) (string-match "_component.html.erb$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.html.erb$" ".rb" fn)))

  (ruled-switch-buffer-define view-component-rb-no-ext
    :matcher (lambda (fn) (string-match "app/components/.*\\.rb$" fn))
    :mappers ((lambda (fn) (replace-regexp-in-string "\\.rb$" ".html.haml" fn))
              (lambda (fn) (replace-regexp-in-string "\\.rb$" ".html.erb" fn))))

  (ruled-switch-buffer-define view-component-haml-no-ext
    :matcher (lambda (fn) (string-match "app/components/.*\\.html\\.haml$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\.html.haml$" ".rb" fn)))

  (ruled-switch-buffer-define app-rb-to-spec-rb
    :matcher (lambda (fn) (string-match "/app/.*.rb$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\(.*\\)/app/\\(.*\\).rb$" "\\1/spec/\\2_spec.rb" fn)))

  (ruled-switch-buffer-define spec-rb-to-app-rb
    :matcher (lambda (fn) (string-match "/spec/.*_spec.rb$" fn))
    :mappers (lambda (fn) (replace-regexp-in-string "\\(.*\\)/spec/\\(.*\\)_spec.rb$" "\\1/app/\\2.rb" fn))))

(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode))

(use-package go-mode
  :ensure t
  :commands (go-mode))

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
(bind-key "M-k" 'kill-current-buffer)
(bind-key "M-r" 'git-grep-symbol-at-point)
(bind-key "C-c C-o" 'xdg-open-at-point)
;; (bind-key "M-]" 'bs-cycle-next)
;; (bind-key "M-[" 'bs-cycle-previous)

(bind-key "C-c g" 'revert-buffer)
(bind-key "C-c t" 'ruled-switch-buffer)
(bind-key "C-c m" 'insert-last-message)

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
 '(select-enable-clipboard t)
 '(ruled-switch-buffer-completing-read-fn 'ido-completing-read))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )




;; TODO: later
;; (add-hook 'kill-buffer-query-functions
;;     ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
;;           (function (lambda ()
;;                       (if (string= "*scratch*" (buffer-name))
;;                           (progn (my-make-scratch 0) nil)
;;                         t))))
