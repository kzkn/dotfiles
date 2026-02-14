(require 'cl-lib)
(require 'ffap)

(defun resize-window ()
  "Resize the current window."
  (interactive)
  (when (one-window-p)
    (error "Cannot resize sole window"))
  (catch 'done
    (while t
      (message "size[%dx%d]" (window-width) (window-height))
      (let ((c (read-char)))
        (condition-case nil
            (cond ((= c ?f) (enlarge-window-horizontally 2))
                  ((= c ?b) (shrink-window-horizontally 2))
                  ((= c ?n) (enlarge-window 2))
                  ((= c ?p) (shrink-window 2))
                  ((= c ?q)
                   (message "quit") (throw 'done t))
                  (t nil))
          (error nil))))))

(defun swap-windows ()
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (cl-first (window-list)))
                (w2 (cl-second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

(defvar git-grep-directory-history nil)

(defun git-project-p ()
  (string=
   (s-chomp (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun git-root-directory ()
  (cond ((git-project-p)
         (s-chomp (shell-command-to-string "git rev-parse --show-toplevel")))
        (t
         "")))

(defun git-grep-directory-previous-or-root (root)
  (if (s-starts-with-p root git-grep-directory-history)
      git-grep-directory-history
    root))

(defun git-grep (grep-dir command-args)
  (interactive
   (let ((root (concat (git-root-directory) "/")))
     (list (ido-read-directory-name "Directory for git grep: "
                                    (git-grep-directory-previous-or-root root) "." t)
           (read-shell-command "Run git-grep (like this): "
                               (format "PAGER='' git grep -I -n -i -e ")
                               'git-grep-history))))
  (setq git-grep-directory-history grep-dir)
  (let ((command (format "cd '%s' && %s" grep-dir command-args)))
    (grep command)))

(defun git-grep-symbol-at-point ()
  (interactive)
  (let ((sym (symbol-at-point)))
    (when sym
      (let* ((symbol-string (regexp-quote (symbol-name sym)))
             (command (format "cd '%s' && PAGER='' git grep -I -n -i '%s'"
                              (git-root-directory) symbol-string)))
        (grep command)))))

(defun rg (grep-dir command-args)
  (interactive
   (let ((root (concat (git-root-directory) "/")))
     (list (ido-read-directory-name "Directory for rg: " nil nil t)
           (read-shell-command "Run rg (like this): " "rg -n -i " 'rg-history))))
  (let ((command (format "cd '%s' && (%s | cat)" grep-dir command-args)))
    (grep command)))

(defun find-file-in-git-ls-files ()
  (interactive)
  (let* ((repo (git-root-directory))
         (files (shell-command-to-string (format "cd '%s' && git ls-files -z" repo)))
         (others (shell-command-to-string (format "cd '%s' && git ls-files -z --others --exclude-standard" repo)))
         (candidates (cl-remove-if (lambda (f) (string= f ""))
                                   (append (split-string files "\0")
                                           (split-string others "\0")))))
    (let ((selected-file (ido-completing-read "Find file: " candidates)))
      (find-file (concat repo "/" selected-file)))))

(defun sh (&optional arg)
  (interactive "P")
  (let* ((start-directory (if arg
                              (ido-read-directory-name "Starting directory: " default-directory)
                            default-directory))
         (tmux (executable-find "tmux"))
         (command (format "%s new-window -c %s" tmux start-directory)))
    (call-process-shell-command command)))

(defun my/exists-p (dir targets)
  (let ((default-directory dir))
    (cl-loop for target in targets
             if (file-exists-p target)
             return target)))

(defun my/find-to-root (start targets )
  (let ((prev "")
        (dir (file-name-as-directory start)))
    (while (and (not (string= dir prev))
                (not (my/exists-p dir targets)))
      (setq prev dir
            dir (file-name-directory (directory-file-name dir))))
    (and (not (string= dir prev)) dir)))

(defun enable-flycheck-if-parent-file-exists (file checker)
  (let* ((curdir (file-name-directory (buffer-file-name)))
         (exists (my/find-to-root curdir (list file))))
    (setq flycheck-checker checker)
    (flycheck-mode (if exists 1 0))))

(defun flycheck-skip-eslint-config-verification (orig-fn &rest args)
  t)

(defun ghq-cd ()
  (interactive)
  (let* ((dirs (shell-command-to-string "/usr/sbin/ghq list"))
         (ghq-root (shell-command-to-string "/usr/sbin/ghq root")))
    (find-file
     (concat (s-chomp ghq-root) "/"
             (ido-completing-read "Select directory: "
                                  (cl-remove-if (lambda (f) (string= f ""))
                                                (split-string dirs "\n")))))))

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun split-window-vertically-n (num-wins)
  (interactive "p")
  (if (= num-wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num-wins)))
      (split-window-vertically-n (- num-wins 1)))))

(defun split-window-horizontally-n (num-wins)
  (interactive "p")
  (if (= num-wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num-wins)))
      (split-window-horizontally-n (- num-wins 1)))))

(defun xdg-open (filename)
  (let ((xdg-open-command (executable-find "xdg-open")))
    (if (and filename xdg-open-command)
        (start-process "xdg-open-at-point" nil xdg-open-command filename))))

(defun xdg-open-at-point ()
  (interactive)
  (let ((filename (ffap-file-at-point)))
    (xdg-open filename)))

(defun current-project-gemfile-lock-path ()
  (let* ((curdir (file-name-directory (buffer-file-name)))
         (gemfile-lock-dir (my/find-to-root curdir (list "Gemfile.lock"))))
    (and gemfile-lock-dir
         (concat (file-name-as-directory gemfile-lock-dir) "Gemfile.lock"))))

(defun gem-in-gemfile-lock-p (gemname)
  (let* ((gemfile-lock-path (current-project-gemfile-lock-path)))
    (and gemfile-lock-path
         (with-temp-buffer
           (insert-file-contents gemfile-lock-path)
           (goto-char (point-min))
           (search-forward gemname nil t)))))

(defun my/wrap-ruby-rubocop (command)
  (if (gem-in-gemfile-lock-p "rubocop")
      (append '("bundle" "exec") command)
    command))

(defun my/wrap-haml-lint (command)
  (if (gem-in-gemfile-lock-p "haml_lint")
      (append '("bundle" "exec") command)
    command))

(defun my/flycheck-command-wrapper-function (command)
  (cond ((eq flycheck-checker 'ruby-rubocop)
         (my/wrap-ruby-rubocop command))
        ((eq flycheck-checker 'haml-lint)
         (my/wrap-haml-lint command))
        ;; TODO: まだ lsp になるでござる
        ((eq major-mode 'web-mode)
         (if (eslint-enable-current-buffer-p)
             (append '("npx") command)
           '("true")))
        (t
         command)))

(defun eslint-enable-p (file-name)
  (member (file-name-extension file-name) '("js" "ts" "jsx" "tsx")))

(defun eslint-enable-current-buffer-p ()
  (and (buffer-file-name) (eslint-enable-p (buffer-file-name))))

(defun eslint-fix-web-mode ()
  (when (eslint-enable-current-buffer-p)
    (eslint-fix-on-save-mode)))

(defun lsp-web-mode ()
  (when (eslint-enable-current-buffer-p)
    (lsp-deferred)))

(defun enable-ruby-flycheck-if-rubocop-yml-exists ()
  (enable-flycheck-if-parent-file-exists ".rubocop.yml" 'ruby-rubocop))

(defun rspec-last-compilation-failed-p ()
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^Failed examples:$" nil t) t nil)))

(defun notify-rspec-finish ()
  (let* ((failed (rspec-last-compilation-failed-p))
         (urgency (if failed "critical" "normal"))
         (message (if failed "Failed" "Succeeded")))
    (shell-command (format "notify-send --urgency %s 'RSpec completed' '%s'" urgency message))))

(defun rspec-runner--lang-ja-jp (orig-fn &rest args)
  (concat "LANG=ja_JP.utf8 " (apply orig-fn args)))

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

(defun my/ssass-indent ()
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

(defun bundle-open (gem-name)
  (interactive
   (let* ((gems (shell-command-to-string "bundle list --name-only"))
          (candidates (split-string gems "\n" t)))
     (list (ido-completing-read "Gem: " candidates))))
  (find-file (s-chomp (shell-command-to-string (concat "bundle info " gem-name " --path")))))

;; TODO: later
;; (defun project-git-repository-p (directory)
;;   (and (my/find-to-root directory (list ".git")) t))

;; (defun project-rubygems-p ()
;;   (and (my/find-to-root directory (list ".git")) t))

;; (defun project-page-open ()
;;   (interactive)
;;   (let ((curdir (file-name-directory (buffer-file-name)))
;;         (cond ((project-git-repository-p curdir)
;;                )
;;                (xdg-

;; SEE: https://unix.stackexchange.com/a/154154
(defun last-message ()
  (save-excursion
    (set-buffer "*Messages*")
    (save-excursion
      (forward-line -2)
      (backward-char)
      (let ((end (point)))
        (forward-line 0)
        (buffer-substring-no-properties (point) end)))))

(defun insert-last-message ()
  (interactive)
  (insert (last-message)))

(defun my/ruby-beginning-of-thing ()
  (interactive)
  (goto-char (sp-get (sp-get-thing t) :beg)))

(defun my/ruby-end-of-thing ()
  (interactive)
  (goto-char (sp-get (sp-get-thing) :end)))

(defun create-scratch-buffer ()
   (interactive)
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (lisp-interaction-mode))

;; SEE: https://www.reddit.com/r/emacs/comments/1d9d1ob/getting_emacs_terminal_transparent/
(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))
