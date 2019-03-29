(require 'cl-lib)

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
  (let ((command (format "cd %s && %s" grep-dir command-args)))
    (grep command)))

(defun git-grep-symbol-at-point ()
  (interactive)
  (let ((sym (symbol-at-point)))
    (when sym
      (let* ((symbol-string (regexp-quote (symbol-name sym)))
             (command (format "cd %s && PAGER='' git grep -I -n -i '%s'"
                              (git-root-directory) symbol-string)))
        (grep command)))))

(defun find-file-in-git-ls-files ()
  (interactive)
  (let* ((repo (git-root-directory))
         (files (shell-command-to-string (format "cd %s && git ls-files" repo)))
         (others (shell-command-to-string (format "cd %s && git ls-files --others --exclude-standard" repo))))
    (find-file
     (concat repo "/"
             (ido-completing-read "Find file: "
                                  (cl-remove-if (lambda (f) (string= f ""))
                                                (append (split-string files "\n")
                                                        (split-string others "\n"))))))))

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

(defun my/find-to-root (start targets)
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

(defun ghq-cd ()
  (interactive)
  (let* ((dirs (shell-command-to-string "/home/kazuki/go/bin/ghq list"))
         (ghq-root (shell-command-to-string "/home/kazuki/go/bin/ghq root")))
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
