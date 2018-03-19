(eval-when-compile
  (require 'cl))

(defun google ()
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (if (region-active-p)
        (buffer-substring (region-beginning) (region-end))
      (read-string "Query: ")))))

(defun buffer-current-line-string ()
  (save-excursion
    (buffer-substring (progn (beginning-of-line) (point))
                      (progn (end-of-line) (point)))))

(defun buffer-current-line-starts-with (prefix)
  (let ((line (buffer-current-line-string)))
    (and (<= (length prefix) (length line))
         (equal prefix (substring line 0 (length prefix))))))

(defun open-path ()
  (interactive)
  (save-excursion
    (let ((path (if (%thunar-path-p)
                    (buffer-current-line-string)
                  (%win-to-thunar))))
      (start-process "open-path" nil "/usr/bin/thunar" path))))

(defun %thunar-path-p ()
  (buffer-current-line-starts-with "smb:"))

(defun %win-to-thunar ()
  (let ((orig-path (buffer-current-line-string)))
    (with-temp-buffer
      (insert orig-path)
      (%replace-path-separators)
      (buffer-string))))

(defun %replace-path-separators ()
  (let ((end (progn (end-of-line) (point))))
    (beginning-of-line)
    (insert "smb:")
    (while (and (search-forward "\\" nil t) (<= (point) end))
      (replace-match "/"))))

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
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
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
  (or (and git-grep-directory-history
           (string= (substring git-grep-directory-history 0 (length root))
                    root)
           git-grep-directory-history)
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

(defun find-file-in-git-ls-files ()
  (interactive)
  (let* ((repo (git-root-directory))
         (files (shell-command-to-string (format "cd %s && git ls-files" repo))))
    (find-file
     (concat repo "/"
             (ido-completing-read "Find file: "
                                  (cl-remove-if (lambda (f) (string= f ""))
                                                (split-string files "\n")))))))

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
    (loop for target in targets
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

(defvar *vim-ft-last-char*)

(defun %vim-ft (char f-or-t)
  (let ((forward (eq f-or-t 'f)))
    (unless char
      (if (eq last-command (if forward 'vim-f 'vim-t))
          (setq char *vim-ft-last-char*)
        (setq char (read-char "Character: "))))
    (setq *vim-ft-last-char* char)

    (let* ((search-fn (if forward 'search-forward 'search-backward))
           (bound-fn (if forward 'point-at-eol 'point-at-bol))
           (off (if forward 1 0))
           (p (save-excursion
                (forward-char off)
                (let ((ep (funcall bound-fn)))
                  (funcall search-fn (char-to-string char) ep nil)))))
      (when p
        (goto-char (if forward (1- p) p))))))

(defun vim-f (&optional char)
  (interactive)
  (%vim-ft char 'f))

(defun vim-t (&optional char)
  (interactive)
  (%vim-ft char 't))
