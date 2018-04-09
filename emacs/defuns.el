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
        (goto-char p)))))

(defun vim-f (&optional char)
  (interactive)
  (%vim-ft char 'f))

(defun vim-t (&optional char)
  (interactive)
  (%vim-ft char 't))

(defun my/split-windows (count)
  (delete-other-windows)
  (save-selected-window
    (ecase count
      (2
       (split-window-horizontally))
      (3
       (split-window-horizontally)
       (select-window (nth 1 (window-list)))
       (split-window-vertically))
      (4
       (split-window-horizontally)
       (split-window-vertically)
       (select-window (nth 2 (window-list)))
       (split-window-vertically)))))

(defun my/open-in-window (file-name window)
  (save-selected-window
    (select-window window)
    (find-file file-name)))

(defun my/build-rails-controller-candidates (modules resource)
  (list (concat "app/controllers/" (s-join "/" modules) "/" (inflection-pluralize-string resource) "_controller.rb")))

(defun my/build-rails-model-candidates (resource)
  (list (concat "app/models/" (inflection-singularize-string resource) ".rb")))

(defun my/build-rails-view-candidates (modules resource action)
  (let ((dir (concat "app/views/" (s-join "/" modules) "/" (inflection-pluralize-string resource) "/")))
    (list (concat dir action ".html.haml")
          (concat dir action ".html.erb")
          (concat dir action ".js.erb"))))

(defun my/build-rails-system-spec-candidates (module resource)
  (let ((spec-rb (concat (s-join "/" modules) "/" (inflection-pluralize-string resource) "_spec.rb")))
    (list (concat "spec/features/" spec-rb)
          (concat "spec/system/" spec-rb))))

(defun my/build-rails-resource-candidates (action-path)
  (let* ((elems (s-split "/" action-path))
         (modules (butlast elems 2))
         (resource (car (last elems 2)))
         (action (cadr (last elems 2))))
    (list :controllers (my/build-rails-controller-candidates modules resource)
          :models (my/build-rails-model-candidates resource)
          :views (my/build-rails-view-candidates modules resource action)
          :specs (my/build-rails-system-spec-candidates modules resource))))

(defun my/find-rails-app-root-dir (dir)
  (cond ((file-exists-p (expand-file-name "Gemfile" dir)) dir)
        ((string= dir "/") "/")
        (t (my/find-rails-app-root-dir (file-name-directory (directory-file-name dir))))))

(defun my/resolve-open-rails-resource-path (rel-file-name)
  (let* ((dir (my/find-rails-app-root-dir default-directory)))
    (expand-file-name rel-file-name dir)))

(defun my/find-files-in-tile (paths)
  (my/split-windows (length paths))
  (cl-loop for path in paths
           for window in (window-list)
           do (my/open-in-window path window)))

(defun my/open-rails-resources (action-path)
  (let* ((candidates (my/build-rails-resource-candidates action-path))
         (controller (car (getf candidates :controllers)))
         (model (car (getf candidates :models)))
         (view (car (getf candidates :views)))
         (spec (car (getf candidates :specs))))
    (let* ((windows (window-list))
           (resolved-paths (mapcar 'my/resolve-open-rails-resource-path (list controller model view spec)))
           (available-paths (remove-if-not 'file-exists-p resolved-paths))
           (available-path-count (length available-paths)))
      (cond ((< 1 available-path-count)
             (my/find-files-in-tile available-paths))
            ((= 1 available-path-count)
             (find-file (car available-paths)))
            (t
             (message "No matches"))))))

(defun open-rails-resources (action-path)
  (interactive
   (list (read-string "Action path: ")))
  (my/open-rails-resources action-path))
