(eval-when-compile (require 'cl-lib))

(defvar rspecr--result-file-prefix "rspecr-")
(defvar rspecr--result-dir-name "rspec-results")

(cl-defstruct rspec-result-desc
  path
  project
  finished-at
  successes
  failures
  pendings
  commit-desc)

(defun rspecr--rspec-result-files-directory ()
  (let ((dir (expand-file-name
              (concat user-emacs-directory
                      (file-name-as-directory rspecr--result-dir-name)))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun rspecr--make-rspec-result-path ()
  (let* ((temporary-file-directory (rspecr--rspec-result-files-directory)))
    (make-temp-file rspecr--result-file-prefix)))

(defun rspecr--rspec-result-file-p (f)
  (string-prefix-p rspecr--result-file-prefix f))

(defun rspecr--read-persisted-result (file)
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun rspecr--persisted-result-result (pr) (nth 0 pr))
(defun rspecr--persisted-result-commit-desc (pr) (nth 1 pr))
(defun rspecr--persisted-result-finished-at (pr) (nth 2 pr))

(defun rspecr--rspec-result-project ()
  (save-restriction
    (narrow-to-region (point) (progn (forward-line) (point)))
    (goto-char (point-min))
    (re-search-forward "; default-directory: \"\\(.*\\)\" -\\*-$")
    (let* ((default-dir (match-string-no-properties 1))
           (root (rspec-project-root default-dir)))
      (file-name-nondirectory (directory-file-name root)))))

(defun rspecr--rspec-result-counts ()
  (goto-char (point-max))
  (re-search-backward "^\\([0-9]+\\) examples?\\(, \\([0-9]+\\) failures?\\(, \\([0-9]+\\) pendings?\\)?\\)")
  (let ((total (match-string-no-properties 1))
        (failures (and (match-beginning 3) (match-string-no-properties 3)))
        (pendings (and (match-beginning 5) (match-string-no-properties 5))))
    (mapcar (lambda (n)
              (if (null n) 0 (string-to-number n)))
            (list total failures pendings))))

(defun rspecr--make-rspec-result-desc (file)
  (let ((persisted-result (rspecr--read-persisted-result file)))
    (with-temp-buffer
      (insert (rspecr--persisted-result-result persisted-result))
      (goto-char (point-min))
      (let* ((project (rspecr--rspec-result-project))
             (finished-at (rspecr--persisted-result-finished-at persisted-result))
             (counts (rspecr--rspec-result-counts))
             (total (car counts))
             (failures (cadr counts))
             (pendings (caddr counts))
             (successes (- total failures pendings)))
        (make-rspec-result-desc
         :path file
         :project project
         :finished-at finished-at
         :successes successes
         :failures failures
         :pendings pendings
         :commit-desc (rspecr--persisted-result-commit-desc persisted-result))))))

(defun rspecr--list-result-descriptions (dir)
  (let ((files (directory-files dir))
        f descs)
    (while files
      (setq f (car files))
      (when (rspecr--rspec-result-file-p f)
        (push (rspecr--make-rspec-result-desc (concat dir f)) descs))
      (setq files (cdr files)))
    descs))

(defun rspecr--result-descriptions ()
  (let ((dir (rspecr--rspec-result-files-directory)))
    (and (file-directory-p dir) (rspecr--list-result-descriptions dir))))

(defun rspecr--print-info (desc)
  (list desc
        `[,(format-time-string "%F %T" (rspec-result-desc-finished-at desc))
          ,(rspec-result-desc-project desc)
          (,(number-to-string (rspec-result-desc-successes desc))
           face ,compilation-info-face)
          (,(number-to-string (rspec-result-desc-failures desc))
           face ,compilation-error-face)
          (,(number-to-string (rspec-result-desc-pendings desc))
           face ,font-lock-function-name-face)
          ,(rspec-result-desc-commit-desc desc)
          ]))

(defun rspecr--refresh ()
  (let ((descs (rspecr--result-descriptions)))
    (tabulated-list-init-header)
    (setq tabulated-list-entries (mapcar 'rspecr--print-info descs))))

(defun rspecr--current-line-file-path ()
  (rspec-result-desc-path (tabulated-list-get-id)))

(defun rspecr--show-persisted-rspec-result (file)
  (let ((persisted-result (rspecr--read-persisted-result file)))
    (let ((buf (get-buffer-create "*RSpec Result*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (rspecr--persisted-result-result persisted-result))
          (hack-local-variables)  ; apply file local variables
          (goto-char (point-min))))
      (switch-to-buffer buf))))

(defun rspecr--finished-at-less-p (a b)
  (time-less-p (rspec-result-desc-finished-at (car a))
               (rspec-result-desc-finished-at (car b))))

(define-derived-mode rspec-result-list-mode tabulated-list-mode "RSpec Result"
  (setq tabulated-list-format `[("Finished" 19 rspecr--finished-at-less-p)
                                ("Project" 18 t)
                                ("Succ" 4 nil)
                                ("Fail" 4 nil)
                                ("Pend" 4 nil)
                                ("Commit" 0 nil)])
  (setq tabulated-list-sort-key (cons "Finished" t))
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook 'rspecr--refresh nil t)
  (tabulated-list-init-header))

(defun rspecr--buffer-substring-first-line ()
  (save-excursion
    (goto-char (point-min))
    (buffer-substring (point) (progn (end-of-line) (point)))))

(defun rspecr--current-git-commit-desc ()
  (with-temp-buffer
    (call-process "git" nil (current-buffer) nil "log" "-1" "--oneline")
    (rspecr--buffer-substring-first-line)))

(defun rspecr-save-rspec-result ()
  (let ((result (buffer-substring-no-properties (point-min) (point-max)))
        (commit-desc (rspecr--current-git-commit-desc))
        (path (rspecr--make-rspec-result-path))
        (time (current-time)))
    (with-temp-buffer
      (prin1 (list result commit-desc time) (current-buffer))
      (write-file path))
    path))

(defun rspecr-show ()
  (interactive)
  (let ((f (rspecr--current-line-file-path)))
    (rspecr--show-persisted-rspec-result f)))

(defun rspecr-mark-delete ()
  (interactive)
  (tabulated-list-put-tag "D" t))

(defun rspecr-unmark ()
  (interactive)
  (tabulated-list-put-tag " " t))

(defun rspecr-unmark-all ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (tabulated-list-put-tag " " t))))

(defun rspecr--delete-current-line-file ()
  (let ((f (rspecr--current-line-file-path)))
    (delete-file f)))

(defun rspecr--execute-current-line ()
  (let ((cmd (char-after)))
    (case cmd
      (?\D (rspecr--delete-current-line-file)))))

(defun rspecr-execute ()
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    (rspecr--execute-current-line)
    (forward-line))
  (tabulated-list-revert))

(defvar rspec-result-list-mode-map (make-sparse-keymap))
(define-key rspec-result-list-mode-map "d" 'rspecr-mark-delete)
(define-key rspec-result-list-mode-map "u" 'rspecr-unmark)
(define-key rspec-result-list-mode-map "U" 'rspecr-unmark-all)
(define-key rspec-result-list-mode-map "x" 'rspecr-execute)
(define-key rspec-result-list-mode-map "\C-m" 'rspecr-show)

(defun rspecr ()
  (interactive)
  (let ((buf (get-buffer-create "*RSpec Results*")))
    (with-current-buffer buf
      (rspec-result-list-mode)
      (rspecr--refresh)
      (tabulated-list-print))
    (switch-to-buffer buf)))
