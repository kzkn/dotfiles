;; 実際したい機能
;;  - [ ] 一覧をキレイに
;;  - [ ] 一覧に git のコミットを表示できる

(defvar rspecr--result-file-prefix "rspecr-")
(defvar rspecr--result-dir-name "rspec-results")

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

(defun rspecr--mtime (file)
  (let* ((attrs (file-attributes file)))
    (file-attribute-modification-time attrs)))

(defun rspecr--result-< (file1 file2)
  (let ((mtime1 (rspecr--mtime file1))
        (mtime2 (rspecr--mtime file2)))
    (time-less-p mtime1 mtime2)))

(defun rspecr--list-result-files (dir)
  (let ((files (directory-files dir))
        f result-files)
    (while files
      (setq f (car files))
      (when (rspecr--rspec-result-file-p f)
        (push (concat dir f) result-files))
      (setq files (cdr files)))
    (reverse (sort result-files 'rspecr--result-<))))

(defun rspecr--result-files ()
  (let ((dir (rspecr--rspec-result-files-directory)))
    (and (file-directory-p dir) (rspecr--list-result-files dir))))

(defun rspecr--insert-header-line ()
  (let ((p (point)))
    (insert "SUCCESS FAILURE PENDING")
    (move-to-column 25 t)
    (insert "PROJECT")
    (move-to-column 45 t)
    (insert "STARTED")
    (newline 1)
    (insert "------- ------- -------")
    (move-to-column 25 t)
    (insert "-------")
    (move-to-column 45 t)
    (insert "-------")
    (put-text-property p (point) 'rspecr-header t)
    (put-text-property p (point) 'face font-lock-type-face))
  (newline 1))

(defun rspecr--insert-result-file-line (file)
  (let* ((count (rspecr--count-spec-results file))
         (total (car count))
         (failures (cadr count))
         (pendings (caddr count))
         (successes (- total failures pendings)))
    (let ((p (point)))
      (insert (format "%7d" successes))
      (put-text-property p (point) 'face compilation-info-face))
    (let ((p (point)))
      (insert " " (format "%7d" failures))
      (put-text-property p (point) 'face compilation-error-face))
    (let ((p (point)))
      (insert " " (format "%7d" pendings))
      (put-text-property p (point) 'face font-lock-function-name-face))
    (move-to-column 25 t)
    (let ((p (point)))
      (insert (rspecr--project-root file))
      (put-text-property p (point) 'rspecr-result-file-name file))
    (move-to-column 45 t)
    (insert (rspecr--started-at file))))

(defun rspecr--refresh ()
  (let ((files (rspecr--result-files))
        (buf (get-buffer-create " *RSpec Results*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (rspecr--insert-header-line)
        (let (f)
          (while files
            (setq f (car files))
            (rspecr--insert-result-file-line f)
            (newline 1)
            (setq files (cdr files))))))
    buf))

(defun rspecr--show-list ()
  (switch-to-buffer (rspecr--refresh))
  (goto-char (point-min)))

(defun rspecr--current-line-file-path ()
  (save-excursion
    (let ((bol (progn (beginning-of-line) (point))))
      (when (get-text-property bol 'rspecr-header)
        (error "Unselectable"))
      (let ((p (next-single-property-change bol 'rspecr-result-file-name)))
        (get-text-property p 'rspecr-result-file-name)))))

(defun rspecr--count-spec-results (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-max))
    (re-search-backward "^\\([0-9]+\\) examples?\\(, \\([0-9]+\\) failures?\\(, \\([0-9]+\\) pendings?\\)?\\)")
    (let ((total (match-string-no-properties 1))
          (failures (and (match-beginning 3) (match-string-no-properties 3)))
          (pendings (and (match-beginning 5) (match-string-no-properties 5))))
      (mapcar (lambda (n)
                (if (null n) 0 (string-to-number n)))
              (list total failures pendings)))))

(defun rspecr--project-root (file)
  (with-temp-buffer
    (insert-file-contents file)
    (narrow-to-region (point) (progn (forward-line) (point)))
    (goto-char (point-min))
    (re-search-forward "; default-directory: \"\\(.*\\)\" -\\*-$")
    (let* ((default-dir (match-string-no-properties 1))
           (root (rspec-project-root default-dir)))
      (file-name-nondirectory (directory-file-name root)))))

(defun rspecr--started-at (file)
  (with-temp-buffer
    (insert-file-contents file)
    (narrow-to-region (progn (forward-line) (point)) (progn (forward-line) (point)))
    (goto-char (point-min))
    (search-forward "RSpec Compilation started at ")
    (buffer-substring (point) (progn (end-of-line) (point)))))

(defun rspecr-save-rspec-result ()
  (let ((path (rspecr--make-rspec-result-path)))
    (write-region (point-min) (point-max) path)
    path))

(defun rspecr-next-line (n)
  (interactive "p")
  (forward-line n))

(defun rspecr-previous-line (n)
  (interactive "p")
  (rspecr-next-line (- n)))

(defun rspecr-show ()
  (interactive)
  (let ((f (rspecr--current-line-file-path)))
    (find-file f)))

(defun rspecr-delete-result-file ()
  (interactive)
  (let ((f (rspecr--current-line-file-path)))
    (when (y-or-n-p (format "Delete %s " f))
      (delete-file f)
      (rspecr--refresh))))

(defun rspecr-refresh ()
  (interactive)
  (rspecr--refresh))

(defvar rspecr-mode-map (make-sparse-keymap))
(define-key rspecr-mode-map "n" 'rspecr-next-line)
(define-key rspecr-mode-map "p" 'rspecr-previous-line)
(define-key rspecr-mode-map "q" 'kill-this-buffer)
(define-key rspecr-mode-map "D" 'rspecr-delete-result-file)
(define-key rspecr-mode-map "g" 'rspecr-refresh)
(define-key rspecr-mode-map "\C-m" 'rspecr-show)

(defun rspecr ()
  (interactive)
  (add-hook 'rspec-after-verification-hook 'rspecr-save-rspec-result)
  (rspecr--show-list)
  (use-local-map rspecr-mode-map)
  (setq major-mode 'rspecr-mode
        mode-name "RSpec Result"
        buffer-read-only t))
