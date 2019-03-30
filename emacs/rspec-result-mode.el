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

(defun rspecr--list-result-files (dir)
  (let ((files (directory-files dir))
        f result-files)
    (while files
      (setq f (car files))
      (when (rspecr--rspec-result-file-p f)
        (push (concat dir f) result-files))
      (setq files (cdr files)))
    result-files))

(defun rspecr--result-files ()
  (let ((dir (rspecr--rspec-result-files-directory)))
    (and (file-directory-p dir) (rspecr--list-result-files dir))))

(defun rspecr--print-info (file)
  (let* ((count (rspecr--count-spec-results file))
         (total (car count))
         (failures (cadr count))
         (pendings (caddr count))
         (successes (- total failures pendings)))
    (list file
          `[,(rspecr--started-at file)
            (,(rspecr--project-root file)
             rspecr-result-file-name ,file)
            (,(number-to-string successes)
             face ,compilation-info-face)
            (,(number-to-string failures)
             face ,compilation-error-face)
            (,(number-to-string pendings)
             face ,font-lock-function-name-face)
            ])))

(defun rspecr--refresh ()
  (let ((files (rspecr--result-files)))
    (tabulated-list-init-header)
    (setq tabulated-list-entries (mapcar 'rspecr--print-info files))))

(defun rspecr--current-line-file-path ()
  (tabulated-list-get-id))

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

(define-derived-mode rspec-result-list-mode tabulated-list-mode "RSpec Result"
  (setq tabulated-list-format `[("Started" 19 t)
                                ("Project" 20 t)
                                ("Succ" 4 nil)
                                ("Fail" 4 nil)
                                ("Pend" 4 nil)])
  (setq tabulated-list-sort-key (cons "Started" t))
  (add-hook 'tabulated-list-revert-hook 'rspecr--refresh nil t)
  (tabulated-list-init-header))

(defun rspecr-save-rspec-result ()
  (let ((path (rspecr--make-rspec-result-path)))
    (write-region (point-min) (point-max) path)
    path))

(defun rspecr-show ()
  (interactive)
  (let ((f (rspecr--current-line-file-path)))
    (find-file f)))

(defun rspecr-delete-result-file ()
  (interactive)
  (let ((f (rspecr--current-line-file-path)))
    (when (y-or-n-p (format "Delete %s " f))
      (delete-file f)
      (tabulated-list-revert))))

(defvar rspec-result-list-mode-map (make-sparse-keymap))
(define-key rspec-result-list-mode-map "D" 'rspecr-delete-result-file)
(define-key rspec-result-list-mode-map "\C-m" 'rspecr-show)

(defun rspecr ()
  (interactive)
  (let ((buf (get-buffer-create "*RSpec Results*")))
    (with-current-buffer buf
      (rspec-result-list-mode)
      (rspecr--refresh)
      (tabulated-list-print))
    (switch-to-buffer buf)))
