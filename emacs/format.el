(defun rufo-success-p (retcode)
  (member retcode '(0 3)))

(reformatter-define rufo
  :program "rufo"
  :lighter " Rufo"
  :exit-code-success-p rufo-success-p)

(reformatter-define eslint-fix
  :program "eslint_d"
  :lighter " ESLint"
  :args (list "--stdin" "--fix-to-stdout" "--stdin-filename" (buffer-file-name)))
