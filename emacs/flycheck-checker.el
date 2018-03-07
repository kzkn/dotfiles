;; Local Variables:
;; eval: (auto-async-byte-compile-mode 0)
;; End:

(flycheck-def-config-file-var flycheck-haml-lintrc haml-lint ".haml-lint.yml"
  :safe #'stringp)

(flycheck-define-checker haml-lint
  "A haml-lint syntax checker"
  :command ("haml-lint"
            (config-file "--config" flycheck-haml-lintrc)
            source)
  :error-patterns
  ((warning line-start
            (file-name) ":" line " [W] "  (message)
            line-end))
  :modes haml-mode)
