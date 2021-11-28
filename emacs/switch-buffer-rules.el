;; (require 'rule-based-switch-buffer "./rule-based-switch-buffer.el")
(require 'rule-based-switch-buffer)

(rule-based-switch-buffer-define view-component-rb
  :matcher (lambda (fn) (string-match "_component.rb$" fn))
  :mappers ((lambda (fn) (replace-regexp-in-string "\\.rb$" ".html.haml" fn))
            (lambda (fn) (replace-regexp-in-string "\\.rb$" ".html.erb" fn))))

(rule-based-switch-buffer-define view-component-haml
  :matcher (lambda (fn) (string-match "_component.html.haml$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\.html.haml$" ".rb" fn)))

(rule-based-switch-buffer-define view-component-erb
  :matcher (lambda (fn) (string-match "_component.html.erb$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\.html.erb$" ".rb" fn)))

(rule-based-switch-buffer-define app-rb-to-spec-rb
  :matcher (lambda (fn) (string-match "/app/.*.rb$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\(.*\\)/app/\\(.*\\).rb$" "\\1/spec/\\2_spec.rb" fn)))

(rule-based-switch-buffer-define spec-rb-to-app-rb
  :matcher (lambda (fn) (string-match "/spec/.*_spec.rb$" fn))
  :mappers (lambda (fn) (replace-regexp-in-string "\\(.*\\)/spec/\\(.*\\)_spec.rb$" "\\1/app/\\2.rb" fn)))
