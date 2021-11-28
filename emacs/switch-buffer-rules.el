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
