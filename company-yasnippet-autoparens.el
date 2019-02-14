;;; company-yasnippet-autoparens.el --- company-mode completion backend for Yasnippet (Auto-parenthesis Extension)

;; Author: Siyuan Chen <chansey97@gmail.com>

(require 'company)
(require 'cl-lib)

(declare-function yas-expand-snippet "yasnippet")
(declare-function yas--template-content "yasnippet")
(declare-function yas--template-expand-env "yasnippet")

(defun company-yasnippet-autoparens--candidates (prefix rest-args)

  (let ((res))
    (let ((pre-backends (car rest-args)))
      (cl-loop for backend in pre-backends
               do (let ((candidates (car backend) ))
                    (cl-loop for candidate in candidates
                             do (when (stringp candidate)
                                  (let* ((key candidate)
                                         (paren-key (concat "(" key ")"))
                                         (template (yas--make-template :table       nil
                                                                       :key         key 
                                                                       :content     (concat "(" key "$1)")
                                                                       :name        paren-key
                                                                       :uuid        paren-key)))
                                    (push (propertize  key
                                                       'yas-annotation paren-key
                                                       'yas-template template
                                                       'yas-prefix-offset 0)
                                          res)))))))

    (let* ((key prefix)
           (paren-key (concat "(" key ")"))
           (template (yas--make-template :table       nil
                                         :key         key 
                                         :content     (concat "(" key "$1)")
                                         :name        paren-key
                                         :uuid        paren-key)))
      (push (propertize  key
                         'yas-annotation paren-key
                         'yas-template template
                         'yas-prefix-offset 0)
            res)
      (push key
            res))))

;;;###autoload
(defun company-yasnippet-autoparens (command &optional arg &rest ignore)
  "`company-mode' backend for `yasnippet'.

This backend should be used with care, because as long as there are
snippets defined for the current major mode, this backend will always
shadow backends that come after it.  Recommended usages:

* In a buffer-local value of `company-backends', grouped with a backend or
  several that provide actual text completions.

  (add-hook 'js-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-dabbrev-code company-yasnippet-autoparens)))))

* After keyword `:with', grouped with other backends.

  (push '(company-semantic :with company-yasnippet-autoparens) company-backends)

* Not in `company-backends', just bound to a key.

  (global-set-key (kbd \"C-c y\") 'company-yasnippet-autoparens)
"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-yasnippet-autoparens))
    (prefix
     (and (bound-and-true-p yas-minor-mode)
          (company-grab-symbol)))
    (annotation
     (let ((annotation (get-text-property 0 'yas-annotation arg)))
       (when annotation
         (concat
          (unless company-tooltip-align-annotations " -> ")
          annotation))))
    (candidates (company-yasnippet-autoparens--candidates arg ignore))
    (no-cache t)
    (post-completion
     (let ((template (get-text-property 0 'yas-template arg))
           (prefix-offset (get-text-property 0 'yas-prefix-offset arg)))
       (when template
         (yas-expand-snippet (yas--template-content template)
                             (- (point) (length arg) prefix-offset)
                             (point)
                             (yas--template-expand-env template))))
     )))

(provide 'company-yasnippet-autoparens)
;;; company-yasnippet-autoparens.el ends here


