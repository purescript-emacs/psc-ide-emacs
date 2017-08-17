;; License: GNU General Public License version 3, or (at your option) any later version

(require 'compile)
(require 'purescript-mode) ;; needed for 'purescript-ident-at-point'

(add-hook 'purescript-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command)
                 (format "pulp --monochrome build" (file-name-nondirectory buffer-file-name)))))

;; Match psc errors
(add-to-list 'compilation-error-regexp-alist
  '("\\(Error\\|Warnin\\g\\) at \\([^ \n]+\\) line \\([0-9]+\\), column \\([0-9]+\\) - line \\([0-9]+\\), column \\([0-9]+\\):"
    2 (3 . 5)  (4 . 6) nil
))

(defun purescript-pursuit (query &optional info)
  "Do a Pursuit search for QUERY.
When `purescript-pursuit-command' is non-nil, this command runs
that.  Otherwise, it opens a Pursuit search result in the browser.
If prefix argument INFO is given, then `purescript-pursuit-command'
is asked to show extra info for the items matching QUERY."
  (interactive
   (let ((def (purescript-ident-at-point)))
     (if (and def (symbolp def)) (setq def (symbol-name def)))
     (list (read-string (if def
                            (format "Pursuit query (default %s): " def)
                          "Pursuit query: ")
                        nil nil def)
           current-prefix-arg)))
  (browse-url (format "http://pursuit.purescript.org/search?q=%s" query)))
