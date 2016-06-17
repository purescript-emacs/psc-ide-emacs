;;; psc-ide-flycheck.el --- Flycheck support for the purescript language -*- lexical-binding: t -*-

;; Copyright (c) 2015 The psc-ide-emacs authors

;; Author: Brian Sermons
;; Package-Requires: ((flycheck "0.24") (emacs "24.4") (let-alist "1.0.4") (psc-ide "0.1.0") (dash "2.12.0") (seq "1.11"))
;; URL: https://github.com/epost/psc-ide-emacs

;;; Commentary:

;; Usage:
;;
;;     (eval-after-load 'flycheck
;;       '(add-hook 'flycheck-mode-hook #'psc-ide-flycheck-setup))

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist))

(require 'seq)
(require 'json)
(require 'dash)
(require 'flycheck)
(require 'psc-ide-protocol)


(flycheck-def-option-var psc-ide-flycheck-ignored-error-codes nil psc-ide
  "List of errors codes to ignore."
  :tag "Flycheck PscIde Ignored Error Codes"
  :type '(repeat string))

(defun psc-ide-flycheck-parse-errors (data checker)
  "Decode purescript json output errors from DATA with CHECKER."
  (let-alist data
    (let ((errors)
          (resultType (pcase .resultType
                        (`"success" 'warning)
                        (_ 'error))))
      (seq-do (lambda (err)
                (let-alist err
                  (unless (member .errorCode psc-ide-flycheck-ignored-error-codes)
                    ;; HACK: for insert suggestions
                    (put-text-property 0 1 :suggestion .suggestion .errorCode)
                    (put-text-property 0 1 :endLine .position.endLine .errorCode)
                    (put-text-property 0 1 :endColumn .position.endColumn .errorCode)
                    (when .suggestion
                      (setq .message (concat .message " ●")))
                    (push (flycheck-error-new-at
                           .position.startLine
                           .position.startColumn
                           resultType
                           .message
                           :id .errorCode
                           :checker checker
                           :filename .filename)
                          errors))))
              .result)
      errors)))

;;;###autoload
(defun psc-ide-flycheck-insert-suggestion ()
  "Replace error with suggestion from psc compiler."
  (interactive)
  (-if-let* ((flycheck-err (car (flycheck-overlay-errors-at (point))))
             (suggestion   (get-text-property 0 :suggestion (flycheck-error-id flycheck-err)))
             (endLine      (get-text-property 0 :endLine    (flycheck-error-id flycheck-err)))
             (endColumn    (get-text-property 0 :endColumn  (flycheck-error-id flycheck-err))))
      (let* ((start (save-excursion
                      (goto-char (point-min))
                      (forward-line (- (flycheck-error-line flycheck-err) 1))
                      (move-to-column (- (flycheck-error-column flycheck-err) 1))
                      (point)))
             (end (save-excursion
                    (goto-char (point-min))
                    (forward-line (- endLine 1))
                    (move-to-column (- endColumn 1))
                    (point))))
        (progn
          (kill-region start end)
          (goto-char start)
          (let ((new-end
                 (save-excursion
                   (let-alist suggestion
                     (insert .replacement))
                   (point))))
            (set-mark start)
            (goto-char new-end)
            (deactivate-mark))
          (save-buffer)
          (flycheck-buffer)))
    (message "No suggestion available")))

(define-key purescript-mode-map (kbd "C-c M-s")
  'psc-ide-flycheck-insert-suggestion)

(defun psc-ide-flycheck-start (checker callback)
  "Start a psc-ide syntax check with CHECKER.

CALLBACK is the status callback passed by flycheck."
  (psc-ide-send (psc-ide-command-rebuild)
                (lambda (result)
                  (condition-case err
                      (progn
                        (let ((errors (psc-ide-flycheck-parse-errors result checker)))
                          (funcall callback 'finished errors)))
                    (`(error debug)
                     (funcall callback 'errored (error-message-string err)))))))

(flycheck-define-generic-checker 'psc-ide
  "A purescript syntax checker using the `psc-ide' interface."
  :start #'psc-ide-flycheck-start
  :modes '(purescript-mode))


;;;###autoload
(defun psc-ide-flycheck-setup ()
  "Setup Flycheck purescript."
  (interactive)
  (add-to-list 'flycheck-checkers 'psc-ide))

(provide 'psc-ide-flycheck)
;;; psc-ide-flycheck.el ends here
