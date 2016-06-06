;;; psc-ide-flycheck.el --- Flycheck support for the purescript language -*- lexical-binding: t -*-

;; Copyright (c) 2015 The psc-ide-emacs authors

;; Author: Brian Sermons
;;         Bodil Stokke <bodil@bodil.org>
;; Package-Requires: ((flycheck "0.24") (emacs "24.4"))
;; URL: https://github.com/epost/psc-ide-emacs

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Usage: (eval-after-load 'flycheck
;;;          '(add-hook 'flycheck-mode-hook #'psc-ide-flycheck-setup))

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'flycheck)


(flycheck-def-option-var psc-ide-flycheck-ignored-error-codes nil psc-ide
  "List of errors codes to ignore."
  :tag "Flycheck PscIde Ignored Error Codes"
  :type '(repeat string))

(defun psc-ide-flycheck-decode-purescript-error (checker type error)
  (let* ((position (assoc 'position error))
         (error-code (cdr (assoc 'errorCode error)))
         ;; (end-line (cdr (assoc 'endLine position)))
         ;; (end-col (cdr (assoc 'endColumn position)))
         (err-msg (cdr (assoc 'message error)))
         (filename (cdr (assoc 'filename error)))
         (start-line (or (cdr (assoc 'startLine position)) 1))
         (start-col (or (cdr (assoc 'startColumn position)) 1)))
    (when (not (-contains? psc-ide-flycheck-ignored-error-codes error-code))
      (flycheck-error-new-at start-line
                             start-col
                             type
                             err-msg
                             :id (concat filename
                                         ":" (number-to-string start-line)
                                         ":" (number-to-string start-col))
                             :checker checker))))

(defun psc-ide-flycheck-read-json (str)
  (let* ((json-array-type 'list))
    (condition-case nil
        (json-read-from-string str)
      (error nil))))

(defun psc-ide-flycheck-parse-errors (data checker)
  "Decode purescript json output errors."
  (let* ((resultType (pcase (cdr (assoc 'resultType data))
                         (`success 'warning)
                         (_ 'error)))
         (result (cdr (assoc 'result data)))
         (errors (-filter #'identity
                          (mapcar
                           (lambda (x) (psc-ide-flycheck-decode-purescript-error checker resultType x))
                           result))))
         errors))

(defun psc-ide-flycheck-save-suggestions (errs)
  (setq-local
   psc-ide-flycheck-suggestions
   (-map
    (lambda (err)
      (let* ((err-filename (cdr (assoc 'filename err)))
             (err-position (cdr (assoc 'position err)))
             (err-line (cdr (assoc 'startLine err-position)))
             (err-column (cdr (assoc 'startColumn err-position)))
             (err-id (concat err-filename ":" (number-to-string err-line)
                             ":" (number-to-string err-column))))
        (cons err-id err)))
    (-filter (lambda (i) (and (cdr (assoc 'position i))
                              (cdr (assoc 'suggestion i))))
             errs))))

(defun psc-ide-flycheck-insert-suggestion ()
  (interactive)
  (let* ((id (flycheck-error-id (car (flycheck-overlay-errors-at (point)))))
          (err (cdr (assoc id psc-ide-flycheck-suggestions)))
          (pos (cdr (assoc 'position err)))
          (sugg (cdr (assoc 'suggestion err))))
    (if (and pos sugg)
        (let* ((start (save-excursion
                        (goto-char (point-min))
                        (forward-line (- (cdr (assoc 'startLine pos)) 1))
                        (move-to-column (- (cdr (assoc 'startColumn pos)) 1))
                        (point)))
                (end (save-excursion
                      (goto-char (point-min))
                      (forward-line (- (cdr (assoc 'endLine pos)) 1))
                      (move-to-column (- (cdr (assoc 'endColumn pos)) 1))
                      (point))))
          (progn
            (kill-region start end)
            (goto-char start)
            (let ((new-end
                    (save-excursion
                      (insert (cdr (assoc 'replacement sugg)))
                      (point))))
              (set-mark start)
              (goto-char new-end)
              (setq deactivate-mark nil))))
      (message "No suggestion available!"))))

(define-key purescript-mode-map (kbd "C-c M-s")
  'psc-ide-flycheck-insert-suggestion)

(defun psc-ide-flycheck-start (checker callback)
  "Start a psc-ide syntax check with CHECKER.

CALLBACK is the status callback passed by flycheck."
    (psc-ide-send (psc-ide-command-rebuild)
                  (lambda (result)
                    (condition-case err
                        (progn
                          (psc-ide-flycheck-save-suggestions (append (cdr (assoc 'result result)) nil))
                          (let ((errors (psc-ide-flycheck-parse-errors result checker)))
                            (funcall callback 'finished errors)))
                      (error
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
