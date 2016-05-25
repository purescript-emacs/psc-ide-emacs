;;; psc-ide-flycheck.el --- Flycheck support for the purescript language -*- lexical-binding: t -*-

;; Copyright (c) 2015 Brian Sermons

;; Author: Brian Sermons
;; Package-Requires: ((flycheck "0.24") (emacs "24.4"))
;; URL: https://github.com/bsermons/psc-ide-flycheck

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
(require 'psc-ide)


(defun psc-ide-flycheck-decode-purescript-error (checker type error)
  (let* ((position (assoc 'position error))
         (start-line (cdr (assoc 'startLine position)))
         (start-col (cdr (assoc 'column position))))
    (flycheck-error-new-at (or start-line 1)
                           (or start-col 1)
                           type
                           (cdr (assoc 'message error))
                           :checker checker)))

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
         (errors (mapcar
                  (lambda (x) (psc-ide-flycheck-decode-purescript-error checker resultType x))
                  (cdr (assoc 'result data)))))
    errors))


(defun psc-ide-flycheck-start (checker callback)
  "Start a psc-ide syntax check with CHECKER.

CALLBACK is the status callback passed by flycheck."
  (condition-case err
    (psc-ide-send (psc-ide-command-rebuild)
                  (lambda (result)
                    (let ((errors (psc-ide-flycheck-parse-errors result checker)))
                      (funcall callback 'finished errors))))
    (error (funcall callback 'errored (error-message-string err)))))

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
