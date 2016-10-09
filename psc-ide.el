;;; psc-ide.el --- Minor mode for PureScript's psc-ide tool. -*- lexical-binding: t -*-

;; Copyright (C) 2015 The psc-ide-emacs authors

;; Author   : Erik Post <erik@shinsetsu.nl>
;;            Dmitry Bushenko <d.bushenko@gmail.com>
;;            Christoph Hegemann
;;            Brian Sermons
;; Homepage : https://github.com/epost/psc-ide-emacs
;; Version  : 0.1.0
;; Package-Requires: ((dash "2.12.1") (dash-functional "1.2.0") (company "0.8.7") (cl-lib "0.5") (s "1.10.0"))
;; Keywords : languages

;;; Commentary:

;; Emacs integration for PureScript's psc-ide tool

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Imports

(require 'company)
(require 'cl-lib)
(require 'dash)
(require 'dash-functional)
(require 's)
(require 'psc-ide-backported)
(require 'psc-ide-protocol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; psc-ide-mode definition

;;;###autoload
(define-minor-mode psc-ide-mode
  "psc-ide-mode definition"
  :lighter " psc-ide"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-s") 'psc-ide-server-start)
            (define-key map (kbd "C-c C-q") 'psc-ide-server-quit)
            (define-key map (kbd "C-c C-l") 'psc-ide-load-all)
            (define-key map (kbd "C-c C-S-l") 'psc-ide-load-module)
            (define-key map (kbd "C-c C-a") 'psc-ide-add-clause)
            (define-key map (kbd "C-c C-c") 'psc-ide-case-split)
            (define-key map (kbd "C-c C-i") 'psc-ide-add-import)
            (define-key map (kbd "C-c C-t") 'psc-ide-show-type)
            (define-key map (kbd "C-c C-b") 'psc-ide-rebuild)
            (define-key map (kbd "M-.") 'psc-ide-goto-definition)
            (define-key map (kbd "M-,") 'pop-tag-mark)
            map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Settings, override as needed.

(defgroup psc-ide nil
  "Settings for psc-ide."
  :prefix "psc-ide-"
  :group 'psc-ide)

(defcustom psc-ide-client-executable "psc-ide-client"
  "Path to the 'psc-ide' executable."
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-server-executable "psc-ide-server"
  "Path to the 'psc-ide-server' executable."
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-port 4242
  "The port that psc-ide-server and the client use."
  :group 'psc-ide
  :type  'integer)

(defcustom psc-ide-source-globs '("src/**/*.purs" "bower_components/purescript-*/src/**/*.purs")
  "The source globs for your PureScript source files."
  :group 'psc-ide
  :type  'sexp)

(defcustom psc-ide-debug nil
  "Whether psc-ide-server should be started with the debug flag"
  :group 'psc-ide
  :type  'boolean)

(defcustom psc-ide-completion-matcher "flex"
  "The method used for completions."
  :options '("flex" "prefix")
  :group 'psc-ide
  :type  'string)

(defcustom psc-ide-add-import-on-completion "t"
  "Whether to add imports on completion"
  :group 'psc-ide
  :type 'boolean)

(defcustom psc-ide-rebuild-on-save nil
  "Whether to rebuild files on save and display errors/warnings
in a buffer"
  :group 'psc-ide
  :type 'boolean)

(defcustom psc-ide-disable-flycheck nil
  "Whether to disable flycheck syntax functionality"
  :group 'psc-ide
  :type  'boolean)

(defconst psc-ide-import-regex
  (rx (and line-start "import" (1+ space)
           (group (and (1+ (any word "."))))
           (opt (1+ space) "hiding")
           (opt (1+ space) "(" (group (0+ not-newline)) ")")
           (opt (1+ space) "as" (1+ space) (group (and (1+ word)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Interactive.

(add-hook 'after-save-hook
          (lambda ()
            (set 'psc-ide-buffer-import-list
                 (psc-ide-parse-imports-in-buffer)))
          nil t)

(defun psc-ide-rebuild-on-save-hook()
  "Rebuilds the current module on safe"
  (when (eq major-mode 'purescript-mode)
    (psc-ide-rebuild)))

(when psc-ide-rebuild-on-save
  (add-hook 'after-save-hook 'psc-ide-rebuild-on-save-hook))

(defun psc-ide-init ()
  (interactive)
  (add-function :before-until (local 'eldoc-documentation-function)
                #'psc-ide-show-type-eldoc)
  (set (make-local-variable 'psc-ide-buffer-import-list)
       (psc-ide-parse-imports-in-buffer)))

(with-eval-after-load 'flycheck
  (when (not psc-ide-disable-flycheck)
    (require 'psc-ide-flycheck)
    (psc-ide-flycheck-setup)))

(defun company-psc-ide-backend (command &optional arg &rest ignored)
  "The psc-ide backend for 'company-mode'."
  (interactive (list 'interactive))

  (when (derived-mode-p 'purescript-mode)
    (cl-case command
      (interactive (company-begin-backend 'company-psc-ide-backend))

      (init (psc-ide-init))

      (prefix (when (and (eq major-mode 'purescript-mode)
                         (not (company-in-string-or-comment)))
                (let ((symbol (company-grab-symbol)))
                  (if symbol
                      ;; We strip of the qualifier so that it doesn't get
                      ;; overwritten when completing.
                      (if (s-contains-p "." symbol)
                          (cons (car (last (s-split "\\." symbol))) t)
                        symbol)
                    'stop))))

      (candidates (psc-ide-company-fetcher arg company--manual-action))

      (sorted t)

      (annotation (psc-ide-annotation arg))

      (meta (psc-ide-string-fontified (get-text-property 0 :type arg)))

      (post-completion
       (unless (or
                ;; Don't add an import when the option to do so is disabled
                (not psc-ide-add-import-on-completion)
                ;; or when a qualified identifier was completed
                (s-contains-p "." (company-grab-symbol)))
         (psc-ide-add-import-impl arg (vector
                                       (psc-ide-filter-modules
                                        (list (get-text-property 0 :module arg))))))))))

(defun psc-ide-server-start (dir-name)
  "Start 'psc-ide-server'."
  (interactive (list (read-directory-name "Project root? "
                                          (psc-ide-suggest-project-dir))))
  (psc-ide-server-start-impl dir-name)
  ;; After 1 second we send a load all command
  (run-at-time "1 sec" nil 'psc-ide-load-all))

(defun psc-ide-server-quit ()
  "Quit 'psc-ide-server'."
  (interactive)
  (psc-ide-send-sync psc-ide-command-quit))

(defun psc-ide-load-module (module-name)
  "Provide module to load"
  (interactive (list (read-string "Module: " (psc-ide-get-module-name))))
  (psc-ide-load-module-impl module-name))

(defun psc-ide-load-all ()
  "Loads all the modules in the current project"
  (interactive)
  (psc-ide-send psc-ide-command-load-all (-const "yay")))

(defun psc-ide-show-type (expand)
  "Show type of the symbol under cursor."
  (interactive "P")
  (let ((ident (psc-ide-ident-at-point)))
    (-if-let (type-description (psc-ide-show-type-impl ident expand))
        (message "%s" (psc-ide-string-fontified type-description))
      (message (concat "Know nothing about type of `%s'. "
                       "Have you loaded the corresponding module?")
               ident))))

(defun psc-ide-goto-definition ()
  "Go to definition of the symbol under cursor."
  (interactive)
  (let ((ident (psc-ide-ident-at-point)))
    (psc-ide-goto-definition-impl ident)))

(defun psc-ide-case-split (type)
  "Case Split on identifier under cursor."
  (interactive "sType: ")
  (let ((new-lines (psc-ide-case-split-impl type)))
    (beginning-of-line) (kill-line) ;; clears the current line
    (insert (mapconcat 'identity new-lines "\n"))))

(defun psc-ide-add-clause ()
  "Add clause on identifier under cursor."
  (interactive)
  (let ((new-lines (psc-ide-add-clause-impl)))
    (beginning-of-line) (kill-line) ;; clears the current line
    (insert (mapconcat 'identity new-lines "\n"))))

(defun psc-ide-add-import ()
  "Add an import for the symbol under the cursor."
  (interactive)
  (psc-ide-add-import-impl (psc-ide-ident-at-point)))

(defun psc-ide-rebuild ()
  "Rebuild the current module"
  (interactive)
  (save-buffer)
  (psc-ide-send (psc-ide-command-rebuild) 'psc-ide-rebuild-handler))

(defun psc-ide-rebuild-handler (response)
  "Accepts a rebuild response and either displays errors/warnings
inside the *psc-ide-rebuild* buffer, or closes it if there were
none."
  (let ((is-success (string= "success" (cdr (assoc 'resultType response))))
        (result (cdr (assoc 'result response))))
    (if (not is-success)
        (psc-ide-display-rebuild-message "Error" (aref result 0))
      (if (<= (length result) 0)
          ;; If there are no warnings we close the rebuild buffer and print "OK"
          (progn
            (delete-windows-on (get-buffer-create "*psc-ide-rebuild*"))
            (message "OK"))
        (psc-ide-display-rebuild-message "Warning" (aref result 0))))))

(defun psc-ide-display-rebuild-message (type rawMsg)
  "Takes a parsed JSON error/warning and displays it in the
rebuild buffer."
  (let ((msg (concat type ": " (psc-ide-pretty-json-error rawMsg))))
    (progn
      (with-current-buffer (get-buffer-create "*psc-ide-rebuild*")
        (compilation-mode)
        (read-only-mode -1)
        (erase-buffer)
        (insert msg)
        (read-only-mode 1))
      (display-buffer "*psc-ide-rebuild*")
      (set-window-point (get-buffer-window "*psc-ide-rebuild*") (point-min)))))

(defun psc-ide-pretty-json-error (err)
  "Formats a parsed JSON error/warning into a format that can be
nicely displayed inside a compilation buffer."
  (let ((err-message (cdr (assoc 'message err)))
        (err-filename (cdr (assoc 'filename err)))
        (err-position (cdr (assoc 'position err))))
    (if (not err-position)
        err-message
      (let ((err-column (cdr (assoc 'startColumn err-position)))
            (err-line (cdr (assoc 'startLine err-position))))
        (concat err-filename
                ":" (number-to-string err-line)
                ":" (number-to-string err-column)
                ":" "\n" err-message)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Non-interactive.

(defun psc-ide-show-type-eldoc ()
  "Show type of the symbol under cursor, but be quiet about failures"
  (let ((ident (psc-ide-ident-at-point)))
    (-when-let (type-description (psc-ide-show-type-impl ident))
      (message "%s" (psc-ide-string-fontified type-description)))))

(defun psc-ide-case-split-impl (type)
  "Case Split on identifier under cursor"
  (let ((reg (psc-ide-ident-pos-at-point)))
    (psc-ide-unwrap-result (psc-ide-send-sync
                            (psc-ide-command-case-split
                             (substring (thing-at-point 'line t) 0 -1)
                             (save-excursion (goto-char (car reg)) (current-column))
                             (save-excursion (goto-char (cdr reg)) (current-column))
                             type)))))

(defun psc-ide-add-clause-impl ()
  "Add clause on identifier under cursor"
  (let ((reg (psc-ide-ident-pos-at-point)))
    (psc-ide-unwrap-result (psc-ide-send-sync
                            (psc-ide-command-add-clause
                             (substring (thing-at-point 'line t) 0 -1) nil)))))

(defun psc-ide-get-module-name ()
  "Return the qualified name of the module in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "^module +\\([A-Z][A-Za-z0-9.]*\\)" nil t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun psc-ide-extract-import-from-match-data (&optional string)

  "Helper function for use when using the `psc-ide-import-regex' to match
imports to extract the relevant info from the groups.  STRING is for
use when the search used was with `string-match'."

  (let* ((data (match-data))
         (len (length data))
         (idx 3)
         result)
    (push `(module . ,(match-string-no-properties 1 string)) result)
    (push `(alias . ,(match-string-no-properties 3 string)) result)
    result))

(defun psc-ide-parse-imports-in-buffer (&optional buffer)
  "Parse the list of imports for the current purescript BUFFER."
  (let ((module-name (psc-ide-get-module-name))
        (matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (or buffer (current-buffer))
          (save-restriction
            (widen)
            (goto-char (point-min))
            (when module-name
              (push `((module . ,module-name)) matches))
            (while (search-forward-regexp psc-ide-import-regex nil t 1)
              (push (psc-ide-extract-import-from-match-data) matches))))))
    matches))


(defun psc-ide-ask-project-dir ()
  "Ask psc-ide-server for the project dir."
  (interactive)
  (psc-ide-send psc-ide-command-cwd
                (-compose 'message 'psc-ide-unwrap-result)))

(defun psc-ide-server-start-impl (dir-name)
  "Start psc-ide-server."
  (apply 'start-process `("*psc-ide-server*" "*psc-ide-server*"
                          ,@(psc-ide-server-command dir-name))))

(defun psc-ide-server-command (dir-name)
  "Tries to find the psc-ide-server-executable and builds up the
  command by appending eventual options. Returns a list that can
  be expanded and passed to start-process"
  (let ((path (executable-find psc-ide-server-executable))
        (port (number-to-string psc-ide-port))
        (directory (expand-file-name dir-name))
        (debug-flag (when psc-ide-debug "--debug"))
        (globs (when (psc-ide--version-gte (psc-ide-server-version) "0.9.2") psc-ide-source-globs)))
    (if path
        (remove nil `(,path "-p" ,port "-d" ,directory ,debug-flag ,@globs))
      (error (s-join " " '("Couldn't locate the psc-ide-server executable. You"
                           "could either customize the psc-ide-server-executable"
                           "setting, or put the executable on your path."))))))

(defun psc-ide-server-version ()
  "Returns the version of the found psc-ide-server executable"
  (let ((path (executable-find psc-ide-server-executable)))
    (s-chomp (shell-command-to-string (s-concat path " --version")))))

(defun psc-ide--version-gte (version1 version2)
  "Determines whether VERSION1 is greater then or equal to VERSION2"
  (let* ((vs (-zip-fill 0
                        (-map 'string-to-int (s-split "\\." version1))
                        (-map 'string-to-int (s-split "\\." version2))))
         ;; drop all the prefix version numbers that are equal
         (v (car (--drop-while (= (cdr it) (car it)) vs))))
    ;; if v is nil, the two versions were completely equal
    (if v
        (>= (car v) (cdr v))
      t)))

(defun psc-ide-load-module-impl (module-name)
  "Load a PureScript module and its dependencies."
  (psc-ide-unwrap-result
   (psc-ide-send-sync (psc-ide-command-load
                       [] (list module-name)))))

(defun psc-ide-add-import-impl (identifier &optional filters)
  "Invoke the addImport command"
  (let* ((tmp-file (make-temp-file "psc-ide-add-import"))
         (filename (buffer-file-name (current-buffer)))
         (result (progn
                   (write-region (point-min) (point-max) tmp-file)
                   (psc-ide-unwrap-result
                    (psc-ide-send-sync (psc-ide-command-add-import identifier filters tmp-file tmp-file))))))
    (if (not (stringp result))
        (let ((selection
               (completing-read "Which Module to import from: "
                                (-map (lambda (x)
                                        (cdr (assoc 'module x))) result))))
          (psc-ide-add-import-impl identifier (vector (psc-ide-filter-modules (vector selection)))))
      (progn (message (concat "Added import for " identifier))
             (save-restriction
               (widen)
               ;; command successful, insert file with replacement to preserve
               ;; markers.
               (insert-file-contents tmp-file nil nil nil t))))
    (delete-file tmp-file)))

(defun psc-ide-company-fetcher (ignored &optional manual)
  "Grabs the symbol at point at creates an asynchronouse
completer. We ignore the prefix we get from company, because it
doesn't contain eventual qualifiers."
  (let ((prefix (company-grab-symbol)))
  `(:async . ,(-partial 'psc-ide-complete-async prefix manual))))

(defun psc-ide-complete-async (prefix manual callback)
  (let ((command (psc-ide-build-completion-command prefix manual))
        (handler (-partial 'psc-ide-handle-completionresponse callback)))
    (psc-ide-send command handler)))

(defun psc-ide-build-completion-command (search manual)
  "Constructs a completion command from the given SEARCH.
The cases we have to cover:
1. List.fil      <- filter by prefix and List module
2. fil| + manual <- don't filter at all
3. fil|          <- filter by prefix and imported modules"
  (let* ((components (s-split "\\." search))
         (prefix (car (last components)))
         (alias (s-join "." (butlast components))))
    (if (not (s-blank? alias))
        ;; 1. List.fil <- filter by prefix and List module
        (psc-ide-qualified-completion-command prefix alias)
      (if manual
          ;; 2. fil| + manual <- don't filter at all
          (psc-ide-command-complete
           (vector (psc-ide-filter-prefix prefix))
           nil
           (psc-ide-get-module-name))
        ;; 3. fil| <- filter by prefix and imported modules"
        (psc-ide-command-complete
         (vector (psc-ide-filter-prefix prefix)
                 (psc-ide-filter-modules (psc-ide-all-imported-modules)))
         nil
         (psc-ide-get-module-name))))))

(defun psc-ide-qualified-completion-command (prefix alias)
  "Builds a completion command for a PREFIX with ALIAS"
  (let ((modules (psc-ide-modules-for-alias alias)))
    (psc-ide-command-complete
     (vector (psc-ide-filter-prefix prefix)
             (psc-ide-filter-modules (vconcat modules)))
     nil
     (psc-ide-get-module-name))))

(defun psc-ide-all-imported-modules ()
  "Retrieves all imported modules for a buffer"
  (-map (lambda (import) (cdr (assoc 'module import)))
        (psc-ide-parse-imports-in-buffer)))

(defun psc-ide-modules-for-alias (alias)
  "Searches the current module's imports for modules that are
  qualified as ALIAS"
  (let ((imports (psc-ide-parse-imports-in-buffer)))
    (-keep (lambda (import)
             (when (equal alias (cdr (assoc 'alias import)))
               (cdr (assoc 'module import)))) imports)))

(defun psc-ide-handle-completionresponse (callback response)
  "Accepts a callback and a completion response from psc-ide,
processes the response into a format suitable for company and
passes it into the callback"
  (let* ((result (psc-ide-unwrap-result response))
         (completions (-map 'psc-ide-annotate-completion result)))
    (funcall callback completions)))

(defun psc-ide-annotate-completion (completion)
  "Turns a completion from psc-ide into a string with
  text-properties, which carry additional information"
  (let ((identifier (cdr (assoc 'identifier completion)))
        (type (cdr (assoc 'type completion)))
        (module (cdr (assoc 'module completion))))
    ;; :qualifier qualifier <- TODO: Add this back in
    (add-text-properties 0 1 (list :type type
                                   :module module) identifier)
    ;; add-text-properties is sideeffecting and doesn't return the modified
    ;; string, so we need to explicitly return the identifier from here
    identifier))

(defun psc-ide-goto-definition-impl (search)
  "Asks for the definition location of SEARCH and jumps to it."
  (let* ((resp (psc-ide-send-sync
                (psc-ide-build-type-command search)))
         (result (psc-ide-unwrap-result resp)))
    (when (not (zerop (length result)))
      (let* ((completion (aref result 0))
             (position (cdr (assoc 'definedAt completion))))
        (if position
            (let* ((file (cdr (assoc 'name position)))
                   (start (cdr (assoc 'start position)))
                   (line (aref start 0))
                   (column (aref start 1)))
              (if (fboundp 'xref-push-marker-stack) ;; Emacs 25
                  (xref-push-marker-stack)
                (with-no-warnings
                  (require 'etags)
                  (ring-insert find-tag-marker-ring (point-marker))))
              (find-file (expand-file-name file))
              (goto-char (point-min))
              (forward-line (1- line))
              (forward-char (1- column)))
          (message (format "No position information for %s" search)))))))

(defun psc-ide-show-type-impl (search &optional expand)
  "Returns a string that describes the type of SEARCH.
Returns NIL if the type of SEARCH is not found."
  (let* ((resp (psc-ide-send-sync
                (psc-ide-build-type-command search)))
         (result (psc-ide-unwrap-result resp)))
    (when (not (zerop (length result)))
      (let* ((completion (aref result 0))
             (type (cdr (assoc (if expand 'expandedType 'type) completion)))
             (module (cdr (assoc 'module completion)))
             (identifier (cdr (assoc 'identifier completion))))
        (s-concat module "." identifier " :: \n  " type)))))

(defun psc-ide-build-type-command (search)
  "Builds a type command from SEARCH."
  (let* ((components (s-split "\\." search))
         (ident (car (last components)))
         (alias (s-join "." (butlast components))))
    (if (not (s-blank? alias))
      (psc-ide-qualified-type-command ident alias)
    (psc-ide-command-show-type
     (vector (psc-ide-filter-modules
              (psc-ide-all-imported-modules)))
     ident
     (psc-ide-get-module-name)))))

(defun psc-ide-qualified-type-command (ident alias)
  "Builds a type command for an IDENT with ALIAS"
  (let ((modules (psc-ide-modules-for-alias alias)))
        (psc-ide-command-show-type
         (vector (psc-ide-filter-modules (vconcat modules)))
         ident
         (psc-ide-get-module-name))))

(defun psc-ide-annotation (s)
  (format " (%s)" (get-text-property 0 :module s)))

(defun psc-ide-suggest-project-dir ()
  (if (fboundp 'projectile-project-root)
      (projectile-project-root)
    (file-name-directory (buffer-file-name))))

(defun psc-ide-string-fontified (str)
  "Takes a string and returns it with syntax highlighting."
  (with-temp-buffer
    (turn-on-purescript-font-lock)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(setq company-tooltip-align-annotations t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Utilities

(add-to-list 'company-backends 'company-psc-ide-backend)

(provide 'psc-ide)

;;; psc-ide.el ends here
