(require 'psc-ide)

(defconst psc-ide-test-example-imports "
module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Halogen

import Halogen.Util (appendToBody, onLoad)
import 		Halogen.HTML.Indexed 		as 	Hd
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as P

")

(defun psc-ide-test-example-with-buffer (f)
  (with-temp-buffer
    (insert psc-ide-test-example-imports)
    (goto-char 0)
    (funcall f)))

(defun psc-ide-test-parse-example-imports ()
  (psc-ide-test-example-with-buffer
    (lambda () (psc-ide-parse-imports-in-buffer))))

(ert-deftest psc-ide-show-type-impl-test ()
  (with-mock
   (mock (psc-ide-send-sync *) => (json-read-from-string "{\"result\":[{\"type\":\"Show-Type\",\"module\":\"Module\",\"identifier\":\"something\"}],\"resultType\":\"success\"}\n"))
   (should (string= "Module.something :: \n  Show-Type"
                    (psc-ide-show-type-impl "something"))))

  (with-mock
   (mock (psc-ide-send-sync *) => (json-read-from-string "{\"result\":[],\"resultType\":\"success\"}\n"))
   (should (not (psc-ide-show-type-impl "something")))))


;; Module  import parsing tests

(ert-deftest test-get-import-matches-in-buffer-should-return-all-imports ()
  (with-temp-buffer
    (insert psc-ide-test-example-imports)
    (goto-char 0)
    (let ((matches (psc-ide-parse-imports-in-buffer)))
      (should (= 10 (length matches))))))

(defun test-import (import name as)
    (string-match psc-ide-import-regex import)
    (let* ((import (psc-ide-extract-import-from-match-data import)))
      (should (equal (assoc 'module import) (cons 'module name)))
      (should (equal (assoc 'alias import) (cons 'alias as)))))

(ert-deftest test-get-import-from-match-data-full ()
  (test-import "import Mod.SubMod (foo, bar) as El"
               "Mod.SubMod"
               "El"))

(ert-deftest test-match-import-single-module ()
  (test-import "import Foo" "Foo" nil))

(ert-deftest test-match-import-with-alias ()
  (test-import "import Foo as F" "Foo" "F"))

(ert-deftest test-match-import-with-single-expose ()
  (test-import "import Foo (test)" "Foo" nil))

(ert-deftest test-match-import-with-multiple-exposings-tight ()
  (test-import "import Foo (test1,test2)" "Foo" nil))

(ert-deftest test-match-import-with-multiple-exposings-loose ()
  (test-import "import Foo ( test1 , test2 )" "Foo" nil))

(ert-deftest test-match-import-with-alias+multiple-exposings-tight ()
  (test-import "import Foo (test1,test2) as F" "Foo" "F"))

(ert-deftest test-match-import-with-alias+multiple-exposings-loose ()
  (test-import
   "import Foo ( test1 , test2 ) as F"
   "Foo"
   "F"))

(ert-deftest test-all-imported-modules ()
  (let ((imports (psc-ide-test-example-with-buffer
                   (lambda () (psc-ide-all-imported-modules)))))
    (should (equal (length imports) 10))))

(ert-deftest test-moduels-for-alias ()
  (let ((imports (psc-ide-test-example-with-buffer
                   (lambda () (psc-ide-modules-for-alias "P")))))
    (should (equal (length imports) 2))))

(ert-deftest test-get-completion-settings ()
  (psc-ide-test-example-with-buffer
    (lambda ()
      (let* ((command (json-read-from-string (psc-ide-build-completion-command "P.a" nil)))
             (params (cdr (assoc 'params command)))
             (filters (append (cdr (assoc 'filters params)) nil))
             (search (-some (lambda (filter)
                               (when (equal "prefix" (cdr (assoc 'filter filter)))
                                 (cdr (assoc 'search (cdr (assoc 'params filter)))))) filters))
             (modules (-some (lambda (filter)
                               (when (equal "modules" (cdr (assoc 'filter filter)))
                                 (cdr (assoc 'modules (cdr (assoc 'params filter)))))) filters)))
        (should (equal search "a"))
        (should (equal (length modules) 2))))))
