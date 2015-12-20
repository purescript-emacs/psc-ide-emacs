(require 'psc-ide)

(defconst psc-ide-test-example-imports "
module Main where

import Prelude

import Control.Monad.Aff (Aff(), runAff, later')
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Exception (throwException)
import Halogen

import Halogen.Util (appendToBody, onLoad)
import 		qualified Halogen.HTML.Indexed 		as 	Hd
import qualified Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Indexed as P

")

(defun psc-ide-test-parse-example-imports ()
  (with-temp-buffer
    (insert psc-ide-test-example-imports)
    (goto-char 0)
    (psc-ide-parse-imports-in-buffer)))

(ert-deftest psc-ide-show-type-impl-test ()
  (with-mock
   (mock (psc-ide-send *) => "{\"result\":[{\"type\":\"Show-Type\",\"module\":\"Module\"}],\"resultType\":\"success\"}\n")
   (should (string= "Show-Type"
                    (psc-ide-show-type-impl "something"))))

  (with-mock
   (mock (psc-ide-send *) => "{\"result\":[],\"resultType\":\"success\"}\n")
   (should (not (psc-ide-show-type-impl "something")))))


;; Module  import parsing tests

(ert-deftest test-get-import-matches-in-buffer-should-return-all-imports ()
  (with-temp-buffer
    (insert psc-ide-test-example-imports)
    (goto-char 0)
    (let ((matches (psc-ide-parse-imports-in-buffer)))
      (should (= 9 (length matches))))))

(defun test-import (import name as exposing)
    (string-match psc-ide-import-regex import)
    (let* ((import (psc-ide-extract-import-from-match-data import)))
      (should (equal (assoc 'module import) (cons 'module name)))
      (should (equal (assoc 'alias import) (cons 'alias as)))
      (should (equal (assoc 'exposing import) (cons 'exposing exposing)))))

(ert-deftest test-get-import-from-match-data-full ()
  (test-import "import qualified Mod.SubMod as El (foo, bar)"
               "Mod.SubMod"
               "El"
               '("foo" "bar")))

(ert-deftest test-match-import-single-module ()
  (test-import "import Foo" "Foo" nil ()))

(ert-deftest test-match-import-with-alias ()
  (test-import "import qualified Foo as F" "Foo" "F" nil))

(ert-deftest test-match-import-with-single-expose ()
  (test-import "import Foo (test)" "Foo" nil '("test")))

(ert-deftest test-match-import-with-multiple-exposings-tight ()
  (test-import "import Foo (test1,test2)" "Foo" nil '("test1" "test2")))

(ert-deftest test-match-import-with-multiple-exposings-loose ()
  (test-import "import Foo ( test1 , test2 )" "Foo" nil '("test1" "test2")))

(ert-deftest test-match-import-with-alias+multiple-exposings-tight ()
  (test-import "import qualified Foo as F (test1,test2)" "Foo" "F" '("test1" "test2")))

(ert-deftest test-match-import-with-alias+multiple-exposings-loose ()
  (test-import
   "import qualified Foo as F ( test1 , test2 )"
   "Foo"
   "F"
   '("test1" "test2")))


(ert-deftest test-filter-bare-imports ()
  (let ((imports (psc-ide-test-parse-example-imports)))
    (should (equal
             (length (psc-ide-filter-bare-imports imports))
             2))))

(ert-deftest test-filter-imports-by-alias ()
  (let ((imports (psc-ide-test-parse-example-imports)))
    (should (equal
             (length (psc-ide-filter-imports-by-alias imports "P"))
             2))))

(ert-deftest test-get-completion-settings ()
  (let ((imports (psc-ide-test-parse-example-imports)))
    (let* ((result (psc-ide-get-completion-settings "P.a" imports))
           (search (car result))
           (modules (cdr result)))
      (should (equal '("a" . "P") search))
      (should (equal 2 (length modules))))))
