(ert-deftest psc-ide-show-type-impl-test ()
  (with-mock
   (mock (psc-ide-send *) => "{\"result\":[{\"type\":\"Show-Type\",\"module\":\"Module\"}],\"resultType\":\"success\"}\n")
   (should (string= "Show-Type"
                    (psc-ide-show-type-impl "something"))))

  (with-mock
   (mock (psc-ide-send *) => "{\"result\":[],\"resultType\":\"success\"}\n")
   (should (not (psc-ide-show-type-impl "something")))))
