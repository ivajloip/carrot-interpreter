(ns carrot-interpreter.eval-test
    (:use clojure.test
          conjure.core
          carrot-interpreter.eval))

; -------------- eval-set -------------------

(deftest eval-set-evaluates-variable-assigning
  (testing "Eval-set calls modify with the right arguments"
    (stubbing [modify :foo evaluate :var]
              (eval-set [:x 'bar :var] {})
              (verify-call-times-for modify 1)
              (verify-call-times-for evaluate 1)
              (verify-first-call-args-for evaluate :var {})
              (verify-first-call-args-for modify {} 'bar :var))))

; -------------- empty-env -------------------

(deftest empty-env-creates-empty-entironment
  (testing "Empty-env creates an empty environment with no parent"
    (let [empty-environment (empty-env)]
      (are [expected actual]
           (= expected actual)
           {} (deref (:bindings empty-environment)) 
           nil (:parent empty-environment)))))

; -------------- type? -------------------

(deftest type?-recognizes-the-type
  (testing "Type? recognizes correctly function"
    (are [value] 
         value
         (type? '(function func () 3.0) 'function)
         (type? '(set! x y) 'set!))))

; -------------- error -------------------

(deftest error-throws-error
  (testing (str "Error method throws the right type of exception" 
                " with the right message")
    (is (thrown-with-msg? RuntimeException
                          #"Error Some message"
                          (error "Error" " Some message")))))


; -------------- lookup -------------------

(deftest lookup-find-var-in-current-env
  (testing "Lookup can find some variable in the current environment"
    (let [env {:bindings (atom {'x 3}) :parent nil}]
      (is (= (lookup env 'x) 3))))
  (testing "Lookup can find some variable in the parent environment"
    (let [env {:bindings (atom {'x 3})
               :parent {:bindings (atom {'y 4}) :parent nil}}]
      (is (= (lookup env 'y) 4))))
  (testing "Lookup throws error on fail"
    (let [env {:bindings (atom {}) :parent nil}]
      (mocking [error]
               (lookup env 'x)
               (verify-call-times-for error 1)
               (verify-first-call-args-for error "Unbound variable: " 'x)))))

