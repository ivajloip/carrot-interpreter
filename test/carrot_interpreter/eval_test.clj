(ns carrot-interpreter.eval-test
    (:use clojure.test
          conjure.core
          carrot-interpreter.eval))

; -------------- helpers -------------------

(defn create-env 
  ([bindings] {:bindings (atom bindings) :parent nil})
  ([bindings parent] {:bindings (atom bindings) :parent parent}))

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

(deftest lookup-find-var-the-env
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

; -------------- primitive -------------------

(deftest primitive-wraps-clojure-code
  (testing "Primitive wraps a clojure code in a easy-to-use way"
    (is (= (primitive '+) {:kind :primitive :code '+}))))

; -------------- extend-env -------------------

(deftest extend-env-makes-correct-new-env
  (testing "Extend-env adds a new environment in the stack of environments"
    (let [extended-env (extend-env {:a 1} {:b 2})]
      (are [expected actual]
           (= expected actual)
           (deref (:bindings extended-env)) {:a 1}
           (:parent extended-env) {:b 2}))))

; -------------- make-function -------------------

(deftest make-function-adds-carrot-function-to-env
  (testing "Make-function adds carrot function to the environmet"
    (let [env (create-env {})]
      (make-function [:foo 'func '(x y) 'x 'x 'y] env)
      (let [func (get @(:bindings env) 'func)]
        (are [expected actual]
             (= expected actual)
             (:kind func) :function
             (:args func) '(x y)
             (:env func) env
             (:body func) '(begin x x y))))))

; -------------- modify -------------------

(deftest modify-adds-variable-to-env
  (testing "Modify adds variable to the current env if no parent is present"
    (let [env (create-env {})]
      (modify env 'variable 3.0)
      (is (= (get @(:bindings env) 'variable) 3.0)))))

(deftest modify-modifies-variable-in-env
  (testing "Modify changes variable value in the current env if it is present"
    (let [env (create-env {'variable 2.0})]
      (modify env 'variable 3.0)
      (is (= (get @(:bindings env) 'variable) 3.0)))))



