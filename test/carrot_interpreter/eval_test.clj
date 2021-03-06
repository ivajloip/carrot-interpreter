(ns carrot-interpreter.eval-test
    (:use clojure.test
          conjure.core
          carrot-interpreter.eval))

; -------------- helpers -------------------

(defn create-env 
  ([bindings] (create-env bindings nil))
  ([bindings parent] {:bindings (atom (conj bindings
                                            ['true true]
                                            ['false false]))
                      :parent parent}))

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

; -------------- eval-function -------------------

(deftest eval-function-adds-carrot-function-to-env
  (testing "eval-function adds carrot function to the environmet"
    (let [env (create-env {})]
      (eval-function [:foo 'func '(x y) 'x 'x 'y] env)
      (let [func (get @(:bindings env) 'func)]
        (are [expected actual]
             (= expected actual)
             (:kind func) :function
             (:args func) '(x y)
             (:env func) env
             (:body func) '(begin x x y))))))

; -------------- eval-module -------------------

(deftest eval-module-adds-module-to-env
  (testing "eval-module adds a module to the environmet"
    (let [env (create-env {})]
      (eval-module [:foo 'Foo '(begin x y)] env)
      (let [module (get @(:bindings env) 'Foo)]
        (are [expected actual]
             (= expected actual)
             (:kind module) :module
             (:body module) '(begin x y))))))

; -------------- include-module -------------------

(deftest include-module-adds-module-to-class-env
  (testing "include-module adds all methods from module to the a class
           environmet"
    (stubbing [evaluate :value lookup {:body :body}]
              (is (= (include-module '(include Bar) :env) :value))
              (verify-call-times-for evaluate 1)
              (verify-first-call-args-for evaluate :body :env)
              (verify-call-times-for lookup 1)
              (verify-first-call-args-for lookup :env 'Bar))))

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

; -------------- eval-if -------------------

(deftest eval-if-evaluates-then
  (testing "Eval-if evaluates then if it is present and cond is true"
    (stubbing [evaluate true]
              (eval-if '(if :cond :correct :incorrect) {})
              (verify-call-times-for evaluate 2)
              (verify-first-call-args-for evaluate :cond {})
              (verify-nth-call-args-for 2 evaluate :correct {}))))

(deftest eval-if-evaluates-alternative
  (testing "Eval-if evaluates alternative if it is present and cond is false"
    (stubbing [evaluate false]
              (eval-if '(if :cond :incorrect :correct) {})
              (verify-call-times-for evaluate 2)
              (verify-first-call-args-for evaluate :cond {})
              (verify-nth-call-args-for 2 evaluate :correct {})))
  (testing "eval-if behaves ok when there is no alternative and cond is false"
    (stubbing [evaluate false]
              (let [result (eval-if '(if :cond :incorrect) {})]
                (verify-call-times-for evaluate 1)
                (verify-first-call-args-for evaluate :cond {})
                (is (not result))))))

; -------------- eval-return -------------------

(deftest eval-return-marks-for-return
  (testing (str "Eval-return marks the environment that an return has occured"
                "and returns the value of the expression")
    (stubbing [evaluate :value modify :false]
              (is (= (eval-return '(return ast) {}) :value))
              (verify-call-times-for modify 1)
              (verify-call-times-for evaluate 1)
              (verify-first-call-args-for modify {} :return true)
              (verify-first-call-args-for evaluate 'ast {}))))

; -------------- evaluate -------------------

; -------------- eval-begin -------------------
(deftest eval-begin-multiple-ops
  (testing "Eval-begin invokes evaluate on all statements if no return" 
    (stubbing [evaluate :value]
              (let [env (create-env {})]
                (eval-begin '(begin x y z) env)
                (verify-call-times-for evaluate 3)
                (verify-first-call-args-for evaluate 'x env)
                (verify-nth-call-args-for 2 evaluate 'y env)
                (verify-nth-call-args-for 3 evaluate 'z env)))))

(deftest eval-begin-stops-on-return
  (testing "Eval-begin stops when return appears in bindings" 
    (stubbing [evaluate :value]
              (let [env (create-env {:return true})]
                (is (= (eval-begin '(begin x y) env) :value))
                (verify-call-times-for evaluate 1)
                (verify-first-call-args-for evaluate 'x env)))))


; -------------- return, evaluate, modify and block -------------------

(deftest return-directly-in-code-block
  (testing "Eval-begin works correctly when return is present" 
    (let [env (create-env {'x :value})]
      (is (= (eval-begin '(begin (return x) y) env) :value))
      (is (= (:return @(:bindings env)) true)))))

; -------------- return, evaluate, eval-if, modify and block -------------------

(deftest return-directly-in-if-in-code-block
  (testing "Eval-begin works correctly when return is reached in if statement" 
    (let [env (create-env {'x :value})]
      (is (= (eval-begin '(begin (if true (return x) y) z) env) :value))
      (is (= (:return @(:bindings env)) true)))))

(deftest return-is-skiped-in-if-in-code-block
  (testing "Eval-begin works correctly when return is skiped in if statement" 
    (let [env (create-env {'z :value 'y :incorrect})]
      (is (= (eval-begin '(begin (if false (return x) y) z) env) :value))
      (is (= (:return @(:bindings env)) nil)))))

; -------------- invoke-func -------------------

(deftest invoke-func-works-for-primitive
  (testing "Invoke-func calls the function body with the correct args for
           primitive functions"
    (is (= (invoke-func {:kind :primitive :code *} '(2 3) :env) 6))))

(deftest invoke-func-fails-for-unknown-kind
  (testing "Invoke-func fails for kind :unknown"
    (is (thrown-with-msg? RuntimeException
                          #"^Don't know how to invoke.*$"
                          (invoke-func {:kind :unknown} '() :env)))))

(deftest invoke-func-works-for-carrot-functions
  (testing "Invoke-func evaluates the body of an carrot function with the
           correct args"
    (stubbing [evaluate :value extend-env {:key :val}]
              (is (= (invoke-func {:kind :function
                                   :args '(x y)
                                   :body '(begin x y)
                                   :env :environment
                                   }
                                  '(2 3) 
                                  :environment)
                     :value))
              (verify-call-times-for evaluate 1)
              (verify-first-call-args-for evaluate '(begin x y) {:key :val})
              (verify-call-times-for extend-env 1)
              (verify-first-call-args-for extend-env {'x 2 'y 3} :environment))))

(deftest invoke-func-works-for-constructor
  (testing "Invoke-func calls create-new-object on constructor"
    (stubbing [create-new-object :value]
              (is (= (invoke-func {:kind :constructor} :params :env) :value))
              (verify-call-times-for create-new-object 1)
              (verify-first-call-args-for create-new-object
                                     {:kind :constructor}
                                     :params
                                     :env))))

; -------------- eval-class -------------------

(deftest eval-class-adds-class-to-env
  (testing "eval-class adds a class definition to the current environment"
    (stubbing [modify :value evaluate :other-value extend-env :new-env]
              (let [env (create-env {})]
                (is (= (eval-class '(class Foo 3.0) env) "#'Foo"))
                (verify-call-times-for extend-env 1)
                (verify-call-times-for evaluate 2)
                (verify-nth-call-args-for 2 evaluate 3.0 :new-env)
                (verify-call-times-for modify 1)
                (verify-first-call-args-for modify
                                            env
                                            'Foo
                                            {:kind :class
                                             :env :new-env
                                             :parent nil})))))

; -------------- make-constructor -------------------

(deftest make-constructor-wraps-class-constructor
  (testing "make-constructor creates a special function that on invoke creates
           new objects"
    (let [constructor (make-constructor 'Class)]
      (are [property value] 
           (= (get constructor property) value)
           (:kind :constructor)
           (:class 'Class)))))

; -------------- make-default-initialize -------------------

(deftest make-default-initialize-creates-ast
  (testing "make-default-initialize makes ast with function returning nil 
           when no parent is given"
    (is (= (make-default-initialize nil) '(function initialize () nil))))
  (testing "make-default-initialize makes ast with function calling
           super.initialize when parent is given"
    (is (= (make-default-initialize true)
           '(function initialize () (dot super (initialize)))))))

; -------------- eval-dot -------------------

(deftest eval-dot-evaluates-object-field
  (testing "Eval-dot evaluates expressions like `obj.<expr>` (obj.a = 3) in the
           context of obj."
    (stubbing [evaluate (fn [x y] (if (= y {}) {:env {:x x}} :result))]
              (is (= (eval-dot (list 'dot :reciever :message) {}) :result))
              (verify-call-times-for evaluate 2)
              (verify-first-call-args-for evaluate :reciever {})
              (verify-nth-call-args-for 2 evaluate :message {:x :reciever
                                                             :parent {}}))))

; -------------- create-new-object -------------------

(deftest create-new-object-tests
  (testing "create-new-object creates new object when it has no parent"
    (stubbing [lookup {:env {:bindings (atom :penv)}}
               extend-env :new-env
               modify :modify
               eval-dot :result]
              (create-new-object {:class :pclass} '(params) :env)
              (verify-call-times-for lookup 1)
              (verify-first-call-args-for lookup :env :pclass)
              (verify-call-times-for extend-env 1)
              (verify-first-call-args-for extend-env :penv :env)
              (verify-call-times-for modify 1)
              (verify-first-call-args-for modify
                                          :new-env
                                          'this
                                          {:kind :object
                                           :class :pclass
                                           :env :new-env})
              (verify-call-times-for eval-dot 1)
              (verify-first-call-args-for eval-dot
                                          '(dot this (initialize params))
                                          :new-env)))
  ;(testing "create-new-object creates new object when it has a parent")
  )
    
              
