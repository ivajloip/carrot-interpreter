(ns carrot-interpreter.eval)

(declare evaluate)

(defn error
  [& args]
  (throw (RuntimeException. (apply str args))))

(defn lookup
  [{:keys (bindings parent)} var-name]
  (cond (find @bindings var-name) (var-name @bindings)
        parent (recur parent var-name)
        :else (error "Unbound variable: " var-name)))

(defn empty-env
  []
  {:bindings (atom {})
   :parent nil})

(defn extend-env
  [bindings parent]
  {:bindings (atom bindings) :parent parent})

(defn primitive
  [clojure-func]
  {:kind :primitive :code clojure-func})

(defn default-env
  []
  (let [mappings [+ * / - > < >= <=]
        opearations    ['+ '* '/ '- '> '< '>= '<=]
        bindings (conj {'print (primitive prn)
                        '== (primitive =)
                        (symbol "^") (primitive (fn [x y]
                                                  (java.lang.Math/pow x y)))
                        '! (primitive not)
                        (symbol "false") false
                        (symbol "true") true
                        (symbol "nil") nil
                        '!= (primitive (fn [x y] (not (= x y))))}
                       (zipmap opearations (map primitive mappings)))]
    (extend-env bindings (empty-env))))

(defn type? 
  [ast tag]
  (and (seq? ast)
       (= tag (first ast))))

(defn modify
  [original-env var-name value]
  (loop [{:keys (bindings parent)} original-env]
    (cond (find @bindings var-name) (swap! bindings assoc var-name value)
          parent (recur parent)
          :else (swap! (:bindings original-env) assoc var-name value))))

(defn eval-function 
  [[_ function-name args & body] env]
  (modify env 
          function-name
          {:kind :function
           :args args
           :body (list* 'begin body)
           :env env})
  (str "#'" function-name))

(defn eval-set
  [[_ var-name expr] env]
  (modify env var-name (evaluate expr env))
  nil)

(defn eval-if
  [[_ condition consequent alternative] env]
  (if (evaluate condition env)
    (evaluate consequent env)
    (if alternative
      (evaluate alternative env))))

(defn eval-begin
  [[_ & asts] env]
  (loop [asts asts]
    (let [statement-value (evaluate (first asts) env)]
      (if (or (= (count asts) 1) (:return @(:bindings env)))
        statement-value
        (recur (rest asts))))))

(defn eval-return
  [[_ ast] env]
  (modify env :return true)
  (evaluate ast env))

(defn make-constructor
  [name]
  {:kind :constructor
   :class name})

(defn make-default-initialize
  [parent]
  (if parent
    '(function initialize () (dot super (initialize)))
    '(function initialize () nil)))
      
(defn eval-class 
  [[_ name body parent] env]
  (let [parent-bindings (if parent @(:bindings (:env (lookup env parent))) {})
        class-env (extend-env (conj parent-bindings
                                    ['new (make-constructor name)])
                              env)]
    (evaluate (make-default-initialize parent) class-env)
    (evaluate body class-env)
    (modify env
            name
            {:kind :class
             :parent parent
             :env class-env}))
  (str "#'" name))

(defn eval-dot
  [[_ reciever-name message] env]
  (let [reciever (evaluate reciever-name env)]
    (evaluate message (conj (:env reciever) [:parent env]))))

(defn create-new-object
  [func params env]
  (let [parent-class (:class func)
        parent (lookup env parent-class)
        new-env (extend-env @(:bindings (:env parent)) env)
        obj {:env new-env :class parent-class :kind :object}]
    (if-let [parent-parent-class (:parent parent)]
            (modify new-env 'super (create-new-object
                                     (lookup (:env parent-parent-class)
                                             'new)
                                     params
                                     new-env)))
    (modify new-env 'this obj)
    (eval-dot (list 'dot 'this (list* 'initialize params)) new-env)))


(defn invoke-func
  [func params env]
  (cond (= (:kind func) :primitive)
        (apply (:code func) params)

        (= (:kind func) :function)
        (evaluate (:body func)
                  (extend-env (zipmap (:args func) params) env))

        (= (:kind func) :constructor)
        (create-new-object func params env)

        :else
        (error "Don't know how to invoke: " func)))

(defn evaluate
  [ast env]
  (cond (or (true? ast) (false? ast) (number? ast)) ast
        (symbol? ast) (lookup env ast)
        (type? ast 'function) (eval-function ast env)
        (type? ast 'set!) (eval-set ast env)
        (type? ast 'return) (eval-return ast env)
        (type? ast 'if) (eval-if ast env)
        (type? ast 'begin) (eval-begin ast env)
        (type? ast 'class) (eval-class ast env)
        (type? ast 'dot) (eval-dot ast env)
        :else 
        (invoke-func (evaluate (first ast) env)
                     (map #(evaluate % env) (rest ast))
                     env)))
