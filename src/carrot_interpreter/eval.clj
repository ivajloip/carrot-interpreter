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
  (extend-env {'print (primitive prn)
               'twice (primitive (fn [a] (+ a a)))
               '+ (primitive +)
               '* (primitive *)
               '== (primitive =)
               '- (primitive -)}
              (empty-env)))

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

(defn make-function 
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
  (println :begin asts env)
  (loop [asts asts]
    (let [statement-value (evaluate (first asts) env)]
      (if (or (= (count asts) 1) (:return @(:bindings env)))
        statement-value
        (recur (rest asts))))))

(defn eval-return
  [[_ ast] env]
  (modify env :return true)
  (evaluate ast env))

(defn invoke-func
  [func params]
  (cond (= (:kind func) :primitive)
        (apply (:code func) params)

        (= (:kind func) :function)
        (evaluate (:body func)
                  (extend-env (zipmap (:args func) params)
                              (:env func)))

        :else
        (error "Don't know how to invoke: " func)))

(defn evaluate
  [ast env]
  (cond (or (true? ast) (false? ast) (number? ast)) ast
        (symbol? ast) (lookup env ast)
        (type? ast 'function) (make-function ast env)
        (type? ast 'set!) (eval-set ast env)
        (type? ast 'return) (eval-return ast env)
        (type? ast 'if) (eval-if ast env)
        (type? ast 'begin) (eval-begin ast env)
        :else 
        (invoke-func (evaluate (first ast) env)
                (map #(evaluate % env) (rest ast)))))
