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

(defn evaluate
  [ast env]
  (cond (number? ast) ast
        (symbol? ast) (lookup env ast)
        (type? ast 'function) (make-function ast env)
        (type? ast 'set!) (eval-set ast env)
        :else ast
        ;(tag? ast '*) (lift * ast env)
        ;(tag? ast 'if) (eval-if ast env)

        ;(tag? ast 'begin) (eval-begin ast env)
        ;(tag? ast 'lambda) (make-lambda ast env)

        ;:else
        ;(invoke (evaluate (first ast) env)
        ;        (map #(evaluate % env) (rest ast)))))
))
