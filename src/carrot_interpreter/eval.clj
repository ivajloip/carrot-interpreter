(ns carrot-interpreter.eval)

(defn default-env [] '())

(defn error
  [& args]
  (throw (RuntimeException. (apply str args))))

(defn lookup
  [{:keys (bindings parent)} var-name]
  (cond (find @bindings var-name) (var-name @bindings)
        parent (recur parent var-name)
        :else (error "Unbound variable: " var-name)))

(defn evaluate
  [ast env]
  (cond (number? ast) ast
        (symbol? ast) (lookup env ast)
        :else ast
        ;(tag? ast '*) (lift * ast env)
        ;(tag? ast 'set!) (eval-set ast env)
        ;(tag? ast 'if) (eval-if ast env)

        ;(tag? ast 'begin) (eval-begin ast env)
        ;(tag? ast 'lambda) (make-lambda ast env)

        ;:else
        ;(invoke (evaluate (first ast) env)
        ;        (map #(evaluate % env) (rest ast)))))
))
