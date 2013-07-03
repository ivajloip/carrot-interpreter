(ns carrot-interpreter.core
  (:use [carrot-interpreter.repl :only (repl run-file)])
  (gen-class :main true))

(defn interpret-file [] '())

(defn -main 
  ([] (repl))
  ([filename] (interpret-file filename)))

