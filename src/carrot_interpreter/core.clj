(ns carrot-interpreter.core
  (:use [carrot-interpreter.repl :only (repl run-file)])
  (gen-class :main true))

(defn -main 
  ([] (repl))
  ([filename] (run-file filename)))

