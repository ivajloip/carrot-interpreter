(ns carrot-interpreter.repl
  (:use [carrot-interpreter.parser :only (parse statement program)]
        [carrot-interpreter.eval :only (default-env evaluate)]))

(def repl-env (default-env))

(defn eval-and-print
  [input]
  (try
    (-> input (parse statement) (evaluate repl-env) println)
    (catch RuntimeException e
      (println "FAIL:" (.getMessage e)))))

(defn parse-and-print
  [input]
  (try
    (-> input (parse statement) println)
    (catch RuntimeException e
      (println "FAIL:" (.getMessage e)))))

(defn run-file
  [file]
  (let [stmts (-> file slurp (parse program))
        env (default-env)]
    (doseq [stm stmts]
      (evaluate stm env))))

(defn repl []
  (print "> ")
  (flush)
  (let [input (read-line)]
    (cond (= \. (get input 0))
          (do (parse-and-print (subs input 1))
              (recur))

          (= "exit" input)
          (println "See ya!")

          :else
          (do (eval-and-print input)
              (recur)))))
