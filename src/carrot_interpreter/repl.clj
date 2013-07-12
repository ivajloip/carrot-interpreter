(ns carrot-interpreter.repl
  (:use [carrot-interpreter.parser :only (parse statement program)]
        [clojure.string :only (join split)]
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

(defn read-statement 
  ([] (read-statement []))
  ([result] 
   (let [input (read-line) len (count input)]
     (if (= \\ (get input (dec len))) 
       (recur (conj result (subs input 0 (dec len))))
       (join "\n" (conj result input))))))

(defn repl []
  (print "> ")
  (flush)
  (let [input (read-statement)]
    (cond (= \. (get input 0))
          (do (parse-and-print (subs input 1))
              (recur))

          (= "exit" input)
          (println "Au revoir!")

          :else
          (do (eval-and-print input)
              (recur)))))
