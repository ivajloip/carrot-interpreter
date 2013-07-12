(ns carrot-interpreter.repl-test
    (:use clojure.test
          conjure.core
          carrot-interpreter.repl))

(deftest eval-class-adds-class-to-env
  (testing "eval-class adds a class definition to the current environment"
    (let [inputs (atom ["def func (x) do\\" "x\\" "end"])]
      (stubbing [read-line (fn [] (let [result (first @inputs)] (swap! inputs rest) result))]
                (is (= (read-statement) "def func (x) do\nx\nend"))))))

