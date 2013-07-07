(ns carrot-interpreter.parser-test
    (:use clojure.test
          (blancas.kern (core :exclude (parse) :as kern))
          carrot-interpreter.lexer
          carrot-interpreter.parser))

; -------------- factor -------------------

(deftest factor-parses-positive-numbers
  (testing "Parsing a positive float number works"
    (is (= (:value (kern/parse factor "1.33")) 1.33))))

(deftest factor-parses-negative-numbers
  (testing "Parsing a negative float number works"
    (is (= (:value (kern/parse factor "-0.33")) -0.33))))

(deftest factor-parses-function-invokation
  (testing "Parsing a function invokation works"
    (is (= (:value (kern/parse factor "test(3)")) '(test 3.0)))))

(deftest factor-parses-variable-reference
  (testing "Parsing a variable reference works"
    (is (= (:value (kern/parse factor "test")) 'test))))

(deftest factor-parses-parens
  (testing "Parsing a value in parens works"
    (is (= (:value (kern/parse factor "(2)")) 2.0))))

; -------------- unary -------------------

(deftest unary-parses-negation
  (testing "Parsing a boolean negatoin works"
    (is (= (:value (kern/parse unary "!2")) '(! 2.0)))))

(deftest unary-parses-sign-change
  (testing "Parsing a sign change works"
    (is (= (:value (kern/parse unary "-2")) '(- 2.0)))))

; -------------- power -------------------
;(deftest unary-parses-negation
;  (testing "Parsing a power expression works"
;    (is (= (:value (kern/parse power "3^2"))
;           '(^ 3.0 2.0)))))

; -------------- term -------------------
(deftest term-parses-multiplication
  (testing "Parsing a single multiplication works"
    (is (= (:value (kern/parse term "2 * 3")) '(* 2.0 3.0)))))

(deftest term-parses-division
  (testing "Parsing a single division works"
    (is (= (:value (kern/parse term "2 / 3")) '(/ 2.0 3.0)))))

(deftest term-parses-remaining
  (testing "Parsing a division remaining works"
    (is (= (:value (kern/parse term "3 % 2")) '(% 3.0 2.0)))))

; -------------- sum -------------------
(deftest term-parses-addition
  (testing "Parsing a single addiiton works"
    (is (= (:value (kern/parse sum "2 + 3")) '(+ 2.0 3.0)))))

(deftest term-parses-subtraciton
  (testing "Parsing a single subtraction works"
    (is (= (:value (kern/parse sum "2 - 3")) '(- 2.0 3.0)))))
