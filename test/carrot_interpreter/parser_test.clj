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
(deftest power-parses-power
  (testing "Parsing a power expression works"
    (is (= (:value (kern/parse power "3^2"))
           (list (symbol "^") 3.0 2.0)))))

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

; -------------- relax -------------------
(deftest relax-parses-equation
  (testing "Parsing a comparison expression =="
    (is (= (:value (kern/parse relex "2 == x")) '(== 2.0 x)))))

(deftest relax-parses-inequality
  (testing "Parsing a comparison expression !="
    (is (= (:value (kern/parse relex "2 != x")) '(!= 2.0 x)))))

(deftest relax-parses-less-or-equal
  (testing "Parsing a comparison expression <="
    (is (= (:value (kern/parse relex "2 <= x")) '(<= 2.0 x)))))

(deftest relax-parses-greater-or-equal
  (testing "Parsing a comparison expression >="
    (is (= (:value (kern/parse relex "2 >= x")) '(>= 2.0 x)))))

(deftest relax-parses-less
  (testing "Parsing a comparison expression <"
    (is (= (:value (kern/parse relex "2 < x")) '(< 2.0 x)))))

(deftest relax-parses-greater
  (testing "Parsing a comparison expression >"
    (is (= (:value (kern/parse relex "2 > x")) '(> 2.0 x)))))

; -------------- orex -------------------
(deftest orex-parses-and
  (testing "Parsing a comparison expression &&"
    (is (= (:value (kern/parse orex "y && x")) '(and y x)))))

; -------------- expr -------------------
(deftest expr-parses-or
  (testing "Parsing a comparison expression ||"
    (is (= (:value (kern/parse expr "y || x")) '(or y x)))))

(deftest expr-parses-or
  (testing "Parsing an expression with different priorities"
    (is (= (:value (kern/parse expr "y || x && z == 3 + 4 * -2"))
           '(or y (and x (== z (+ 3.0 (* 4.0 (- 2.0))))))))))

; -------------- assign -------------------
(deftest assign-parses-assigning
  (testing "Parsing an assigning"
    (is (= (:value (kern/parse assign "y = x")) '(set! y x)))))

; -------------- var-ref -------------------
(deftest var-ref-parses-var-references
  (testing "Parsing a variable references"
    (is (= (:value (kern/parse var-ref "my_variable")) 'my_variable))))

; -------------- block -------------------
(deftest block-parses-simple-code-block
  (testing "Parsing a simple code block"
    (is (= (:value (kern/parse block "do 3 end")) '(3.0)))))

(deftest block-parses-code-block-with-multiple-statements
  (testing "Parsing a code block with multipliple statemets"
    (is (= (:value (kern/parse block "do 3 \n4 end")) '(3.0 4.0)))))

(deftest block-parses-nested-code-block
  (testing "Parsing a nested code block"
    (is (= (:value (kern/parse block "do do 3 end\n4 end")) '((3.0) 4.0)))))
