(ns carrot-interpreter.parser-test
    (:use clojure.test
          (blancas.kern (core :exclude (parse) :as kern))
          conjure.core
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

(deftest var-ref-does-not-parse-end
  (testing "Parsing key-word end fails a variable references"
    (is (not (:ok (kern/parse var-ref "end"))))))

(deftest var-ref-does-not-parse-do
  (testing "Parsing key-word do fails a variable references"
    (is (not (:ok (kern/parse var-ref "do"))))))

(deftest var-ref-does-not-parse-def
  (testing "Parsing key-word def fails a variable references"
    (is (not (:ok (kern/parse var-ref "def"))))))

(deftest var-ref-does-not-parse-return
  (testing "Parsing key-word return fails a variable references"
    (is (not (:ok (kern/parse var-ref "return"))))))

(deftest var-ref-does-not-parse-class
  (testing "Parsing key-word class fails a variable references"
    (is (not (:ok (kern/parse var-ref "class"))))))

(deftest var-ref-does-not-parse-module
  (testing "Parsing key-word module fails a variable references"
    (is (not (:ok (kern/parse var-ref "module"))))))

(deftest var-ref-does-not-parse-extends
  (testing "Parsing key-word extends fails a variable references"
    (is (not (:ok (kern/parse var-ref "extends"))))))

; -------------- block -------------------

(deftest block-parses-simple-code-block
  (testing "Parsing a simple code block"
    (is (= (:value (kern/parse block "do\n3\nend")) '(3.0)))))

(deftest block-parses-code-block-with-multiple-statements
  (testing "Parsing a code block with multipliple statemets"
    (is (= (:value (kern/parse block "do\n3 \n4\nend")) '(3.0 4.0)))))

(deftest block-parses-nested-code-block
  (testing "Parsing a nested code block"
    (is (= (:value (kern/parse block "do\ndo\n3\nend\n4\nend")) '((3.0) 4.0)))))

(deftest block-parses-blocks-with-empty-lines
  (testing "Parsing code block with empty lines succeeds"
    (is (= (:value (kern/parse block "do\n3\n\n4\nend") '(3.0 4.0))))))

; -------------- function-def -------------------

(deftest function-def-parses-function-definitions
  (testing "Parsing a function definition"
    (is (= (:value (kern/parse function-def "def func(a) do\n3\nend"))
           '(function func (a) 3.0)))))

; -------------- condition -------------------

(deftest condition-parses-if-expression
  (testing "Parsing a if condition"
    (is (= (:value (kern/parse condition "if (x == 3) do\n 2\nend"))
           '(if (== x 3.0) 2.0)))))

(deftest function-def-parses-function-definitions
  (testing "Parsing a function definition"
    (is (= (:value (kern/parse condition
                               "if (x == 3) do\n 2\nend else do\n4\nend"))
           '(if (== x 3.0) 2.0 4.0)))))

; -------------- funcall -------------------

(deftest funcall-parses-function-calls-one-arg
  (testing "Parsing a function call with only one argument"
    (is (= (:value (kern/parse funcall "foo(x)")) '(foo x)))))

(deftest funcall-parses-function-calls-many-args
  (testing "Parsing a function call with many arguments"
    (is (= (:value (kern/parse funcall "foo(x, y)")) '(foo x y)))))

; -------------- return-stm -------------------

(deftest return-stm-parses-return-statements
  (testing "Parsing a return statement with factor"
    (is (= (:value (kern/parse return-stm "return x")) '(return x))))
  (testing "Parsing a return statement with expression"
    (is (= (:value (kern/parse return-stm "return x * x)"))
           '(return (* x x))))))

; -------------- class-stm -------------------

(deftest class-stm-parses-simplest-class
  (testing "Parsing a simple class definition"
    (is (= (:value (kern/parse class-stm "class Foo do 
                                            3
                                          end"))
           '(class Foo 3.0)))))

(deftest class-stm-parses-class-with-function
  (testing "Parsing a simple class definition with funciton definition"
    (is (= (:value (kern/parse class-stm "class Foo do 
                                            def test (x) do
                                              x
                                            end
                                          end"))
           '(class Foo (function test (x) x))))))

(deftest class-stm-parses-class-with-parent
  (testing "Parsing a class definition with a parent class"
    (is (= (:value (kern/parse class-stm "class Foo extends Bar do
                                            3
                                          end"))
           '(class Foo 3.0 Bar)))))

(deftest class-stm-parses-class-with-few-function
  (testing "Parsing a simple class definition with a few funciton definition"
    (is (= (:value (kern/parse class-stm "class Foo do 
                                            def test (x) do
                                              x
                                            end

                                            def foo (x) do
                                              x
                                            end
                                          end"))
           '(class Foo (begin (function test (x) x) (function foo (x) x)))))))

; -------------- dot-op -------------------

(deftest dot-op-parses-object-field-or-method-access
  (testing "Dot-op parses object field access"
    (is (= (:value (kern/parse dot-op "x.y")) '(dot x y))))
  (testing "Dot-op parses chained object field access"
    (is (= (:value (kern/parse dot-op "x.y.z")) '(dot x (dot y z)))))
  (testing "Dot-op parses object method access"
    (is (= (:value (kern/parse dot-op "x.y()")) '(dot x (y)))))
  (testing "Dot-op parses method invokation result as object"
    (is (= (:value (kern/parse dot-op "x().y")) '(dot (x) y))))
  (testing "Dot-op parses object property assignment"
    (is (= (:value (kern/parse dot-op "x.y = 3")) '(dot x (set! y 3.0))))))
