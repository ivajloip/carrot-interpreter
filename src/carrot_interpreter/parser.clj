(ns carrot-interpreter.parser
  (:use (blancas.kern (core :exclude (parse) :as kern))
        blancas.kern.expr
        carrot-interpreter.lexer))

(declare condition)
(declare statement)
(declare expr)
(declare block)

(def assign
  (bind [id identifier
         _ (token "=")
         ex expr]
    (return (list 'set! (symbol id) ex))))

(def var-ref
  (bind [id identifier]
    (return (symbol id))))

(def block
  (bind [_ (token "do") stmts (sep-by new-line (fwd statement)) _ (token "end")]
    (return (apply list stmts))))


(def function-def
  (bind [_ (token "def")
         function-name identifier
         params (parens (sep-by comma identifier))
         body block]
    (return (list* 'function (symbol function-name)
                   (apply list (map symbol params))
                   body))))

(defn- begin
  [ast]
  (if (= (count ast) 1)
    (first ast)
    (list* 'begin ast)))

(def condition
  (bind [_ (token "if")
         conditional (parens expr)
         consequent block
         alternative (optional (>> (token "else") block))]
    (return
      (if alternative
        (list 'if conditional (begin consequent) (begin alternative))
        (list 'if conditional (begin consequent))))))

(def fun-tbl
  "Definition of built-in functions."
  {"abs"   #(Math/abs %)   "atan" #(Math/atan %)      "cos"  #(Math/cos %)
   "exp"   #(Math/exp %)   "int"  #(Math/round %)     "log"  #(Math/log %)
   "log10" #(Math/log10 %) "sin"  #(Math/sin %)       "sqrt" #(Math/sqrt %)
   "tan"   #(Math/tan %)   "tanh" #(Math/tanh %)      "ceil" #(Math/ceil %)
   "floor" #(Math/floor %) "deg"  #(Math/toDegrees %) "rad"  #(Math/toRadians %)})

(def funcall
"This parser corresponds a syntax rule: FUN := ID (expr)
 It returns the result of applying the primitive function to the
 result of the expression."
  (bind [id identifier ex (parens expr)]
      (return ((fun-tbl id) ex))))

(def factor
  "Evaluates a number (double), a call to a buil-in function, or an
   expression in parens to (for example) change the order of evaluation."
  (<|> float-lit funcall (parens (fwd expr))))

;; Chain calls parse multiple occurrences of a kind of operator
;; working on operands that may be expressions. In this case,
;; uni-op operators have the highest precedence while the and-op
;; operator has the lowest.

(def unary (prefix1 factor uni-op))  ;; -(10), !(3>0)
(def power (chainr1 unary  pow-op))  ;; 2^32
(def term  (chainl1 power  mul-op))  ;; 3 * 34 * ...
(def sum   (chainl1 term   add-op))  ;; 5 + 2*3 + ...
(def relex (chainl1 sum    rel-op))  ;; sin(0.5) > 0
(def orex  (chainl1 relex  or-op))   ;; sqrt(10) > 0 || tan(0) == 1 || ...
(def expr  (chainl1 orex   and-op))  ;; sqrt(10) > 0 && tan(0) == 1 || sin(0) > 0 & ...







(def statement (<|> (<:> assign) condition (<:> (fwd function-def)) string-lit (<:> (fwd block)) expr))

(def program (bind [statements (many1 statement)]
                (return (apply list statements))))

(defn parse
  ([input] (parse input program))
  ([input parser] 
   (let [result (kern/parse parser input)]
     (when-not (:ok result)
       (println "I failed"))
     (:value result))))
