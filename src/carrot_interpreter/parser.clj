(ns carrot-interpreter.parser
  (:use (blancas.kern (core :exclude (parse) :as kern))
        ;blancas.kern.expr
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
    (return (list* 'function
                   (symbol function-name)
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
         _ (optional new-line)
         alternative (optional (>> (token "else") block))]
    (return
      (if alternative
        (list 'if conditional (begin consequent) (begin alternative))
        (list 'if conditional (begin consequent))))))

(def funcall
  (bind [func (<|> (fwd var-ref) (parens (fwd expr)))
         exprs (parens (sep-by comma expr))]
      (return (list* func exprs))))
;"This parser corresponds a syntax rule: FUN := ID (expr)
; It returns the result of applying the primitive function to the
; result of the expression."
;  (bind [id identifier ex (parens expr)]
;      (return ((fun-tbl id) ex))))

(def factor
  "Evaluates a number (double), a call to a buil-in function, or an
   expression in parens to (for example) change the order of evaluation."
  (<|> float-lit (<:> funcall) var-ref (parens (fwd expr))))

(defn prefix1
  [p op]
  (<|> (bind [f op
              a (prefix1 p op)]
          (return (list f a)))
       (bind [a p] (return a))))

(defn chainl1
  [p op]
  (letfn [(rest [a] (<|> (bind [f op b p] (rest (list f a b)))
                         (return a)))]
    (bind [a p] (rest a))))

(defn chainr1
  [p op]
  (bind [a p]
        (<|> (bind [f op b (chainr1 p op)]
                   (return (list f a b)))
             (return a))))

(defn ops
  [& symbols]
  (bind [op (apply token symbols)]
    (return (symbol op))))

(defn rename
  [op n]
  (>> (token op) (return n)))

;; Chain calls parse multiple occurrences of a kind of operator
;; working on operands that may be expressions. In this case,
;; uni-op operators have the highest precedence while the and-op
;; operator has the lowest.

(def unary (prefix1 factor (ops "!" "-")))
(def power (chainr1 unary  (ops "^")))
(def term  (chainl1 power  (ops "*" "/" "%")))
(def sum   (chainl1 term   (ops "+" "-")))
(def relex (chainl1 sum    (ops "==" "!=" ">=" "<=" ">" "<")))
(def orex  (chainl1 relex  (rename "&&" 'and)))
(def expr  (chainl1 orex   (rename "||" 'or)))

(def statement (<|> (<:> assign)
                    condition
                    (<:> (fwd function-def))
                    string-lit
                    (<:> (fwd block))
                    expr))

(def program (bind [statements (many1 statement)]
                (return (apply list statements))))

(defn parse
  ([input] (parse input program))
  ([input parser] 
   (let [result (kern/parse parser input)]
     (when-not (:ok result)
       (print-error result)
       (throw (RuntimeException. "Parsing failed")))
     (:value result))))
