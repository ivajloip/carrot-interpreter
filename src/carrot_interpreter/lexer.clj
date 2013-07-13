(ns carrot-interpreter.lexer
  (:use (blancas.kern (core :exclude (parse) :as kern))
        blancas.kern.expr)
  (:require [blancas.kern.lexer :as lex]))

(def hoc-style
  (assoc lex/basic-def
         :comment-start       "(*"
         :comment-end         "*)"
         :identifier-letter   (<|> alpha-num (one-of* "_-?"))
         :reserved-names      ["end"
                               "do"
                               "extends"
                               "if"
                               "else"
                               "return"
                               "def"
                               "class"
                               "include"
                               "module"]
         :case-sensitive      true
         :trim-newline        false))

(def- rec (lex/make-parsers hoc-style))

(def trim       (:trim       rec))
(def sym        (:sym        rec))
(def new-line   (:new-line   rec))
(def word       (:word       rec))
(def string-lit (:string-lit rec))
(def dec-lit    (:dec-lit    rec))
(def float-lit  (:float-lit  rec))
(def parens     (:parens     rec))
(def braces     (:braces     rec))
(def comma-sep  (:comma-sep  rec))
(def comma      (:comma      rec))
(def token      (:token      rec))

(def identifier (:identifier rec))
