#lang pl



#| The grammar:
 
 <FLANG> ::= <num> ;; Rule 1
           | { + <FLANG> <FLANG> } ;; Rule 2
           | { - <FLANG> <FLANG> } ;; Rule 3
           | { * <FLANG> <FLANG> } ;; Rule 4
           | { / <FLANG> <FLANG> } ;; Rule 5
           | { with { <id> <FLANG> } <FLANG> } ;; Rule 6
           | <id> ;; Rule 7
           | { fun { <id> } <FLANG> } ;; Rule 8
           | { call <FLANG> <FLANG> } ;; Rule 9
           | <True> add rule for True ;; Rule 10
           | <False> Rule 11
           | { = <FLANG> <FLANG>} add rule for = ;; Rule 12
           | { > <FLANG> <FLANG>} Rule 13
           | { < <FLANG> <FLANG>} Rule 14
           | { NOT <FLANG>} Rule 15
           | {if <FLANG> <FLANG> <FLANG>} add rule 16 for (the above) if
expressions
|#

;;Use the above test examples to complete the missing parts of the FLANG
;;type definition and the parse-sexpr procedure.
(define-type FLANG
 [Num Number]
 [Add FLANG FLANG]
 [Sub FLANG FLANG]
 [Mul FLANG FLANG]
 [Div FLANG FLANG]
 [With Symbol FLANG FLANG];name, named-expr, body
 [Id Symbol]
 [Fun Symbol FLANG]; parameter-name, body
 [Call FLANG FLANG]
 [Bool Boolean]
 [Bigger FLANG FLANG]
 [Smaller FLANG FLANG]
 [Equal FLANG FLANG]
 [Not FLANG]
 [If FLANG FLANG FLANG])


(: parse-sexpr : Sexpr -> FLANG)
 ;; to convert s-expressions into FLANGs
 (define (parse-sexpr sexpr)
         (match sexpr
           [(number: n) (Num n)]
           [(symbol: name) (Id name)]
           [(cons 'with more)
            ( match sexpr
               [(list 'with (list (symbol: name) named-expr) body)
                                      (With name (parse-sexpr named-expr) (parse-sexpr body))]
               [else (error 'parse-sexpr "bad with syntax!!")])]
           [(cons 'fun more)
           ( match sexpr
              [(list 'fun (list (symbol: name)) body) (Fun name (parse-sexpr body))]
              [else (error 'parse-sexpr "bad fun syntax!!")])]
           [(list 'call fun-expr arg-expr) (Call (parse-sexpr fun-expr) (parse-sexpr arg-expr))]
           [(list '+ l r) (Add (parse-sexpr l) (parse-sexpr r))]
           [(list '- l r) (Sub (parse-sexpr l) (parse-sexpr r))]
           [(list '* l r) (Mul (parse-sexpr l) (parse-sexpr r))]
           [(list '/ l r) (Div (parse-sexpr l) (parse-sexpr r))]  
           ['True (Bool true)]
           ['False (Bool false)]
           [(symbol: name) (Id name)]
           [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
           [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
           [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
           [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
           [(list 'not exp) (parse-sexpr exp)]
           [(cons 'if rest)
            (match rest
              [(list 'if condition 'then then-do 'else else-do) (If (parse-sexpr condition) (parse-sexpr then-do) (parse-sexpr else-do))]
              [else (error 'parse-sexpr "bad 'if' syntax in ~s" rest)])]
           [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

;; tests
(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{> 3 44}") => false)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{with {x 8}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0}
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{with {c True}
 {if c {then-do {> 2 1}} {else-do 2}}}")
 => true)
(test (run "{with {foo {fun {x}
 {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}")
 => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {x 0}
 {if {> x 0} {/ 2 x} x}}")
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)")
 (test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}")
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")