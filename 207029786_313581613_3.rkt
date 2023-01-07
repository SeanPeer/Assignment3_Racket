#lang pl

#|
In this question we fill ">" "<" "=" "Not" "If" "True" "False"
It was very simple and there wasn't difficulties
 it wes took 5 minutes
|#

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

#|
In this question we fill all Boolean conditions
The main difficult was to fill 'If' and we solve this problem in that we saw the grammer.
This question took 10 minutes.
|#

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

#|
This question took 15 minutes,
the main difficult wat to fill the If line And we solve this quesion with tests
|#

(: parse-sexpr : Sexpr -> FLANG)
 ;; to convert s-expressions into FLANGs
 (define (parse-sexpr sexpr)
         (match sexpr
           [(number: n) (Num n)]
           [(symbol: name) (Id name)]
           [(cons 'with more)
           (match sexpr
             [(list 'with (list (symbol: name) named-expr) body)
              (With name (parse-sexpr named-expr) (parse-sexpr body))]
             [else (error 'parse-sexpr "bad with syntax in ~s" sexpr )])]
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
           [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
           [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
           [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
           [(list 'not exp) (Not (parse-sexpr exp))]
           [(cons 'if rest)
            (match rest;split if 
              [(cons  condition  con)(match con
                                        [(list (list 'then-do do)(list 'else-do else))(If (parse-sexpr condition) (parse-sexpr do) (parse-sexpr else))]
                                                                   [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])
                                                                   ])]
                                                                             
           [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]
))
(: parse : (String -> FLANG))
(define (parse code)
  (parse-sexpr (string->sexpr code))
  )

;test parse-sexpr
(test (parse "{> 3 2}")=> (Bigger (Num 3) (Num 2)))
(test (parse "{< {+ 3 4} {- 11 2}}")=> (Smaller (Add (Num 3) (Num 4)) (Sub (Num 11) (Num 2))))
(test (parse "{if {not {> 2 4}} {then-do {+ 3 2}} {else-do 5}}")=> (If (Not (Bigger (Num 2) (Num 4))) (Add (Num 3) (Num 2)) (Num 5)))
(test (parse "{fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}")=> (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (parse "{with {x 6} {* x {/ x x} }}")=> (With 'x (Num 6) (Mul (Id 'x) (Div (Id 'x) (Id 'x)))))

#|
Formal Substitution rules: 
 
subst: 
 N[v/x] = N 
 {+ E1 E2}[v/x] = {+ E1[v/x] E2[v/x]} 
 {- E1 E2}[v/x] = {- E1[v/x] E2[v/x]} 
 {* E1 E2}[v/x] = {* E1[v/x] E2[v/x]} 
{/ E1 E2}[v/x] = {/ E1[v/x] E2[v/x]} 
y[v/x] = y 
x[v/x] = v 
 {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x 
 {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2} 
 {call E1 E2}[v/x] = {call E1[v/x] E2[v/x]} 
 {fun {y} E}[v/x] = {fun {y} E[v/x]} ; if y =/= x 
 {fun {x} E}[v/x] = {fun {x} E} 
 B[v/x] = B ;; B is Boolean 
 {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]} 
 {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]} 
 {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]} 
 { not E}[v/x] = {not E[v/x]} 
 {if Econd {then-do Edo} {else-do Eelse}}[v/x] 
 = {if Econd[v/x] {then-do Edo[v/x]} {else-do 
Eelse[v/x]}}
|#

#|
In this question we fill the subst and there wasn't any difficults.
This question took 10 minutes.
|#


(: subst : FLANG Symbol FLANG -> FLANG) 
 ;; substitutes the second argument with the third argument in the 
 ;; first argument, as per the rules of substitution; the resulting 
 ;; expression contains no free instances of the second argument 
 (define (subst expr from to) 
 (cases expr 
  [(Num n)expr]
  [(Add l r)(Add (subst l from to) (subst r from to))]
  {(Sub l r) (Sub (subst l from to) (subst r from to))}
  [(Mul l r) (Mul (subst l from to) (subst r from to))]
  {(Div l r)  (Div (subst l from to) (subst r from to))}
  [(With name named body) (With name
                                (subst named from to)
                                (if (eq? named from) body
                                    (subst body from to)))]
  [(Id name)(if(eq? name from) to expr)]
  [(Fun name body) (Fun name (if (eq? name from) body (subst body from to)))]; parameter-name, body
 [(Call fun-expr arg-expr) (Call (subst fun-expr from to)(subst arg-expr from to))]   
 [(Bool b) (Bool b)] 
 [(Equal l r)(Equal (subst l from to)(subst r from to))] 
 [(Bigger l r)(Bigger (subst l from to)(subst r from to))] 
 [(Smaller l r)(Smaller (subst l from to)(subst r from to))] 
 [(Not E)(Not (subst E from to))] 
 [(If f1 f2 f3)(If (subst f1 from to)(subst f2 from to)(subst f3 from to))]; do subst for each part in If
   ))


;test Subst
(test (subst (Fun 'x (Add (Id 'x) (Id 'y))) 'x (Num 4)) => (Fun 'x (Add (Id 'x) (Id 'y))))
(test (subst (Bigger (Id 'x) (Num 3)) 'x (Num 2))=> (Bigger (Num 2) (Num 3)))
(test (subst (Not (Equal (Id 'x) (Add (Add (Num 2) (Num 4)) (Mul (Div (Num 4) (Num 2)) (Num 4))))) 'x (Num 5))=>(Not (Equal (Num 5) (Add (Add (Num 2) (Num 4)) (Mul (Div (Num 4) (Num 2)) (Num 4))))))
(test (subst (With 'z  (Add (Id 'x) (Id 'x)) (Bigger (Id 'x) (Num 5))) 'x (Num 7))
      =>(With 'z (Add (Num 7) (Num 7)) (Bigger (Num 7) (Num 5))))

(test (subst (If (Bigger (Id 'x) (Add (Num 4) (Id 'x)))(Add (Id 'x) (Num 5))(Num 4)) 'x (Num 5))
      =>(If (Bigger (Num 5) (Add (Num 4) (Num 5))) (Add (Num 5) (Num 5)) (Num 4)))


#|
The main problem in this question is that we needed to return #t if we didn't get boolean in flang-bool
This quesion took 15 minutes.
|# 
;; The following function is used in multiple places below, 
 ;; hence, it is now a top-level definition 
 (: Num->number : FLANG -> Number) 
 ;; gets a FLANG -- presumably a Num variant -- and returns the 
 ;; unwrapped number 
(define (Num->number e) 
 (cases e 
 [(Num n) n] 
 [else (error 'Num->number "expected a number, got: ~s" e)])) 
 (: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG) ;;***
 ;; gets a Racket numeric binary operator, and uses it within a FLANG 
 ;; `Num' wrapper 
 (define (arith-op op expr1 expr2) 
 (Num (op (Num->number expr1) (Num->number expr2)))) 
 (: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG) 
 ;; gets a Racket Boolean binary operator (on numbers), and applies it 
 ;; to two `Num' wrapped FLANGs 
 (define (logic-op op expr1 expr2) 
    (Bool (op (Num->number expr1) (Num->number expr2)))) 
   
   
 (: flang->bool : FLANG -> Boolean) 
 ;; gets a Flang E (of any kind) and returns a its appropiate 
 ;; Boolean value -- which is true if and only if E does not 
;; represent false 
 ;; Remark: the `flang->bool` function will also be top-level 
 ;; since it's used in more than one place. 
 (define (flang->bool e) 
 (cases e 
 [(Bool b) b] ;if b is boolean return b
[else #t ])); if e isn't boolean return true
 ;; gets a Racket Boolean binary operator (on numbers), and applies it 
 ;; to two `Num' wrapped FLANGs 

#|
 The main difficult was to fill the "If" line
and we solve this problem after 20 minutes by using tests.
|#
(: eval : FLANG -> FLANG) 
 ;; evaluates FLANG expressions by reducing them to *expressions* 
 (define (eval expr) 
 (cases expr
  [(Num n)expr]
  [(Add l r) (arith-op + (eval l) (eval r))]
  [(Sub l r) (arith-op - (eval l) (eval r))]
  [(Mul l r) (arith-op * (eval l) (eval r))]
  [(Div l r) (arith-op / (eval l) (eval r))]
  [(With name named-expr body)
  (eval (subst body name  (eval named-expr)))]
  [(Fun name body)expr]
  [(Call fun-expr arg-expr)
   (let ([fval (eval fun-expr)])
     (cases fval
       [(Fun name body)   (eval (subst body
                name
                (eval arg-expr)))]
         [else (error 'eval "expected a function, got: ~s" fun-expr)]))]

 [(Bool b) expr]
 [(Equal l r)(logic-op = (eval l) (eval r))] 

 [(Bigger l r)(logic-op > (eval l) (eval r))] 
 [(Smaller l r)(logic-op < (eval l) (eval r))]
 [(If l m r) 
  (let ([boll (flang->bool (eval l))]) 
 (if (eq?  boll #f)  (eval r)(eval m)))]
 [(Id name)(match name
             ['True (Bool #t)]
             ['False(Bool #f)]
             [else (error 'eval "free identifier: ~s" name) ])]
  
 [(Not exp)(Bool (not (flang->bool (eval exp))))])) 


(test (eval (Not(Bool #t)))=> (Bool #f))
(test (eval (Bigger (Num 2) (Num 3)))=>(Bool false))
(test (eval(Not (Equal (Num 5) (Add (Add (Num 2) (Num 4)) (Mul (Div (Num 4) (Num 2)) (Num 4))))))=>(Bool true))
(test (eval (With 'z (Add (Num 7) (Num 7)) (Bigger (Num 7) (Num 5)))) => (Bool #t))
(test (eval (If (Bigger (Num 5) (Add (Num 4) (Num 5))) (Add (Num 5) (Num 5)) (Num 4)))
      =>(Num 4))
(test (eval (Bigger (Num 3) (Num 2)))=>(Bool true))
(test (eval(Bigger (Add (Num 3) (Num 4)) (Sub (Num 11) (Num 2))))=>(Bool false))
(test (eval (If (Not (Equal (Num 2) (Num 4))) (Add (Num 3) (Num 2)) (Num 5)))=>(Num 5))
(test (eval(With 'x (Num 6) (Mul (Id 'x) (Div (Id 'x) (Id 'x)))))=> (Num 6))


#|
it was very easy question and it's took 5 minutes.

|#

(: run : String -> (U Number Boolean FLANG)) 
 ;; evaluate a FLANG program contained in a string 
 (define (run str) 
 (let ([result (eval (parse str))]) 
(cases result 
 [(Num n)n]
 [(Bool b) b]
  [(Fun name body) (Fun name body)]
 [else (error 'run "evaluation returned a non -number ~s" result)] )))

(test (run "{> 2 3}")=> false)
;; tests 
(test (run "True") => true) 
(test (run "{not True}") => false) 
(test (run "{> 3 44}") => false) 
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4) 
(test (run "{with {x 8} 
 {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4) 
(test (run "{with {x 0} 
{if {> x 0} {then-do {+ 2 x}} {else-do x}}}") => 0) 
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true) 
(test (run "{with {c True} 
 {if c {then-do {> 2 1}} {else-do 2}}}") 
 => true) 
(test (run "{with {foo {fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}") 
 => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{call {with {foo {fun {x} {* x 4}}} foo} 3}") => 12)

(test (run "{with {x 0} 
 {if {> x 0} {/ 2 x} x}}") 
 =error> "parse-sexpr: bad `if' syntax in (if (> x 0) (/ 2 x) x)") 
 (test (run "true") =error> "eval: free identifier: true") 
(test (run "{< false 5}") =error> "eval: free identifier: false") 
(test (run "{< False 5}") 
 =error> "Num->number: expected a number, got: #(struct:Bool #f)")