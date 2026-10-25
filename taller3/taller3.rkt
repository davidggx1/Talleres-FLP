#lang eopl
(require racket/string)

;*******************************************************************************************
; Interpretador

;; La definición BNF para las expresiones del lenguaje:
;;
;;  <programa>          := <expresion>
;;                         un-program (exp)
;;  <expresion>         := <numero>
;;                         numero-lit (num)
;;                      := <texto>
;;                         text-lit (txt)
;;                      := <identificador>
;;                         var-exp (id)
;;                      := (<expresion> <primitiva-binaria> <expresion>)
;;                         primapp-bin-exp (exp1 prim-binaria exp2)
;;                      := <primitiva-unaria> (<expresion>)
;;                         primapp-un-exp (prim-unaria exp)
;;                      := si <expresion> {<expresion>} sino {<expresion>}
;;                         condicional-exp
;;                      := declarar (arbno <var-exp> = <exp> ;) {<exp>}
;;                         variable-local-exp
;;                      := procedimiento (separated-list <identificador> ,) {<exp>}
;;                      := procedmiento-exp
;;                      := evaluar
;;                      := letrec {identificador ({identificador}*(,)) = <expresion>}* in <expresion>
;;                         <letrec-exp proc-names idss bodies bodyletrec>

;;  <primitiva-binaria> := + (primitiva-suma)
;;                      := ~ (primitiva-resta)
;;                      := / (primitiva-div)
;;                      := * (primitiva-multi)
;;                      := concat (primitiva-concat)
;;                      := > (primitiva-mayor)
;;                      := < (primitiva-menor)
;;                      := >= (primitiva-mayor-igual)
;;                      := <= (primitiva-menor-igual)
;;                      := != (primitiva-diferente)
;;                      := == (primitiva-comparador-igual)

;;  <primitiva-unaria>  := longitud (primitiva-longitud)
;;                      := add1 (primitiva-add1)
;;                      := sub1 (primitiva-sub1)
;;                      := neg (primitiva-negacion-booleana)

;*******************************************************************************************

;*******************************************************************************************
; Especificación Léxica

; Instrucciones para crear la especificación léxica
; <scanner-spec      := ({<exp-reg-y-acción>}*)
; <exp-reg-y-acción> := (<nombre> ({<exp-reg>}*) <salida>)
; <nombre>           := <símbolo>
; <exp-reg>          := <cadena> | letter | digit | whitespace | any
;                    := (not <caracter>) | (or {<exp-reg>}*)
;                    := (arbno <exp-reg>) | (concat {<exp-reg>}*)
; <salida>           := skip | symbol | number | string

(define scanner-interpretador
  '((espacio-blanco (whitespace) skip)
    (comentario ("%" (arbno (not #\newline))) skip)
    (decimal-positivo (digit(arbno digit) "." digit (arbno digit)) number)
    (decimal-negativo ("-" digit(arbno digit) "." digit (arbno digit)) number)
    (entero-positivo (digit (arbno digit)) number)
    (entero-negativo ("-" digit (arbno digit)) number)
    (texto ((or letter "_") (arbno (or letter digit "_" ":"))) string)
    (identificador ("@" letter (arbno letter)) symbol)

    ; Reglas para entradas inválidas.
    ; Números mal formados: "3." , "-3." , ".5"
    (numero-invalido (digit (arbno digit) ".") string)
    (numero-invalido ("-" digit (arbno digit) ".") string)
    (numero-invalido ("." digit (arbno digit)) string)
    ))
;*******************************************************************************************

;*******************************************************************************************
; Especificacion Sintáctica (Gramatica)

; Instrucciones para crear la especificación sintática
; <gramática>   := ({<producción>}*)
; <producción>  := (<lhs> ({rhs-item>}*) <nombre-prod>)
; <lhs>         := <símbolo>
; <rhs-item>    := <símbolo> | <cadena>
;               := (arbno {<rhs-item>}*)
;               := (separated-list {<rhs-item>}* <cadena>)
; <nombre-prod> := <símbolo>

; Nota: rhs = nombre de la especificación lexica si solo si esta definido en la léxica

(define grammar-interpretador
  '((programa (expresion) un-programa)

    ; Cualquier numero a numero-lit
    (expresion (entero-positivo) numero-lit)
    (expresion (entero-negativo) numero-lit)
    (expresion (decimal-positivo) numero-lit)
    (expresion (decimal-negativo) numero-lit)

    ; Variables
    (expresion (identificador) var-exp)

    ; Texto
    (expresion ("\""texto"\"") texto-lit)


    ; Primitiva Binaria
    (expresion ("("expresion primitiva-binaria expresion")") primitiva-binaria-expresion)

    ; Primitiva Unaria
    (expresion (primitiva-unaria "(" expresion ")") primitiva-unaria-expresion)

    ; Extensiones de la gramatica:
    ;punto 4 If-else (condicionales)
    (expresion ("Si" expresion
                "{"expresion"}" ; #t
                "sino" "{"expresion"}") ; #f
               condicional-exp) ; name
    
    ;punto 5 declarar (variables locales)
    (expresion ("declarar" "("(arbno identificador "=" expresion ";")")"
                           "{" expresion "}") variable-local-exp)

    
    ;punto 6 procedimiento
    (expresion ("procedimiento"
                "(" (separated-list identificador ",") ")" ; # parametros
                "{" expresion "}")
                procedimiento-exp)

    ;punto 7 evaluar
    (expresion ("evaluar" expresion
                "(" (separated-list expresion ",") ")"
                "finEval") aplicar-exp)

    ;punto 8 letrec (llamados recursivos)
    (expresion  ("letrec" (arbno identificador "[" (separated-list identificador ",") "]" "=" expresion ";") "in" expresion ";")
                letrec-exp)

    ; Primitiva Binaria
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)
    
    ; Primitiva Uninaria
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)
    
    ))
;*******************************************************************************************

;*******************************************************************************************
; La función show-the-datatypes permite consultar las definiciones de tipos de datos
; que corresponden a la gramática.

(sllgen:make-define-datatypes scanner-interpretador grammar-interpretador)
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-interpretador grammar-interpretador)))
;*******************************************************************************************

;*******************************************************************************************
; FrontEnd => Análisis léxico (scanner) y sintáctico (parser) integrados
( define scan&parse
   ( sllgen:make-string-parser
     scanner-interpretador
     grammar-interpretador))

; Analizador Léxico (Scanner)
(define just-scan
  (sllgen:make-string-scanner scanner-interpretador grammar-interpretador))

; Interpretador (FrontEnd + Evaluación + Señal para lectura)
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (evaluar-programa  pgm))
    (sllgen:make-stream-parser 
      scanner-interpretador
      grammar-interpretador)))


;*******************************************************************************************

;*******************************************************************************************
;                                      Nuestro intreprete

; evaluar-programa: <programa> -> valor
; función que evalúa un programa teniendo en cuenta un ambiente
; dado (se inicializa dentro del programa)
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (ambiente-inicial))))))

(define evaluar-expresion 
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (numero) numero)
      (var-exp (identificador) (buscar-variable amb identificador))
      (texto-lit (texto) texto)

      (primitiva-binaria-expresion (expresion1 operador expresion2)
                   (evaluar-primitiva-binaria
                            operador
                            (evaluar-expresion expresion1 amb)
                            (evaluar-expresion expresion2 amb)))
      
      (primitiva-unaria-expresion (operador expresion)
                                  (evaluar-primitiva-unaria
                                   operador
                                   (evaluar-expresion expresion amb)))

      (condicional-exp (test-exp true-exp false-exp) ;4
              (if (valor-verdad (evaluar-expresion test-exp amb))
                  (evaluar-expresion true-exp amb)
                  (evaluar-expresion false-exp amb)))
      
      ;5    
      (variable-local-exp (ids exps cuerpo)
                         (let((valores (eval-exps exps amb)))
                           (evaluar-expresion cuerpo(ambiente-extendido ids valores amb))
                           ))

      ;6
      (procedimiento-exp (ids cuerpo) (cerradura ids cuerpo amb))

      ;7
      (aplicar-exp (exp exps)
               (let ((proc (evaluar-expresion exp amb))
                     (args (eval-exps exps amb)))
                 (if (procVal? proc)
                     (aplicar-procedimiento proc args)
                     (eopl:error 'evaluar-expresion "Attempt to apply non-procedure ~s" proc))))

     ;8
      (letrec-exp (proc-names idss bodies letrec-body)
                 (evaluar-expresion letrec-body
                  (ambiente-extendido-recursivo proc-names idss bodies amb))))))

(define evaluar-primitiva-binaria
  (lambda (primitiva expresion1 expresion2)
    (cases primitiva-binaria primitiva
      (primitiva-suma () (+ expresion1 expresion2))
      (primitiva-resta () (- expresion1 expresion2))
      (primitiva-div () (/ expresion1 expresion2))
      (primitiva-multi () (* expresion1 expresion2))
      (primitiva-concat () (string-append expresion1 expresion2))
      (primitiva-mayor () (condicional? '> expresion1 expresion2))
      (primitiva-menor () (condicional? '< expresion1 expresion2))
      (primitiva-mayor-igual () (condicional? '>= expresion1 expresion2))
      (primitiva-menor-igual () (condicional? '<= expresion1 expresion2))
      (primitiva-diferente () (condicional? '!= expresion1 expresion2))
      (primitiva-comparador-igual () (condicional? '== expresion1 expresion2))
    )))

(define evaluar-primitiva-unaria
  (lambda (primitiva expresion)
    (cases primitiva-unaria primitiva
      (primitiva-longitud () (string-length expresion))
      (primitiva-add1 () (+ expresion 1))
      (primitiva-sub1 () (- expresion 1))
      (primitiva-negacion-booleana () (negacion-booleana expresion))
      ))) 

;*******************************************************************************************

;*******************************************************************************************
; Creación de Ambiente

; definición del tipo de dato ambiente
(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido-record (simbolos (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env ambiente?))
  (ambiente-extendido-recursivo-record
                              (proc-names(list-of symbol?))
                              (idss (list-of (list-of symbol?)))
                              (bodies (list-of expresion?))
                              (env ambiente?)))

;datatype de procedimientos (cerradura == closure == clausura)
(define-datatype procVal procVal?
  (cerradura
   (list-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)))

(define scheme-value? (lambda (v) #t))

; Ambiente inicial
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (ambiente-vacio))))

; Ambiente extendido
(define ambiente-extendido 
  (lambda (simbolos vals env)
    (ambiente-extendido-record simbolos vals env)))

; Ambiente extendido recursivo
(define ambiente-extendido-recursivo 
  (lambda (proc-names idss bodies old-env)
    (ambiente-extendido-recursivo-record proc-names idss bodies old-env)))


;Apply-env
(define buscar-variable
  (lambda (env sym)
    (cases ambiente env
      (ambiente-vacio () "Error, la variable no existe")
      (ambiente-extendido-record (simbolos vals env)
                           (let ((pos (list-find-position sym simbolos)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env sym))))
      (ambiente-extendido-recursivo-record (proc-names idss bodies old-env)
            (let ((pos (list-find-position sym proc-names)))
               (if (number? pos)
                   (cerradura (list-ref idss pos)
                            (list-ref bodies pos)
                            env)
                   (buscar-variable old-env sym)))))))
     

;Funciones Auxiliares

; logica inversa de valor-verdad?
(define negacion-booleana
  (lambda (numero)
    (if (zero? numero) 1 0))) 

(define mayor-numero?
  (lambda (numero)
    (if (> numero 0) 1 0)))

(define mayor-igual-numero?
  (lambda (numero)
    (if (eqv? numero 0)
        1
        (mayor-numero? numero))))

(define igualdad
  (lambda (expresion1 expresion2)
    (if (eqv? expresion1 expresion2) 1 0)))

(define desigualdad
  (lambda (expresion1 expresion2)
    (if (eqv? expresion1 expresion2) 0 1)))

(define condicional?
  (lambda (operador expresion1 expresion2)
    (cond
      [(equal? '> operador) (mayor-numero? (- expresion1 expresion2))]
      [(equal? '< operador) (mayor-numero? (- expresion2 expresion1))]
      [(equal? '>= operador) (mayor-igual-numero? (- expresion1 expresion2))]
      [(equal? '<= operador) (mayor-igual-numero? (- expresion2 expresion1))]
      [(equal? '!= operador) (desigualdad expresion1 expresion2)]
      [(equal? '== operador) (igualdad expresion1 expresion2)]
      [else (eopl:error 'condicional? "Operador no soportado ~s" operador)])))

; Funcion para evaluar cada expresión de la lista en el ambiente dado y devuelve la lista
; de resultados en el mismo orden
(define eval-exps
  (lambda (list-exps amb)
    (if (null? list-exps)
        '()
        (cons
         (evaluar-expresion (car list-exps) amb)
         (eval-exps (cdr list-exps) amb)))))

;Funcion para evaluar el cuerpo de un procedimiento en el ambiente extendido
;recibe un procedimiento, una lista de valores y retorna un valor
(define aplicar-procedimiento
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body amb)
                 (evaluar-expresion body (ambiente-extendido ids args amb))))))
                 
                 


; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

; Prueba
(ambiente-extendido '(@a @b @c @d @e) '(1 2 3 "hola" "FLP") (ambiente-vacio))

; return:
;#(struct:ambiente-extendido-record
  ;(@a @b @c @d @e)
  ;(1 2 3 "hola" "FLP")
  ;#(struct:ambiente-vacio))

; Funcion para simular booleanos
(define valor-verdad
  (lambda (numero)
    (not (zero? numero))))


;*******************************************************************************************
;EJERCICIOS CALIFICABLES
;*******************************************************************************************
;
;9. a (Sumar Dígitos)
;letrec
;  @cuantas[@n] = Si (@n < 10) { 0 } sino { (1 + evaluar @cuantas((@n ~ 10)) finEval) };
;  @sumarDigitos[@n] = Si @n
;  { ((@n ~ (10 * evaluar @cuantas(@n) finEval)) + evaluar @sumarDigitos(evaluar @cuantas(@n) finEval) finEval) }
;  sino { 0 };
;in
;  evaluar @sumarDigitos(147) finEval;
;
;------------------------------------------------------
;
; 9. b (Suma Naturales N)
; letrec
; @sumaN[@n] = Si @n
; {(@n + evaluar @sumaN(sub1(@n)) finEval)}
; sino {0};
; in
;  evaluar @sumaN(5) finEval; -->Resultado: 15
;
; letrec
; @sumaN[@n] = Si @n
; {(@n + evaluar @sumaN(sub1(@n)) finEval)}
; sino {0};
; in
;  evaluar @sumaN(10) finEval; -->Resultado: 55

;------------------------------------------------------
;
; 9. c
; letrec
; @multiplicar[@a, @b] = Si @a
; {(@b + evaluar @multiplicar(sub1(@a), @b) finEval)}
; sino {0};
; in
;  evaluar @multiplicar(3, 4) finEval;
;------------------------------------------------------
;
;---9d--- 
;
;;@esPar recibe un entero no negativo n y devuelve 1 (verdadero) si es par o 0 (falso) si es impar.
;;Como el lenguaje no tiene modulo, resta 2 en cada llamada recursiva (si n es par entonces n-2 tambien lo es)
;;hasta llegar a uno de los dos casos base: 0 es par y 1 es impar.
;;
;;@sumaParesRango recibe los enteros a y b (con a < b) y devuelve la suma de los numeros pares
;;en el rango [a, b]. Si a ya supero a b devuelve 0. Si a es par lo suma y sigue con a+1;
;;si es impar solo sigue con a+1.
;;
;;Resultados esperados: (2, 7) -> 12 (2+4+6), (2, 8) -> 20 (2+4+6+8) y (2, 16) -> 72 (2+4+6+8+10+12+14+16)
;
; letrec
; @esPar[@n] = Si (@n == 0)
;                {1}
;              sino
;                { Si (@n == 1)
;                    {0}
;                  sino
;                    { evaluar @esPar(sub1(sub1(@n))) finEval } };
;
; @sumaParesRango[@a, @b] = Si (@a > @b)
;                {0}
;              sino
;                { Si evaluar @esPar(@a) finEval
;                    { (@a + evaluar @sumaParesRango(add1(@a), @b) finEval) }
;                  sino
;                    { evaluar @sumaParesRango(add1(@a), @b) finEval } };
; in
;  evaluar @sumaParesRango (2, 7) finEval;
;
; --> Resultado: 12
;
; Con otros rangos:
; evaluar @sumaParesRango (2, 8) finEval;
; --> Resultado: 20
;
; evaluar @sumaParesRango (2, 16) finEval;
; --> Resultado: 72
;-------------------------------------------------------
;---9e---
;;@integrantes es un procedimiento el cual declara dentro cuatro variables locales y las devuelve concatenadas
;;
;;@saludar es un procedimiento que recibe otro procedimiento y devuelve otro procedimiento que al ejecutarse llama
;;a @f y le concatena "Hola:" al inicio de su resultado
;;
;;@decorate guarda el procedimiento que devuelve @saludar al recibir @integrantes y al invocarlo con 'evaluar @decorate() finEval'
;;se obtiene el saludo
;;
;;Resultado esperado: "Hola:Daniel_y_Brandon_y_David_y_Lasprilla"
;
;declarar(
;
;@integrantes = procedimiento () { declarar (
;@nomuno = "Daniel_y_";
;@nomdos = "Brandon_y_";
;@nomtres = "David_y_";
;@nomcuatro = "Lasprilla";
;) {
;((@nomuno concat @nomdos) concat (@nomtres concat @nomcuatro))
;}
;};
;
;
;@saludar = procedimiento (@f) { procedimiento () { ("Hola:"  concat evaluar @f() finEval) } };
;){ declarar (
;@decorate = evaluar @saludar(@integrantes) finEval;
;){
;evaluar @decorate() finEval
;}
;}
;
;------------------------------------------------------
;
; 9. f
;
;
;
;------------------------------------------------------


;; Declaración de IA: Se uso la IA para identificación de errores, deficiones de funciones y
;; conceptos de SLLGEN 
