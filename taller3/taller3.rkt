#lang eopl
(require racket/string)

;Daniel Andres Micolta Gongora - 2422033
;Brandon Alexis Franco Flor - 2435998
;David Alejandro Garcia Grueso 2438175
;Jorge Luis Junior Lasprilla Prada - 2420662
;URL REPOSITORIO GITHUB: https://github.com/davidggx1/Talleres-FLP

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
;;                      := =! (primitiva-diferente)
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
; Crea automaticamente los tipos de datos (define-datatype) de la gramatica:
; programa, expresion, primitiva-binaria y primitiva-unaria.

(sllgen:make-define-datatypes scanner-interpretador grammar-interpretador)

; show-the-datatypes:
; Proposito:
; () -> L : Procedimiento sin parametros que retorna la lista con las definiciones de los tipos de
; datos (define-datatype) que sllgen genera a partir de la especificacion lexica y la gramatica.
; Sirve para consultar como quedaron los tipos de datos del lenguaje.
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-interpretador grammar-interpretador)))
;*******************************************************************************************

;*******************************************************************************************
; FrontEnd => Análisis léxico (scanner) y sintáctico (parser) integrados
; scan&parse:
; Proposito:
; S -> Pgm : Procedimiento que recibe un string S con el codigo fuente de un programa del lenguaje,
; lo pasa por el scanner (analisis lexico) y por el parser (analisis sintactico) y retorna su arbol
; de sintaxis abstracta (un-programa). Si el texto no cumple la especificacion lexica o la gramatica,
; lanza un error. Es la entrada de evaluar-programa.
;
; Gramatica:
;     <programa> := <expresion>
;                   un-programa (exp)
( define scan&parse
   ( sllgen:make-string-parser
     scanner-interpretador
     grammar-interpretador))

; just-scan:
; Proposito:
; S -> L : Procedimiento que recibe un string S y retorna la lista de tokens que produce el scanner,
; sin pasar por el parser. Sirve para depurar la especificacion lexica (por ejemplo, ver como se
; escanea un texto o un numero).
;
; Gramatica:
;     <lista-tokens> := ()
;                    := (<token> <lista-tokens>)
(define just-scan
  (sllgen:make-string-scanner scanner-interpretador grammar-interpretador))

; interpretador:
; Proposito:
; () -> () : Procedimiento sin parametros que inicia el ciclo lectura-evaluacion-impresion (REPL)
; con el prompt "--> ". En cada vuelta lee un programa, lo analiza con el parser, lo evalua con
; evaluar-programa e imprime el resultado. Es la forma de ejecutar el lenguaje de forma interactiva.
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (evaluar-programa  pgm))
    (sllgen:make-stream-parser 
      scanner-interpretador
      grammar-interpretador)))


;*******************************************************************************************

;*******************************************************************************************
;                                      Nuestro intreprete

; evaluar-programa:
; Proposito:
; Pgm -> V : Procedimiento que recibe el arbol de sintaxis abstracta de un programa y retorna su valor.
; Evalua el cuerpo del programa en el ambiente inicial (ambiente-inicial), que se crea dentro de la
; funcion. Es el punto de entrada de la evaluacion.
;
; Gramatica:
;     <programa> := <expresion>
;                   un-programa (exp)
(define evaluar-programa
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (ambiente-inicial))))))

; evaluar-expresion:
; Proposito:
; Exp x Amb -> V : Procedimiento que recibe una expresion Exp y un ambiente Amb y retorna el valor
; de la expresion. Segun la variante de Exp:
;   numero-lit, texto-lit: retorna el numero o el texto.
;   var-exp: busca la variable en el ambiente con buscar-variable.
;   primitiva-binaria-expresion, primitiva-unaria-expresion: evalua los operandos y aplica la primitiva.
;   condicional-exp: evalua la condicion con valor-verdad? y evalua solo la rama que corresponde.
;   variable-local-exp (declarar): evalua las expresiones en el ambiente actual y evalua el cuerpo
;     en el ambiente extendido con las variables.
;   procedimiento-exp: crea una cerradura que guarda parametros, cuerpo y el ambiente donde se declaro.
;   aplicar-exp (evaluar): evalua el procedimiento y los argumentos y los aplica con
;     aplicar-procedimiento. Si el valor no es un procedimiento, lanza un error.
;   letrec-exp: evalua el cuerpo en un ambiente extendido recursivo para permitir recursion.
;
; Gramatica:
;     <expresion> := <numero> | <texto> | <identificador>
;                 := (<expresion> <primitiva-binaria> <expresion>)
;                 := <primitiva-unaria> (<expresion>)
;                 := Si <expresion> {<expresion>} sino {<expresion>}
;                 := declarar ({<identificador> = <expresion> ;}*) {<expresion>}
;                 := procedimiento ({<identificador>}*(,)) {<expresion>}
;                 := evaluar <expresion> ({<expresion>}*(,)) finEval
;                 := letrec {<identificador> [{<identificador>}*(,)] = <expresion> ;}* in <expresion> ;
(define evaluar-expresion 
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (numero) numero)
      (var-exp (identificador) (buscar-variable identificador amb))
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
              (if (valor-verdad? (evaluar-expresion test-exp amb))
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

; evaluar-primitiva-binaria:
; Proposito:
; PB x V x V -> V : Procedimiento que recibe una primitiva binaria y los valores ya evaluados de sus
; dos operandos, y retorna el resultado de aplicarla. Suma, resta (~), division y multiplicacion
; operan sobre numeros; concat une dos textos; las comparaciones (>, <, >=, <=, !=, ==) delegan en
; condicional? y retornan 1 (verdadero) o 0 (falso).
;
; Gramatica:
;     <primitiva-binaria> := + | ~ | / | * | concat | > | < | >= | <= | != | ==
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

; evaluar-primitiva-unaria:
; Proposito:
; PU x V -> V : Procedimiento que recibe una primitiva unaria y el valor ya evaluado de su operando,
; y retorna el resultado de aplicarla. longitud retorna la cantidad de caracteres de un texto; add1 y
; sub1 suman y restan 1 a un numero; neg retorna la negacion booleana (usa negacion-booleana).
;
; Gramatica:
;     <primitiva-unaria> := longitud | add1 | sub1 | neg
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

; scheme-value?:
; Proposito:
; V -> B : Predicado que recibe cualquier valor y siempre retorna #t. Se usa en el datatype ambiente
; para indicar que las variables del ambiente pueden guardar valores de cualquier tipo (numeros,
; textos o cerraduras).
(define scheme-value? (lambda (v) #t))

; ambiente-inicial:
; Proposito:
; () -> Amb : Procedimiento sin parametros que retorna el ambiente con el que empieza todo programa:
; las variables (@a @b @c @d @e) ligadas a los valores (1 2 3 "hola" "FLP") sobre un ambiente vacio.
; Lo usa evaluar-programa.
;
; Gramatica:
;     <ambiente> := (ambiente-vacio)
;                := (ambiente-extendido-record {<simbolo>}* {<valor>}* <ambiente>)
;                := (ambiente-extendido-recursivo-record {<simbolo>}* {{<simbolo>}*}* {<expresion>}* <ambiente>)
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (ambiente-vacio))))

; ambiente-extendido:
; Proposito:
; L x L x Amb -> Amb : Procedimiento que recibe una lista de simbolos, una lista de valores del mismo
; tamano y un ambiente. Retorna un nuevo ambiente donde cada simbolo queda ligado a su valor y el resto
; de variables se conservan del ambiente recibido. Si un simbolo ya existia, el nuevo valor lo oculta.
; Lo usan declarar (variables locales) y aplicar-procedimiento (parametros de un procedimiento).
;
; Gramatica:
;     <lista-simbolos> := ()
;                      := (<simbolo> <lista-simbolos>)
(define ambiente-extendido 
  (lambda (simbolos vals env)
    (ambiente-extendido-record simbolos vals env)))

; ambiente-extendido-recursivo:
; Proposito:
; L x L x L x Amb -> Amb : Procedimiento que recibe los nombres de los procedimientos recursivos, la
; lista con los parametros de cada uno, la lista con los cuerpos de cada uno y un ambiente. Retorna un
; ambiente extendido recursivo. No crea las cerraduras aqui: buscar-variable las crea al buscar un
; nombre, usando este mismo ambiente como ambiente de la cerradura, y asi un procedimiento puede
; llamarse a si mismo (o a los otros del mismo letrec).
; Lo usa letrec.
;
; Gramatica:
;     <lista-parametros> := ()
;                        := (<lista-simbolos> <lista-parametros>)
;     <lista-cuerpos>    := ()
;                        := (<expresion> <lista-cuerpos>)
(define ambiente-extendido-recursivo 
  (lambda (proc-names idss bodies old-env)
    (ambiente-extendido-recursivo-record proc-names idss bodies old-env)))


; buscar-variable:
; Proposito:
; Sym x Amb -> V : Procedimiento que recibe un simbolo (identificador) y un ambiente y retorna el valor
; ligado a ese simbolo. Busca desde el ambiente mas reciente hacia el mas antiguo. Si el ambiente es
; recursivo y el simbolo es el nombre de un procedimiento, retorna una cerradura cuyo ambiente es ese
; mismo ambiente recursivo. Si el simbolo no esta en ningun ambiente, retorna el texto
; "Error, la variable no existe".
;
; Gramatica:
;     <ambiente> := (ambiente-vacio)
;                := (ambiente-extendido-record {<simbolo>}* {<valor>}* <ambiente>)
;                := (ambiente-extendido-recursivo-record {<simbolo>}* {{<simbolo>}*}* {<expresion>}* <ambiente>)
(define buscar-variable
  (lambda (sym env)
    (cases ambiente env
      (ambiente-vacio () "Error, la variable no existe")
      (ambiente-extendido-record (simbolos vals env)
                           (let ((pos (list-find-position sym simbolos)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable sym env))))
      (ambiente-extendido-recursivo-record (proc-names idss bodies old-env)
            (let ((pos (list-find-position sym proc-names)))
               (if (number? pos)
                   (cerradura (list-ref idss pos)
                            (list-ref bodies pos)
                            env)
                   (buscar-variable sym old-env)))))))
     

;Funciones Auxiliares

; negacion-booleana:
; Proposito:
; N -> N : Procedimiento que recibe un numero y retorna su negacion booleana: 1 si el numero es 0
; (falso) y 0 en cualquier otro caso (verdadero). Es la logica inversa de valor-verdad? y la usa la
; primitiva neg.
(define negacion-booleana
  (lambda (numero)
    (if (zero? numero) 1 0))) 

; mayor-numero?:
; Proposito:
; N -> N : Procedimiento que recibe un numero y retorna 1 si es mayor que 0 y 0 en caso contrario.
; Como las comparaciones se hacen sobre la resta de los operandos, esta funcion indica el signo del
; resultado. La usa condicional? para > y <.
(define mayor-numero?
  (lambda (numero)
    (if (> numero 0) 1 0)))

; mayor-igual-numero?:
; Proposito:
; N -> N : Procedimiento que recibe un numero y retorna 1 si es 0 o mayor que 0, y 0 en caso contrario.
; La usa condicional? para >= y <=.
(define mayor-igual-numero?
  (lambda (numero)
    (if (eqv? numero 0)
        1
        (mayor-numero? numero))))

; igualdad:
; Proposito:
; V x V -> N : Procedimiento que recibe dos valores y retorna 1 si son equivalentes segun eqv? y 0
; en caso contrario. La usa condicional? para el operador ==.
(define igualdad
  (lambda (expresion1 expresion2)
    (if (eqv? expresion1 expresion2) 1 0)))

; desigualdad:
; Proposito:
; V x V -> N : Procedimiento que recibe dos valores y retorna 0 si son equivalentes segun eqv? y 1
; en caso contrario. Es lo contrario de igualdad y la usa condicional? para el operador !=.
(define desigualdad
  (lambda (expresion1 expresion2)
    (if (eqv? expresion1 expresion2) 0 1)))

; condicional?:
; Proposito:
; Sym x V x V -> N : Procedimiento que recibe el simbolo de un operador de comparacion (>, <, >=, <=,
; != o ==) y dos valores, y retorna 1 si la comparacion se cumple y 0 si no. Para el orden resta los
; operandos y revisa el signo con mayor-numero? o mayor-igual-numero?; para != y == usa desigualdad
; e igualdad. Si el operador no es soportado, lanza un error.
;
; Gramatica:
;     <operador> := > | < | >= | <= | != | ==
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

; eval-exps:
; Proposito:
; L x Amb -> L' : Procedimiento que recibe una lista de expresiones y un ambiente. Evalua cada
; expresion en ese ambiente y retorna la lista de resultados en el mismo orden. La usan declarar
; (valores de las variables) y evaluar (argumentos del llamado).
;
; Gramatica:
;     <lista-expresiones> := ()
;                         := (<expresion> <lista-expresiones>)
(define eval-exps
  (lambda (list-exps amb)
    (if (null? list-exps)
        '()
        (cons
         (evaluar-expresion (car list-exps) amb)
         (eval-exps (cdr list-exps) amb)))))

; aplicar-procedimiento:
; Proposito:
; ProcVal x L -> V : Procedimiento que recibe una cerradura y la lista de valores de los argumentos.
; Extiende el ambiente donde fue declarada la cerradura ligando cada parametro a su argumento y evalua
; el cuerpo en ese ambiente. Retorna el valor del cuerpo.
; Gramatica:
;     <procVal> := (cerradura {<simbolo>}* <expresion> <ambiente>)
(define aplicar-procedimiento
  (lambda (proc args)
    (cases procVal proc
      (cerradura (ids body amb)
                 (evaluar-expresion body (ambiente-extendido ids args amb))))))
                 
                 


; list-find-position:
; Proposito:
; Sym x L -> N o #f : Procedimiento que recibe un simbolo y una lista de simbolos. Retorna la posicion
; (empezando en 0) de la primera aparicion del simbolo en la lista, o #f si no esta. Usa list-index
; con eqv?. La usa buscar-variable para ubicar el valor de una variable dentro de un ambiente.
;
; Gramatica:
;     <lista-simbolos> := ()
;                      := (<simbolo> <lista-simbolos>)
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

; list-index:
; Proposito:
; P x L -> N o #f : Procedimiento que recibe un predicado P y una lista L. Retorna la posicion
; (empezando en 0) del primer elemento de L que cumple P, o #f si ninguno lo cumple. Es auxiliar de
; list-find-position.
;
; Gramatica:
;     <lista> := ()
;             := (<valor-de-scheme> <lista>)
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

; valor-verdad?:
; Proposito:
; N -> B : Procedimiento que recibe un numero y retorna #f si es 0 y #t en cualquier otro caso. Asi
; se simulan los booleanos del lenguaje (0 es falso, cualquier otro numero es verdadero). La usa
; condicional-exp para decidir que rama evaluar.
(define valor-verdad?
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
; @sumaNaturales[@n] = Si @n
; {(@n + evaluar @sumaNaturales(sub1(@n)) finEval)}
; sino {0};
; in
;  evaluar @sumaNaturales(5) finEval; -->Resultado: 15
;
; letrec
; @sumaNaturales[@n] = Si @n
; {(@n + evaluar @sumaNaturales(sub1(@n)) finEval)}
; sino {0};
; in
;  evaluar @sumaNaturales(10) finEval; -->Resultado: 55

;------------------------------------------------------
;
; 9. c
; letrec
; @multiplicar[@a, @b] = Si @a
; {(@b + evaluar @multiplicar(sub1(@a), @b) finEval)}
; sino {0};
; in
;  evaluar @multiplicar(4, 3) finEval;
;------------------------------------------------------
; 
; 9. d
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
;
; 9. e
;;@integrantes es un procedimiento el cual declara dentro cuatro variables locales y las devuelve concatenadas
;;
;;@saludar es un procedimiento que recibe otro procedimiento y devuelve otro procedimiento que al ejecutarse llama
;;a @f y le concatena "Hola:" al inicio de su resultado
;;
;;@decorate guarda el procedimiento que devuelve @saludar al recibir @integrantes y al invocarlo con 'evaluar @decorate() finEval'
;;se obtiene el saludo
;;
;;Resultado esperado: "Hola:Daniel_Brandon_David_y_Lasprilla"
;
;declarar(
;
;@integrantes = procedimiento () { declarar (
;@nomuno = "Daniel_";
;@nomdos = "Brandon_";
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


;; Declaración de IA:
;; Se uso como apoyo de estudio, revision, aclarar conceptos, explicar, revisar la redaccion de
;; comentarios y entender errores. Para aclarar conceptos de SLLGEN, cerraduras, recursion y
;; decoradores.
;; Ademas, la IA redacto los comentarios de proposito de los procedimientos del interpretador
