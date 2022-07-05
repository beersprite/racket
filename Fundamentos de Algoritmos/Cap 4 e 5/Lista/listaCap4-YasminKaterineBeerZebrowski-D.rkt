;; Lista 2 - Capítulos 4 e 5 - Fundamentos de Algoritmos
;; Yasmin Katerine Beer Zebrowski
;; Turma D

;; Questão 1

(require test-engine/racket-tests)

;; Contrato:
;;      seleciona-cor: número -> string
;; Objetivo: Entrar com um número qualquer, se ele for menor que 50, imprime "orange"
;; se ele for maior ou igual a 50, imprime "red"
;; Exemplo: (seleciona-cor 59) deve produzir "red"

(check-expect(seleciona-cor 5)"orange")
(check-expect(seleciona-cor 51)"red")

;; Função principal
;;Definição (Cabeçalho + Corpo)

(define (seleciona-cor numero)
(cond
[(< numero valor) "orange"]
[(>= numero valor) "red"]))

;; Funções auxiliares
;; Definição de algumas variáveis para troca rápida de valores

(define valor 50)

;; Testes
(seleciona-cor 50)

;; Valor esperado
;; "red"


;; Questão 2

(require test-engine/racket-tests)

;; Contrato:
;;      numero-raizes: número número número string -> string
;; Objetivo: Dados os coeficientes a, b e c de uma equação do segundo grau,
;; e uma string x ("true" para português e "false" para inglês), devolve o
;; número de raízes reais da equação em português ("duas", "uma", "zero") ou inglês
;; ("two", "one", "zero")
;; Exemplo: (numero-raizes 1 0 -4 false) deve produzir "one"

(check-expect(numero-raizes 1 5 7 "false") "zero")
(check-expect(numero-raizes 1 12 -13 "true") "duas")
(check-expect(numero-raizes 1 12 -13 "false") "two")
(check-expect(numero-raizes 0 0 0 "true") "uma")
(check-expect(numero-raizes 0 0 0 "false") "one")

;;Definição (Cabeçalho + Corpo)

(define (numero-raizes a b c x)

  (cond
    [(and (string=? "true" x) (= (- (expt b 2) (* 4 (* a c)))a zero)) "uma"]
    [(and (string=? "false" x) (= (- (expt b 2) (* 4 (* a c))) zero)) "one"]
    [(and (string=? "true" x) (> (- (expt b 2) (* 4 (* a c))) zero)) "duas"]
    [(and (string=? "false" x) (> (- (expt b 2) (* 4 (* a c))) zero)) "two"]
    [else  "zero"]))


;; Funções Auxiliares
;; Definição de algumas variáveis para troca rápida de valores

(define zero 0)

;; Teste
(numero-raizes 1 5 7 "true")

;; Valor esperado
;; "zero"



;; Questão 3

(require test-engine/racket-tests)

;; Contrato:
;;      calcula-proporcao: número número -> número
;; Objetivo: Dados dois números, a e b, onde a<=b calcula a proporção do primeiro
;; em relação ao segundo, no formato de valor inteiro. Caso b = 0, retorna -1.
;; Exemplo: (calcula-proporcao 30 60) deve produzir 50

(check-expect(calcula-proporcao 30 60)50)
(check-expect(calcula-proporcao 40 60)67)
(check-expect(calcula-proporcao 30 0)-1)


;;Definição (Cabeçalho + Corpo)

(define (calcula-proporcao a b)
 
  (cond
    [(= b zero) -1]
    [else  (round (* (/ a b) cem))]))


;; Funções Auxiliares
;; Definição de algumas variáveis para troca rápida de valores

(define zero 0)
(define cem 100)

;; Teste
(calcula-proporcao 30 0)

;; Valor esperado
;; -1



;; Questão 4

(require test-engine/racket-tests)

;; Contrato:
;;      desenha-barra: número -> imagem
;; Objetivo: Entrar com um número qualquer, saída será um retângulo de cor n (*)
;; com n de largura, 30 de altura, e com uma string de n centralizada em cima do
;; retângulo com tamanho 15px e na cor preta. Todavia, a função seleciona-cor não
;; foi definida então o programa não executará.
;; Exemplo: (desenha-barra 500) deveria produzir uma barra 500x30 de cor n (*) com 500
;; em cima escrito em preto.
;;
;; (*) função seleciona-cor não definida, não poderá imprimir retângulo que espera
;; uma cor como quarto argumento.

;; Cabeçalho
;;(define (desenha-barra n) ...)

;;Definição (Cabeçalho + Corpo)
(define (desenha-barra n)
  (overlay (text (number->string n) 15 "black")
           (rectangle n 30 "solid" (seleciona-cor n))))




;; Questão 5

;; Função Principal
;; Contrato:
;;      visualiza-resultados: string string número número -> imagem imagem
;; Objetivo: dados 2 nomes de grupos (a e b), o número de membros do primeiro grupo (x) e
;; o número de membros do segundo grupo (y), gera um diagrama de barras correspondendo a 100% da amostra,
;; dividida em duas partes: uma vermelha mostrando a porcentagem do grupo com maior número de elementos,
;; e uma laranja, mostrando a porcentagem do grupo com menor número de elementos, e abaixo da barra,
;; o tamanho da amostra. Se os nomes dos grupos forem iguais, ou um dos grupos não tiver membros, o
;; resultado deve ser uma barra vermelha contendo o valor 100.
;; Exemplo: (visualiza-resultados "A" "A" 234 120) deve produzir barra vermelha 100 com tamanho
;; da amostra: 354


;; Definição (Cabeçalho + Corpo)

(define (visualiza-resultados a b x y)
  
  (cond
    [(or (or (= x zero) (= y zero)) (string=? a b)) (barracem a b x y)]
    [(and (<= x y) (not (= x 0))) (xmenor a b x y)]
    [(> x y) (ymenor a b x y)]))


;; Funções Auxiliares

;; Definição de algumas variáveis para troca rápida de valores
;; Definição (Cabeçalhos)
(define zero 0)
(define cem 100)
(define tamfonte 14)


;; Contrato:
;;	xmenor: string string número número -> imagem
;; Objetivo: recebe as entradas da função visualiza-resultados e desenha a barra do grupo "a" laranja, e do grupo "b" vermelha com respectivos
;; valores percentuais em inteiros.
;; Exemplo: (xmenor "A" "B" 25 35) deve produzir barra laranja 42 e barra vermelha 58 com tamanho
;; da amostra: 60

;; Definição (Cabeçalho + corpo)

(define (xmenor a b x y)
  (above (beside (overlay (text (number->string (round (* (/ x (+ x y)) cem))) tamfonte "black")
                          (rectangle (round (* (/ x (+ x y)) cem)) 40 "solid" "orange"))
                 (overlay (text (number->string (- cem (round (* (/ x (+ x y)) cem)))) tamfonte "black")
                          (rectangle (- cem (round (* (/ x (+ x y)) cem))) 40 "solid" "red"))) 
         (beside (text "Tamanho da amostra: " tamfonte "black")
                 (text (number->string (+ x y)) tamfonte "black"))))



;; Contrato:
;;	ymenor: string string número número -> imagem
;; Objetivo:  Objetivo: recebe as entradas da função visualiza-resultados e desenha a barra do grupo "a" vermelha, e do grupo "b" laranja,
;; com respectivas percentagens em inteiro.
;; Exemplo: (ymenor "A" "B" 35 25) deve produzir barra vermelha 58 e barra laranja 42 com tamanho
;; da amostra: 60

;; Definição (Cabeçalho + Corpo)

(define (ymenor a b x y)
  (above (beside (overlay (text (number->string (- cem (round (* (/ y (+ x y)) cem)))) tamfonte "black")
                          (rectangle (- cem (round (* (/ y (+ x y)) cem))) 40 "solid" "red"))
                 (overlay (text (number->string (round (* (/ y (+ x y)) cem))) tamfonte "black")
                          (rectangle (round (* (/ y (+ x y)) cem)) 40 "solid" "orange")))
         (beside (text "Tamanho da amostra: " tamfonte "black")
                 (text (number->string (+ x y)) tamfonte "black"))))



;; Contrato:
;;	barracem: string string número número -> imagem
;; Objetivo: recebe as entradas da função visualiza-resultados e desenha a barra vermelha com valor de 100.
;; Exemplo: (barracem "A" "B" 0 35) deve produzir barra vermelha 100 com tamanho
;; da amostra: 35

;; Definição (Cabeçalho + Corpo)

(define (barracem a b x y)

  (above(overlay (text (number->string cem) tamfonte "black")
                 (rectangle cem 40 "solid" "red"))
        (beside (text "Tamanho da amostra: " tamfonte "black")
                (text (number->string (+ x y)) tamfonte "black"))))



;; Testes
 (visualiza-resultados "a" "b" 25 35)



