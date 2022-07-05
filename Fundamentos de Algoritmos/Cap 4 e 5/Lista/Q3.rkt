;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Lista 2 - Capítulos 4 e 5 - Fundamentos de Algoritmos
;; Yasmin Katerine Beer Zebrowski
;; Turma D

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

(define zero 0)
(define cem 100)

(define (calcula-proporcao a b)
 
  (cond
    [(= b zero) -1]
    [else  (round (* (/ a b) cem))]))

;; Teste
(calcula-proporcao 30 0)

;; Valor esperado
;; -1