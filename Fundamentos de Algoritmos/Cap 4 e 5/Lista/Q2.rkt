;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Lista 2 - Capítulos 4 e 5 - Fundamentos de Algoritmos
;; Yasmin Katerine Beer Zebrowski
;; Turma D

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
(define zero 0)

(define (numero-raizes a b c x)

  (cond
    [(and (string=? "true" x) (= (- (expt b 2) (* 4 (* a c)))a zero)) "uma"]
    [(and (string=? "false" x) (= (- (expt b 2) (* 4 (* a c))) zero)) "one"]
    [(and (string=? "true" x) (> (- (expt b 2) (* 4 (* a c))) zero)) "duas"]
    [(and (string=? "false" x) (> (- (expt b 2) (* 4 (* a c))) zero)) "two"]
    [else  "zero"]))


;; Teste
(numero-raizes 1 5 7 "true")

;; Valor esperado
;; "zero"