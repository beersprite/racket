;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
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

;;Definição (Cabeçalho + Corpo)
(define valor 50)

(define (seleciona-cor numero)
(cond
[(< numero valor) "orange"]
[(>= numero valor) "red"]))

;; Testes
(seleciona-cor 50)

;; Valor esperado
;; "red"