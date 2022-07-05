;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q4) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Lista 2 - Capítulos 4 e 5 - Fundamentos de Algoritmos
;; Yasmin Katerine Beer Zebrowski
;; Turma D

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

