;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Q5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Lista 2 - Capítulos 4 e 5 - Fundamentos de Algoritmos
;; Yasmin Katerine Beer Zebrowski
;; Turma D

;; Questão 5

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


;;Definições (Cabeçalhos, Cabeçalhos e corpos)

(define zero 0)
(define cem 100)
(define tamfonte 14)

(define (xmenor a b x y)
  (above (beside (overlay (text (number->string (round (* (/ x (+ x y)) cem))) tamfonte "black")
                          (rectangle (round (* (/ x (+ x y)) cem)) 40 "solid" "orange"))
                 (overlay (text (number->string (- cem (round (* (/ x (+ x y)) cem)))) tamfonte "black")
                          (rectangle (- cem (round (* (/ x (+ x y)) cem))) 40 "solid" "red"))) 
         (beside (text "Tamanho da amostra: " tamfonte "black")
                 (text (number->string (+ x y)) tamfonte "black"))))

(define (ymenor a b x y)
  (above (beside (overlay (text (number->string (- cem (round (* (/ y (+ x y)) cem)))) tamfonte "black")
                          (rectangle (- cem (round (* (/ y (+ x y)) cem))) 40 "solid" "red"))
                 (overlay (text (number->string (round (* (/ y (+ x y)) cem))) tamfonte "black")
                          (rectangle (round (* (/ y (+ x y)) cem)) 40 "solid" "orange")))
         (beside (text "Tamanho da amostra: " tamfonte "black")
                 (text (number->string (+ x y)) tamfonte "black"))))

(define (barracem a b x y)

  (above(overlay (text (number->string cem) tamfonte "black")
                 (rectangle cem 40 "solid" "red"))
        (beside (text "Tamanho da amostra: " tamfonte "black")
                (text (number->string (+ x y)) tamfonte "black"))))


(define (visualiza-resultados a b x y)
  
  (cond
    [(or (or (= x zero) (= y zero)) (string=? a b)) (barracem a b x y)]
    [(and (<= x y) (not (= x 0))) (xmenor a b x y)]
    [(> x y) (ymenor a b x y)]))

;; Teste
 (visualiza-resultados "a" "b" 25 35)

