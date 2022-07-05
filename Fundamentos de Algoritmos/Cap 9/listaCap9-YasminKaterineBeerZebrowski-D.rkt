;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listaCap9-YasminKaterineBeerZebrowski-D) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #t () #f)))
;; Lista 9 - Listas
;; INF05008
;; Yasmin Katerine Beer Zebrowski
;; Turma D

;;===================================================================================
;; Questão 1
;;===================================================================================

;;------------------------------
;; TIPO CARRO
;;------------------------------
(define-struct carro(ano modelo valor ac dh ve))
;; Um elemento do conjunto carro tem o formato:
;; (make-carro ano modelo valor ac dh ve)
;; onde:
;;         ano: número, representa o ano de fabricação do carro
;;         modelo: string, representa o modelo do carro
;;         valor: número, representa o valor de mercado do carro
;;         ac: booleano, representa se o carro tem ar-condicionado ou não
;;         dh: booleano, representa se o carro tem direção hidráulica ou não
;;         ve: booleano, representa se o carro tem vidros elétricos ou não
;;------------------------------

;; Criando elementos do tipo carro:
(define carro1(make-carro 1998 "chev" 5000 false false false))
(define carro2(make-carro 2005 "uno" 7000 true false false))
(define carro3(make-carro 2020 "hyu" 80000 true true true))
(define carro4(make-carro 2011 "gol" 12000 true false true))
(define carro5(make-carro 2010 "chev" 10000 false true true))

;;-----------------------------
;; CONJUNTO CADASTRODECARROS
;;-----------------------------
;; Um CadastroDeCarros pode ser
;;    1. vazio (empty), ou
;;    2. (cons c), onde
;;        c : Carro

;; Definindo alguns CadastroDeCarros
(define cadastro1(cons carro5 (cons carro4 (cons carro3 (cons carro2 (cons carro1 empty))))))
(define cadastro2(cons carro1 (cons carro5 empty)))
(define cadastro3(cons carro1 (cons carro2 empty)))


;;===================================================================================
;; Questão 2
;;===================================================================================

;; Função temAC?
;; Contrato:
;;            temAC? : CadastroDeCarros -> String
;; Objetivo: dado um cadastro, retorna se há algum carro com ar-condicionado (a/c).
;; Exemplos: (temAC? cadastro1) deve retornar "Tem pelo menos um carro com a/c"
;;           (temAC? cadastro2) deve retornar "Não há carros com a/c"

(check-expect(temAC? cadastro1) "Tem pelo menos um carro com a/c")
(check-expect(temAC? cadastro2) "Não há carros com a/c")

;; Definição (cabeçalho + corpo)
(define (temAC? cadastro)
  (cond
    ;; Se lista está vazia, devolve string
    [(empty? cadastro) "Não há carros com a/c"]
    ;; Se não está vazia, checa o primeiro termo
    [(carro-ac (first cadastro)) "Tem pelo menos um carro com a/c"]
    ;; Se não tem no primeiro, checa o resto por recursão
    [else (temAC? (rest cadastro))]))

;; Testes
(temAC? cadastro1)
;; Resultado esperado:
;; "Tem pelo menos um carro com a/c"
(temAC? cadastro2)
;; Resultado esperado:
;; "Não há carros com a/c"
(temAC? cadastro3)
;; Resultado esperado:
;; "Tem pelo menos um carro com a/c"


;;===================================================================================
;; Questão 3
;;===================================================================================

;; Função quantosVE
;; Contrato:
;;            quantosVE : CadastroDeCarros -> Número
;; Objetivo: dado um cadastro, retorna o número de carros com vidros elétricos.
;; Exemplos: (quantosVE cadastro1) deve retornar 3
;;           (quantosVE cadastro2) deve retornar 1

(check-expect(quantosVE cadastro1) 3)
(check-expect(quantosVE cadastro2) 1)

;; Definição (cabeçalho + corpo)
(define (quantosVE cadastro)
  (cond
    ;; Se lista está vazia, devolve 0
    [(empty? cadastro) 0]
    ;; Soma 1 cada vez que encontra true para os vidros
    [(carro-ve (first cadastro)) (+ 1 (quantosVE (rest cadastro)))]
    ;; Recursão
    [else (quantosVE (rest cadastro))]))

;; Testes
(quantosVE cadastro1)
;; Resultado esperado:
;; 3
(quantosVE cadastro2)
;; Resultado esperado:
;; 1
(quantosVE cadastro3)
;; Resultado esperado:
;; 0


;;===================================================================================
;; Questão 4
;;===================================================================================

;; Função valor
;; Contrato:
;;            valor : CadastroDeCarros -> Número
;; Objetivo: dado um cadastro, devolve a soma dos valores dos carros do cadastro
;; Exemplos: (valor cadastro1) deve retornar 114000
;;           (valor cadastro2) deve retornar 15000

(check-expect(valor cadastro1) 114000)
(check-expect(valor cadastro2) 15000)

;; Definição (cabeçalho + corpo)
(define (valor cadastro)
  (cond
    ;; Se lista está vazia, devolve 0
    [(empty? cadastro) 0]
    ;; Soma valor para cada carro
    [else (+ (carro-valor (first cadastro)) (valor (rest cadastro)))]))

;; Testes
(valor cadastro1)
;; Resultado esperado:
;; 114000
(valor cadastro2)
;; Resultado esperado:
;; 15000
(valor cadastro3)
;; Resultado esperado:
;; 12000


;;===================================================================================
;; Questão 5
;;===================================================================================

;;-----------------------------
;; TIPO carroOUstring
;;-----------------------------
;; Um elemento do conjunto carroOUstring pode ser
;;    1. um elemento do tipo carro, ou
;;    2. uma string



;; Função decValor
;; Contrato:
;;            decValor : CadastroDeCarros Número -> carroOUstring
;; Objetivo: dado um cadastro e o número do ano corrente, retorna um carro qualquer daquele ano,
;; com 10% a menos do valor original, ou se não tiverem carros daquele ano corrente, retorna a
;; string "Não há carros deste ano no cadastro"
;; Exemplos: (decValor cadastro1) deve retornar (make-carro 2011 "gol" 10800 true false true)
;;           (decValor cadastro2) deve retornar (make-carro 1998 "chev" 4500 false false false)

(check-expect(decValor cadastro1 2011) (make-carro 2011 "gol" 10800 true false true))
(check-expect(decValor cadastro2 1998) (make-carro 1998 "chev" 4500 false false false))

;; Definição (cabeçalho + corpo)
(define (decValor cadastro anocorrente)
  (cond
    ;; Se lista está vazia, devolve string
    [(empty? cadastro) "Não há carros deste ano no cadastro"]
    ;; Se anos forem iguais, muda o valor e imprime o carro novo
    [(= (carro-ano (first cadastro)) anocorrente) (make-carro (carro-ano (first cadastro))
                                                              (carro-modelo (first cadastro))
                                                              (* (carro-valor (first cadastro)) 0.9)
                                                              (carro-ac (first cadastro))
                                                              (carro-dh (first cadastro))
                                                              (carro-ve (first cadastro)))]
    ;; Recursão
    [else (decValor (rest cadastro) anocorrente)]))

;; Testes
(decValor cadastro1 2011)
;; Resultado esperado:
;; (make-carro 2011 "gol" 10800 true false true)
(decValor cadastro2 1998)
;; Resultado esperado:
;; (make-carro 1998 "chev" 4500 false false false)
(decValor cadastro3 2005)
;; Resultado esperado:
;; (make-carro 2005 "uno" 6300 true false false)





