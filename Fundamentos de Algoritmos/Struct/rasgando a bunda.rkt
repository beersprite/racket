;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |rasgando a bunda|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Questão 4

;; Letra (a)

;; Criando a estrutura carro:
(define-struct carro(ano modelo valormercado arcond dirhid vidroseletricos))
;; Um elemento do conjunto carro tem o formato
;;  (make-carro ano modelo valor ac dir vid)
;; onde:
;;     ano : número representando o ano de fabricação do carro
;;     modelo :  string representando o modelo do carro
;;     valor : número representando o valor de mercado em reais
;;     ac : número, 1 para tem ar-condicionado ou 0 para não tem ar-condicionado
;;     dir : número, 1 para tem direção hidráulica ou 0 para não tem direção hidráulica
;;     vid : número, 1 para tem vidros elétricos ou 0 para não tem vidros elétricos


;; Criando a estrutura moto:
(define-struct moto(ano modelo valormercado))
;; Um elemento do conjunto moto tem o formato
;;  (make-moto ano modelo valor ac dir vid)
;; onde:
;;     ano : número representando o ano de fabricação da moto
;;     modelo : string representando o modelo da moto
;;     valor : número representando o valor de mercado em reais



;; Letras (b) e (c)


;; Funções Auxiliares



;; Definindo alguns carros e motos para testes
(define carroA(make-carro 2005 "uno" 5000 0 0 0))
(define carroB(make-carro 2020 "dos" 85000 1 1 1))
(define motoA(make-moto 2008 "uno" 3000))
(define motoB(make-moto 2011 "tres" 10000))



;;Função valor-base-carro
;; Contrato:
;;     valor-base-carro: carro -> número
;; Objetivo: dado um carro como entrada, calcula o valor base da locação
;; Exemplo: (valor-base-carro carroA) deve resultar em 12.5
;; (valor-base-carro carroB) deve resultar em 212.5

(check-expect(valor-base-carro carroA)12.5)
(check-expect(valor-base-carro carroB)212.5)

;; Definição (cabeçalho + corpo)
(define (valor-base-carro carro)
  (* (carro-valormercado carro) 0.0025))




;;Função extras
;; Contrato:
;;     extras: carro -> número
;; Objetivo: dado um carro como entrada, calcula quantos reais a mais ele deve pagar
;; por extra no carro
;; Exemplo: (extras carroA) deve resultar em 00
;; (extras carroB) deve resultar em 90

(check-expect(extras carroA)0)
(check-expect(extras carroB)90)

;; Definição (cabeçalho + corpo)
(define (extras carro)
  (* (+ (+ (carro-arcond carro) (carro-dirhid carro)) (carro-vidroseletricos carro)) 30))




;;Função valor-base-moto
;; Contrato:
;;     valor-base-moto: moto -> número
;; Objetivo: dado uma moto como entrada, calcula o valor base da locação
;; Exemplo: (valor-base-moto motoA) deve resultar em 25
;; (valor-base-moto motoB) deve resultar em 7.5

(check-expect(valor-base-moto motoA)7.5)
(check-expect(valor-base-moto motoB)25)

;; Definição (cabeçalho + corpo)
(define (valor-base-moto moto)
  (* (moto-valormercado moto) 0.0025))



;; Função seguro
;; Contrato:
;;     seguro: moto -> número
;; Objetivo: dado uma moto como entrada, retorna 70 se o ano de fabricação dela
;; for antes de 2011 ou retorna 90 se o ano for após 2011, inclusive.
;; Exemplo: (seguro motoA) deve resultar 70
;; (seguro motoB) deve resultar 90

(check-expect(seguro motoA)70)
(check-expect(seguro motoB)90)

;; Definição (cabeçalho + corpo)
(define (seguro moto)
  (cond
    [(< (moto-ano moto) 2011) 70]
    [else 90]
    ))




;; Função Principal para a letra (b)

;; Contrato:
;;     cdc: carro -> número
;; Objetivo: dado um carro de entrada, efetua o cálculo da diária de locação do carro
;; Exemplos: (cdc carroA) deve resultar em 12.5
;; (cdc carroB) deve resultar em 302.5

(check-expect(cdc carroA)12.5)
(check-expect(cdc carroB)302.5)


;; Definição (cabeçalho + corpo)
(define (cdc carro)
  (+ (extras carro) (valor-base-carro carro) ))


;; Testes

(cdc carroB)
;; Resultado esperado
;; 302.5

(cdc carroA)
;; Resultado esperado
;; 12.5




;; Função Principal para a letra (c)

;; Contrato:
;;     cdm: moto -> número
;; Objetivo: dado uma moto de entrada, efetua o cálculo da diária de locação da moto
;; Exemplos: (cdm motoA) resulta em 77.5
;; (cdm motoB) resulta em 115

(check-expect(cdm motoA)77.5)
(check-expect(cdm motoB)115)

;; Definição (cabeçalho + corpo)

(define (cdm moto)
  (+ (valor-base-moto moto) (seguro moto)))

;; Testes

(cdm motoA)
;; Resultado esperado
;;77.5

(cdm motoB)
;; Resultado esperado
;;115


