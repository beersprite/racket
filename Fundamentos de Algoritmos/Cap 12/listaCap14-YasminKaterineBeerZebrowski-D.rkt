;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listaCap14-YasminKaterineBeerZebrowski-D) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Lista de Exercícios Cap. 14 – Estruturas com autorreferência
;; Yasmin Katerine Beer Zebrowski
;; Turma D

;;=============================================================================
;; Questão 1
;;=============================================================================

;;-------------------
;; TIPO DATA
;;-------------------
; Um elemento do conjunto Data é um elemento do cjto. NumerosInteiros

;; Função d1<=d2?
;; Contrato:
;;           d1<=d2? : Data Data -> Boolean
;; Objetivo: Dadas 2 datas (apenas o ano), verifica se a primeira é menor ou igual a segunda
;; Exemplos:  (d1<=d2? 2014 2014) = true
;;            (d1<=d2? 2014 2013) = false
(define (d1<=d2? d1 d2)
(cond
  [(<= d1 d2) true]
  [else false]))

;; Testes
(check-expect (d1<=d2? 2014 2014) true)
(check-expect (d1<=d2? 2014 2013) false)

;;=============================================================================
;; Questão 2
;;=============================================================================

;;--------------------
;; TIPO FILHO
;;--------------------
(define-struct filho (pai mãe nome data olhos))
; Um elemento nó de um conjunto Nó (de uma árvore genealógica) é:
; 1. empty, representando a falta de informação, ou
; 2. (make-filho p m n d o), onde:
;    p: empty ou filho, representa o pai da pessoa
;    m: empty ou filho, representa a mãe da pessoa
;    n: Símbolo, representa o nome da pessoa
;    d: Número, representa o ano de nascimento da pessoa
;    o: Símbolo, representa a cor dos olhos da pessoa


(define Althea (make-filho empty empty 'Althea 1915 'brown))
(define Jack (make-filho empty empty 'Jack 1948 'brown))
(define Judy (make-filho empty Althea 'Judy 1945 'green))
(define Monica (make-filho Jack Judy 'Monica 1968 'blue))
(define Ross (make-filho Jack Judy 'Ross 1966 'brown))
(define Sandra (make-filho empty empty 'Sandra 1947 'brown))
(define Leonard (make-filho empty empty 'Leonard 1947 'brown))
(define Rachel (make-filho Leonard Sandra 'Rachel 1969 'blue))
(define Nora (make-filho empty empty 'Nora 1948 'blue))
(define Charles (make-filho empty empty 'Charles 1948 'blue))
(define Chandler (make-filho Charles Nora 'Chandler 1966 'blue))
(define GloriaTribbiani (make-filho empty empty 'GloriaTribbiani 1950 'brown))
(define MrTribbiani (make-filho empty empty 'MrTribbiani 1949 'brown))
(define Joey (make-filho MrTribbiani GloriaTribbiani 'Joey 1969 'brown))
(define Frank (make-filho empty empty 'Frank 1940 'brown))
(define LilyBuffay (make-filho empty empty 'LilyBuffay 1940 'blue))
(define Phoebe (make-filho Frank LilyBuffay 'Phoebe 1965 'blue))
(define Carol (make-filho empty empty 'Carol 1965 'blue))
(define Ben (make-filho Ross Carol 'Ben 1994 'blue))
(define Emma (make-filho Ross Rachel 'Emma 2002 'blue))



;;=============================================================================
;; Questão 3
;;=============================================================================

;; Função maisIdoso
;; Contrato:
;;           maisIdoso: Nó -> Pessoa
;; Objetivo: Dado um nó, retorna o ancestral mais idoso da família. Se os mais idosos tiverem
;; a mesma idade, retorna um deles.
;; Exemplos:
;; (maisIdoso Althea) = Althea
;; (maisIdoso Phoebe) = Frank
;; (maisIdoso Emma) = Althea

;; Definição (cabeçalho + corpo)
(define (maisIdoso nó)
  (cond
    ;; Se o nó é vazio, retorna empty
    [(empty? nó) empty]
    ;; Se não é vazio, temos várias opções:
    ;; Não tem pais, então retorna si mesmo
    [(and (empty? (filho-mãe nó)) (empty? (filho-pai nó))) nó]
    ;; Tem mãe, mas não tem pai, então faz a recursão com a mãe
    [(empty? (filho-pai nó)) (maisIdoso (filho-mãe nó))]
    ;; Tem pai, mas não tem mãe, então faz a recursão com o pai
    [(empty? (filho-mãe nó)) (maisIdoso (filho-pai nó))]
    ;; Tem ambos os pais, então faz a recursão com os avós
    [else (cond
            [(<= (filho-data (maisIdoso (filho-mãe nó))) (filho-data (maisIdoso (filho-pai nó)))) (maisIdoso (filho-mãe nó))]
            [else (maisIdoso (filho-pai nó))])
    ] ))


;; Testes
(check-expect (maisIdoso Althea) Althea)
(check-expect (maisIdoso Ben) Althea)
(check-expect (maisIdoso Joey) MrTribbiani)

;;=============================================================================
;; Questão 4
;;=============================================================================

;;--------------------
;; TIPO NÓ
;;--------------------
(define-struct nó (id conteúdo esq dir))
;; Um elemento do conjunto AB (Árvore Binária) pode ser
;; 1. empty, representando a falta de informação, ou
;; 2. (make-nó id c e d)
;; onde:
;;   id : Número, representa o identificador do nó
;;   c : String, representa o conteúdo do nó
;;   e : AB, representa a sub-árvore da esquerda
;;   d : AB, representa a sub-árvore da direita


;;Definindo algumas árvores para os testes.
 (define AB1(make-nó 10 "A"(make-nó 3 "B" empty empty)(make-nó 17 "C" (make-nó 15 "D"(make-nó 11 "F" empty empty) empty)(make-nó 20 "E" empty empty))))
 (define AB2(make-nó 1 "A" empty empty))
 (define AB3(make-nó 10 "A"(make-nó 3 "B" empty empty)(make-nó 17 "C" (make-nó 25 "D" empty empty) empty)))

;;-------------------------------------------------------------
;;Funções Auxiliares

;;Função id-minimo
;; Contrato: id-minimo : nó -> Número
;; Objetivo: dado um nó, retorna o valor mínimo da árvore do nó.
;;Exemplos:  (id-minimo AB1) = 3
;;           (id-minimo AB2) = 1

(define (id-minimo ab)
  (min (nó-id ab)
       (cond
         ;; Se os nós estão vazios, retorna o nó origem
         [(and (empty? (nó-esq ab)) (empty? (nó-dir ab))) (nó-id ab)]
         ;; Se o nó esquerdo está vazio, faz a recursão com o nó direito.
         [(empty? (nó-esq ab)) (id-minimo (nó-dir ab))]
         ;; Se o nó direito está vazio, faz a recursão com o nó esquerdo.
         [(empty? (nó-dir ab)) (id-minimo (nó-esq ab))]
         ;; Se não, encontra o mínimo entre o nó esquerdo e o direito
         [else (min (id-minimo (nó-esq ab)) (id-minimo (nó-dir ab)))]
        )))

;; Testes
(check-expect (id-minimo AB1) 3)
(check-expect (id-minimo AB2) 1)
    

;; Função id-maximo
;; Contrato:
;;            id-maximo: nó -> número
;; Objetivo: dado um nó, retorna o valor máximo da árvore do nó.
;; Exemplos: (id-maximo AB1) = 20
;;           (id-maximo AB2) = 1

(define (id-maximo ab)
  (max (nó-id ab)
       (cond
         ;; Se os nós estão vazios, retorna o nó origem
         [(and (empty? (nó-esq ab)) (empty? (nó-dir ab))) (nó-id ab)]
         ;; Se o nó direito está vazio, faz a recursão com o nó esquerdo.
         [(empty? (nó-dir ab)) (id-maximo (nó-esq ab))]
         ;; Se o nó esquerdo está vazio, faz a recursão com o nó direito.
         [(empty? (nó-esq ab)) (id-maximo (nó-dir ab))]
         ;; Se não, encontra o máximo entre o nó esquerdo e o direito
         [else (max (id-maximo (nó-esq ab)) (id-maximo (nó-dir ab)))]
       )))

;; Testes
(check-expect (id-maximo AB1) 20)
(check-expect (id-maximo AB3) 25)

;;-------------------------------------------------------------
;; Função Principal
;; Função é-abp?
;; Contrato:
;;           é-abp? : nó -> Boolean
;; Objetivo: Dada uma AB (árvore binária), diz se ela é
;; uma ABP (árvore binária de pesquisa)
;; Exemplos:  (é-abp? AB1) = true
;;            (é-abp? AB2) = true
;;            (é-abp? AB3) = false

(define (é-abp? ab)
  (cond
   ;; Se a AB é vazia, é ABP
   [(empty? ab) true]
   
   ;; Se a AB não é vazia, checa nós
    ;; Se o nó da direita não for vazio, e for menor que o nó origem, então não é ABP 
    [(and (not (empty? (nó-dir ab))) (< (id-minimo (nó-dir ab)) (nó-id ab))) false]
    ;; Se o nó da esquerda não for vazio, e for maior que o nó origem, então não é ABP 
    [(and (not (empty? (nó-esq ab))) (> (id-maximo (nó-esq ab)) (nó-id ab))) false]

    ;; Checa por recursão se o próximo nó dos lados é de ABP. Se não for, então a árvore não é ABP.
    [(or (and (not (empty? (nó-esq ab))) (not (é-abp? (nó-esq ab))))
         (and (not (empty? (nó-dir ab))) (not (é-abp? (nó-dir ab))))) false]

    ;; Se não, é ABP.
    [else true]
  ))

;; Testes
 (check-expect (é-abp? AB1) true)
 (check-expect (é-abp? AB2) true)
 (check-expect (é-abp? AB3) false)