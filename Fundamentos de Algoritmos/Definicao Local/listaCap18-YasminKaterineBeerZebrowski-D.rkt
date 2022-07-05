;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname listaCap18-YasminKaterineBeerZebrowski-D) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Lista de Exercícios Cap.s 12 e 18 – Composição de funções e Expressões locais
;; Yasmin Katerine Beer Zebrowski
;; Turma D

;;=============================================================================
;; Questão 1
;;=============================================================================

;;-------------------
;; TIPO DATA
;;-------------------
; Um elemento do conjunto Data é um elemento do cjto. NumerosInteiros

;;--------------------
;; TIPO NÓ
;;--------------------
(define-struct filho (pai mãe nome data olhos))
; Um elemento nó de um conjunto Nó (de uma árvore genealógica) é:
; 1. empty, representando a falta de informação, ou
; 2. (make-filho p m n d o), onde:
;    p: Nó, representa o pai da pessoa
;    m: Nó, representa a mãe da pessoa
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


;; Função maisIdoso-l
;; Contrato:
;;           maisIdoso-l: Nó -> Nó
;; Objetivo: Dado um nó, retorna o ancestral mais idoso da família. Se os mais idosos tiverem
;; a mesma idade, retorna um deles.
;; Exemplos:
;; (maisIdoso-l Althea) = Althea
;; (maisIdoso-l Phoebe) = Frank
;; (maisIdoso-l Emma) = Althea

(define (maisIdoso-l nó)

  (local

    (
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

       ;;Definição das recursões
       (define maisIdoso-mãe(cond
                               [(empty? (filho-mãe nó)) empty]
                               [else (maisIdoso-l (filho-mãe nó))]))
       (define maisIdoso-pai(cond
                                [(empty? (filho-pai nó)) empty]
                                [else (maisIdoso-l (filho-pai nó))]))
     )
  
    (cond
      ;; Se o nó é vazio, retorna empty
      [(empty? nó) empty]
      ;; Se não é vazio, temos várias opções:
      ;; Não tem pais, então retorna si mesmo
      [(and (empty? (filho-mãe nó)) (empty? (filho-pai nó))) nó]
      ;; Tem mãe, mas não tem pai, então faz a recursão com a mãe
      [(empty? (filho-pai nó)) maisIdoso-mãe]
      ;; Tem pai, mas não tem mãe, então faz a recursão com o pai
      [(empty? (filho-mãe nó)) maisIdoso-pai]
      ;; Tem ambos os pais, então faz a recursão com os avós
      [else (cond
              [(d1<=d2? (filho-data maisIdoso-mãe) (filho-data maisIdoso-pai)) maisIdoso-mãe]
              [else maisIdoso-pai])
      ]
     )
  )
)

;; Testes
(check-expect (maisIdoso-l Althea) Althea)
(check-expect (maisIdoso-l Ben) Althea)
(check-expect (maisIdoso-l Joey) MrTribbiani)

;;=============================================================================
;; Questão 2
;;=============================================================================
  
;; Nesse caso, a vantagem é ter variáveis que só serão utilizadas dentro daquela função, além de ganhar tempo de
;; processamento, já que a recursão maisIdoso-l tanto para filho-mãe nó como para filho-pai nó já terá sido processada
;; anteriormente.

;;=============================================================================
;; Questão 3
;;=============================================================================
;;Contratos, objetivos e exemplos de uso

;; -----------
;; CONJUNTO ListaDeAncestrais:
;; -----------
;; Uma ListaDeAncestrais pode ser
;; 1. vazia (empty), ou
;; 2. (cons pessoa lista), onde
;;    pessoa: Nó
;;    lista : ListaDeAncestrais


;;(a) loa (lista ordenada de ancestrais) que, dado um nó, retorna uma lista contendo este
;; nó e todos seus ancestrais, ordenados pela data de nascimento, em ordem crescente
;; do ano;

;;   Contrato:     loa : Nó -> ListaDeAncestrais
;;   Objetivo: dado um nó, retorna uma lista contendo este nó e todos seus ancestrais,
;; ordenados pela data de nascimento, em ordem crescente do ano.
;;   Exemplos: (loa Monica) = (cons Althea (cons Judy (cons Jack (cons Monica empty))))
;;             (loa Emma) = (cons Sandra (cons Leonard (cons Rachel (cons Emma empty))))


;;(b) das adaptações (para os tipos adequados usados neste projeto) das funções ordena
;;e insere vistas em aula;

;;   Contrato:     insere : Nó ListaDeAncestrais -> ListaDeAncestrais
;;   Objetivo: Dado um nó e uma lista ordenada, insere o ancestral conforme o ano de
;; nascimento no lugar correto, gerando uma lista ordenada.
;;   Exemplos: (insere Ben (cons Emma (cons Rachel (cons Sandra (cons Leonard empty))))) = (cons Ben (cons Emma (cons Rachel (cons Sandra (cons Leonard empty)))))
;;             (insere Ross (cons Phoebe (cons Frank (cons LilyBuffay empty)))) = (cons Ross (cons Phoebe (cons Frank (cons LilyBuffay empty))))

;;   Contrato:     ordena : ListaDeAncestrais -> ListaDeAncestrais
;;   Objetivo: Dada uma lista de ancestrais, ordena a lista em ordem crescente de ano
;; de nascimento dos membros da família.
;;   Exemplos:  (ordena (cons LilyBuffay(cons Frank(cons Phoebe empty))) = (cons Phoebe (cons Frank (cons LilyBuffay empty)))
;;              (ordena (cons Rachel (cons Leonard (cons Sandra (cons Emma empty))))) = (cons Emma (cons Rachel (cons Sandra (cons Leonard empty)))))

;;(c) qualquer / quaisquer outra(s) função / funções auxiliar(es) que precise(m) ser utilizada(s) no seu projeto de solução.

  
;;=============================================================================
;; Questão 4
;;=============================================================================

;;-------------------
;; TIPO CARRO
;;-------------------
(define-struct carro (ano modelo valor ar? direção? vidros-el?))
;;Um elemento carro do conjunto Carro é uma estrutura
;;(make-carro ano modelo valor ar? direção? vidros-el?)
;;onde:
;   ano : Número, é o ano de fabricação do veículo
;   modelo : String, é o modelo do veiculo
;   valor : Número, é o valor em reais do veículo
;   ar? : Booleano, indica se veículo possui ar condicionado
;   vidros-el? : Booleano, indica se o veículo possui vidros elétricos
;   direção? : Booleano, indica se veículo possui direção hidráulica.


;;----------------
;; CONJUNTO LISTADECARROS
;;----------------

; uma listaDeCarros é:
;;   1. vazia (empty), ou
;;   2. (cons c l), onde
;;     c : carro
;;     l : listaDeCarros


;;=============================================================================
;; Questão 5
;;=============================================================================

;; Criando elementos do tipo carro:
(define carro1(make-carro 1998 "chev" 5000 false false false))
(define carro2(make-carro 2005 "uno" 7000 true false false))
(define carro3(make-carro 2020 "hyu" 80000 true true true))
(define carro4(make-carro 2011 "gol" 12000 true false true))
(define carro5(make-carro 2010 "chev" 10000 false true true))


;; Definindo alguns listaDeCarros
(define cadastro1(cons carro5 (cons carro4 (cons carro3 (cons carro2 (cons carro1 empty))))))
(define cadastro2(cons carro1 (cons carro5 empty)))
(define cadastro3(cons carro1 (cons carro2 empty)))
(define cadastro4(cons carro2 (cons carro1 empty)))
(define cadastro5(cons carro5 (cons carro2 (cons carro1 empty))))

;;----------------------------------------------------------------------------------------
;; (a) no caso da primeira versão, considerar que a lista não está necessariamente ordenada pelo atributo preço de mercado; denomine a função maiorvalor.
;;----------------------------------------------------------------------------------------

;; Funções auxiliares

;; Função procura-maior-valor
;;   Contrato:     procura-maior-valor : carro listaDeCarros -> carro
;;   Objetivo: Dados um carro e uma lista de carros, retorna o carro com maior valor de mercado.
;;   Exemplos: (encontra-maior-valor cadastro1) = carro3
;;             (encontra-maior-valor cadastro2) = carro5

(define (procura-maior-valor maior-valor-carro cadastro)
  (cond
    ;; Se o cadastro é vazio, então o carro com maior valor é o próprio carro da entrada
    [(empty? cadastro) maior-valor-carro]
    ;; Se não, compara valor do primeiro carro com o carro que foi salvo. Se for maior ou igual, checa o resto da lista
    [(>= (carro-valor maior-valor-carro) (carro-valor (first cadastro)))        (procura-maior-valor maior-valor-carro (rest cadastro))]
    ;; Se não, coloca o carro como maior valor, o resto da lista como o cadastro, e faz a recursão
    [else (procura-maior-valor (first cadastro) (rest cadastro))]
  )
)

;; Testes
(check-expect (procura-maior-valor carro5 cadastro1) carro3)
(check-expect (procura-maior-valor carro1 cadastro2) carro5)



;; Função principal
;;Função maiorvalor
;;   Contrato:     maiorvalor : listaDeCarros -> carro
;;   Objetivo: Dada uma lista de carros, devolve o carro com maior valor de mercado. Se a lista for vazia, devolve empty.
;;   Exemplos: (maiorvalor cadastro1) = (carro3)
;;             (maiorvalor cadastro2) = (carro5)
(define (maiorvalor cadastro)
  (cond
    [(empty? cadastro) empty]
    [else (procura-maior-valor (first cadastro) cadastro)]
  )
)

;; Testes
(check-expect (maiorvalor cadastro1)carro3)
(check-expect (maiorvalor cadastro2)carro5)



;;----------------------------------------------------------------------------------------
;; (b) no caso da segunda versão, faça a função maiorvalor_ord considerando que a
;; lista recebida está, necessariamente, ordenada em ordem decrescente, por preço de
;; mercado.
;;----------------------------------------------------------------------------------------

;;Função maiorvalor_ord
;;   Contrato:     maiorvalor_ord : listaDeCarros -> carro
;;   Objetivo: Dada uma lista de carros ordenada em valores de mercado descrescentes, devolve o carro com maior valor de mercado (primeiro elemento).
;; Se a lista for vazia, devolve empty.
;;   Exemplos: (maiorvalor_ord cadastro4) = (carro2)
;;             (maiorvalor_ord cadastro5) = (carro5)
  (define (maiorvalor_ord cadastro)
    (cond
      [(empty? cadastro) empty]
      [else (first cadastro)]
      )
    )

 ;;Testes
(check-expect (maiorvalor_ord cadastro4) carro2)
(check-expect (maiorvalor_ord cadastro5) carro5)


;;=============================================================================
;; Questão 6
;;=============================================================================

;; 6. Defina a função ordena-l que encapsula, numa única função, as funções ordena e
;; insere, cuja finalidade, como visto em aula, é ordenar uma lista de números. O encapsulamento se dá através do uso de uma expressão local no escopo da própria ordena-l.

;; Definindo algumas listas para os testes
(define lista1(list 1 2 4 3 6 2))
(define lista2(list 0 4 2 6 7 8 3 2 1))


;;Função ordena-l
;;   Contrato:     ordena-l : lista -> lista
;;   Objetivo: Dada uma lista de números, devolve a lista em ordem decrescente dos valores.
;; Se a lista for vazia, devolve uma lista empty.
;;   Exemplos: (ordena-l lista1) = (list 6 4 3 2 2 1)
;;             (ordena-l lista2) = (list 8 7 6 4 3 2 2 1 0)
    
      (define (ordena-l lista)
        (local
          (
            (define (insere n lista)
              (cond
                [(empty? lista) (cons n empty)]
                [(>= n (first lista)) (cons n lista)]
                [(< n (first lista))
                 (cons
                  (first lista)
                  (insere n (rest lista)))]
                )
             )  
           )
       
           (cond
             [(empty? lista) empty]
             [else
              (insere
               (first lista)
               (ordena-l (rest lista)))]
            )
           )
       )
        
;; Testes
(check-expect (ordena-l lista1) (list 6 4 3 2 2 1))
(check-expect (ordena-l lista2) (list 8 7 6 4 3 2 2 1 0))



