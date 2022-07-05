;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listaCap10-YasminBeer-D) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Lista 6 - Capítulo 10
;; Yasmin Katerine Beer Zebrowski
;; Turma D

;; -----------------
;; TIPO CARTA-NUMERO:
;; -----------------
(define-struct carta-numero (cor valor))
;; Um elemento do conjunto Carta-numero é
;; (make-carta-numero c v) onde
;; c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo" ou "vermelho"
;; v : Número, é o número da carta

;; -----------------
;; TIPO CARTA-ESPECIAL:
;; -----------------
(define-struct carta-especial (cor tipo))
;; Um elemento do conjunto Carta-especial é
;; (make-carta-especial c t) onde
;; c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo", "vermelho" ou "qualquer"
;; t : String, é o tipo da carta especial, que pode ser "Compra4", "Compra2", "Inverte", "PulaVez" e "TrocaCor"

;; -----------
;; TIPO CARTA:
;; -----------
;; Um elemento do conjunto Carta pode ser
;; 1. um elemento do conjunto Carta-numero;
;; 2. um elemento do conjunto Carta-especial


;;=================================================================================
;; Questão 1
;;=================================================================================

;; -----------
;; CONJUNTO ListaDeCartas:
;; -----------
;; Uma ListaDeCartas pode ser
;; 1. vazia (empty), ou;
;; 2. (cons carta lista), onde
;;    carta : Carta
;;    lista : ListaDeCartas

;; Exemplos de Cartas
(define azul0(make-carta-numero "azul" 0))
(define amarelo5(make-carta-numero "amarelo" 5))
(define vermelho5(make-carta-numero "vermelho" 5))
(define compra4(make-carta-especial "qualquer" "Compra4"))
(define trocacor(make-carta-especial "qualquer" "TrocaCor"))
(define verdeinverte(make-carta-especial "verde" "Inverte"))

;; Exemplos de ListaDeCartas
(define lista1 (cons verdeinverte (cons trocacor empty)))
(define lista2 (cons azul0 (cons vermelho5 (cons compra4 (cons trocacor empty)))))
(define lista3 (cons amarelo5 (cons amarelo5 empty)))
(define lista4 (cons vermelho5 (cons verdeinverte empty)))


;; -----------------
;; TIPO JOGADOR:
;; -----------------
(define-struct jogador (nome lista pontuacao))
;; Um elemento do conjunto Jogador é
;; (make-jogador n l p) onde
;;    n : String, é o nome do jogador
;;    l : ListaDeCartas, é a lista de cartas da mão do jogador
;;    p : Número, é o número de pontos do jogador


;; Exemplos do tipo jogador:
(define Alphie(make-jogador "Alphie" lista1 0))
(define Alisaie(make-jogador "Alisaie" lista2 0))
(define Cid(make-jogador "Cid" lista2 0))



;;=================================================================================
;; Questão 2
;;=================================================================================


;;--------------------------------------------------------------------------------------------------------
;;Função Principal
;;--------------------------------------------------------------------------------------------------------
  ;; Função insere-carta
  ;; Contrato:  
  ;;            insere-carta: Jogador Carta -> Jogador
  ;; Objetivo:   Dado um jogador e um elemento do conjunto carta, adiciona a carta à lista de cartas do jogador e imprime o jogador.
  ;; Exemplos:  (insere-carta Alphie "azul0") resulta em (make-jogador "Alphie" (cons "azul0"(cons verdeinverte(cons trocacor empty))) 0)
  ;;            (insere-carta Alisaie "amarelo5") resulta em  (make-jogador "Alisaie" (cons "amarelo5" (cons azul0 (cons vermelho5 (cons compra4 (cons trocacor empty))))) 0)


;; Definição (cabeçalho + corpo)
(define (insere-carta jogador carta-comprada)
    (make-jogador (jogador-nome jogador) (cons carta-comprada (jogador-lista jogador)) (jogador-pontuacao jogador)))

;; Testes
(check-expect (insere-carta Alphie "azul0") (make-jogador "Alphie" (cons "azul0"(cons verdeinverte(cons trocacor empty))) 0))
(check-expect (insere-carta Alisaie "amarelo5") (make-jogador "Alisaie" (cons "amarelo5" (cons azul0 (cons vermelho5 (cons compra4 (cons trocacor empty))))) 0))


;;=================================================================================
;; Questão 3
;;=================================================================================

;;--------------------------------------------------------------------------------------------------------
;; Função auxiliar
;;--------------------------------------------------------------------------------------------------------
  ;; Função cor-da-carta
  ;; Contrato:  
  ;;            cor-da-carta: Carta -> String
  ;; Objetivo:   Dado um elemento do conjunto carta, retorna a cor da carta em português
  ;; Exemplos:   (cor-da-carta azul0) resulta em "azul"
  ;;             (cor-da-carta amarelo5) resulta em "amarelo"

;; Definição (cabeçalho + corpo)
(define (cor-da-carta carta)
  (cond
    [(carta-numero? carta) (carta-numero-cor carta)]
    [(carta-especial? carta) (carta-especial-cor carta)]))

;; Testes
(check-expect(cor-da-carta verdeinverte)"verde")
(check-expect(cor-da-carta trocacor)"qualquer")

;;--------------------------------------------------------------------------------------------------------
;; Função principal
;;--------------------------------------------------------------------------------------------------------
  ;; Função seleciona-cartas-cor
  ;; Contrato: 
  ;;             seleciona-cartas-cor: ListaDeCartas String -> ListaDeCartas
  ;; Objetivo:   Dada uma lista e uma cor, retorna uma nova lista contendo todas as cartas compatíveis com aquela cor.
  ;; Exemplos:  (seleciona-cartas-cor lista3 "qualquer") resulta em (cons amarelo5 (cons amarelo5 empty)))
  ;;            (seleciona-cartas-cor lista4 "verde") resulta em (cons verdeinverte empty))

;; Definição (cabeçalho + corpo)
(define (seleciona-cartas-cor lista cor)
    (cond
    ;; Se lista está vazia, devolve empty
    [(empty? lista) empty]

    ;; Se as cores forem iguais ou a carta tiver cor "qualquer", imprime a lista de cartas compatíveis
    [(or (string=? (cor-da-carta (first lista)) cor) (string=? "qualquer" cor))      (cons (first lista)
                                                                                           (seleciona-cartas-cor (rest lista) cor))]
    
    ;; Recursão
    [else (seleciona-cartas-cor (rest lista) cor)]))


;;Testes
   (check-expect (seleciona-cartas-cor lista3 "qualquer") (cons amarelo5 (cons amarelo5 empty)))
   (check-expect (seleciona-cartas-cor lista4 "verde")  (cons verdeinverte empty))



;;=================================================================================
;; Questão 4
;;=================================================================================

;;--------------------------------------------------------------------------------------------------------
;; Funções auxiliares
;;--------------------------------------------------------------------------------------------------------

;; Função pontos-da-carta
  ;; Contrato: 
  ;;             pontos-da-carta: Carta -> Número
  ;; Objetivo:   Dado um elemento do conjunto Carta, retorna o número de pontos daquela carta
  ;;     Os pontos das cartas de Uno são os seguintes:
  ;;        • as cartas numeradas valem o seu número;
  ;;        • as cartas "Compra2", "Inverte" e "PulaVez" valem 20;
  ;;        • as cartas "Compra4" e "TrocaCor" valem 50.
  ;; Exemplos:  (pontos-da-carta azul0) deve retornar 0
  ;;            (pontos-da-carta trocacor) deve retornar 50


;; Definição (cabeçalho + corpo)
(define (pontos-da-carta carta)
  (cond
    [(carta-numero? carta) (carta-numero-valor carta)]
    [(and (carta-especial? carta) (string=? (carta-especial-tipo carta) "TrocaCor")) 50]
    [(and (carta-especial? carta) (string=? (carta-especial-tipo carta) "Compra4")) 50]                           
    [else 20]))

;; Testes
(check-expect(pontos-da-carta azul0) 0)
(check-expect(pontos-da-carta verdeinverte)20)

;;--------------------------------------------------------------------------------------------------------
;; Função soma-dos-pontos
  ;; Contrato: 
  ;;             soma-dos-pontos: ListaDeCartas -> Número
  ;; Objetivo:   Dado um conjunto de ListaDeCartas, retorna a soma dos pontos de todas as cartas daquela lista.
  ;; Exemplos:  (soma-dos-pontos lista3) deve retornar 10
  ;;            (soma-dos-pontos lista4) deve retornar 25

;; Definição (cabeçalho + corpo)
(define (soma-dos-pontos lista)
  (cond
    ;; Se lista está vazia, devolve 0
    [(empty? lista) 0]
    ;; Se não, soma os pontos da primeira carta da lista e usa recursão para somar os demais.
    [else (+ (pontos-da-carta (first lista)) (soma-dos-pontos (rest lista))) ]))

;; Testes
(check-expect (soma-dos-pontos lista1) 70)
(check-expect (soma-dos-pontos lista2) 105)


;;--------------------------------------------------------------------------------------------------------
;; Função principal
;;--------------------------------------------------------------------------------------------------------
;; Função vencedor
  ;; Contrato: 
  ;;             vencedor: Jogador Jogador -> String
  ;; Objetivo:   Dados dois elementos do tipo Jogador, retorna uma string com o nome do jogador com menor número de pontos, isto é, o jogador vencedor, ou a string
  ;; "Empate" caso ambas as pontuações forem iguais.
  ;; Exemplos:   (vencedor Alisaie Alphie) resulta em "Alphie"
  ;;             (vencedor Alisaie Cid) resulta em "Empate"

;; Definição (cabeçalho + corpo)
(define (vencedor jogador1 jogador2)
  (cond
    [(=    (+ (soma-dos-pontos (jogador-lista jogador1)) (jogador-pontuacao jogador1))    (+ (soma-dos-pontos (jogador-lista jogador2)) (jogador-pontuacao jogador2))) "Empate"]
    [(<    (+ (soma-dos-pontos (jogador-lista jogador1)) (jogador-pontuacao jogador1))    (+ (soma-dos-pontos (jogador-lista jogador2)) (jogador-pontuacao jogador2))) (jogador-nome jogador1)]
    [else (jogador-nome jogador2)]))

;; Testes
(check-expect (vencedor Alisaie Alphie) "Alphie")
(check-expect (vencedor Alisaie Cid) "Empate")


;;=================================================================================
;; Questão 5
;;=================================================================================

;; -----------
;; TIPO CartaOUString:
;; -----------
;; Um elemento do conjunto CartaOUString pode ser
;; 1. um elemento do conjunto Carta;
;; 2. uma string


;;--------------------------------------------------------------------------------------------------------
;; Funções auxiliares
;;--------------------------------------------------------------------------------------------------------
;; Função tipo-da-carta
  ;; Contrato: 
  ;;             tipo-da-carta: Carta -> String
  ;; Objetivo:   Dado um elemento do conjunto Carta, retorna o tipo ou o valor dela em string
  ;; Exemplos:  (tipo-da-carta azul0) deve retornar "0"
  ;;            (tipo-da-carta compra4) deve retornar "Compra4"

;; Definição (cabeçalho + corpo)
(define (tipo-da-carta carta)
  (cond
    [(carta-numero? carta) (number->string (carta-numero-valor carta))]
    [(carta-especial? carta) (carta-especial-tipo carta)]
    [else "livre"]))

;; Testes
(check-expect(tipo-da-carta verdeinverte)"Inverte")
(check-expect(tipo-da-carta trocacor)"TrocaCor")


;;--------------------------------------------------------------------------------------------------------
;; Função joga-carta
  ;; Contrato:  
  ;;           joga-carta: Carta ListaDeCartas -> CartaOUString
  ;; Objetivo:  Dado um elemento do conjunto Carta e um conjunto ListaDeCartas, retorna uma carta que pode ser jogada, ou a string
  ;; "Jogada impossível" caso não haja cartas válidas para serem jogadas.
  ;; Exemplos:   (joga-carta vermelho5 lista2) resulta em (make-carta-numero "vermelho" 5)
  ;;             (joga-carta amarelo5 lista2) resulta em (make-carta-numero "vermelho" 5)


;; Definição (cabeçalho + corpo)
(define(joga-carta carta-da-mesa lista)
  
    (cond
    ;; Se lista está vazia, devolve "Jogada impossível"
    [(empty? lista) "Jogada impossível"]
    
    ;; Se não está vazia, checa a primeira carta.
        ;; Se ambas as cartas tiverem cores iguais, retorna a carta.
    [(string=? (cor-da-carta (first lista)) (cor-da-carta carta-da-mesa)) (first lista)]
    
        ;; Se ambas as cartas tiverem tipos iguais, retorna a carta.
    [(string=? (tipo-da-carta (first lista)) (tipo-da-carta carta-da-mesa)) (first lista)]

       ;; Se a carta da mão for TrocaCor ou Compra4, independente da carta da mesa, retorna a carta.
    [(or (string=? (tipo-da-carta (first lista)) "Compra4") (string=? (tipo-da-carta (first lista)) "TrocaCor")) (first lista)]
    
    ;; Se não, checa o resto por recursão
    [else (joga-carta carta-da-mesa (rest lista))] ))


;; Testes
(check-expect (joga-carta compra4 lista1) (make-carta-especial "qualquer" "TrocaCor"))
(check-expect (joga-carta vermelho5 lista2) (make-carta-numero "vermelho" 5))
(check-expect (joga-carta amarelo5 lista2) (make-carta-numero "vermelho" 5))

;;--------------------------------------------------------------------------------------------------------
;;Função principal
;;--------------------------------------------------------------------------------------------------------
;; Função joga
  ;; Contrato:  
  ;;            joga: Carta Jogador -> CartaOUString
  ;; Objetivo:   Dado um elemento do conjunto Carta e um elemento do tipo Jogador, retorna uma carta que pode ser jogada, ou a string
  ;; "Jogada impossível" caso não haja cartas válidas para serem jogadas.
  ;; Exemplos: (joga vermelho5 Alisaie) resulta em (make-carta-numero "vermelho" 5)
  ;;           (joga compra4 Alphie) resulta em (make-carta-especial "qualquer" "TrocaCor")

;; Definição (cabeçalho + corpo)
(define (joga carta-da-mesa jogador)
  (joga-carta carta-da-mesa (jogador-lista jogador)))


;; Testes
(check-expect (joga compra4 Alphie) (make-carta-especial "qualquer" "TrocaCor"))
(check-expect (joga vermelho5 Alisaie) (make-carta-numero "vermelho" 5))
(check-expect (joga amarelo5 Alisaie) (make-carta-numero "vermelho" 5))



;;=================================================================================
;; Questão 6
;;=================================================================================

 (require 2htdp/image)

;; ========================================================================
;;                                QUESTÃO 4
;; Adapte e complete a função mostra-jogada no arquivo mostra-jogada.rkt ou mostra-jogada.txt
;; para funcionar com suas definições de dados. O objetivo desta função é, dados
;; a carta da mesa e um jogador, mostrar a jogada escolhida (gerando uma imagem
;; com a jogada, incluindo o nome do jogador). Se você quiser usar suas imagens
;; para as cartas e para mostrar a jogada (desenvolvidas na lista 4),
;; pode modificar essas definições para usá-las.
;; =========================================================================
;; Função mostra-jogada
  ;; Contrato:  
  ;;            mostra-jogada :  Carta Jogador -> Imagem
  ;; Objetivo:   Dados um elemento do conjunto Carta e um elemento do tipo Jogador,
  ;; a função gera uma imagem mostrando a mão, a carta da mesa e a carta selecinada
  ;; para jogar, caso haja uma carta para jogar. Se não for possível jogar nenhuma
  ;; carta da mão, a função retorna uma mensagem.

;; Definição (cabeçalho + corpo)
(define (mostra-jogada mesa jog)
  (above/align "left"
  ;; desenha o nome do jogador jog:
    (text (jogador-nome jog) 30 "black")
  ;; desenha cartas da mão, lado a lado:
    (beside  (text "Mão: " 30 "black")
             (desenha-cartas-mao (jogador-lista jog)))
  ;; desenha carta da mesa, dentro de um círculo marrom:
    (beside  (text "Mesa: " 30 "black")
             (overlay (desenha-carta mesa) (circle 150 "solid"  "brown")))
  ;; desenha carta selecionada ou a mensagem de jogada impossível:
    (cond
      ;; se o resultado da jogada é uma mensagem, escrever a msg "Jogada impossível":
      [(string? (joga mesa jog)) (text "Jogada impossível" 30 "black")]
      ;; senão, mostrar a carta selecionada na jogada:
      [else (beside  (text "Carta selecionada: " 30 "black")
                     (desenha-carta (joga mesa jog) ))])))

;;--------------------------------------------------------------------------------------------------------
;; Função desenha-cartas-mao
  ;; Contrato: 
  ;;            desenha-cartas-mao: ListaDeCartas -> Imagem
  ;; Objetivo:  Dada uma lista de cartas, gera uma imagem com as cartas lado a lado.

;; Definição (cabeçalho + corpo)
(define (desenha-cartas-mao lista)
  (cond
       ;; se a lista estiver vazia, desenha a imagem vazia
       [(empty? lista) empty-image]
       ;; senão, desenha a imagem da primeira carta da lista ao lado
       ;;        das imagens do resto das cartas da lista
       [else (beside (desenha-carta (first lista))
                     (desenha-cartas-mao (rest lista)))]))


;;--------------------------------------------------------------------------------------------------------
;; Função desenha-carta
  ;; Contrato:
  ;;            desenha-carta : Carta -> Imagem
  ;; Objetivo: A função cria uma imagem para a carta que é recebida de entrada.

;; Definição (cabeçalho + corpo)
(define (desenha-carta carta)
  (overlay
         ;; desenhar o número da carta número ou texto da carta especial:
         (cond
            ;; se a carta for do tipo número, desenha este número
           [(carta-numero? carta)
            (text (number->string (carta-numero-valor carta)) 70 "black")]
            ;; se a carta for do tipo especial, desenha o texto da carta especial
           [(carta-especial? carta)
            (text  (carta-especial-tipo carta) 20 "black")])
         ;; sobre o fundo da carta da cor correspondente:
         (circle 45 "solid" "white")
         (rectangle 100 150 "solid" (cor-em-ingles (carta-cor carta)))
         (rectangle 110 160 "outline" "black")))

;;--------------------------------------------------------------------------------------------------------
;; Função carta-cor
  ;; Contrato: 
  ;;            carta-cor: Carta -> String
  ;; Objetivo:  Dada uma carta, devolve sua cor.
  ;; Exemplos:
  ;;    (carta-cor (make-carta-numero "azul" 3)) = "azul"
  ;;    (carta-cor (make-carta-numero "verde" 5)) = "verde"
  ;;    (carta-cor (make-carta-numero "amarelo" 8)) = "amarelo"
  ;;    (carta-cor (make-carta-especial "qualquer" "Compra4")) = "qualquer"

;; Definição (cabeçalho + corpo)
(define (carta-cor carta)
  (cond
     ;; se a carta for um numero, devolve sua cor
     [(carta-numero? carta)  (carta-numero-cor carta)]
     ;; se a carta for especial, devolve sua cor
     [(carta-especial? carta) (carta-especial-cor carta)] ))

;; Testes
(check-expect (carta-cor (make-carta-especial "qualquer" "Compra4")) "qualquer")
(check-expect (carta-cor (make-carta-numero "amarelo" 8)) "amarelo")


;;--------------------------------------------------------------------------------------------------------
;; Função cor-em-ingles
  ;; Contrato:
  ;;           cor-em-ingles : String -> String
  ;; Objetivo: a função recebe uma palavra, que pode ser
  ;; "azul", "verde", "amarelo" ou "vermelho", e retorna a respectiva cor
  ;; em ingles, ou seja, "blue", "green", "yellow" ou "red". Se a palavra
  ;; dada não for nenhuma destas, devolve "black".
  ;; Exemplos:
  ;;    (cor-em-ingles "azul") = "blue"
  ;;    (cor-em-ingles "verde") = "green"
  ;;    (cor-em-ingles "anil") = "black"

;; Definição (cabeçalho + corpo)
(define (cor-em-ingles pal)
  (cond
    [(string=? "azul" pal) "blue"]
    [(string=? "verde" pal) "green"]
    [(string=? "vermelho" pal) "red"]
    [(string=? "amarelo" pal) "yellow"]
    [else "black"]))

;; Testes
(check-expect (cor-em-ingles "azul") "blue")
(check-expect (cor-em-ingles "anil") "black")



;; Testes para a função mostra-jogada
(mostra-jogada amarelo5 Alphie)
(mostra-jogada trocacor Alisaie)


;;=================================================================================
;; Questão 7
;;=================================================================================

;; Função cartas-possiveis
  ;; Contrato:
  ;;           cartas-possiveis: Carta ListaDeCartas -> ListaDeCartas
  ;; Objetivo: Dadas uma carta e uma lista de cartas, retorna uma lista de todas as cartas que podem ser jogadas
  ;; Exemplos:    (cartas-possiveis vermelho5 lista2) resulta em (cons vermelho5 (cons compra4 (cons trocacor empty)))
  ;;              (cartas-possiveis amarelo5 lista4) resulta em (cons vermelho5 empty)

;; Definição (cabeçalho + corpo)
(define(cartas-possiveis carta-da-mesa lista)
  
    (cond
    ;; Se lista está vazia, devolve empty
    [(empty? lista) empty]
    
    ;; Se não está vazia, checa a primeira carta.
        ;; Se ambas as cartas tiverem cores iguais, coloca a carta na lista
    [(string=? (cor-da-carta (first lista)) (cor-da-carta carta-da-mesa)) (cons (first lista)
                                                                                (cartas-possiveis carta-da-mesa (rest lista)))]
    
        ;; Se ambas as cartas tiverem tipos iguais,  coloca a carta na lista
    [(string=? (tipo-da-carta (first lista)) (tipo-da-carta carta-da-mesa)) (cons (first lista)
                                                                                  (cartas-possiveis carta-da-mesa (rest lista)))]

       ;; Se a carta da mão for TrocaCor ou Compra4, independente da carta da mesa, coloca a carta na lista
    [(or (string=? (tipo-da-carta (first lista)) "Compra4") (string=? (tipo-da-carta (first lista)) "TrocaCor")) (cons (first lista)
                                                                                                                       (cartas-possiveis carta-da-mesa (rest lista)))]
    
    ;; Se não, checa o resto por recursão
    [else (cartas-possiveis carta-da-mesa (rest lista))] ))


;; Testes
  (check-expect (cartas-possiveis amarelo5 lista4)  (cons vermelho5 empty))
  (check-expect (cartas-possiveis vermelho5 lista2)  (cons vermelho5 (cons compra4 (cons trocacor empty))))


;; Função mostra-jogadas-possiveis
  ;; Contrato: 
  ;;            mostra-jogadas-possiveis :  Carta Jogador -> Imagem
  ;; Objetivo: A função gera uma imagem mostrando a mão, a carta da mesa e a carta selecinada
  ;; para jogar, caso haja uma carta para jogar. Se não for possível jogar nenhuma
  ;; carta da mão, a função retorna uma mensagem.         

;; Definição (cabeçalho + corpo)
(define (mostra-jogadas-possiveis mesa jog)
  (above/align "left"
  ;; desenha o nome do jogador jog:
    (text (jogador-nome jog) 30 "black")
  ;; desenha cartas da mão, lado a lado:
    (beside  (text "Mão: " 30 "black")
             (desenha-cartas-mao (jogador-lista jog)))
  ;; desenha carta da mesa, dentro de um círculo marrom:
    (beside  (text "Mesa: " 30 "black")
             (overlay (desenha-carta mesa) (circle 150 "solid"  "brown")))
  ;; desenha carta selecionada ou a mensagem de jogada impossível:
    (cond
      ;; se o resultado da jogada é uma mensagem, escrever a msg "Jogada impossível":
      [(string? (joga mesa jog)) (text "Jogada impossível" 30 "black")]
      ;; senão, mostrar todas as cartas que podem ser jogadas:
      [else (beside  (text "Jogadas possíveis: " 30 "black")
                     (desenha-cartas-mao (cartas-possiveis mesa (jogador-lista jog)) ))])))


;; Testes
(mostra-jogadas-possiveis amarelo5 Alisaie)
(mostra-jogadas-possiveis amarelo5 Alphie)


