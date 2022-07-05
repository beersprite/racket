;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname listaCap28-YasminKaterineBeerZebrowski-D) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "draw.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Capítulo 28 - Grafos
;; Yasmin Katerine Beer Zebrowski
;; Turma D

;;Considere a definição de dados a seguir para os exercícios 1 a 6 desta lista (defina o tipo ListaDeString).

;;-----------------------
;; TIPO LISTADESTRING
;;-----------------------
;; Uma ListaDeString pode ser
;; 1. empty, ou
;; 2. (cons n), onde
;;     n: String, representa o nome do nodo


;;-----------------------
;; TIPO NODO
;;-----------------------
(define-struct nodo (nome vizinhos))
;; Um elemento do conjunto Nodo é um par
;;            (make-nodo n v), onde
;;     n : String, representa o nome do nodo
;;     v : ListaDeString, representa os (nomes dos) nodos vizinhos
;; Um Grafo é
;; 1. empty, ou
;; 2. (cons n g), onde
;;     n : Nodo
;;     g : Grafo



;;=========================================================
;; QUESTÃO 1
;;=========================================================

;; 1. Definir uma constante para representar o grafo de cidades do slide 9 do arquivo com os slides da aula (desconsidere
;; o tamanho dos arcos e o fato de existirem mais de um arco entre dois nodos em alguns casos). Seu grafo deve
;; ser uma lista com 16 nodos, um para cada cidade deste mapa. Dê o nome de MAPA para esta constante.

(define MAPA(list
                 (make-nodo "Winnipeg" (list "Sault St. Marie" "Duluth" "Helena"))
                 (make-nodo "Helena" (list "Winnipeg" "Duluth" "Omaha" "Denver"))
                 (make-nodo "Duluth" (list "Winnpeg" "Sault St. Marie" "Toronto" "Chicago" "Omaha" "Helena"))
                 (make-nodo "Sault St. Marie" (list "Winnipeg" "Duluth" "Toronto"))
                 (make-nodo "Toronto" (list "Sault St. Marie" "Pittsburgh" "Chicago" "Duluth"))
                 (make-nodo "Denver" (list "Omaha" "Kansas City" "Oklahoma City" "Santa Fé" "Helena"))
                 (make-nodo "Omaha" (list "Denver" "Helena" "Duluth" "Chicago" "Kansas City"))
                 (make-nodo "Chicago" (list "Duluth" "Toronto" "Pittsburgh" "Saint Louis" "Omaha"))
                 (make-nodo "Pittsburgh" (list "Toronto" "Chicago" "Saint Louis" "Nashville"))
                 (make-nodo "Santa Fé" (list "Oklahoma City" "Denver"))
                 (make-nodo "Kansas City" (list "Oklahoma City" "Little Rock" "Saint Louis" "Omaha" "Denver"))
                 (make-nodo "Saint Louis" (list "Kansas City" "Chicago" "Pittsburgh" "Nashville" "Little Rock"))
                 (make-nodo "Little Rock" (list "Nashville" "Saint Louis" "Oklahoma City"))
                 (make-nodo "Nashville" (list "Atlanta" "Pittsburgh" "Saint Louis" "Little Rock"))
                 (make-nodo "Atlanta" (list "Nashville"))
                 (make-nodo "Oklahoma City" (list "Little Rock" "Kansas City" "Denver" "Santa Fé"))
                 )
  )


;;=========================================================
;; QUESTÃO 2
;;=========================================================

;; está-na-lista? : String ListaDeString -> Booleano
;; Obj.: dadas uma string e uma lista de strings, retorna se o nome
;; está na lista
;; Exemplos:    (está-na-lista? "Chicago" (list "Nashville")) = false
;;              (está-na-lista? "Little Rock" (list "Atlanta" "Pittsburgh" "Saint Louis" "Little Rock")) = true

(define (está-na-lista? nome lista)
  (cond
    ;; Caso trivial: lista é vazia, então o nome não está nela.
    [(empty? lista) false]
    ;; Se não, checa primeiro elemento da lista
    [(string=? nome (first lista)) true]
    ;; Se não, checa o resto da lista por recursão
    [else (está-na-lista? nome (rest lista))]
    )
  )

;; Testes
(check-expect (está-na-lista? "Chicago" (list "Nashville")) false)
(check-expect (está-na-lista? "Little Rock" (list "Atlanta" "Pittsburgh" "Saint Louis" "Little Rock")) true)

;; Argumentação de Terminação: a função sempre termina, pois há apenas duas possibilidades: ou o nome está na lista, ou não está.
;; Como a função percorre toda a lista, então ela deve terminar em algum momento.

;; subtrai-lista : ListaDeString ListaDeString -> Lista
;; Obj:  dadas duas listas de strings, devolve todos os
;; elementos da primeira lista que não estão na segunda (ou seja,
;; subtrai a segunda lista da primeira).
;; Exemplos:   (subtrai-lista (list "Little Rock" "Nashville" "Atlanta" "Chicago") (list "Chicago" "Nashville")) = (list "Little Rock" "Atlanta" )
;;             (subtrai-lista (list "Atlanta" "Pittsburgh" "Saint Louis" "Little Rock") (list "Atlanta" "Pittsburgh" "Saint Louis" "Little Rock")) = (list empty)

(define (subtrai-lista lista1 lista2)
   (cond
    ;; Caso trivial: lista1 é vazia, então devolve lista vazia
    [(empty? lista1) (list empty)]
    ;; Caso lista2 é vazia, então devolve a lista1 inalterada.
    [(empty? lista2) lista1]
    ;; Se não, remove elementos da lista1
    [else  (subtrai-lista (filter (lambda (x) (not (eq? (first lista2) x))) lista1) (rest lista2))]
    )
  )

;; Testes
(check-expect (subtrai-lista (list "Chicago") (list "Nashville")) (list "Chicago"))
(check-expect (subtrai-lista (list "Atlanta" "Pittsburgh" "Saint Louis" "Little Rock") (list "Atlanta" "Pittsburgh" "Saint Louis" "Little Rock")) (list empty))
(check-expect (subtrai-lista (list "Little Rock" "Nashville" "Atlanta" "Chicago") (list "Chicago" "Nashville")) (list "Little Rock" "Atlanta" ))
  

;;=========================================================
;; QUESTÃO 3
;;=========================================================

;; 3. Analise a função vizinhos mostrada no slide 10 e construa uma função chamada vizinhos da seguinte forma:
;; a entrada deve ser o nome de um nodo, um grafo e uma lista de nomes de nodos (já visitados), nesta ordem, e
;; a saída deve ser a lista dos nomes dos vizinhos deste nodo que não constam da lista de (nomes de nodos de )
;; entrada.

;;------------------------
;; CONJUNTO LISTADENODOS
;;------------------------
;; Um conjunto ListadeNodos é uma lista que pode ser
;; 1. empty, ou
;; 2. (cons nv), onde
;;     nv : ListadeNodos, representando uma lista de nomes de nodos já visitados


;; vizinhos: String Grafo ListadeNodos -> ListaDeString
;; Obj.: Dados o nome de um nodo, um grafo, e uma lista de nodos já visitados, devolve os nomes de
;; todos os nodos vizinhos do nodo dado que não constam na lista de entrada.
;;     Obs.: O nodo dado deve fazer parte do grafo.
;; Exemplos:     (vizinhos "Winnipeg" MAPA (list "Denver")) = (list "Sault St. Marie" "Duluth" "Helena")
;;               (vizinhos "Pittsburgh" MAPA (list "Toronto" "Chicago")) = (list "Saint Louis" "Nashville")
;;               (vizinhos "Santa Fé" MAPA (list "Denver")) = (list "Oklahoma City")

(define (vizinhos nome grafo nodos-visitados)
 (cond
   ;; Se o grafo estiver vazio, retornar a lista vazia.
   [(empty? grafo) empty]
   ;; Se o nome do primeiro nodo do grafo for nome, devolve os vizinhos deste nodo menos os já visitados
   [(string=? nome (nodo-nome (first grafo))) (subtrai-lista (nodo-vizinhos (first grafo)) nodos-visitados)]
   ;; Senão, subtrai nodo visitado do grafo e procura os vizinhos do nome nos outros nodos do grafo.
   [else (vizinhos nome (rest grafo) nodos-visitados)]
   )
 )

;; Testes
(check-expect (vizinhos "Winnipeg" MAPA (list "Denver")) (list "Sault St. Marie" "Duluth" "Helena"))
(check-expect (vizinhos "Pittsburgh" MAPA (list "Toronto" "Chicago")) (list "Saint Louis" "Nashville"))
(check-expect (vizinhos "Santa Fé" MAPA (list "Denver")) (list "Oklahoma City"))

;; Argumentação de terminação: a função vizinhos sempre encerrará ao encontrar um nome igual ao dado de entrada. Como nessa função só pode
;; ser usado um nome que já seja parte do grafo, então ela sempre termina.


;;=========================================================
;; QUESTÃO 4
;;=========================================================

;; 4. Usando a função vizinhos construída na questão anterior, desenvolva as funções encontra-caminho e
;; encontra-caminho-vizinhos que encontram um caminho em um grafo, se existir, sem entrar em loop infinito
;; quando forem encontrados ciclos (ver nos slides 24 e 25 os esboços destes programas).


;;--------------------------------
;; TIPO LISTADESTRINGOUFALSE
;;--------------------------------
;; Uma ListaDeStringOUFalse pode ser
;; 1. ListaDeString, ou
;; 2. Booleano de valor false


;; encontra-lista-caminhos: ListaDeStrings String Grafo -> FalseOuListaDeStrings
;;Obj.: Dada uma lista de nomes de nodos, o nome de um nodo destino e um grafo, encontra um caminho de algum dos nodos da lista para o destino, usando o grafo.
;;      Se tal caminho não existir, devolve false
;;Exemplos:      (encontra-lista-caminhos (list "Atlanta") "Atlanta" MAPA) = (list "Atlanta")
;;               (encontra-lista-caminhos (list "Atlanta") "Nashville" MAPA) = (list "Atlanta" "Nashville")

(define (encontra-lista-caminhos lista-de-cidades destino grafo)
  (cond
    [(empty? lista-de-cidades) false]
    [else
      (local
        (
          (define caminho-possível (encontra-caminho (first lista-de-cidades) destino grafo))
        )
        (cond
          [(boolean? caminho-possível) (encontra-lista-caminhos (rest lista-de-cidades) destino grafo)]
          [else caminho-possível]
        )
      )
    ]
  )
)

(check-expect (encontra-lista-caminhos (list "Atlanta") "Atlanta" MAPA) (list "Atlanta"))
(check-expect (encontra-lista-caminhos (list "Atlanta") "Nashville" MAPA) (list "Atlanta" "Nashville"))


;; encontra-caminho: String String Grafo ListaDeString -> ListaDeStringOUFalse
;; Obj.: Dados os nomes das cidades origem e destino, um grafo (mapa) e uma lista de cidades já visitadas, encontra um caminho entre a
;; origem e o destino. Se não existir caminho, devolve false.
;;    Obs.: As cidades dadas devem fazer parte do grafo.
;;Exemplos:      (encontra-caminhos "Atlanta" "Nashville" MAPA) = (list "Atlanta" "Nashville")
;;               (encontra-caminhos "Chicago" "Chicago" MAPA) = (list "Chicago" )
;;               (encontra-caminho "Atlanta" "Pittsburgh" MAPA) = (list "Atlanta" "Nashville" "Pittsburgh")

(define (encontra-caminho origem destino grafo)
  (cond
    [(string=? origem destino) (list destino)]
    [else
      (local
        (
          (define caminho (encontra-lista-caminhos (vizinhos origem grafo empty) destino grafo))
        )
        (cond
          [(boolean? caminho) false]
          [else(cons origem caminho)]
        )
      )
    ]
  )
)




;; Testes
(check-expect (encontra-caminhos "Atlanta" "Nashville" MAPA) (list "Atlanta" "Nashville"))
(check-expect (encontra-caminhos "Chicago" "Chicago" MAPA) (list "Chicago" ))
(check-expect (encontra-caminho "Atlanta" "Pittsburgh" MAPA) (list "Atlanta" "Nashville" "Pittsburgh"))
 


;; encontra-caminho-vizinhos: ListaDeString String Grafo ListaDeString -> ListaDeStringOUFalse
;; Obj.: Dados uma lista de cidades origem, um destino, um grafo (mapa) e uma lista de cidades já visitadas,
;; encontra um caminho entre alguma das origens e o destino. Se não existir caminho, devolve false.
;; Obs.: As cidades dadas devem fazer parte do grafo.
;;(define (encontra-caminho-vizinhos listaOrigens destino grafo visitadas)
 

