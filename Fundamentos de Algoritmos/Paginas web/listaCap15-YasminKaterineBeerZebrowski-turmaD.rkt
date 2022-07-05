;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listaCap15-YasminKaterineBeerZebrowski-turmaD) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Lista 8
;; Yasmin Katerine Beer Zebrowski
;; Turma D


;; ----------------------
;; TIPO PAG-WEB
;; ----------------------
(define-struct pag-web (nome conteudo))
;; Um elemento do conjunto Pag-web é uma estrutura
;; (make-pag-web um-nome um-cont) onde
;;   um-nome: String, é o nome da página e
;;   um-cont: Lista-conteudo, é uma lista de conteúdo de páginas-web.
;; -----------------------------------
;; TIPO LISTA-CONTEUDO:
;; -----------------------------------
;; Uma Lista-conteudo é
;;   1. empty (vazia), ou
;;   2. (cons p lc), onde
;;      p : String
;;      lc : Lista-conteúdo, ou
;;   3. (cons i lc), onde
;;      i : Imagem
;;      lc : Lista-conteúdo, ou
;;   4. (cons p lc), onde
;;      p : Pag-web
;;      lc : Lista-conteúdo.


;;==================================================================
;; Questão 1
;;==================================================================

;;1. (Fácil) Dê exemplos de elementos dos conjuntos Pag-web e Lista-conteudo. Dê, pelo menos, 4 exemplos de
;; elementos de cada conjunto, sendo que um dos elementos de Pag-web deve ter no mínimo 2 níveis de sub-páginas
;; (ou seja, uma página que contém outra página que contém outra página).

(define imagemgenerica(circle 5 "solid" "red"))


(define Neopets(make-pag-web "Neopets"
                             (list (make-pag-web "Mundo" (cons imagemgenerica empty))
                                   (make-pag-web "Meu Pet" (list (make-pag-web "Personalizar" (cons imagemgenerica empty))
                                                                 (make-pag-web "Alimentar" (cons imagemgenerica
                                                                                                 (cons imagemgenerica empty)))))
                                   (make-pag-web "Inventário" empty)
                                   (make-pag-web "Fórum" (make-pag-web "Postar" (cons "text text text" empty))))))

(define DeviantArt(make-pag-web "DeviantArt"
                             (list (make-pag-web "Home" (list
                                                         (make-pag-web "Galeria" (list (make-pag-web "Pesquisar"  (cons "text text text" empty))
                                                                                       (cons imagemgenerica
                                                                                             (cons imagemgenerica empty))))
                                                         (make-pag-web "Postar" empty)
                                                         (make-pag-web "Perfil" (list (make-pag-web "Favoritos" empty)
                                                                                      (make-pag-web "Comentários"  (cons "text text text" (cons "text text text" empty))))))))))
(define pudim(make-pag-web "Pudim"
                           (list (make-pag-web "Home" (cons imagemgenerica empty)))))

(define Google (make-pag-web "Google"
                             (list (make-pag-web "Home" (list(make-pag-web "Tudo" empty)
                                                             (make-pag-web "Imagens" (cons imagemgenerica
                                                                                           (cons imagemgenerica empty)))
                                                             (make-pag-web "Mapa" (cons imagemgenerica empty))
                                                             (make-pag-web "Notícias"  (cons "text text text" empty)))))))


;;==================================================================
;; Questão 2
;;==================================================================

;; Função lista-palavras
;; Contrato:
;;            lista-palavras : Pag-web -> Lista
;; Objetivo:  dada uma página web, devolve a lista de palavras que a página
;;contém, sem considerar suas sub-páginas.
;; Exemplos:  (lista-palavras (make-pag-web "Comentários" (cons "text text text" (cons "text text text" empty))) = (cons "text text text" (cons "text text text" empty))
;;            (lista-palavras pudim) = empty

(define (lista-palavras paginaweb)
  (cond
    ;; Se a lista está vazia, retorna empty
    [(empty? (pag-web-conteudo paginaweb)) empty]
    ;; Se não, checa se é lista de strings e coloca na lista
    [(string? (first (pag-web-conteudo paginaweb)))  (cons (first (pag-web-conteudo paginaweb))
                                                                  (lista-palavras (make-pag-web (pag-web-nome paginaweb) (rest (pag-web-conteudo paginaweb)))))]
    ;; Se não, procura mais por recursão
    [else (lista-palavras(make-pag-web (pag-web-nome paginaweb) (rest (pag-web-conteudo paginaweb))))]
  )
)

                                                                                  
;; Testes                                      
(check-expect (lista-palavras(make-pag-web "Comentários"(cons "text text text" (cons "text text text" empty)))) (cons "text text text" (cons "text text text" empty)))
(check-expect (lista-palavras pudim) empty)



;;==================================================================
;; Questão 3
;;==================================================================

;;Função mostra-imagens
;; Contrato:
;;          mostra-imagens : Pag-web -> Imagem Imagem
;;Objetivo:  Dada uma página web, devolve uma imagem contendo as
;;imagens contidas na página e em suas sub-páginas, lado a lado. Acima de cada imagem, deve ser colocado o
;;nome da página onde a imagem aparece, usando fonte 15 e cor preta.
;;Exemplos:


(define (mostra-imagens paginaweb)
  
  (cond
    ;; Se a lista está vazia, retorna empty
    [(empty? (pag-web-conteudo paginaweb)) empty]
    ;; Se não, imprime a imagem se tiver
    [(image? (first (pag-web-conteudo paginaweb))) (beside (above(text (pag-web-nome paginaweb) 15 "black")
                                                                 (first (pag-web-conteudo paginaweb)))
                                                           (mostra-imagens (make-pag-web (pag-web-nome paginaweb) (rest (pag-web-conteudo paginaweb)))))]
    ;; Se não, procura mais imagens por recursão
    [else (mostra-imagens (make-pag-web (pag-web-nome paginaweb) (rest (pag-web-conteudo paginaweb))))]))


(mostra-imagens pudim)
(mostra-imagens Neopets)






;;não consegui terminar, lista muito difícil.





;;==================================================================
;; Questão 4
;;==================================================================


;;Função profundidade
;; Contrato:
;;          profundidade : Pag-web -> Número
;;Objetivo:   Calcula a profundidade de uma página web (ou seja, o número
;; máximo de níveis que ela contém). Uma página web sem sub-páginas tem profundidade zero
;;Exemplos:





;;==================================================================
;; Questão 5
;;==================================================================


;;Função gera-imagem-pagina
;; Contrato:
;;          gera-imagem-pagina : Pag-web -> Imagem
;;Objetivo:   gera uma imagem para uma página web, mostrando seu
;;conteúdo de forma estruturada, indicando o nome da página bem como seu conteúdo.
;;Exemplos: 



