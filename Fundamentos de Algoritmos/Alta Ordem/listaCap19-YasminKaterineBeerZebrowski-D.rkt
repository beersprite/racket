;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname listaCap19-YasminKaterineBeerZebrowski-D) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; Capítulo 19 - Funções de Alta-Ordem
;; Yasmin Katerine Beer Zebrowski
;; Turma D

;; ==============================================================
;; Questão 1
;; ==============================================================

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================
;; Considere as definições de dados e funções a seguir.
;; Reescreva as funções (A) a (E) usando as funções filter, map e/ou foldl.
;; O código das novas funções não pode ter condicional (cond).
;; Inclua pelo menos 2 testes para cada função de (A) a (E).
;; ========================================================================
;; ============================
;; DEFINIÇÕES DE TIPOS DE DADOS
;; ============================
;;   TIPO RETANGULO:
(define-struct retangulo (lado1 lado2 cor nome))
;; Um elemento de Retangulo tem o formato
;;    (make-retangulo  l1 l2 c n), onde
;;     l1: Número, é a largura do retângulo;
;;     l2: Número, é a altura do retângulo;
;;     c : String, é a cor do retângulo;
;;     n : String, é o nome do retângulo.
(define R1 (make-retangulo 30 60 "red" "R1"))
(define R2 (make-retangulo 40 40 "green" "R2"))

;;   TIPO TRIANGULO:
(define-struct triangulo (lado cor nome))
;; Um elemento de Triangulo tem o formato
;;    (make-triangulo  l c n), onde
;;     l: Número, é o lado do triângulo;
;;     c : String, é a cor do triângulo;
;;     n : String, é o nome do triângulo.
(define T1 (make-triangulo 30  "blue" "T1"))
(define T2 (make-triangulo 40  "red" "T2"))

;;   TIPO ELIPSE:
(define-struct elipse (largura altura cor nome))
;; Um elemento de Elipse tem o formato
;;    (make-elipse  l a c n), onde
;;     l: Número, é a largura da elipse;
;;     a: Número, é a altura da elipse;
;;     c : String, é a cor da elipse;
;;     n : String, é o nome da elipse.
(define E1 (make-elipse 30 50 "pink" "E1"))
(define E2 (make-elipse 40 20 "red" "E2"))

;; Uma Forma pode ser
  ;; 1. um Retangulo, ou
  ;; 2. um Triangulo, ou
  ;; 3. uma Elipse

 ;; Uma ListaFormas é
  ;; 1. vazia (empty), ou
  ;; 2. (cons f lf), onde
  ;;     f : Forma
  ;;     lf: ListaFormas

  (define L1 (cons E1 empty))
  (define L2 (cons E1 (cons E2 (cons T1 (cons T2 (cons R1 (cons R2 empty)))))))
  (define L3 (cons T1 (cons T1 (cons T1 (cons T2 (cons R1 (cons R1 empty)))))))
  (define L4 (cons E2 (cons E2 (cons E2 (cons T2 empty)))))

;; =====================
;; DEFINIÇÕES DE FUNÇÕES
;; =====================

;; desenha : Forma -> Imagem
;; Dada uma forma, gera uma imagem desta forma.
;; Exemplos:
;; (desenha E1) = .
;; (desenha (T2) = .
  (define (desenha f)
    (cond
      [(retangulo? f) (rectangle (retangulo-lado1 f) (retangulo-lado2 f) "solid" (retangulo-cor f))]
      [(triangulo? f) (triangle  (triangulo-lado f)  "solid"(triangulo-cor f))]
      [(elipse? f)    (ellipse   (elipse-largura f)  (elipse-altura f) "solid" (elipse-cor f))]))
;; Testes:
(check-expect (desenha E1) (ellipse 30 50 "solid" "pink"))
(check-expect (desenha T2) (triangle 40 "solid" "red"))

;; (A) lista-elipses
;; lista-elipses : ListaFormas -> ListaFormas
;; Objetivo: Dada uma lista de formas, devolve uma lista com todas as elipses
;;      da lista original.
;; Exemplos:
;;  (lista-elipses L3) =  empty
;;  (lista-elipses L2) = (cons E1 (cons E2 empty))
(define (lista-elipses lf)
   (filter elipse? lf)
)
;; Testes:
(check-expect (lista-elipses L1) (cons E1 empty))
(check-expect (lista-elipses L2) (cons E1 (cons E2 empty)))
(check-expect (lista-elipses L3)  empty)
(check-expect (lista-elipses L4) (cons E2 (cons E2 (cons E2 empty))))

;; (B) lista-nomes-elipses
;; lista-nomes-elipses : ListaFormas -> ListaString
;; Objetivo: Dada uma lista de formas, devolve uma lista com os nomes de todas as elipses
;;      da lista original.
;; Exemplos:
;;  (lista-nomes-elipses L3) =  empty
;;  (lista-nomes-elipses L2) = (cons "E1" (cons "E2" empty))
(define (lista-nomes-elipses lf)
  (map elipse-nome (filter elipse? lf))
) 
;; Testes:
(check-expect (lista-nomes-elipses L3) empty)
(check-expect (lista-nomes-elipses L2) (cons "E1" (cons "E2" empty)))

;; (C) lista-elipses-img
;; lista-elipses-img : ListaFormas -> Imagem
;; Objetivo: Dada uma lista de formas, devolve uma imagem com todas
;;       as elipses da lista original, na ordem inversa na qual
;;       elas aparecem na lista
;; Exemplo: (lista-elipses-img L2) = .
(define (lista-elipses-img lf)
  (foldl
   (lambda (a b) (beside(desenha a) b))
   empty-image
   (filter elipse? lf)
  )
 )
;; Testes:
(check-expect (lista-elipses-img L2) (beside (desenha E2) (desenha E1)))
(check-expect (lista-elipses-img L1) (beside (desenha E1) empty-image))

;; (D) soma-lados-triangulos
;; soma-lados-triangulos : ListaFormas -> Numero
;; Objetivo:Dada uma lista de formas, devolve a soma dos tamanhos dos lados dos triangulos da lista.
;; Exemplo: (soma-lados-triangulos L3) = 130
(define (soma-lados-triangulos lf)
   (foldl
   (lambda (a b) (+(triangulo-lado a) b))
   0
   (filter triangulo? lf)
  )
)
;; Testes:
(check-expect (soma-lados-triangulos L3) 130)
(check-expect (soma-lados-triangulos L1) 0)

;; (E) desenha-lista-formas
;; desenha-lista-formas: ListaFormas -> Imagem
;; Objetivo: Dada uma lista de formas, monta uma imagem com todas as formas lado a lado.
;; Exemplos:
;;  (desenha-lista-formas L2) = .
;;  (desenha-lista-formas L3) = .
(define (desenha-lista-formas lf)
  (foldl
   (lambda (a b) (beside b (desenha a)))
   empty-image
   lf
  )
)
;; Testes:
(check-expect (desenha-lista-formas L1) (beside (desenha E1) empty-image))
(check-expect (desenha-lista-formas L2) (beside (desenha E1) (desenha E2) (desenha T1) (desenha T2) (desenha R1) (desenha R2)))
(check-expect (desenha-lista-formas L3) (beside (desenha T1) (desenha T1) (desenha T1) (desenha T2) (desenha R1) (desenha R1)))
(check-expect (desenha-lista-formas L4) (beside (desenha E2) (desenha E2) (desenha E2) (desenha T2)))


;; ==============================================================
;; Questão 2
;; ==============================================================


;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================

;; (a) Definições de dados:

;;------------------
;;   TIPO ALUNO
;;------------------
(define-struct Aluno (nome turma prova1 prova2))
;; Um elemento do tipo Aluno tem o formato
;;    (make-Aluno n t p1 p2), onde
;;      n: String, é o nome do aluno;
;;      t: String, é a turma do aluno;
;;      p1: Número, é a nota da prova 1 do aluno;
;;      p2: Número, é a nota da prova 2 do aluno.
(define aluno1 (make-Aluno "Alisaie" "A" 4 2))
(define aluno2 (make-Aluno "Alphinaud" "B" 10 10))

;;------------------
;;   CONJUNTO LISTADEALUNOS
;;------------------
;; Uma ListaDeAlunos é
;;  1. vazia (empty), ou
;;  2. (cons a l), onde
;;      a : Aluno
;;      l: ListaDeAlunos
(define listaAlunos1 (list aluno1))
(define listaAlunos2 (list aluno1 aluno2))

;; ==============================================================
;; (b) lista-alunos:


;;Função lista-alunos-turma
;; Contrato: lista-alunos-turma: ListaDeAlunos String -> ListaDeAlunos
;; Objetivo: Dada uma lista de alunos e uma turma, devolve os alunos daquela turma.
;; Exemplos:  (lista-alunos listaAlunos1 "A") = (list aluno1)
;;            (lista-alunos listaAlunos2 "B") = (list aluno2)

(define (lista-alunos lista turma)
   (filter
    (lambda (a) (string=? (Aluno-turma a) turma))
    lista
   )
)

;; Testes:
(check-expect (lista-alunos listaAlunos1 "A") (list aluno1))
(check-expect (lista-alunos listaAlunos2 "B") (list aluno2))

;; ==============================================================
;; (c) gera-conceitos:

;;------------------
;;   TIPO PAR
;;------------------
(define-struct par (nome conceito))
;; Um elemento de par tem o formato
;;    (make-par n c), onde
;;     n: String, é o nome do aluno;
;;     c: String, é o conceito do aluno;

(define par1 (make-par "Alisaie" "D"))
(define par2 (make-par "Alphinaud" "A"))

;;------------------
;;   CONJUNTO LISTADEPARES
;;------------------
;; Uma ListaDePares é
;; 1. vazia (empty), ou
;; 2. (cons p lp), onde
;;     p: par
;;     lp: ListaDePares

(define listaPares1 (list par1))
(define listaPares2 (list par1 par2))

;;-------------------------------------
;;Função calcula-media-aluno
;; Contrato: calcula-media : Aluno -> Número
;; Objetivo: Dada um aluno, calcula a média das notas das provas.
;; Exemplos:  (calcula-media aluno1) = 3
;;            (calcula-media aluno2) = 10

(define (calcula-media aluno)
   (/ (+ (Aluno-prova1 aluno) (Aluno-prova2 aluno)) 2)
)
;; Testes
(check-expect (calcula-media aluno1) 3)
(check-expect (calcula-media aluno2) 10)

;;-------------------------------------
;;Função calcula-conceito
;; Contrato: calcula-conceito : Aluno -> String
;; Objetivo: Dada um aluno, calcula seu conceito baseado em suas notas de prova.
;; Exemplos:  (calcula-conceito aluno1) = "D"
;;            (calcula-conceito aluno2) = "A"

(define (calcula-conceito aluno)
   (cond
     [(>= (calcula-media aluno) 9) "A"]
     [(>= (calcula-media aluno) 7.5) "B"]
     [(>= (calcula-media aluno) 6) "C"]
     [else "D"]
    )
)
;;Testes
(check-expect (calcula-conceito aluno1) "D")
(check-expect (calcula-conceito aluno2) "A")


;;-------------------------------------
;;Função gera-conceitos
;; Contrato: gera-conceitos : ListaDeAlunos String -> ListaDePares
;; Objetivo: Dada uma lista de alunos e uma turma, devolve os alunos daquela turma.
;; Exemplos:  (gera-conceitos listaAlunos1) = listaPares1
;;            (gera-conceitos listaAlunos2) = listaPares2

(define (gera-conceitos lista)
   (map
    (lambda (a)
      (make-par
       (Aluno-nome a)
       (calcula-conceito a)
      )
    )
    lista
   )
)

;;Testes
(check-expect (gera-conceitos listaAlunos1)  listaPares1)
(check-expect (gera-conceitos listaAlunos2)  listaPares2)

;; ==============================================================
;; (d) ordena-turma:


;;Função ordena-turma
;; Contrato: ordena-turma : Função ListaDeAlunos -> ListaDeAlunos
;; Objetivo: Dados um critério de ordenação, a lista de alunos de um professor
;; e uma turma, ordena os alunos da turma segundo este critério, devolvendo
;; a lista de alunos desta turma ordenada. 
;; Exemplos:  (ordena-turma (lambda (a b) (string<? (Aluno-nome a) (Aluno-nome b))) listaAlunos2) = listaAlunos2
;;            (ordena-turma (lambda (a b) (string>? (Aluno-nome a) (Aluno-nome b))) listaAlunos2) = (list aluno2 aluno1)

(define (ordena-turma criterio lista)
  (local
    (
      (define (insere n lista)
        (cond
          [(empty? lista) (cons n empty)]
          [(criterio n (first lista)) (cons n lista)]
          [else (cons (first lista) (insere n (rest lista)))]
        )
      )
    )
    (cond
      [(empty? lista) empty]
      [else 
        (insere
          (first lista)
          (ordena-turma criterio (rest lista))
        )
      ]
    )
  )
)

;;Testes
(check-expect (ordena-turma (lambda (a b) (string<? (Aluno-nome a) (Aluno-nome b))) listaAlunos2) listaAlunos2)
(check-expect (ordena-turma (lambda (a b) (string>? (Aluno-nome a) (Aluno-nome b))) listaAlunos2) (list aluno2 aluno1))
(check-expect (ordena-turma (lambda (a b) (< (Aluno-prova1 a) (Aluno-prova1 b))) listaAlunos2) (list aluno1 aluno2))