;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listaCap6-YasminKaterineBeerZebrowski-D) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;;     Lista Capítulo 6 - Estruturas
;;     Yasmin Katerine Beer Zebrowski
;;     Turma D


;;==============================================================================================
;; QUESTÃO 1
;;==============================================================================================

;; Aluno
;; Criando a estrutura Aluno:
(define-struct Aluno(nome matricula curso instituicao))
;; Um elemento do conjunto Aluno tem o formato
;;  (make-Aluno n m c InstEnsino)
;; onde:
;; n : string representando o nome do aluno
;; m : número representando número de matrícula do aluno
;; c : string representando o curso do aluno
;; InstEnsino : string representando a instituição

;;---------------------------------------------------------------------------------------------
;; InstEnsino
;; Criando a estrutura InstEnsino:
(define-struct InstEnsino(nomeinst anofundacao))
;; Um elemento do conjunto InstEnsino tem o formato
;;  (make-InstEnsino ni ano)
;; onde:
;; ni : string representando o nome da instituição
;; ano : número representando o ano de fundação da instituição

(check-expect (Aluno-nome(make-Aluno "Ana" 111 "BIO" "UFRGS" ))'Ana)
(check-expect (Aluno-curso(make-Aluno "Ana" 111 "BIO" "UFRGS" ))'BIO)
(check-expect (Aluno-matricula(make-Aluno "Ana" 111 "BIO" "UFRGS" ))111)
(check-expect (InstEnsino-nomeinst(make-InstEnsino "UFRGS" 1895))'UFRGS)

;;---------------------------------------------------------------------------------------------
;; Elementos do conjunto + Testes

;; Alunos fictícios para exemplo:
(make-Aluno "Ana" 111 "BIO" "UFRGS")
(make-Aluno "Luna" 123 "CIC" "UFSM")

;; Instituições para exemplo:
(make-InstEnsino "UFRGS" 1895)
(make-InstEnsino "UFSM" 1960)


;;==============================================================================================
;; QUESTÃO 2
;;==============================================================================================

;; Aluno

;; Criando a estrutura Aluno:
(define-struct Aluno(nome matricula curso instituicao))
;; Um elemento do conjunto Aluno tem o formato
;;  (make-Aluno n m c i)
;; onde:
;; n : string representando o nome do aluno
;; m : número representando número de matrícula do aluno
;; c : string representando o curso do aluno
;; i : string representando a instituição

;;---------------------------------------------------------------------------------------------
;; InstEnsino

;; Criando a estrutura InstEnsino:
(define-struct InstEnsino(nome ano))
;; Um elemento do conjunto InstEnsino tem o formato
;;  (make-InstEnsino ni ano)
;; onde:
;; ni : string representando o nome da instituição
;; ano : número representando o ano de fundação da instituição

(check-expect (mesmaInstituição? A B X)"Não")
(check-expect (mesmaInstituição? A C X)"Sim")
(check-expect (mesmaInstituição? A B Y)"Não")

;;---------------------------------------------------------------------------------------------
;; Função Principal

;; Contrato:
;;    mesmaInstituição?: aluno aluno InstEnsino -> string
;; Objetivo: dado dois alunos e uma instituição de ensino, verifica se ambos
;; estão matriculados naquela instituição.
;; Exemplos: (mesmaInstituição? A B X) deve retornar "Não"
;; (mesmaInstituição? A C Y) deve retornar "Não"

;; Definição (Cabeçalho + Corpo)
(define (mesmaInstituição? a b inst)
  (cond
    [(and (string=? (Aluno-instituicao a) (Aluno-instituicao b)) (string=? (Aluno-instituicao a) (InstEnsino-nome inst)))"Sim"]
    [else "Não"]
    )
  )

;;---------------------------------------------------------------------------------------------
;; Funções auxiliares

;; Definindo alunos fictícios
(define A(make-Aluno "Aluno1" 111 "BIO" "UFRGS"))
(define B(make-Aluno "Aluno2" 121 "CIC" "UFSM"))
(define C(make-Aluno "Aluno3" 131 "FIS" "UFRGS"))

;; Definindo Instituições 
(define X(make-InstEnsino "UFRGS" 1895))
(define Y(make-InstEnsino "UFSM" 1960))

;;---------------------------------------------------------------------------------------------
;; Testes:

(mesmaInstituição? A B X)
;; Resultado esperado
;; "Não"

(mesmaInstituição? A C X)
;; Resultado esperado
;; "Sim"



;;==============================================================================================
;; QUESTÃO 3
;;==============================================================================================

;; Aluno

;; Criando a estrutura Aluno:
(define-struct Aluno(nome matricula curso instituicao))
;; Um elemento do conjunto Aluno tem o formato
;;  (make-Aluno n m c i)
;; onde:
;; n : string representando o nome do aluno
;; m : número representando número de matrícula do aluno
;; c : string representando o curso do aluno
;; i : string representando a instituição

;;---------------------------------------------------------------------------------------------
;; InstEnsino

;; Criando a estrutura InstEnsino:
(define-struct InstEnsino(nome ano))
;; Um elemento do conjunto InstEnsino tem o formato
;;  (make-InstEnsino ni ano)
;; onde:
;; ni : string representando o nome da instituição
;; ano : número representando o ano de fundação da instituição

(check-expect (idade A B X)-1)
(check-expect (idade A C X)125)
(check-expect (idade B B Y)60)

;;---------------------------------------------------------------------------------------------
;; Função Principal

;; Contrato:
;;    idade: Aluno Aluno InstEnsino -> número
;; Objetivo: dado dois alunos e uma instituição de ensino, verifica se ambos
;; estão matriculados naquela instituição e imprime a idade da intituição caso estejam.
;; Caso contrário, imprime -1.
;; Exemplos: (idade A C X) deve retornar 125
;; (idade A C Y) deve retornar -1

;; Definição (Cabeçalho + Corpo)
(define (idade a b inst)
  (cond
    [(and (string=? (Aluno-instituicao a) (Aluno-instituicao b)) (string=? (Aluno-instituicao a) (InstEnsino-nome inst))) (- 2020 (InstEnsino-ano inst))]
    [else -1]
    )
  )

;;---------------------------------------------------------------------------------------------
;; Funções auxiliares

;; Definindo alunos fictícios
(define A(make-Aluno "Aluno1" 111 "BIO" "UFRGS"))
(define B(make-Aluno "Aluno2" 121 "CIC" "UFSM"))
(define C(make-Aluno "Aluno3" 131 "FIS" "UFRGS"))

;; Definindo Instituições 
(define X(make-InstEnsino "UFRGS" 1895))
(define Y(make-InstEnsino "UFSM" 1960))

;;---------------------------------------------------------------------------------------------
;; Testes:

(idade A B X)
;; Resultado esperado
;; -1

(idade A C X)
;; Resultado esperado
;; 125



;;==============================================================================================
;; QUESTÃO 4
;;==============================================================================================

;;        Letra (a)

;;---------------------------------------------------------------------------------------------
;; Carro

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

;;---------------------------------------------------------------------------------------------
;; Moto

;; Criando a estrutura moto:
(define-struct moto(ano modelo valormercado))
;; Um elemento do conjunto moto tem o formato
;;  (make-moto ano modelo valor ac dir vid)
;; onde:
;;     ano : número representando o ano de fabricação da moto
;;     modelo : string representando o modelo da moto
;;     valor : número representando o valor de mercado em reais


;;---------------------------------------------------------------------------------------------

;;       Letras (b) e (c)

;;---------------------------------------------------------------------------------------------
;; Funções Auxiliares

;; Definindo alguns carros e motos para testes
(define carroA(make-carro 2005 "uno" 5000 0 0 0))
(define carroB(make-carro 2020 "dos" 85000 1 1 1))
(define motoA(make-moto 2008 "uno" 3000))
(define motoB(make-moto 2011 "tres" 10000))


;;---------------------------------------------------------------------------------------------
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


;;---------------------------------------------------------------------------------------------
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


;;---------------------------------------------------------------------------------------------
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


;;---------------------------------------------------------------------------------------------
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

;;---------------------------------------------------------------------------------------------
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


;;---------------------------------------------------------------------------------------------
;; Testes letra (b)

(cdc carroB)
;; Resultado esperado
;; 302.5

(cdc carroA)
;; Resultado esperado
;; 12.5


;;---------------------------------------------------------------------------------------------
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

;;---------------------------------------------------------------------------------------------
;; Testes letra (c)

(cdm motoA)
;; Resultado esperado
;;77.5

(cdm motoB)
;; Resultado esperado
;;115


;;==============================================================================================
;; QUESTÃO 5
;;==============================================================================================

;; Posn

;; Criando a estrutura Posn:
(define-struct Posn(coord-x coord-y))
;; Um elemento do conjunto Posn tem o formato
;;  (make-Posn x y)
;; onde:
;; x : número representando a coordenada x do ponto
;; y : número representando a coordenada y do ponto

;;---------------------------------------------------------------------------------------------
;; Funções Auxiliares

;; Definindo ponto central da circunferência (0,0)
(define centro(make-Posn 0 0))

;; Definindo alguns pontos para os testes
(define pontoA(make-Posn 1 -1))
(define pontoB(make-Posn 0 7))
(define pontoC(make-Posn 0 13))

;;--------------------------------------------------------------------------------------------
;; Função Principal

;; Contrato:
;;   área: Posn Posn -> número
;; Objetivo: dados dois elementos de Posn, o centro do círculo e um ponto em sua circunferência,
;; a função retorna a área do círculo.
;; Exemplos: (área centro pontoB) deve produzir #i153.94 (arredondado)
;; (área centro pontoA) deve produzir #i6.28 (arredondado)

(check-within(área centro pontoB)153 154)
(check-within(área centro pontoC)530 531)

;; Definição (Cabeçalho + Corpo)
(define (área centro ponto)

(* pi (expt (sqrt (+ (expt (- (Posn-coord-x ponto) (Posn-coord-x centro)) 2) (expt (- (Posn-coord-y ponto) (Posn-coord-y centro)) 2)))2))
  
)

;;---------------------------------------------------------------------------------------------
;; Testes

(área centro pontoA)
;; Resultado esperado
;; #i6.28 (arredondado)

(área centro pontoB)
;; Resultado esperado
;; #i153.94 (arredondado)

(área centro pontoC)
;; Resultado esperado
;; #i530.93 (arredondado)