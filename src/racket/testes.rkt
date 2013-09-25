#lang racket

(require rackunit)
(require rackunit/text-ui)
(require "tetra-tipos.rkt")
(require "base.rkt")
(require "tetris.rkt")

;; Constantes usadas nos testes
(define TIMEOUT 14)

(define TS0 (tetramino S_TIPOS 0 (posn 0 3) S_COR))
(define TS0_ROT (tetramino S_TIPOS 1 (posn 0 3) S_COR))

(define TO0 (tetramino O_TIPOS 0 (posn 0 1) O_COR))
(define TO0_MOVE_DIREITA (tetramino O_TIPOS 0 (posn 0 2) O_COR))
(define TO0_MOVE_ESQUERDA (tetramino O_TIPOS 0 (posn 0 0) O_COR))

(define TO0_CANTO_DIREITO (tetramino O_TIPOS 0 (posn 0 5) O_COR))
(define TO0_CANTO_DIREITO_BAIXO (tetramino O_TIPOS 0 (posn 1 5) O_COR))
(define TO0_CANTO_ESQUERDO (tetramino O_TIPOS 0 (posn 0 0) O_COR))
(define TO0_MOVE_DIREITA_COLIDE (tetramino O_TIPOS 0 (posn 0 6) O_COR))
(define TO0_MOVE_ESQUERDA_COLIDE (tetramino O_TIPOS 0 (posn 0 -1) O_COR))

(define TO0_CANTO_ESQUERDO_FIXADA (tetramino O_TIPOS 0 (posn 1 1) O_COR))
(define TO0_MOVE_ESQUERDA_COLIDE_FIXADA (tetramino O_TIPOS 0 (posn 1 0) O_COR))


(define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
(define TT1_POS (list (posn 1 1)
                      (posn 2 1) (posn 2 2)
                      (posn 3 1)))
(define TT1_CENTRA_10 (tetramino T_TIPOS 1 (posn 1 3) T_COR))

(define TZ2 (tetramino Z_TIPOS 2 (posn 2 3) Z_COR))
(define TZ2_POS (list (posn 3 3) (posn 3 4)
                      (posn 4 4) (posn 4 5)))
(define TZ2_CENTRA_15 (tetramino Z_TIPOS 2 (posn 2 6) Z_COR))

(define TI0 (tetramino I_TIPOS 0 (posn -1 1) I_COR))
(define TI0_POS (list (posn 0 1) (posn 0 2) (posn 0 3) (posn 0 4)))
(define TI0_CENTRA_12 (tetramino I_TIPOS 0 (posn -1 4) I_COR))

(define C3 (list (list 0 0 0 0 0 0 0)   ; 0
                 (list 0 0 0 0 0 0 0)   ; 1
                 (list 2 2 2 2 0 0 0)   ; 2
                 (list 4 0 2 4 0 0 1)   ; 3
                 (list 3 4 0 0 0 0 3)   ; 4
                 (list 1 2 4 0 2 5 6))) ; 5
                 ;     0 1 2 3 4 5 6

(define C3_O34 (list (list 0 0 0 0 0 0 0)   ; 0
                     (list 0 0 0 0 0 0 0)   ; 1
                     (list 2 2 2 2 0 0 0)   ; 2
                     (list 4 0 2 4 4 4 1)   ; 3
                     (list 3 4 0 0 4 4 3)   ; 4
                     (list 1 2 4 0 2 5 6))) ; 5
                     ;     0 1 2 3 4 5 6

(define O00 (tetramino O_TIPOS 0 (posn 0 0) O_COR))
(define O14 (tetramino O_TIPOS 0 (posn 1 4) O_COR))
(define O24 (tetramino O_TIPOS 0 (posn 2 4) O_COR))
(define O34 (tetramino O_TIPOS 0 (posn 3 4) O_COR))
(define O44 (tetramino O_TIPOS 0 (posn 4 4) O_COR))

(define I03 (tetramino I_TIPOS 0 (posn 0 3) I_COR))
(define I03_1 (tetramino I_TIPOS 1 (posn 0 3) I_COR))
(define I13_1 (tetramino I_TIPOS 1 (posn 1 3) I_COR))
(define T03 (tetramino T_TIPOS 0 (posn 0 3) T_COR))
(define T03_1 (tetramino T_TIPOS 1 (posn 0 3) T_COR))
(define T03_2 (tetramino T_TIPOS 2 (posn 0 3) T_COR))
(define T03_3 (tetramino T_TIPOS 3 (posn 0 3) T_COR))

(define C1 (list (list 0 0 0 0 0 0 0)   ; 0
                 (list 0 0 0 0 0 0 0)   ; 1
                 (list 6 0 0 0 0 0 0)   ; 2
                 (list 4 0 2 4 6 1 1)   ; 3
                 (list 3 4 0 0 0 0 0)   ; 4
                 (list 1 2 4 3 2 5 6))) ; 5
                 ;     0 1 2 3 4 5 6

(define C1_FIXA_TO0 (list (list 0 0 0 0 0 0 0)   ; 0
                          (list 0 0 0 0 0 4 4)   ; 1
                          (list 6 0 0 0 0 4 4)   ; 2
                          (list 4 0 2 4 6 1 1)   ; 3
                          (list 3 4 0 0 0 0 0)   ; 4
                          (list 1 2 4 3 2 5 6))) ; 5
                          ;     0 1 2 3 4 5 6

(define C1_LARGURA 7)
(define C1_ALTURA 6)
;; algumas posições ocupadas em C1
(define C1_OCUPADAS (list (posn 2 0) (posn 3 2) (posn 4 1)))
;; algumas posições livres em C1
(define C1_LIVRES (list (posn 0 0) (posn 3 1) (posn 4 2)))

(define C T_COR)

; Representa C1 com o tetraminó TT1 fixado no campo
(define C1_FIXA_TT1 (list (list 0 0 0 0 0 0 0)   ; 0
                          (list 0 C 0 0 0 0 0)   ; 1
                          (list 6 C C 0 0 0 0)   ; 2
                          (list 4 C 2 4 6 1 1)   ; 3
                          (list 3 4 0 0 0 0 0)   ; 4
                          (list 1 2 4 3 2 5 6))) ; 5
;     0 1 2 3 4 5 6

; Representa C1_FIXA_TT1 sem as linha completas
(define C1_FIXA_TT1_LIMPA (list (list 0 0 0 0 0 0 0)   ; 0
                                (list 0 0 0 0 0 0 0)   ; 1
                                (list 0 0 0 0 0 0 0)   ; 2
                                (list 0 C 0 0 0 0 0)   ; 3
                                (list 6 C C 0 0 0 0)   ; 4
                                (list 3 4 0 0 0 0 0))) ; 5
;     0 1 2 3 4 5 6

(define C2 (list (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)
                 (list 0 0 0 0 0)))

(define C2_FULL (list 
                 (list 0 0 5 0 0)
                 (list 0 0 1 0 0)
                 (list 0 0 1 0 0)
                 (list 0 5 2 5 0)
                 (list 0 0 3 0 0)
                 (list 0 0 4 0 0)
                 (list 0 0 5 0 0)))

(define C2_LARGURA 5)
(define C2_ALTURA 7)

(define make-linha-tests
  (test-suite
   "make-linha tests"
   (check-equal? (make-linha 0) empty)
   (check-equal? (make-linha 5) (list 0 0 0 0 0))))

(define make-campo-tests
  (test-suite
   "make-campo tests"
   (check-equal? (make-campo C2_LARGURA C2_ALTURA) C2)))

(define centraliza-tests
  (test-suite
   "centraliza tests"
   (check-equal? (centraliza TT1 10)
                 TT1_CENTRA_10)
   (check-equal? (centraliza TZ2 15)
                 TZ2_CENTRA_15)
   (check-equal? (centraliza TI0 12)
                 TI0_CENTRA_12)))

(define make-tetris-tests
  (test-suite
   "make-tetris tests"
   (check-equal? (make-tetris C2_LARGURA C2_ALTURA (list TT1 TZ2 TI0) TIMEOUT)
                 (tetris C2
                         C2_LARGURA
                         C2_ALTURA
                         (centraliza TT1 C2_LARGURA)
                         (list TZ2 TI0)
                         TIMEOUT))))

(define percorre-lin-tests
  (test-suite
   "percorre-lin tests"
   (check-equal? (percorre-lin S3 (posn 2 2) 0 0)
                (list (posn 2 2) (posn 3 2) (posn 3 3) (posn 4 3)))))

(define percorre-col-tests
  (test-suite
   "percorre-lin tests"
   (check-equal? (percorre-col (list-ref S3 0) (posn 2 2) 0 0)
                (list (posn 2 2)))
   (check-equal? (percorre-col (list-ref S3 1) (posn 2 2) 1 0)
                (list (posn 3 2) (posn 3 3)))
   (check-equal? (percorre-col (list-ref S3 2) (posn 2 2) 2 0)
                (list (posn 4 3) ))))

(define trata-tecla-tests
  (test-suite
   "trata-tecla tests"
   (check-equal? (trata-tecla (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO empty TIMEOUT) "right")
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO empty TIMEOUT))
   (check-equal? (trata-tecla (tetris C1 C1_LARGURA C1_ALTURA TO0 empty TIMEOUT) "right")
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_DIREITA empty TIMEOUT))
   (check-equal? (trata-tecla (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_ESQUERDO empty TIMEOUT) "left")
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_ESQUERDO empty TIMEOUT))
   (check-equal? (trata-tecla (tetris C1 C1_LARGURA C1_ALTURA TO0 empty TIMEOUT) "left")
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_ESQUERDA empty TIMEOUT))
   (check-equal? (trata-tecla (tetris C1 C1_LARGURA C1_ALTURA TO0 empty TIMEOUT) "left")
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_ESQUERDA empty TIMEOUT))
   (check-equal? (trata-tecla (tetris C1 C1_LARGURA C1_ALTURA TS0 empty TIMEOUT) "up")
                 (tetris C1 C1_LARGURA C1_ALTURA TS0_ROT empty TIMEOUT))))

(define modifica-col-tests
  (test-suite 
   "modifica-col tests"
   (check-equal? (modifica-col add1 (posn 1 0)) (posn 1 1))
   (check-equal? (modifica-col add1 (posn 2 2)) (posn 2 3))
   (check-equal? (modifica-col sub1 (posn 0 0)) (posn 0 -1))
   (check-equal? (modifica-col sub1 (posn 4 5)) (posn 4 4))))

(define modifica-lin-tests
  (test-suite 
   "modifica-lin tests"
   (check-equal? (modifica-lin add1 (posn 1 0)) (posn 2 0))
   (check-equal? (modifica-lin add1 (posn 2 2)) (posn 3 2))
   (check-equal? (modifica-lin sub1 (posn 0 0)) (posn -1 0))
   (check-equal? (modifica-lin sub1 (posn 4 5)) (posn 3 5))))

(define move-direita-tests
  (test-suite
   "move-direita tests"
   (check-equal? (move-direita (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO empty TIMEOUT))
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO empty TIMEOUT))
   (check-equal? (move-direita (tetris C1 C1_LARGURA C1_ALTURA TO0 empty TIMEOUT))
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_DIREITA empty TIMEOUT))))

(define move-esquerda-tests
  (test-suite
   "move-esqeurda tests"
   (check-equal? (move-esquerda (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_ESQUERDO empty TIMEOUT))
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_ESQUERDO empty TIMEOUT))
   (check-equal? (move-esquerda (tetris C1 C1_LARGURA C1_ALTURA TO0 empty TIMEOUT))
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_ESQUERDA empty TIMEOUT))))

(define move-baixo-tests
  (test-suite
   "move-baixo tests"
   (check-equal? (move-baixo (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO empty TIMEOUT))
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO_BAIXO empty TIMEOUT))))

(define move-se-nao-colidiu-tests
  (test-suite
   "move-se-nao-colidiu tests"
   (check-equal? (move-se-nao-colidiu (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_DIREITA_COLIDE empty TIMEOUT)
                                      (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO empty TIMEOUT))
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO empty TIMEOUT))
   (check-equal? (move-se-nao-colidiu (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_ESQUERDA_COLIDE empty TIMEOUT)
                                      (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_ESQUERDO empty TIMEOUT))
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_ESQUERDO empty TIMEOUT))
   (check-equal? (move-se-nao-colidiu (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_DIREITA empty TIMEOUT)
                                      (tetris C1 C1_LARGURA C1_ALTURA TO0 empty TIMEOUT))
                 (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_DIREITA empty TIMEOUT))))

(define fixa-se-colidiu-tests
  (test-suite
   "fixa-se-colidiu tests"
   (check-equal? (fixa-se-colidiu (tetris C3 C1_LARGURA C1_ALTURA O24 empty 1) 
                                  (tetris C3 C1_LARGURA C1_ALTURA O14 empty 1))
                 (tetris C3 C1_LARGURA C1_ALTURA O24 empty 1))
   
   (check-equal? (tetris-campo (fixa-se-colidiu (tetris C3 C1_LARGURA C1_ALTURA O44 (stream-tetraminos) 1) 
                                                (tetris C3 C1_LARGURA C1_ALTURA O34 (stream-tetraminos) 1)))
                 C3_O34)))

(define trata-tick-tests 
  (test-suite
   "trata-tick tests"

   (check-equal? (trata-tick (tetris C3 C1_LARGURA C1_ALTURA O24 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA O24 empty 1))
   (check-equal? (trata-tick (tetris C3 C1_LARGURA C1_ALTURA O24 empty 1))
                 (tetris C3 C1_LARGURA C1_ALTURA O24 empty 0))
   (check-equal? (trata-tick (tetris C3 C1_LARGURA C1_ALTURA O24 empty 0))
                 (tetris C3 C1_LARGURA C1_ALTURA O34 empty TIMEOUT-PADRAO))
   (check-equal? (tetris-campo (trata-tick (tetris C3 C1_LARGURA C1_ALTURA O34 (stream-tetraminos) 0)))
                 C3_O34)
   (check-equal? (tetris-timeout (trata-tick (tetris C3 C1_LARGURA C1_ALTURA O34 (stream-tetraminos) 0)))
                 TIMEOUT-PADRAO)))

(define game-over?-tests
  (test-suite
   "game-over? tests"
   (check-equal? (game-over? (tetris C2_FULL C2_LARGURA C2_ALTURA (centraliza O00 C2_LARGURA) empty 2))
                 #t)
   (check-equal? (game-over? (tetris C3 C1_LARGURA C1_ALTURA O24 empty 2))
                 #f)))

(define rotaciona-tests
  (test-suite
   "rotaciona-tests"
   (check-equal? (rotaciona (tetris C3 C1_LARGURA C1_ALTURA O24 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA O24 empty 2))
   (check-equal? (rotaciona (tetris C3 C1_LARGURA C1_ALTURA I03 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA I03_1 empty 2))
   (check-equal? (rotaciona (tetris C3 C1_LARGURA C1_ALTURA I03_1 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA I03 empty 2))
   (check-equal? (rotaciona (tetris C3 C1_LARGURA C1_ALTURA I13_1 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA I13_1 empty 2))
   (check-equal? (rotaciona (tetris C3 C1_LARGURA C1_ALTURA T03 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA T03_1 empty 2))
   (check-equal? (rotaciona (tetris C3 C1_LARGURA C1_ALTURA T03_1 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA T03_2 empty 2))
   (check-equal? (rotaciona (tetris C3 C1_LARGURA C1_ALTURA T03_2 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA T03_3 empty 2))
   (check-equal? (rotaciona (tetris C3 C1_LARGURA C1_ALTURA T03_3 empty 2))
                 (tetris C3 C1_LARGURA C1_ALTURA T03 empty 2))))

(define colidiu?-tests
  (test-suite
   "colidiu? tests"
   (check-equal? (colidiu? (tetris C1 C1_LARGURA C1_ALTURA TO0 empty TIMEOUT))
                 #f)
   (check-equal? (colidiu? (tetris C1 C1_LARGURA C1_ALTURA TO0_MOVE_DIREITA_COLIDE empty TIMEOUT))
                 #t)))

(define tetramino->pos-tests
  (test-suite
   "tetramino->pos tests"
   (check-equal? (tetramino->lista-pos TT1) TT1_POS)
   (check-equal? (tetramino->lista-pos TZ2) TZ2_POS)
   (check-equal? (tetramino->lista-pos TI0) TI0_POS)))

(define lop-validas?-tests
  (test-suite
   "lop-validas? tests"
   (check-equal? (lop-validas? empty 5 8)
                 #t)
   ;; testa os extremos
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn (sub1 C1_ALTURA) 0)
                                     (posn 0 (sub1 C1_LARGURA))
                                     (posn (sub1 C1_ALTURA) (sub1 C1_LARGURA)))
                               C1_LARGURA
                               C1_ALTURA)
                 #t)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn C1_ALTURA 0) ; linha inválida
                                     (posn 1 2))
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn  2 3)
                                     (posn -1 3)) ; linha inválida
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn 0 C1_LARGURA) ; coluna inválida
                                     (posn 1 2))
                               C1_LARGURA
                               C1_ALTURA)
                 #f)
   (check-equal? (lop-validas? (list (posn 0 0)
                                     (posn 1 -1)) ; coluna inválida
                               C1_LARGURA
                               C1_ALTURA)
                 #f)))

(define lop-livres?-tests
  (test-suite
   "lop-livres? tests"
   (check-equal? (lop-livres? C1_LIVRES C1) #t)
   (check-equal? (lop-livres? C1_OCUPADAS C1) #f)
   (check-equal? (lop-livres? (append C1_LIVRES (list (first C1_OCUPADAS))) C1) #f)))


(define fixa-tests
  (test-suite
   "fixa tests"
   (check-equal? (fixa (tetris C1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))
                 (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))
   (check-equal? (fixa (tetris C1 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO_BAIXO empty TIMEOUT))
                 (tetris C1_FIXA_TO0 C1_LARGURA C1_ALTURA TO0_CANTO_DIREITO_BAIXO empty TIMEOUT))))

(define fixa-linha-tests
  (test-suite
   "fixa-linha tests"
   (check-equal? (fixa-linha (list) 7 (list 0 0 0 0 1 3 6 4 5 2)) 
                 (list 0 0 0 0 1 3 6 4 5 2))
   (check-equal? (fixa-linha (list 1 2 3) 7 (list 0 0 0 0 1 3 6 4 5 2)) 
                 (list 0 7 7 7 1 3 6 4 5 2))
   (check-equal? (fixa-linha (list 0 1) 7 (list 0 0 0 0 1 3 6 4 5 2)) 
                 (list 7 7 0 0 1 3 6 4 5 2))))

(define contem-tests 
  (test-suite 
   "contem-tests"
   (check-equal? (contem? 1 (list 1 3 5 9)) #t)
   (check-equal? (contem? 9 (list 1 3 5 9)) #t)
   (check-equal? (contem? 5 (list 1 3 5 0 9)) #t)
   (check-equal? (contem? 1 (list)) #f)
   (check-equal? (contem? 2 (list 1 3 5 9)) #f)))

(define get-cols-tests 
  (test-suite 
   "get-cols-tests"
   (check-equal? (get-cols (list (posn 0 2) (posn 0 3) (posn 1 5) (posn 1 2) (posn 1 3) (posn 2 3)) 1)
                 (list 5 2 3))
   (check-equal? (get-cols (list (posn 0 2) (posn 0 3) (posn 1 5) (posn 1 2) (posn 1 3) (posn 2 3)) 0)
                 (list 2 3))
   (check-equal? (get-cols (list (posn 0 2) (posn 0 3) (posn 1 5) (posn 1 2) (posn 1 3) (posn 2 3)) 2)
                 (list 3))
   (check-equal? (get-cols (list) 1)
                 (list))))

(define limpa-tests
  (test-suite
   "limpa tests"
   (check-equal? (limpa (tetris C1_FIXA_TT1 C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))
                 (tetris C1_FIXA_TT1_LIMPA C1_LARGURA C1_ALTURA TT1 empty TIMEOUT))))

(define linha-completa?-tests 
  (test-suite 
   "linha-completa? tests"
   (check-equal? (linha-completa? '(0 1 2 3 4 5 6 7 5 6)) #f)
   (check-equal? (linha-completa? '(1 1 2 3 4 5 5 6 6 0)) #f)
   (check-equal? (linha-completa? '(5 5 5 5 5 5 5 5 5 5)) #t)
   (check-equal? (linha-completa? '(5 6 4 6 0 0 0 7 6 4)) #f)
   (check-equal? (linha-completa? '()) #t)
   ))

;; ---------------------------------------------------------------------

;; Função que executa um grupo de testes.
(define (executar-testes . testes)
  (run-tests (test-suite "Todos os testes" testes))
  (void))

;; Chama a função para executar os testes.
(executar-testes make-linha-tests
                 make-campo-tests
                 centraliza-tests
                 make-tetris-tests
                 trata-tecla-tests
                 percorre-lin-tests
                 percorre-col-tests
                 trata-tick-tests
                 rotaciona-tests
                 game-over?-tests
                 modifica-col-tests
                 modifica-lin-tests
                 move-direita-tests
                 move-esquerda-tests
                 move-baixo-tests
                 move-se-nao-colidiu-tests
                 fixa-se-colidiu-tests
                 colidiu?-tests
                 tetramino->pos-tests
                 lop-validas?-tests
                 lop-livres?-tests
                 fixa-tests
                 fixa-linha-tests
                 contem-tests
                 get-cols-tests
                 limpa-tests
                 linha-completa?-tests)
