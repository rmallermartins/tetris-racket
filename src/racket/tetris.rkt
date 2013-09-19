#lang racket

;; Você deve implementar as funções neste arquivo. Novas funções podem ser
;; criadas, mas todas as funções devem ter testes (no arquivo testes.rkt).
;;
;; Observe que algumas destas funções não tem testes, faz parte do trabalho
;; criar estes testes.
;;
;; Você não precisa se preocupar com ler a tecla pressionada ou desenhar o jogo
;; na tela. O arquivo main.rkt chama uma função que faz isso. Basta você
;; implementar as funções deste arquivo que o jogo funciona.
;;
;; Para ter uma ideia do processo de execução do jogo, execute o arquivo
;; main.rkt sem mudar nada neste arquivo. Uma janela irá aparecer. Pressione
;; algumas teclas e observe a saída no console do DrRacket. Veja o corpo
;; inicial das funções make-tetris-padrao, trata-tecla, trata-tick e desenha.

(require "base.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide make-tetris-padrao
         tetramino->lista-pos
         lop-validas?
         lop-livres?
         fixa
         fixa-linha
         contem
         get-cols
         limpa
         linha-completa?
         trata-tecla
         trata-tick
         desenha)

;; -> Tetris
;; Cria o jogo inicial.
;; Esta função é chamada no arquivo main.rkt.
(define (make-tetris-padrao)
  (make-tetris LARGURA-PADRAO ALTURA-PADRAO (stream-tetraminos) TIMEOUT-PADRAO))

;; Jogo String -> Jogo
;; Esta função é chamada quando uma tecla é pressionada.
;; Devolve um jogo com o tetraminó que está caindo movido de acordo com a tecla
;;   "right" - tenta mover para direita
;;   "left"  - tenta mover para esquerda
;;   "up"    - tenta rotacionar
;;   "down"  - tenta mover para baixo
;;
;; Se a tecla for "right", "left" ou "up" e o movimento não puder ser
;; realizado, o jogo é devolvido sem modificações.
;;
;; Se a tecla for "down" e o movimento não puder ser realizado, tetra é fixado
;; no campo, as linhas completas são removidas, o próximo tetraminó é
;; selecionada para cair e o contador de automovimento retorna ao valor
;; inicial.
;;
;; Se o movimento puder ser realizado, o jogo após o movimento é devolvido.
;;
;; Use a função key=? para comparar o tecla com os valores "right", "left, "up"
;; e "down".
(define (trata-tecla jogo tecla)
  (printf "\ntrata-tecla:~a\n" tecla)
  jogo)

;; Jogo -> Jogo
;; Função que trata um tick. Esta função é chamada 28 vezes por segundo, ela
;; deve mover o tetra para baixo depois que uma determinada quantidade
;; (TIMEOUT-PADRAO) de ticks. Se o jogador mover para baixo e fixar o
;; tetraminó, a contagem deve reiniciar.
(define (trata-tick jogo)
  (printf "t")
  jogo)

;; Tetris -> Imagem
;; Esta função é chamada quando o jogo precisa ser desenhado na tela. Devolve
;; uma imagem que representa o jogo.
;; Veja as funções pré-definidas rectangle, beside, above e overlay no pacote
;; 2htdp/image.
(define (desenha jogo)
  (printf "d")
  (rectangle (* (tetris-largura jogo) Q-LARGURA)
             (* (tetris-altura jogo) Q-ALTURA)
             "solid"
             "green"))

;; Tetramino -> Lista(Posn)
;; Devolve a lista de posições que t ocupa no campo considerando a rotação e a
;; posição (translação em relação a origem).
;; 
;; Por exemplo, seja TT1 definido como
;; (define TT1 (tetramino T_TIPOS 1 (posn 1 0) T_COR))
;; este tetraminó está na rotação 1 e na posição (posn 1 0). O elemento na
;; posição 1 de T_TIPOS é T1 que é a seguinte lista de listas (definida em
;; tetra-tipos.rkt)
;;    0 1 2     ; colunas
;;              ; linhas
;; '((0 1 0)    ; 0
;;   (0 1 1)    ; 1
;;   (0 1 0)))  ; 2
;;
;; As as posições ocupadas por T1 são marcadas com 1, ou seja, as posições
;; ocupadas por T1 são (posn 0 1) (posn 1 1) (posn 1 2) e (posn 2 1). Estas São
;; as posições em relação a (posn 0 0), mas o tetraminó está na posição
;; (posn 1 0), desta forma, precisamos fazer a translação das posições. Para
;; isto, somamos o ponto (posn 1 0) a cada ponto de T1, o que resulta em
;; (pos 1 1) (posn 2 1) (posn 2 2) (posn 3 1). Observe que é posível ter
;; um deslocamento em relação a origem negativa. Por exemplo, se a posição de
;; TT1 fosse (posn 0 -1), obteríamos como resposta da função a lista com
;; as posições (posn 0 0) (posn 1 0) (pos 1 1) (pos 2 0).
;;
;; Veja os testes para outros exemplos de como esta função deve funcionar.
(define (tetramino->lista-pos t)
  (define tetra-tipo (tetramino-tipo t))
  (define tetra-rot (list-ref tetra-tipo (tetramino-rot t)))
  (define tetra-pos (tetramino-pos t))
  (percorre-lin tetra-rot tetra-pos 0 0))

;; Tetramino-rot Tetramino-pos Int Int -> List(Posn)
;; Percorre as linhas da rotação do tetra criando as Posn em cada coluna
;; que achar 1, 
(define (percorre-lin tetra-rot tetra-pos linha coluna)
  (cond
    [(empty? tetra-rot) empty]
    [else
     (append (percorre-col (first tetra-rot) tetra-pos linha coluna)
             (percorre-lin (rest tetra-rot) tetra-pos (add1 linha) coluna))]))

;; Tetramino-rot Tetramino-pos Int Int -> Posn
;; Percorre as colunas da linha da rotação do tetra e cria um Posn caso ache 1
(define (percorre-col tetra-rot-linha tetra-pos linha coluna)
  (cond
    [(empty? tetra-rot-linha) empty]
    [else
     (if (= (first tetra-rot-linha) 1)
         (cons (posn (+ linha (posn-lin tetra-pos)) (+ coluna (posn-col tetra-pos)))
               (percorre-col (rest tetra-rot-linha) tetra-pos linha (add1 coluna)))
         (percorre-col (rest tetra-rot-linha) tetra-pos linha (add1 coluna)))]))

;; Lista(Posn) Natural Natural -> Boolean
;; Devolve verdadeiro se todas as posições de lp são válidas, isto é, estão
;; dentro de um campo de tamanho largura x altura. Devolve falso caso
;; contrário.
(define (lop-validas? lp largura altura)
  (cond
    [(empty? lp) #t]
    [else
     (and
      (and
       (and (< (posn-lin (first lp)) altura) (>= (posn-lin (first lp)) 0))
       (and (< (posn-col (first lp)) largura) (>= (posn-col (first lp)) 0)))
      (lop-validas? (rest lp) largura altura))]))

;; Lista(Posn) Campo -> Boolean
;; Devolve verdadeiro se todas as posição de lp estão livres no campo. Devolve
;; falso caso contrário.
;; Requer que todas as posições em lp sejam válidas.
(define (lop-livres? lp campo)
  (cond
    [(empty? lp) #t]
    [else
     (and
      (= (list-ref (list-ref campo (posn-lin (first lp))) (posn-col (first lp))) 0)
      (lop-livres? (rest lp) campo))]))


;; Jogo -> Jogo
;; Preenche as posições ocupadas pelo tetraminó (que está caindo) no campo do
;; jogo.
;; Requer que tetraminó não possa ser movido para baixo.

(define (fixa jogo) 
 (struct-copy tetris jogo [campo (fixa-tetramino (tetris-tetra jogo) (tetris-campo jogo))]))

(define (fixa-tetramino tetramino campo) 
  
  (define posicoes (tetramino->lista-pos tetramino))
  (define cor (tetramino-cor tetramino))
  
  (define (laço i lst)
  (cond [(empty? lst) empty]
        [else (cons (fixa-linha (get-cols posicoes i) cor (first lst) ) 
                    (laço (add1 i) (rest lst)))]))
  (laço 0 campo)
         
)

;; cols, cor e linha -> linha
;; recebe a colunas a serem marcadas na linha pela cor indicada     
(define (fixa-linha cols cor linha)
  
  (define (laço i lst)
  (cond [(empty? lst) empty]
        [(contem i cols)
         (cons cor (laço (add1 i) (rest lst)))]
        [else 
         (cons (first lst) (laço (add1 i) (rest lst)))]))
  (cond [(empty? cols) linha]
        [else (laço 0 linha)])
  )

;; elemento, lista -> boolean
;; verifica se o elemento esta na lista 
;; verdadeiro se achar, falso do contrario

(define (contem elem lista)
  (cond [(equal? (member elem lista) #f) #f]
        [else #t]))

;; Lista de posn e n-linha -> lista de col
;; Devolve uma lista de colunas que fazem par com a linha informada

(define (get-cols posicoes n)
  (cond [(empty? posicoes) empty]
        [(= (posn-lin (first posicoes)) n) 
         (cons (posn-col (first posicoes))
               (get-cols (rest posicoes) n))]
        [else (get-cols (rest posicoes) n)]))

;; Jogo -> Jogo
;; Devolve um jogo sem as linhas que estão completas, isto é, as linhas que não
;; tem nenhum quadrado vazio. O jogo devolvido tem o mesmo tamanho do jogo de
;; entrada.
(define (limpa jogo) 
  (struct-copy tetris jogo [campo (limpa-campo
                                 (tetris-campo jogo)
                                 (tetris-altura jogo)
                                 (tetris-largura jogo))]))
  
(define (limpa-campo campo altura largura)
  (define campo-parcial (filter-not linha-completa? campo))
  
  (define (completa-campo n) 
    (cond [(equal? n 0) campo-parcial]
          [else (cons (make-linha largura) 
                      (completa-campo (sub1 n)))]))
  
  (completa-campo (- altura (length campo-parcial))))

;; Linha -> boolean
;; Verifica se a linha está completa (elementos diferentes de 0).
;; Retorna True se esta completa, false caso contrario.

(define (linha-completa? linha) 
  (cond [(empty? linha) #t]
        [(equal? (first linha) 0) #f]
        [else (linha-completa? (rest linha))]))

;; -> Stream(Tetraminstream racketo)
;; Cria um stream randômico de tetraminós.
;; Esta função não precisa de testes.
;; Você tem que implementar esta função, o corpo incial deve ser descartado.
(define (stream-tetraminos)
  (stream-cons (list-ref TETRAMINOS (add1 (random 7))) (stream-tetraminos)))
