#lang racket

;; Once per month, we organize a Lottery. For every draw, Pascal uses his old
;; abacus containing 50 balls numbered from 1 to 50. After deeply shuffling
;; the balls, he picks randomly 3 winning balls which respectively correspond
;; to 75%,15% and 10% of the total of available prices.  That total correspond
;; to 50% of the total amount contained in the cash box for the draw.

;;  e.g : cash box is 200$, if we draw now , prices will be given that way :

;; 75$ for the 1st ball
;; 15$ for the 2nd ball
;; 10$ for the 3rd ball

;; For the next draw, everybody can get a ticket for 10$ with an associated
;; ball number. (so 50 tickets available)

;; Pascal wants to automate that process, the program should be a console
;; script with the following commands :

;; “buy” : I want to buy a ticket by providing a name. When buying a ticket,
;; the number of the ball should be displayed on the screen.
;; “draw” : I want to be able to trigger a draw.
;; “winners“ : I want to display the winners as follow :
;; 1rst ball : number X > Bob > 75$
;; 2nd ball : number Y > Lino > 15$
;; 3nd ball : number Z > Dominic > 10$

(require math/base
         racket/match)

;; the test approach is to use the module+ approach
;; http://docs.racket-lang.org/guide/Module_Syntax.html?q=submodule#%28part._main-and-test%29

(define account%
  ;; holds the cash balance and related operations. The relation of
  ;; this class with player% and lottery% is that both have cash
  ;; balances that can dimish or increase depending on their luck.
  (class object%
    (super-new)
    (init (init-cash-balance 0))
    (define cash-balance init-cash-balance)
    (define/public (deduct! amount)
      (if (>= (- cash-balance amount) 0)
          (begin (set! cash-balance (- cash-balance amount))
                 true)
          false))
    (define/public (deposit! amount)
      (set! cash-balance (+ cash-balance amount))
      true)
    (define/public (get-balance)
      cash-balance)))

(define player%
  ;; Holds numbers drawn and money. The assumption is that there will
  ;; be a bunch of these guys losing money :)
  (class account%
    (init init-name
           (init-cash-balance 50))
    (super-new (init-cash-balance init-cash-balance))
    (define numbers empty)
    (field (name init-name))
    (define/public (has-number? number)
      (member number numbers))
    (define/public (add-number! number)
      (set! numbers (cons number numbers)))
    (define/public (clear-numbers! number)
      (set! numbers empty))))

(define lottery%
  ;; Holds the playing and prizing logic for a given lotto game. The
  ;; actual payment is made by the game% class.
  (class account%
    (init (amount-of-numbers 50)
          (init-cost         10)
          (init-draw-amount  3)
          (init-cash-balance 200))
    (super-new (init-cash-balance init-cash-balance))
    (field (cost init-cost))

    (when (> init-draw-amount amount-of-numbers)
      (error "Draw is bigger than the amount of numbers to draw from."))

    (define selected-numbers empty)
    (define numbers          amount-of-numbers)
    (define draw-amount      init-draw-amount)

    (define/public (get-number)
      (define selected-number (random-natural numbers))
      (set! selected-numbers (cons selected-number selected-numbers))
      selected-number)
    (define/public (draw)
      (let loop ((i draw-amount)
                 (winners empty))
        (if (> i 0)
            (let ((current-draw (random-natural numbers)))
              (if (member current-draw winners)
                  (loop i winners)
                  (loop (sub1 i)
                        (cons current-draw winners))))
            winners)))
    (define/public (get-prize-list)
      (define half-balance (/ (send this get-balance) 2))
      (list (* half-balance 0.75)
            (* half-balance 0.15)
            (* half-balance 0.10)))))

(module+ test
  ;; tests for the account entities
  (require rackunit
           rackunit/text-ui)
  (define draw-amount 3)
  (define test-player (new player%
                           (init-name "chuck norris")))
  (define test-lottery (new lottery% (init-draw-amount draw-amount)))

  (check-equal? (send test-player get-balance) 50)
  (check-true (send test-player deduct! 50))
  (check-equal? (send test-player get-balance) 0)
  (check-false (send test-player deduct! 1))
  (check-true (send test-player deposit! 100))
  (check-equal? (send test-player get-balance) 100)

  (check-equal? (length (send test-lottery draw))
                draw-amount)
  ;; test to see if a failed lottery cannot be build
  (check-exn exn:fail? (λ () (new lottery% (amount-of-numbers 2)))))

;;; auxiliary functions

(define (get-non-selected-number selected-numbers numbers-possible)
  ;; auxialiry function to draw a number that wasn't dawn before.
  (define current-draw (random-natural numbers-possible))
  (if (member current-draw selected-numbers)
      (get-non-selected-number selected-numbers numbers-possible)
      current-draw))

(define (player-with-number number players)
  ;; finds who has the winning number
  (findf (λ (player) (send player has-number? number))
         players))

(define (compute-winners numbers players)
  ;; maps the winning numbers to their players. Used to compute the
  ;; winner list for showing and distributing the prizes.
  (map (λ (draw-number)
          (player-with-number draw-number players))
       numbers))

(define (make-winner-list prizes players)
  ;; builds up a list of pairs of winners/prizes. Used to show who won
  ;; what.
  (for/list ((player players)
             (prize prizes))
    (list player prize)))

(define (player-exists? player-name players)
  (findf (λ (player)
            (equal? (get-field name player)
                    player-name))
         players))

(module+ test
  ;; tests for auxiliary functions
  (define test-player2 (new player%
                            (init-name "bob")
                            (init-cash-balance 10)))
  (define loser-player (new player% (init-name "jack")))
  (send test-player add-number! 1)
  (send test-player2 add-number! 2)
  (send loser-player add-number! 10)
  (for ([x (in-range 10)])
    (check-equal? (get-non-selected-number '(1 2) 3) 0))
  (check-equal? (player-exists? "bob" (list test-player2 test-player loser-player))
                test-player2)
  (check-false (player-exists? "ghost" (list test-player2 test-player loser-player)))
  (check-equal? (player-with-number 2 (list test-player test-player2))
                test-player2)
  (check-equal? (compute-winners (list 3 2 11) (list test-player test-player2 loser-player))
                (list #f test-player2 #f))
  (check-equal? (compute-winners (list 1 2 11) (list test-player test-player2 loser-player))
                (list test-player test-player2 #f))
  (check-equal? (make-winner-list (list 75 15 10) (list #f test-player2 #f))
                `((#f 75) ,(list test-player2 15) (#f 10))))

(define user-interface%
  ;; this is the only point of direct contact with the user, providing
  ;; an abstraction over the IO, making it possible to swap
  ;; text/gui/web/...
  (class object%
    (super-new)
    (define/public (get-player-name)
      (displayln "Please input the player name.")
      (read-line))
    (define/public (show-winners current-winners)
      (for-each (match-lambda
                 [(list maybe-name prize)
                  (printf "~a --> ~a~n"
                          (if maybe-name
                              maybe-name
                              "nobody")
                          prize)])
                current-winners))
    (define/public (no-winners)
      (printf "No draw has been made so far.~n"))
    (define/public (no-balance player-name)
      (printf "Player ~s has no funds for buying.~n" player-name))
    (define/public (get-command valid-commands)
      ;; waits for the user to input some command, and validates them.
      (printf "Please input the valid commands: ~s.~n" valid-commands)
      (define command (read-line))
      (if (member command valid-commands)
          command
          (begin
            (displayln "unknown command")
            (send this get-command valid-commands))))))

(define game%
  ;; The manager/relayer of the game. This communicates between all
  ;; the pieces, making payments and informing and requesting info
  ;; from the user through the UI.
  (class object%
    (super-new)

    (define user-interface
      ;; singleton object to communicate with the user.
      (new user-interface%))

    (define players
      ;; holds the mutable collection of players.
      empty)

    (define lottery
      ;; singleton of the lottery that is being played
      (new lottery%))

    (define current-winners
      ;; list representing the current winners of this game.
      empty)

    (define/private (get-command)
      (send user-interface
            get-command
            '("buy" "draw" "winners")))

    (define/private (get-player)
      ;; responsible for handling player management
      (define player-name (send user-interface get-player-name))
      (define player (player-exists? player-name players))
      (unless player
        (set! player (new player% [init-name player-name]))
        (set! players (cons player players)))
      player)

    ;; game options
    (define/private (buy)
      (define player (get-player))
      (define cost (get-field cost lottery))
      (if (send player deduct! cost)
          (begin (send lottery deposit! cost)
                 (send player add-number! (send lottery get-number)))
          (send user-interface no-balance (get-field name player)))
      (new-command))

    (define/private (draw)
      (define draw-numbers (send lottery draw))
      (define prizes (send lottery get-prize-list))
      (set! current-winners
            (map (match-lambda
                  [(list maybe-player prize)
                   (list
                    (if maybe-player
                        ;; the house cannot be broken, as it only pays
                        ;; a % of its current assets. So no need to
                        ;; check the balance.
                        (begin (send maybe-player deduct! prize)
                               (send maybe-player deposit! prize)
                               (get-field name maybe-player))
                        #f)
                    prize)])
                 (make-winner-list prizes
                                   (compute-winners draw-numbers players))))
      (new-command))

    (define/private (show-winners)
      (if (empty? current-winners)
          (send user-interface no-winners)
          (send user-interface show-winners current-winners))
      (new-command))

    (define/private (new-command)
      (case (get-command)
        (("buy") (buy))
        (("draw") (draw))
        (("winners") (show-winners))
        (else (error "bang"))))

    (define/public (start-game)
      ;; interface for starting a game.
      (new-command))))

(define game (new game%))
;; (send game start-game)
