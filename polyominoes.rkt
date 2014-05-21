;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname polyominoes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "a10.rkt")

;;
;; ************************************************
;;
;; CS135 Assignment 10, Question 1
;; Fall 2013
;; Weijie Wang  20505037
;;
;; ************************************************
;;

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
(require "kanoodle.rkt")

;; A Grid is a (ne-listof (ne-listof Character))

(define-struct pos (x y))
;; A Pos is a (make-pos Int Int)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

;; A temporary neighbours function that always fails.  
;; Provide only the contract, purpose and function definition.
;;(define (neighbours s)
;;  empty)
;;Q5
;;neighbours: State -> (listof State)
(define (neighbours state0)
  (local 
    [;;build-lstate: Grid (listof Grid)->(listof State)
     ;;Purpose: Consume a Grid statex and a (listof Grid) lpiece,
     ;; produce a (listof State) based on valid process of those
     ;; two things.
     (define (build-lstate statex lpiece)
       (local 
         [;;build-state1: Grid Grid->(listof State)
          ;;Purpose: Consume two Grids state1 and piece, produce
          ;; a (listof State) based on valid process of those two
          ;; things.
          (define (build-state1 state1 piece)
            (local [(define thislstate (build-state state1 piece))
                    ;;thisbuild: Grid Grid->(listof State)
                    ;;Purpose: Consume two Grids state1 and piece, produce
                    ;; a (listof State) based on valid process of those two
                    ;; things.
                    (define (thisbuild thisls thispiece)
                      (local [;;Constant define
                              (define thislpiece (delete thispiece
                                                         (state-pieces state0)))]
                        (cond [(empty? thisls) empty]
                              [else 
                               (cons (make-state (first thisls)
                                                 thislpiece)
                                     (thisbuild (rest thisls) thispiece))])))]
              (thisbuild thislstate piece)))
          ;;delete: Grid (listof Grid)->(listof Grid)
          ;;Purpose: Consume a Grid piece and a (listof Grid) listpiece0,
          ;; produce a (listof Grid) in which piece has been deleted.
          (define (delete piece listpiece0)
            (cond 
              [(equal? (first listpiece0) piece)
               (rest listpiece0)]
              [else (cons (first listpiece0) 
                          (delete piece (rest listpiece0)))]))]
         (cond [(empty? lpiece) empty]
               [else (append (build-state1 statex (first lpiece))
                             (build-lstate statex (rest lpiece)))])))
     ;;build-state: Grid Grid->(listof Grid)
     ;;Purpose: Consume two Grids base and top and produce a
     ;; (listof Grid) in which all valid orientation has been
     ;; moved and all invalid case has been deleted.
     (define (build-state base top)
       (local [;;Constant define:
               (define lori1
                 (all-orientations top))
               ;;exhaustion: (listof Grid) Pos->(listof Grid)
               ;;Purpose: Consume a (listof Grid) lori0 and a Pos
               ;; thispos and produce a (listof Grid) in which all
               ;; valid orientation has been moved and all invalid
               ;; case has been deleted.
               (define (exhaustion lori0 thispos)
                 (cond 
                   [(empty? lori0) empty]
                   [else
                    (local [(define actual-pos
                              (make-pos (- (pos-x thispos)
                                           (first-non-empty (first (first lori0)) 0))
                                        (pos-y thispos)))]
                      (cond [(superimpose? base (first lori0) actual-pos)
                             (cons (superimpose base (first lori0)
                                                actual-pos)
                                   (exhaustion (rest lori0) thispos))]
                            [else (exhaustion (rest lori0) thispos)]))]))]
         (exhaustion lori1 (first-empty-pos base))))
     ;;Q1 (a)
     ;;build-2dlist: Num Num (Pos->X) -> (listof (listof X))
     ;;Purpose: consumes two natural numbers width and height
     ;; , representing the width and height of a grid, and a
     ;; function that is applied to all positions corresponding
     ;; to positions in the grid, in that order, and produce a
     ;; grid with that height and width.
     (define (build-2dlist x y fn)
       (map (lambda (n) (build-list x (lambda (x0)
                                        (fn x0 n))))
            (build-list y (lambda (y) y))))
     ;;first-non-empty: (listof Char) Nat->Nat
     ;;Purpose: Consume a (listof Char) ltop and an accumulator
     ;; and produce a Nat to represent the position of first not
     ;; #\. char.
     (define (first-non-empty ltop n)
       (cond [(not (char=? (first ltop) #\.)) n]
             [else (first-non-empty 
                    (rest ltop) (+ n 1))]))
     ;;Q3
     ;;first-empty-pos: Grid -> (union Pos false)
     ;;Purpose: Comsume a Grid grid and prduce the leftest of
     ;; the topest #\. of that grid. If there do not exist a
     ;; #\., produce false.
     (define (first-empty-pos grid)
       (local [;;find-first-empty: Grid Pos-> (union Pos false)
               ;;Purpose: Comsume a Grid grid and an Pos
               ;; accumulator pprduce the leftest of the topest
               ;; #\. of that grid. If there do not exist a
               ;; #\., produce false.
               (define (find-first-empty grid p)
                 (cond [(empty? grid) #f]
                       [(empty? (first grid))
                        (find-first-empty 
                         (rest grid)
                         (make-pos 0 (+ (pos-y p) 1)))]
                       [(char=? (first (first grid)) #\.) p]
                       [else (find-first-empty 
                              (cons (rest (first grid))
                                    (rest grid))
                              (make-pos (+ (pos-x p) 1) 
                                        (pos-y p)))]))]
         (find-first-empty grid (make-pos 0 0))))
     ;;superimpose?: Grid Grid Pos -> Boolean
     ;;Purpose: Consume two Grids, cgrid and polyomino,
     ;; and a Pos, position, and produce Whether or not
     ;; this is a valid superimpose
     (define (superimpose? cgrid polyomino pos)
       (local [;;overwrite?: Grid Grid Pos -> Boolean
               ;;Purpose: Consume two Grids, cgrid0 and poly,
               ;; and a Pos, position, and produce Whether or not
               ;; this is a valid superimpose.
               (define (overwrite? cgrid0 poly p0 y)
                 (cond [(< y (pos-y p0))
                        (overwrite? (rest cgrid0) poly
                                    p0 (+ y 1))]
                       [else (overwrite2? 
                              cgrid0 poly (pos-x p0))]))
               ;;overwrite2?: Grid Grid Nat -> Boolean
               ;;Purpose: Consume two Grids, cgrid1 and poly1,
               ;; and an x-coordinate,x , and produce whether
               ;; or not this is a valid superimpose.
               (define (overwrite2? cgrid1 poly1 x)
                 (cond [(empty? poly1) #t]
                       [(empty? cgrid1) #f]
                       [else 
                        (and (over-write-line?
                              (first cgrid1)
                              (first poly1) x 0)
                             (overwrite2?
                              (rest cgrid1)
                              (rest poly1) x))]))
               ;;over-write-line?: (listof Char)
               ;; (listof Char) Nat Nat->Boolean
               ;;Purpose: Consume two (listof Char)s, lcgrid
               ;; and lpoly, an x-coordinate,x , and an
               ;; accumulator and produce whether or not
               ;; this is a valid superimpose.
               (define (over-write-line? lcgrid lpoly x n)
                 (cond [(empty? lpoly) #t]
                       [(empty? lcgrid) #f]
                       [(< n x)
                        (over-write-line? (rest lcgrid) lpoly x (+ n 1))]
                       [(and (not (char=? (first lpoly) #\.))
                             (not (char=? (first lcgrid) #\.))) #f]
                       [else (over-write-line? (rest lcgrid) (rest lpoly) x n)]))]
         (overwrite? cgrid polyomino pos 0)))
     ;;Q2
     ;;all-orientations: Grid -> (listof Grid)
     ;;Purpose: consumes a Grid grid representing a single polyomino,
     ;; and produces a list of Grids containing all distinct
     ;; rotations and reflections of that polyomino and delete
     ;; duplicated items.
     (define (all-orientations grid)
       (local [;;allori: Grid->(listof Grid)
               ;;Purpose: Consume a Grid g and produce a
               ;; (listof Grid) with all 8 orientations of
               ;; g. 
               (define (allori grid0)
                 (local [;;constant define:
                         (define ccw (rotate-ccw grid0))
                         (define ccwccw
                           (rotate-ccw ccw))
                         (define ccwccwccw
                           (rotate-ccw ccwccw))
                         (define rg (reflect grid0))
                         (define rccw (reflect ccw))
                         (define rccwccw 
                           (reflect ccwccw))
                         (define rccwccwccw 
                           (reflect ccwccwccw))]
                   (list ccwccw grid0 ccw rccwccwccw ccwccw rccwccw
                         rg ccwccwccw rccw)))
               ;;count-size: Grid->(listof Nat)
               ;;Purpose: Consume a Grid grid0 and Produce the
               ;; length and height of grid0 in a list.
               (define (count-size grid0)
                 (cons (foldr (lambda (x y) (+ 1 y))
                              0 (first grid0)) 
                       (cons (foldr (lambda (x y)
                                      (+ 1 y)) 0 grid0) empty)))
               ;;get-value: (Nat Nat->(Grid->Char))
               ;;Purpose: Consume a Grid grid0 and Produce a function which
               ;; can consume two Nats and Produce the Char at that
               ;; position.
               (define (get-value grid0)
                 (lambda (y x) (cond [(and (= x 0)
                                           (= y 0))
                                      (first (first grid0))]
                                     [(= y 0)
                                      ((get-value (cons (rest (first grid0))
                                                        (rest grid0))) y (- x 1))]
                                     [else ((get-value (rest grid0)) (- y 1) x)])))
               ;;rotate-ccw: Grid->Grid
               ;;Purpose: Consume a Grid grid0 and produce a
               ;; Grid that grid0 has been rotated clockwise.
               (define (rotate-ccw grid0)
                 (reflect (build-2dlist (second (count-size grid0)) 
                                        (first (count-size grid0))
                                        (get-value grid0))))
               ;;reflect: Grid->Grid
               ;;Purpose: Consume a Grid grid0 and produce a
               ;; Grid that grid0 has been reflect upside down.
               (define (reflect grid0)
                 (reverse grid0))
               ;;grid-equal?: Grid Grid->Boolean
               ;;Purpose: Consume two Grids grid1 and grid2 and
               ;; produce whether or not hese two Grids are
               ;; equivalent.
               (define (grid-equal? grid1 grid2)
                 (local [;;nest-equiv?: Grid Grid->Boolean
                         ;;Purpose: Consume two Grids grid1 and grid2 and
                         ;; produce whether or not hese two Grids are
                         ;; equivalent.
                         (define (nest-equiv? grid1 grid2)
                           (cond
                             [(and (empty? grid1)
                                   (empty? grid2)) #t]
                             [(or (empty? grid1)
                                  (empty? grid2)) #f]
                             [else 
                              (and (ele-equiv? (first grid1) 
                                               (first grid2))
                                   (nest-equiv? (rest grid1)
                                                (rest grid2)))]))
                         ;;ele-equiv?: (listof X) (listof X)->Boolean
                         ;;Purpose: Consume two (listof X) l1 and l2
                         ;; and determine whether or not they are
                         ;; equivalent.
                         (define (ele-equiv? l1 l2)
                           (cond 
                             [(and (empty? l1)
                                   (empty? l2)) #t]
                             [(or (empty? l1)
                                  (empty? l2)) #f]
                             [(char=? (first l1)
                                      (first l2))
                              (ele-equiv? (rest l1)
                                          (rest l2))]
                             [else #f]))]
                   (nest-equiv? grid1 grid2)))
               ;;delete-dup: (listof Grid)->(listof Grid)
               ;;Purpose: Consume a (listof Grid) lgrid and produce
               ;; a new (listof Grid) with all the duplicated items
               ;; removed.
               (define (delete-dup lgrid)
                 (foldr (lambda (x y) (cond [(empty? y) (cons x empty)]
                                            [(ormap (lambda (n) 
                                                      (grid-equal? x n)) y) y]
                                            [else (cons x y)])) empty lgrid))]
         (delete-dup (allori grid))))
     ;;Q4
     ;;superimpose: Grid Grid Pos -> Grid
     ;;Purpose: Consume two Grids, base and top, and a Pos,
     ;; position, and produce a Grid that the top is already
     ;; laid over the base at that pos. Any #\n. characters in
     ;; top do not overwrite the contents of base.
     (define (superimpose base top p)
       (local [;;overwrite: Grid Grid Pos Nat->Grid
               ;;Purpose: Consume two Grids, base0 and top0, a Pos,
               ;; p0, and an accumulator y0, and produce a Grid that
               ;; the top0 is already laid over the base0 at that pos.
               (define (overwrite base0 top0 p0 y0)
                 (cond [(empty? base0) empty]
                       [(< y0 (pos-y p0))
                        (cons (first base0)
                              (overwrite 
                               (rest base0) 
                               top0 p0 
                               (+ y0 1)))]
                       [else (overwrite2 
                              base0 top0 (pos-x p0))]))
               ;;overwrite2: Grid Grid Nat->Grid
               ;;Purpose: Consume two Grids, base0 and top0, a
               ;; x-coordinate x, and produce a Grid that the
               ;; top0 is already laid over the base0 at that pos.
               (define (overwrite2 base0 top0 x)
                 (cond [(empty? base0) empty]
                       [(empty? top0) base0]
                       [else 
                        (cons (over-write-line
                               (first base0)
                               (first top0) x 0)
                              (overwrite2 
                               (rest base0)
                               (rest top0) x))]))
               ;;over-write-line: (listof Char) (listof Char)
               ;; Nat Nat->Grid
               ;;Purpose: Consume two (listof Char)s, lbase
               ;; and ltop, a x-coordinate x, and an accumulator
               ;; n and produce a Grid that the ltop is already
               ;; laid over the lbase at that pos.
               (define (over-write-line lbase ltop x n)
                 (cond [(empty? lbase) empty]
                       [(empty? ltop) lbase]
                       [(< n x)
                        (cons (first lbase)
                              (over-write-line
                               (rest lbase) ltop x (+ n 1)))]
                       [(char=? (first ltop) #\.)
                        (cons (first lbase)
                              (over-write-line (rest lbase)
                                               (rest ltop) x n))]
                       [else (cons (first ltop)
                                   (over-write-line 
                                    (rest lbase)
                                    (rest ltop) x n))]))]
         (overwrite base top p 0)))]
    (build-lstate (state-puzzle state0)
                  (state-pieces state0))))
;; solve-puzzle: Grid (listof Grid) Symbol -> (union (listof String) false)
;; Solve a polyomino puzzle, given the initial empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.
;;
;; You don't need to modify this function at all.  It is provided for you
;; so that you can test your puzzle solving algorithm interactively.  If
;; you decide you want to write check-expect tests using solve-puzzle
;; (which you don't have to do, but can if you want), be sure to pass in
;; 'offline for viz-style.

;; Some Examples are included below after the solve-puzzle function definition.

;; DO NOT MODIFY THIS CODE
(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)

;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)

;; Display every step of the search as it progresses.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'interactive)



;;Q1 (a)
;;build-2dlist: Num Num (Pos->X) -> (listof (listof X))
;;Purpose: consumes two natural numbers width and height
;; , representing the width and height of a grid, and a
;; function that is applied to all positions corresponding
;; to positions in the grid, in that order, and produce a
;; grid with that height and width.
;;Example:
(check-expect (build-2dlist 0 0 (lambda (a b) (+ a b))) empty)
(check-expect (build-2dlist 5 7 (lambda (a b) (+ a b))) 
              (list
               (list 0 1 2 3 4)
               (list 1 2 3 4 5)
               (list 2 3 4 5 6)
               (list 3 4 5 6 7)
               (list 4 5 6 7 8)
               (list 5 6 7 8 9)
               (list 6 7 8 9 10)))

(define (build-2dlist width height function)
  (map (lambda (this) (build-list width (lambda (this0)
                                          (function this0 this))))
       (build-list height (lambda (height) height))))

;;Test:
(check-expect (build-2dlist 0 0 (lambda (a b) (- a b))) empty)
(check-expect (build-2dlist 1 0 (lambda (a b) (- a b))) empty)
(check-expect (build-2dlist 0 1 (lambda (a b) (- a b))) '(()))
(check-expect (build-2dlist 1 1 (lambda (a b) (- a b))) (list (list 0)))
(check-expect (build-2dlist 3 2 (lambda (x y) (list x y))) 
              (list
               (list (list 0 0) (list 1 0) (list 2 0))
               (list
                (list 0 1)
                (list 1 1)
                (list 2 1))))
(check-expect (build-2dlist 4 2 (lambda (a b) (* a b))) 
              (list (list 0 0 0 0) (list 0 1 2 3)))

;;Q1 (b)
;;all-positions: Nat Nat
;;Purpose: consumes two positive natural numbers, w
;; and h, and produces a (listof Pos) containing all possible
;; positions in a grid with width w and height h.
;;Example:
(check-expect (lists-equiv? (all-positions 0 0) empty) #t)
(check-expect (lists-equiv? (all-positions 2 3) 
                            (list
                             (make-pos 0 0)
                             (make-pos 1 0)
                             (make-pos 0 1)
                             (make-pos 1 1)
                             (make-pos 0 2)
                             (make-pos 1 2))) #t)

(define (all-positions w h)
  (foldr append '()(build-2dlist w h(lambda (x y)(make-pos x y)))))

;;Test:
(check-expect (lists-equiv? (all-positions 0 0) empty) #t)
(check-expect (lists-equiv? (all-positions 1 0) empty) #t)
(check-expect (lists-equiv? (all-positions 0 1) empty) #t)
(check-expect (lists-equiv? (all-positions 1 1) 
                            (list (make-pos 0 0))) #t)
(check-expect (lists-equiv? (all-positions 2 1)
                            (list (make-pos 0 0) (make-pos 1 0)))#t)
(check-expect (lists-equiv? (all-positions 1 2)
                            (list (make-pos 0 0) (make-pos 0 1))) #t)
(check-expect (lists-equiv? (all-positions 2 2) 
                            (list
                             (make-pos 0 0)
                             (make-pos 1 0)
                             (make-pos 0 1)
                             (make-pos 1 1))) #t)
(check-expect (lists-equiv? (all-positions 6 4) 
                            (list
                             (make-pos 0 0)
                             (make-pos 1 0)
                             (make-pos 2 0)
                             (make-pos 3 0)
                             (make-pos 4 0)
                             (make-pos 5 0)
                             (make-pos 0 1)
                             (make-pos 1 1)
                             (make-pos 2 1)
                             (make-pos 3 1)
                             (make-pos 4 1)
                             (make-pos 5 1)
                             (make-pos 0 2)
                             (make-pos 1 2)
                             (make-pos 2 2)
                             (make-pos 3 2)
                             (make-pos 4 2)
                             (make-pos 5 2)
                             (make-pos 0 3)
                             (make-pos 1 3)
                             (make-pos 2 3)
                             (make-pos 3 3)
                             (make-pos 4 3)
                             (make-pos 5 3))) #t)

;;Q2
;;all-orientations: Grid -> (listof Grid)
;;Purpose: consumes a Grid representing a single polyomino,
;; and produces a list of Grids containing all distinct
;; rotations and reflections of that polyomino and delete
;; duplicated items.
;;Example:
(check-expect (lists-equiv? (all-orientations 
                             (list (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\A #\.))) 
                            (list
                             (list
                              (list #\A #\A)
                              (list #\A #\A)
                              (list #\A #\.))
                             (list
                              (list #\A #\A #\.)
                              (list #\A #\A #\A))
                             (list
                              (list #\. #\A)
                              (list #\A #\A)
                              (list #\A #\A))
                             (list
                              (list #\A #\A #\A)
                              (list #\. #\A #\A))
                             (list
                              (list #\A #\.)
                              (list #\A #\A)
                              (list #\A #\A))
                             (list
                              (list #\A #\A #\A)
                              (list #\A #\A #\.))
                             (list
                              (list #\A #\A)
                              (list #\A #\A)
                              (list #\. #\A))
                             (list
                              (list #\. #\A #\A)
                              (list #\A #\A #\A)))) #t)
(check-expect (lists-equiv? (all-orientations (list (list #\B))) 
                            (list (list (list #\B)))) #t)

(define (all-orientations grid)
  (local [;;allori: Grid->(listof Grid)
          ;;Purpose: Consume a Grid g and produce a
          ;; (listof Grid) with all 8 orientations of
          ;; g. 
          (define (allori grid0)
            (local [;;constant define:
                    (define ccw (rotate-ccw grid0))
                    (define ccwccw
                      (rotate-ccw ccw))
                    (define ccwccwccw
                      (rotate-ccw ccwccw))
                    (define rg (reflect grid0))
                    (define rccw (reflect ccw))
                    (define rccwccw 
                      (reflect ccwccw))
                    (define rccwccwccw 
                      (reflect ccwccwccw))]
              (list ccwccw grid0 ccw rccwccwccw ccwccw rccwccw
                    rg ccwccwccw rccw)))
          ;;count-size: Grid->(listof Nat)
          ;;Purpose: Consume a Grid grid0 and Produce the
          ;; length and height of grid0 in a list.
          (define (count-size grid0)
            (cons (foldr (lambda (x y) (+ 1 y))
                         0 (first grid0)) 
                  (cons (foldr (lambda (x y)
                                 (+ 1 y)) 0 grid0) empty)))
          ;;get-value: (Nat Nat->(Grid->Char))
          ;;Purpose: Consume a Grid grid0 and Produce a function which
          ;; can consume two Nats and Produce the Char at that
          ;; position.
          (define (get-value grid0)
            (lambda (y x) (cond [(and (= x 0)
                                      (= y 0))
                                 (first (first grid0))]
                                [(= y 0)
                                 ((get-value (cons (rest (first grid0))
                                                   (rest grid0))) y (- x 1))]
                                [else ((get-value (rest grid0)) (- y 1) x)])))
          ;;rotate-ccw: Grid->Grid
          ;;Purpose: Consume a Grid grid0 and produce a
          ;; Grid that grid0 has been rotated clockwise.
          (define (rotate-ccw grid0)
            (reflect (build-2dlist (second (count-size grid0)) 
                                   (first (count-size grid0))
                                   (get-value grid0))))
          ;;reflect: Grid->Grid
          ;;Purpose: Consume a Grid grid0 and produce a
          ;; Grid that grid0 has been reflect upside down.
          (define (reflect grid0)
            (reverse grid0))
          ;;grid-equal?: Grid Grid->Boolean
          ;;Purpose: Consume two Grids grid1 and grid2 and
          ;; produce whether or not hese two Grids are
          ;; equivalent.
          (define (grid-equal? grid1 grid2)
            (local [;;nest-equiv?: Grid Grid->Boolean
                    ;;Purpose: Consume two Grids grid1 and grid2 and
                    ;; produce whether or not hese two Grids are
                    ;; equivalent.
                    (define (nest-equiv? grid1 grid2)
                      (cond
                        [(and (empty? grid1)
                              (empty? grid2)) #t]
                        [(or (empty? grid1)
                             (empty? grid2)) #f]
                        [else 
                         (and (ele-equiv? (first grid1) 
                                          (first grid2))
                              (nest-equiv? (rest grid1)
                                           (rest grid2)))]))
                    ;;ele-equiv?: (listof X) (listof X)->Boolean
                    ;;Purpose: Consume two (listof X) l1 and l2
                    ;; and determine whether or not they are
                    ;; equivalent.
                    (define (ele-equiv? l1 l2)
                      (cond 
                        [(and (empty? l1)
                              (empty? l2)) #t]
                        [(or (empty? l1)
                             (empty? l2)) #f]
                        [(char=? (first l1)
                                 (first l2))
                         (ele-equiv? (rest l1)
                                     (rest l2))]
                        [else #f]))]
              (nest-equiv? grid1 grid2)))
          ;;delete-dup: (listof Grid)->(listof Grid)
          ;;Purpose: Consume a (listof Grid) lgrid and produce
          ;; a new (listof Grid) with all the duplicated items
          ;; removed.
          (define (delete-dup lgrid)
            (foldr (lambda (x y) (cond [(empty? y) (cons x empty)]
                                       [(ormap (lambda (n) 
                                                 (grid-equal? x n)) y) y]
                                       [else (cons x y)])) empty lgrid))]
    (delete-dup (allori grid))))

;;Test:
(check-expect (lists-equiv? (all-orientations (list (list #\Q))) 
                            (list (list (list #\Q)))) #t)
(check-expect (lists-equiv? (all-orientations (list (list #\C)
                                                    (list #\C))) 
                            (list
                             (list (list #\C) (list #\C))
                             (list (list #\C #\C)))) #t)
(check-expect (lists-equiv? (all-orientations (list (list #\W)
                                                    (list #\W))) 
                            (list
                             (list (list #\W) (list #\W))
                             (list (list #\W #\W)))) #t)
(check-expect (lists-equiv? (all-orientations (list (list #\W #\W #\W)
                                                    (list #\. #\W #\.)
                                                    (list #\. #\W #\.))) 
                            (list (list
                                   (list #\. #\W #\.)
                                   (list #\. #\W #\.)
                                   (list #\W #\W #\W))
                                  (list
                                   (list #\W #\. #\.)
                                   (list #\W #\W #\W)
                                   (list #\W #\. #\.))
                                  (list
                                   (list #\W #\W #\W)
                                   (list #\. #\W #\.)
                                   (list #\. #\W #\.))
                                  (list
                                   (list #\. #\. #\W)
                                   (list #\W #\W #\W)
                                   (list #\. #\. #\W)))) #t)
(check-expect (lists-equiv? (all-orientations (list (list #\. #\W #\.)
                                                    (list #\W #\W #\W)
                                                    (list #\. #\W #\.))) 
                            (list
                             (list
                              (list #\. #\W #\.)
                              (list #\W #\W #\W)
                              (list #\. #\W #\.)))) #t)
(check-expect (lists-equiv? (all-orientations (list (list #\W #\W #\.)
                                                    (list #\. #\W #\.)
                                                    (list #\. #\W #\W))) 
                            (list
                             (list
                              (list #\W #\W #\.)
                              (list #\. #\W #\.)
                              (list #\. #\W #\W))
                             (list
                              (list #\. #\. #\W)
                              (list #\W #\W #\W)
                              (list #\W #\. #\.))
                             (list
                              (list #\. #\W #\W)
                              (list #\. #\W #\.)
                              (list #\W #\W #\.))
                             (list
                              (list #\W #\. #\.)
                              (list #\W #\W #\W)
                              (list #\. #\. #\W)))) #t)
(check-expect (lists-equiv? (all-orientations 
                             (list (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\A #\.))) 
                            (list (list
                                   (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\A #\.))
                                  (list
                                   (list #\A #\A #\A #\.)
                                   (list #\A #\A #\A #\A))
                                  (list
                                   (list #\. #\A)
                                   (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\A #\A))
                                  (list
                                   (list #\A #\A #\A #\A)
                                   (list #\. #\A #\A #\A))
                                  (list
                                   (list #\A #\.)
                                   (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\A #\A))
                                  (list
                                   (list #\A #\A #\A #\A)
                                   (list #\A #\A #\A #\.))
                                  (list
                                   (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\A #\A)
                                   (list #\. #\A))
                                  (list
                                   (list #\. #\A #\A #\A)
                                   (list #\A #\A #\A #\A)))) #t)

;;Q3
;;first-empty-pos: Grid -> (union Pos false)
;;Purpose: Comsume a Grid grid and prduce the leftest of
;; the topest #\. of that grid. If there do not exist a
;; #\., produce false.
;;Example:
(check-expect (first-empty-pos empty) #f)
(check-expect (first-empty-pos (list (list #\Q #\.)))
              (make-pos 1 0))

(define (first-empty-pos grid)
  (local [;;find-first-empty: Grid Pos-> (union Pos false)
          ;;Purpose: Comsume a Grid grid and an Pos
          ;; accumulator pprduce the leftest of the topest
          ;; #\. of that grid. If there do not exist a
          ;; #\., produce false.
          (define (find-first-empty grid p)
            (cond [(empty? grid) #f]
                  [(empty? (first grid))
                   (find-first-empty 
                    (rest grid)
                    (make-pos 0 (+ (pos-y p) 1)))]
                  [(char=? (first (first grid)) #\.) p]
                  [else (find-first-empty 
                         (cons (rest (first grid))
                               (rest grid))
                         (make-pos (+ (pos-x p) 1) 
                                   (pos-y p)))]))]
    (find-first-empty grid (make-pos 0 0))))

;;Example:
(check-expect (first-empty-pos empty) #f)
(check-expect (first-empty-pos (list (list #\R #\R))) #f)
(check-expect (first-empty-pos (list (list #\R))) #f)
(check-expect (first-empty-pos (list (list #\R)
                                     (list #\R))) #f)
(check-expect (first-empty-pos (list (list #\R #\R)
                                     (list #\R #\R))) #f)
(check-expect (first-empty-pos (list (list #\. #\D))) 
              (make-pos 0 0))
(check-expect (first-empty-pos (list (list #\. #\.))) 
              (make-pos 0 0))
(check-expect (first-empty-pos (list (list #\R #\.)
                                     (list #\. #\R)))
              (make-pos 1 0))
(check-expect (first-empty-pos (list (list #\R #\R)
                                     (list #\. #\.)))
              (make-pos 0 1))
(check-expect (first-empty-pos (list (list #\R #\.)
                                     (list #\R #\.)))
              (make-pos 1 0))
(check-expect (first-empty-pos (list (list #\R #\. #\R #\R)
                                     (list #\. #\. #\R #\.)
                                     (list #\R #\R #\. #\R)
                                     (list #\R #\. #\R #\.)))
              (make-pos 1 0))

;;Q4
;;superimpose: Grid Grid Pos -> Grid
;;Purpose: Consume two Grids, base and top, and a Pos,
;; position, and produce a Grid that the top is already
;; laid over the base at that pos. Any #\n. characters in
;; top do not overwrite the contents of base.
;;Example:
(check-expect (superimpose empty empty (make-pos 0 0)) empty)
(check-expect (superimpose '((#\. #\.)) '((#\A #\.)) 
                           (make-pos 0 0)) '((#\A #\.)))

(define (superimpose base top p)
       (local [;;overwrite: Grid Grid Pos Nat->Grid
               ;;Purpose: Consume two Grids, base0 and top0, a Pos,
               ;; p0, and an accumulator y0, and produce a Grid that
               ;; the top0 is already laid over the base0 at that pos.
               (define (overwrite base0 top0 p0 y0)
                 (cond [(empty? base0) empty]
                       [(< y0 (pos-y p0))
                        (cons (first base0)
                              (overwrite 
                               (rest base0) 
                               top0 p0 
                               (+ y0 1)))]
                       [else (overwrite2 
                              base0 top0 (pos-x p0))]))
               ;;overwrite2: Grid Grid Nat->Grid
               ;;Purpose: Consume two Grids, base0 and top0, a
               ;; x-coordinate x, and produce a Grid that the
               ;; top0 is already laid over the base0 at that pos.
               (define (overwrite2 base0 top0 x)
                 (cond [(empty? base0) empty]
                       [(empty? top0) base0]
                       [else 
                        (cons (over-write-line
                               (first base0)
                               (first top0) x 0)
                              (overwrite2 
                               (rest base0)
                               (rest top0) x))]))
               ;;over-write-line: (listof Char) (listof Char)
               ;; Nat Nat->Grid
               ;;Purpose: Consume two (listof Char)s, lbase
               ;; and ltop, a x-coordinate x, and an accumulator
               ;; n and produce a Grid that the ltop is already
               ;; laid over the lbase at that pos.
               (define (over-write-line lbase ltop x n)
                 (cond [(empty? lbase) empty]
                       [(empty? ltop) lbase]
                       [(< n x)
                        (cons (first lbase)
                              (over-write-line
                               (rest lbase) ltop x (+ n 1)))]
                       [(char=? (first ltop) #\.)
                        (cons (first lbase)
                              (over-write-line (rest lbase)
                                               (rest ltop) x n))]
                       [else (cons (first ltop)
                                   (over-write-line 
                                    (rest lbase)
                                    (rest ltop) x n))]))]
         (overwrite base top p 0)))

(check-expect (superimpose empty empty (make-pos 0 0)) empty)
(check-expect (superimpose '((#\. #\A)) empty 
                           (make-pos 0 0)) '((#\. #\A)))
(check-expect (superimpose '((#\. #\A)) '((#\A #\.)) 
                           (make-pos 0 0)) '((#\A #\A)))
(check-expect (superimpose '((#\. #\A)) '((#\A)) 
                           (make-pos 0 0)) '((#\A #\A)))
(check-expect (superimpose '((#\.)) '((#\A)) 
                           (make-pos 0 0)) '((#\A)))
(check-expect (superimpose '((#\A)) '((#\B)) 
                           (make-pos 0 0)) '((#\B)))
(check-expect (superimpose '((#\.)) '((#\.)) 
                           (make-pos 0 0)) '((#\.)))
(check-expect (superimpose '((#\A)) '((#\.)) 
                           (make-pos 0 0)) '((#\A)))
(check-expect (superimpose '((#\.)) '((#\A)) 
                           (make-pos 2 0)) '((#\.)))

(check-expect (superimpose (list (list #\A #\. #\. #\. #\.)
                                 (list #\A #\A #\A #\. #\.)
                                 (list #\. #\A #\. #\. #\.))
                           (list (list #\B #\B)
                                 (list #\B #\B)
                                 (list #\B #\.)) (make-pos 4 1))
              '((#\A #\. #\. #\. #\.) (#\A #\A #\A #\. #\B)
                                      (#\. #\A #\. #\. #\B)))

;;(solve-puzzle kanoodle-6/2-g kanoodle-6/2-p 'interactive)

(solve-puzzle pent-grid-4 pent-pieces-4 'interactive)

;;(time (solve-puzzle kanoodle-6/1-g kanoodle-6/1-p 'interactive))

;;(neighbours (make-state (list (list #\a #\. #\. #\.)
;;                              (list #\a #\. #\. #\.)
;;                              (list #\a #\a #\. #\.)
;;                              (list #\. #\. #\. #\.))
;;                        (list (list (list #\a #\.)
;;                                    (list #\a #\a))
;;                              (list (list #\a #\a)
;;                                    (list #\a #\a)
;;                                    (list #\a #\.)))))