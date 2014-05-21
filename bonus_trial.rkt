;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus_trial) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define my-map
  (lambda (fn list)
    ((lambda (map0 fn1 list1) (cond [(empty? list1) empty]
                                    [else (cons (fn1 (first list1))
                                                (map0 map0 (rest list1) fn1))]))
     (lambda (map1 list0 fn0) (cond [(empty? list0) empty]
                                    [else (cons (fn0 (first list0))
                                                (map1 map1 (rest list0) fn0))])) fn list)))
(my-map sub1 '(1 2 3))

(define my-foldr
  (lambda (fn base list)
    ((lambda (foldr0 fn1 list1)
       (cond [(empty? list1) base]
             [else (fn1 (first list1) (foldr0 foldr0 (rest list1) fn1))]))
     (lambda (foldr1 list0 fn0) 
       (cond [(empty? list0) base]
             [else (fn0 (first list0) (foldr1 foldr1 (rest list0) fn0))])) fn list)))

(my-foldr + 0 '(1 2 3))