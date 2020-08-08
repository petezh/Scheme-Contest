;;; Scheme Recursive Art Contest Entry
;;;
;;; Please do not include your name or personal info in this file.
;;;
;;; Title: <Your title here>
;;;
;;; Description:
;;;   <It's your masterpiece.
;;;    Use these three lines to describe
;;;    its inner meaning.>

(define (draw)
  ; YOUR CODE HERE
  (define (line) (fd 50))
  (define sqrt3 1.732050)
  (define sqrt2 1.414213)
  (define 2sqrt2 2.8284271)
  (define (twice fn) (fn) (fn))
  (define (repeat k fn) (fn) (if (> k 1) (repeat (- k 1) fn)))
  (define (tri fn) (repeat 3 (lambda () (fn) (lt 120))))
  
  ;; config stuff


  ; letter stuff
  (define (lh x y)
    (seth 0)
    (penup) (setposition x y) (pendown))

  (define (let-a x y s) (lh x y) (lt 150) (fd (/ s (/ sqrt3 2))) (lt 180) (fd (/ s sqrt3)) (rt 60) (fd (* s 0.5)) (rt 60) (fd (/ s sqrt3)) (bk (/ s (/ sqrt3 2))))
  (define (let-l x y s) (lh x y) (bk s) (rt 90) (fd (/ s 2)))
  (define (let-w x y s) (lh x y) (rt 165) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))))
  (define (let-y x y s) (lh x y) (rt 150) (fd (/ s sqrt3)) (rt 30) (fd (/ s 2)) (bk (/ s 2)) (lt 150) (fd (/ s sqrt3)))
  (define (let-s x y s) (lh x y) (lt 135) (fd (/ s (* sqrt2 2))) (lt 90) (fd (/ s sqrt2)) (rt 90) (fd (/ s (* sqrt2 2))))
  ;; (define (let-s x y s) (lh x y) (penup) (bk (/ s 4)) (pendown) (circle 4 90) (penup) (bk (/ s 2)) (pendown) (circle 5 90))
  (define (let-e x y s) (lh x y) (rt 90) (fd s) (lh x y) (bk s) (rt 90) (fd s) (lh x y) (bk (* s 2)) (rt 90) (fd s) )
  (define (let-h x y s) (lh x y) (bk s) (fd (/ s 2)) (rt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)) (bk s))
  (define (let-b x y s) (lh x y) (bk s) (rt 45) (fd (/ s 2sqrt2)) (lt 90) (fd (/ s 2sqrt2)) (rt 90) (fd (/ s 2sqrt2)) (lt 90) (fd (/ s 2sqrt2)))
  (define (let-n x y s) (lh x y) (fd s) (rt 135) (fd (* s 1.5)) (lt 135) (fd s))
  (define (let-i x y s) (lh x y) (rt 90) (fd (/ s 2)) (bk (/ s 4)) (lt 90) (bk s) (rt 90) (fd (/ s 4)) (bk (/ s 2)))
  (define (let-c x y s) (lh x y) (rt 45) (bk (/ s sqrt2)) (lt 90) (bk (/ s sqrt2)))
  (define (let-t x y s) (lh x y) (rt 90) (fd s) (bk (/ s 2)) (lt 90) (bk s))
  (define (let-m x y s) (lh x y) (rt 15) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))))
  (define (let-1 x y s) (lh x y) (rt 45) (bk (/ s 6)) (lh x y) (bk s))
  (define (let-6 x y s) (lh x y) (bk s) (rt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)))
  (define (let-k x y s) (lh x y) (bk s) (fd (/ s 2)) (rt 45) (fd (/ s sqrt2)) (lt x y) (bk (/ s 2)) (rt 45) (bd (/ s sqrt2)))

  (speed 0)
  (penup)

  ; Background
  (bgcolor "#EEEEEE")


  ; Circle
  (setposition 40 206)
  (color "#ED4E33")
  (circle 40)

;; yee
(define percentlen .90)
(define twistspd -10)

  	(define (drawspine len bearing velocity)
  		;take a parentflame and draw its child
  		(cond
  			((< len 10)
  				(begin (fd len) (end_fill) (pu))
  			) (else
  				(begin (setheading bearing) (fd len) (end_fill) (begin_fill) (bk (* .8 len))) 
  				(drawspine (* percentlen len) (+ bearing twistspd) velocity)
  			)
  		)
  	)

  	(define (helper bearing counter r g b) 
  		(pd) (color (rgb r g b))
  		(drawspine 200 bearing 0)
  		(goto 0 0)
  		(cond
  			((= counter 0) nil)
  			((< (modulo counter 6) 2) (helper (+ bearing 59) (- counter 1) (- 1 (/ counter 80)) 0 0))
  			((< (- (modulo counter 6) 2) 2) (helper (+ bearing 59) (- counter 1) 0 (- 1 (/ counter 80)) 0))
  			((< (- (modulo counter 6) 4) 2) (helper (+ bearing 59) (- counter 1) (- 1 (/ counter 80)) (- 1 (/ counter 80)) 0))
  		)
  	)

	(bgcolor (rgb 0 0 0))
  	(speed 10)
  	(helper 0 60 0.25 0 0)
  	(begin (goto 0 120) (setheading 270) (color (rgb 1 1 1)) (begin_fill) (circle 120) (end_fill))
  	(begin (goto 0 100) (setheading 270) (color (rgb 0 0 1)) (begin_fill) (circle 100) (end_fill))
;; bee

  ;; (let-c 0   455  15)


  ;; (let-a 0   455  15)
  ;; (let-l 10  455  15)
  ;; (let-w 17  455  15)
  ;; (let-a 42  455  15)
  ;; (let-y 47  455  15)
  ;; (let-s 60  455  15)

  ;; (let-h 50 455  15)
  ;; (let-a 67 455  15)
  ;; (let-s 73 455  15)

  ;; (let-b 0   455  15)
  ;; (let-e 0   455  15)
  ;; (let-e 0   455  15)
  ;; (let-n 0   455  15)
  (exitonclick))


;; y doesnt this work :/
;; (define (draw)
;; 	(circle 55)
;;   (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)