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
  (bgcolor "#030306")
  (define scheme-red "#ED3833")
  (define scheme-blue "#1230B4")

  ; letter stuff
  (define (lh x y)
    (seth 0) (color "#D0D0CE")
    (penup) (setposition x y) (pendown))

  ;; Stars
  ;; to do: make asterisk, fix distribution
  (lh 0 0)
  (color "#ffffff")
  (define (stars-h n x y) (lh (- x 500) (- y 500)) (circle 2 360) (if (equal? n 0) nil (stars-h (- n 1) (modulo (+ x 420) 1000) (modulo (+ 177 y) 1000))))
  (stars-h 100 400 -200)

  (define (let-a x y s) (lh x y) (lt 150) (fd (/ s (/ sqrt3 2))) (lt 180) (fd (/ s sqrt3)) (rt 60) (fd (* s 0.5)) (rt 60) (fd (/ s sqrt3)) (bk (/ s (/ sqrt3 2))))
  (define (let-l x y s) (lh x y) (bk s) (rt 90) (fd (/ s 2)))
  (define (let-w x y s) (lh x y) (rt 165) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))))
  (define (let-y x y s) (lh x y) (rt 150) (fd (/ s sqrt3)) (rt 30) (fd (/ s 2)) (bk (/ s 2)) (lt 150) (fd (/ s sqrt3)))
  (define (let-s x y s) (let-c x y (/ s 2)) (lh x (- y s)) (seth 90) (circle (/ s 4) 180)) ;; do later maybe
  (define (let-h x y s) (lh x y) (bk s) (fd (/ s 2)) (rt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)) (bk s))
  (define (let-b x y s) (lh x y) (bk s) (rt 45) (fd (/ s 2sqrt2)) (lt 90) (fd (/ s 2sqrt2)) (rt 90) (fd (/ s 2sqrt2)) (lt 90) (fd (/ s 2sqrt2)))
  (define (let-e x y s) (lh x y) (rt 90) (fd s) (lh x y) (bk s) (rt 90) (fd s) (lh x y) (bk (* s 2)) (rt 90) (fd s) )
  (define (let-n x y s) (lh x y) (fd s) (rt 135) (fd (* s 1.5)) (lt 135) (fd s))
  (define (let-i x y s) (lh x y) (rt 90) (fd (/ s 2)) (bk (/ s 4)) (lt 90) (bk s) (rt 90) (fd (/ s 4)) (bk (/ s 2)))
  (define (let-t x y s) (lh x y) (rt 90) (fd s) (bk (/ s 2)) (rt 90) (fd s))
  (define (let-c x y s) (lh x y) (seth -90) (circle (/ s 2) 180))
  (define (let-m x y s) (lh x y) (rt 15) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))))
  (define (let-6 x y s) (lh x y) (bk s) (rt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)))
  (define (let-1 x y s) (lh x y) (rt 45) (bk (/ s 6)) (lh x y) (bk s))
  (define (let-k x y s) (lh x y) (bk s) (fd (/ s 2)) (rt 45) (fd (/ s sqrt2)) (lt x y) (bk (/ s 2)) (rt 45) (bd (/ s sqrt2)))
  (define (let-? x y s) (lh x y) (lt 225) (fd (/ s 2sqrt2)) (rt 90) (fd (/ s 2sqrt2)) (lh x y) (pu) (bk s) (pd) (fd (/ s sqrt2)) (pu) (fd (/ s sqrt2)) (pd) (fd (/ s 10)))

  ;; Scheme Circle
  (lh -275 275)
  (color "#2F62F6")
  (seth -90)
  (define (circle-h n) (circle n 360) (if (equal? n 0) nil (circle-h (- n 1))))
;;   (circle-h 175)

  ;; Scheme logo
  (lh -275 175)
  (color scheme-red)
(define percentlen .90)
(define twistspd -10)

  	(define (draw-spine len bearing velocity)
  		(cond
  			((< len 10)
  				(begin (fd len) (end_fill))
  			) (else
  				(begin (setheading bearing) (fd len) (end_fill) (begin_fill) (bk (* .8 len))) 
  				(draw-spine (* percentlen len) (+ bearing twistspd) velocity)
  			)
  		)
  	)

	(define (draw-rect len width)
		(rt 90) (fd 0.1) (lt 90) (fd len) (bk len)
		(if (equal? width 0) nil
		(draw-rect len (- width 1))))

	(define (draw-lambda x y scale)
		(lh x y)
		(draw-spine (* 50 scale) 90 10)
		(lh x y)
		(pu)
		(fd (* 40 scale))
		(seth 90) (fd (* 55 scale))
		(seth 150)
		(pd)
		(draw-rect (* 40 scale) (* 100 scale))
		(pu) (fd (* 34 scale)) (pd)
		(seth 90)
		(draw-rect (* 12 scale) (* 36 scale))
		)
	;; (draw-lambda -400 20 3)


  ;; Astronaut helmet
  (lh 75 -95)
  (color "#C7CAC7")
  (circle 35 360)
  (lh 65 -95)
  (begin_fill)
  (color "#A3F9F8")
  (circle 25 360)
  (end_fill)
  ;; Astronaut body
  (color "#C7CAC7")
  (seth 180)
  (circle 25 180)
  (lh 25 -195)
  (fd 20)
  (lh 75 -195)
  (fd 20)
  (seth -180)
  (circle 25 180)
  ;; Astronaut 61a
  (let-6 30 -155 5)
  (let-1 35 -155 5)
  (let-a 40 -155 5)



  ;; SCHEME
;;   (let-s -331 215 25)
;;   (let-c -311 215 25)
;;   (let-h -305 215 25)
;;   (let-e -285 215 12)
;;   (let-m -270 190 25)
;;   (let-e -235 215 12)

  ;; ALWAYS HAS BEEN
;;   (let-a 0   455 25)
;;   (let-l 14  455 25)
;;   (let-w 22  455 25)
;;   (let-a 65  455 25)
;;   (let-y 75  455 25)
;;   (let-s 96  455 25)

;;   (let-h 115 455 25)
;;   (let-a 144 455 25)
;;   (let-s 159 455 25)

;;   (let-b 177 455 25)
;;   (let-e 187 455 12)
;;   (let-e 203 455 12)
;;   (let-n 219 430 23)


;;   ;; WAIT, IT'S ALL LINKED LISTS?
;;   (let-w -100 -55 25)
;;   (let-a -57  -55 25)
;;   (let-i -40  -55 25)
;;   (let-t -25  -55 25)

;;   (let-i 25   -55 25)
;;   (let-t 40   -55 25)
;;   (let-s 73   -55 25)

;;   (let-a 110  -55 25)
;;   (let-l 127  -55 25)
;;   (let-l 144  -55 25)

  (let-l 175  -55 25)
  (let-l 192  -55 25)
  (let-s 210  -55 25)
  (let-? 220  -55 25)

  ;; EECS
;;   (let-e 325  75 4)
;;   (let-e 330  75 4)
;;   (let-c 325  85 10)
;;   (let-s 330  85 10)

  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)

