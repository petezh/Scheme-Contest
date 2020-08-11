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
    (seth 0) (penup) (setposition x y) (pendown))

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
  (define (let-? x y s) (lh x y) (lh x (- y (/ s 2))) (seth 90) (circle (/ s 4) 180) (lh x y) (pu) (bk s) (pd) (fd (/ s 25)) (lh x y) (pu) (bk (/ s 1.2)) (pd) (fd (/ s 2.5)) (pu))

  ;; Scheme Circle
  (lh -275 275)
  (color "#2F62F6")
  (seth -90)
  (define (circle-h n) (circle n 360) (if (equal? n 0) nil (circle-h (- n 1))))
  (circle-h 175)

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

	(define (draw-rect width len)
		(begin_fill) (bk width) (rt 90) (fd len) (lt 90) (fd width) (lt 90) (fd len) (rt 90) (end_fill))
	
	(define (draw-lambda x y scale c)
		(lh x y) (color c)
		(draw-spine (* 50 scale) 90 10)
		(lh x y) (color c)
		(pu)
		(fd (* 5 scale))
		(seth 90) (fd (* 75 scale))
		(seth 150)
		(pd)
		(draw-rect (* 40 scale) (* 10 scale))
		(seth 90)
		(draw-rect (* 12 scale) (* 5 scale))
	) (draw-lambda -400 20 3 scheme-red)


  ;; Astronaut helmet
  (lh 75 -145)
  (color "#C7CAC7")
  (circle 55 360)
  (lh 65 -145)
  (begin_fill)
  (color "#A3F9F8")
  (circle 45 360)
  (end_fill)
  ;; Astronaut body
  (color "#C7CAC7")
  (lh -69 -190)
  (draw-rect 185 145)
  ;; Astronaut 61a
  (color "#ED3833")
  (let-6 30 -155 5)
  (let-1 35 -155 5)
  (let-a 40 -155 5)


  ;; Denero
  (define (righttriangle horizontal vertical)
  (begin_fill)
  (forward vertical)
  (right 90)
  (forward horizontal)
  (end_fill)
  (backward horizontal)
  (left 90)
  (backward vertical)
)

(define (johns-face)
  (seth 0)
  (color "#ffceb4")
  (begin_fill)
  (circle 50 180)
  (forward 40)
  (circle 50 180)
  (forward 40)
  (end_fill)
  (backward 16)
  (left 90)
  (forward 100)
  (glasses)
  (backward 12)
  (seth -90)
  (forward 20)
  (color "#000000")
  (begin_fill)
  (circle 5)
  (end_fill)
  (color "#3D3331")
  (begin_fill)
  (circle 2)
  (end_fill)
  (forward 46)
  (color "#000000")
  (begin_fill)
  (circle 5)
  (end_fill)
  (color "#3D3331")
  (begin_fill)
  (circle 2)
  (end_fill)
  (backward 23)
  (seth 180)
  (forward 20)
  (pendown)
  ;nose
  (right 30) (forward 10) (seth 90) (forward 5)
  ;end nose
  (penup) (right 90) (forward 15) (seth 90) (pendown)
  ;mouth
  (circle 20 45) (right 180) (circle -20 90)
  ;end mouth
  (penup) (right 180) (circle -20 45) (seth 0) (forward 70) (seth -90)
  ;left brow
  (pendown) (forward 25) (left 45) (forward 10)
  ;end left brow
  (penup) (backward 10) (right 45) (backward 40) (seth 90) (pendown)
  ;right brow
  (forward 25) (right 45) (forward 10)
  ;end right brow
  (penup) (seth 180) (forward 10) (right 90) (forward 92) (seth 0)
  ;begin hair
  (color "#3D3331")  (righttriangle 25 50)  (forward 50)  (begin_fill)  (right 30)  (forward 35)  (seth 90)  (forward 70)
  (seth 180) (left 30)  (forward 35)  (end_fill)  (seth 180)  (forward 50)  (seth 0)  (righttriangle -25 50)
  ;end hair
  (seth -90)  (forward 90)  (left 90)  (forward 85)
)

(define (glasses)
  (seth 180)
  (left 37)
  (pendown)
  (color "black")
  (forward 12) ;left ear thing
  (define (frame)
    (seth 0)
    (backward 10)
    (rectangle 40 20)
    (penup)
    (forward 10)
    (right 90)
    (forward 40)
  )
  (frame)
  (pendown)
  (forward 6) ;middle thing
  (frame)
  (pendown)
  (seth 0)
  (right 37)
  (pendown)
  (forward 12) ;right ear thing
  (penup)
)

(define (rectangle horizontal vertical)
  (seth 0) (pendown)
  (define (twosides)
    (forward vertical)
    (right 90)
    (forward horizontal)
    (right 90)
  )
  (twosides)
  (twosides)

)

  ;; DRAW FACE
  (lh 350 250)
  (johns-face)

  ;; SCHEME
  (color "#D0D0CE")
  (let-s -331 215 25)
  (let-c -311 215 25)
  (let-h -305 215 25)
  (let-e -285 215 12)
  (let-m -270 190 25)
  (let-e -235 215 12)

  ;; ALWAYS HAS BEEN
  (let-a 0   455 25)
  (let-l 14  455 25)
  (let-w 22  455 25)
  (let-a 65  455 25)
  (let-y 75  455 25)
  (let-s 97  455 25)

  (let-h 115 455 25)
  (let-a 144 455 25)
  (let-s 159 455 25)

  (let-b 177 455 25)
  (let-e 187 455 12)
  (let-e 203 455 12)
  (let-n 220 430 23)


  ;; WAIT, IT'S ALL LINKED LISTS?
  (let-w -120 -55 25)
  (let-a -77  -55 25)
  (let-i -60  -55 25)
  (let-t -45  -55 25)

  (let-i 5   -55 25)
  (let-t 20   -55 25)
  (let-s 53   -55 25)

  (let-a 90  -55 25)
  (let-l 107  -55 25)
  (let-l 124  -55 25)

  (let-l 155  -55 25)
  (let-l 172  -55 25)
  (let-s 190  -55 25)
  (let-? 200  -55 25)

  ;; EECS
  (color "#083362")
  (lh 312 30)
  (begin_fill)
  (rectangle 50 50)
  (end_fill)
  (color "#46B4E5")
  (let-e 325  75 10)
  (let-e 340  75 10)
  (color "#F6B142")
  (let-c 335  45 15)
  (let-s 345  45 15)

  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)

