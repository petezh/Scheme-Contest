; Scheme Recursive Art Contest Entry
;
; Please do not include your name or personal info in this file.
;
; Title: Denero's Devilish Scheme
;
; Description:
;   <It's your masterpiece.
;    Use these three lines to describe
;    its inner meaning.>

(define (draw)
  ; YOUR CODE HERE
  (define (line) (fd 50))
  (define sqrt3 1.732050)
  (define sqrt2 1.414213)
  (define 2sqrt2 2.8284271)
  (define (twice fn) (fn) (fn))
  (define (repeat k fn) (fn) (if (> k 1) (repeat (- k 1) fn)))
  (define (tri fn) (repeat 3 (lambda () (fn) (lt 120))))
  
  ; config stuff
  (bgcolor "#030306")
  (define scheme-red "#ED3833")
  (define scheme-blue "#1230B4")

  ; letter stuff
  (define (lh x y)
    (seth 0) (penup) (setposition x y) (pendown))

  ; Stars
  ; to do: make asterisk, fix distribution
  (lh 0 0)
  (color "#ffffff")
  (define (stars-h n x y) (lh (- x 500) (- y 500)) (circle 2 360) (if (equal? n 0) nil (stars-h (- n 1) (modulo (+ x 420) 1000) (modulo (+ 177 y) 1000))))
  (stars-h 100 400 -200)

  (define (let-a x y s) (lh x y) (lt 150) (fd (/ s (/ sqrt3 2))) (lt 180) (fd (/ s sqrt3)) (rt 60) (fd (* s 0.5)) (rt 60) (fd (/ s sqrt3)) (bk (/ s (/ sqrt3 2))))
  (define (let-l x y s) (lh x y) (bk s) (rt 90) (fd (/ s 2)))
  (define (let-w x y s) (lh x y) (rt 165) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))))
  (define (let-y x y s) (lh x y) (rt 150) (fd (/ s sqrt3)) (rt 30) (fd (/ s 2)) (bk (/ s 2)) (lt 150) (fd (/ s sqrt3)))
  (define (let-s x y s) (let-c x y (/ s 2)) (lh x (- y s)) (seth 90) (circle (/ s 4) 180)) ; do later maybe
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
  (define (let-k x y s) (lh x y) (bk s) (fd (/ s 2)) (rt 45) (fd (/ s sqrt2)) (lh x y) (bk (/ s 2)) (lt 45) (bk (/ s sqrt2)))
  (define (let-? x y s) (lh x y) (lh x (- y (/ s 2))) (seth 90) (circle (/ s 4) 180) (lh x y) (pu) (bk s) (pd) (fd (/ s 25)) (lh x y) (pu) (bk (/ s 1.2)) (pd) (fd (/ s 2.5)) (pu))
  (define (let-d x y s) (lh x y) (seth 90) (circle (/ s 2) 180) (lh x y) (fd s))

  ; Scheme Circle
  (lh -275 275)
  (color "#2F62F6")
  (seth -90)
  (define (circle-b n) (circle n 180) (if (equal? n 0) nil (circle-b (- n 1))))
  (circle-b 175)

  ; Scheme logo
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
		(draw-rect (* 12 scale) (* 5 scale)))
	(draw-lambda -400 20 3 "#4CA954")


  ; Astronaut helmet
  (lh 75 -145)
  (color "#C7CAC7")
  (circle 60 360)
  
  (lh 65 -145)
  (begin_fill)
  (color "#A3F9F8")
  (circle 50 360)
  (end_fill)
  ; Astronaut body
  (color "#C7CAC7")
  (begin_fill)
  (lh 90 -300)
  (circle 75 180)
  (lt 90)
  (fd 150)
  (end_fill)
  (lh -60 -300)
  (draw-rect 200 150)
  ; Astronaut arm
  (color "#adb1ad")
  (lh 15 -320)
  (draw-rect 150 60)
  ; Astronaut 61a
  (color "#ED3833")
  (let-6 0 -140 20)
  (let-1 15 -140 20)
  (let-a 30 -140 20)



  ; Denero
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

  ;(color "#ffd9b3")
  (color "#ffceb4")
  (begin_fill)
  (circle 100 180)
  (forward 80)
  (circle 100 180)
  (forward 80)
  (end_fill)
  (backward 32)
  (left 90)
  (forward 200)
  (glasses)
  (backward 24)
  (seth -90)
  (forward 40)
  (color "#000000")
  (begin_fill)
  (circle 10)
  (end_fill)
  (color "#3D3331")
  (begin_fill)
  (circle 10)
  (end_fill)
  (forward 92)
  (color "#000000")
  (begin_fill)
  (circle 10)
  (end_fill)
  (color "#3D3331")
  (begin_fill)
  (circle 4)
  (end_fill)
  (backward 46)
  (seth 180)
  (forward 40)
  (pendown)
  ;nose
  (right 30) (forward 20) (seth 90) (forward 10) 
  ;end nose
  (penup) (right 90) (forward 30) (seth 90) (pendown)
  ;mouth
  (circle 40 45) (right 180) (circle -40 90)
  ;end mouth
  (penup) (right 180) (circle -40 45) (seth 0) (forward 140) (seth -90)
  ;left brow
  (pendown) (forward 50) (left 45) (forward 20)
  ;end left brow
  (penup) (backward 20) (right 45) (backward 80) (seth 90) (pendown)
  ;right brow
  (forward 50) (right 45) (forward 20)
  ;end right brow
  (penup) (seth 180) (forward 20) (right 90) (forward 184) (seth 0)
  ;begin hair
  (color "#3D3331")  (righttriangle 50 100) (forward 50) (righttriangle 80 50)   (forward 50)  (begin_fill)  (right 30)  (forward 70)  (seth 90)  (forward 140)
  (seth 180) (left 30)  (forward 70)  (end_fill)  (seth 180)  (forward 50)  (seth 0) (righttriangle -80 50) (backward 50) (righttriangle -50 100)
  ;end hair
  (seth -90)  (forward 180)  (left 90)  (forward 170)
)

(define (glasses)
  (seth 180)
  (left 37)
  (pendown)
  (color "black")
  (forward 24) ;left ear thing
  (define (frame)
    (seth 0)
    (backward 20)
    (rectangle 80 40)
    (penup)
    (forward 20)
    (right 90)
    (forward 80)
  )
  (frame)
  (pendown)
  (forward 12) ;middle thing
  (frame)
  (pendown)
  (seth 0)
  (right 37)
  (pendown)
  (forward 24) ;right ear thing
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

; John body
; back arm
(color "#adb1ad")
  (lh 200 -200)
  (seth -60)
  (draw-rect 200 60)
  (color "#C7CAC7")
  (begin_fill)
  (lh 500 -150)
  (circle 100 180)
  (lt 90)
  (fd 200)
  (end_fill)
  (lh 300 -150)
  (draw-rect 400 200)
  (color "#adb1ad")
  (lh 360 -120)
  (draw-rect 200 100)
  (lh 350 -460)
  (seth -150)
  (draw-rect 200 75)
  ;john gun
  (color "#545954")
  (lh 220 -160)
  (draw-rect 60 20)
  (lh 200 -170)
  (draw-rect 20 5)
  (lh 200 -185)
  (draw-rect 5 20)
  
  (color "#C7CAC7")
  (lh 220 -160)
  (seth 90)
  (draw-rect 80 20)
  
  (color "#adb1ad")
  (rt 90)
  (fd 5)
  (lt 90)
  (draw-rect 80 5)
  
  ; John face
  (lh 480 70)
  (johns-face)

  ; John helmet
  (lh 500 30)
  (color "#C7CAC7")
  (circle 120 360)

  ; SCHEME
  (color "#5beb34")
  (let-s -331 216 25)
  (let-c -311 216 25)
  (let-h -305 216 25)
  (let-e -285 216 12)
  (let-m -270 191 25)
  (let-e -235 216 12)

  ; ALWAYS HAS BEEN
  (let-a -120  350 60)
  (let-l -80   350 60)
  (let-w -55   353 60)
  (let-a  40   350 60)
  (let-y  65   350 60)
  (let-s  115  350 60)

  (let-h 155 350 60)
  (let-a 223 350 60)
  (let-s 259 350 60)

  (let-b 309 350 60)
  (let-e 329 350 30)
  (let-e 369 350 30)
  (let-n 420 290 55)


  ; WAIT, IT'S ALL LINKED LISTS?
  (let-w -260 -55 25)
  (let-a -217 -55 25)
  (let-i -200 -55 25)
  (let-t -185  -55 25)

  (let-i -135  -55 25)
  (let-t -120  -55 25)
  (let-s  -87  -55 25)

  (let-a  -50  -55 25)
  (let-l  -33  -55 25)
  (let-l  -16  -55 25)

  (let-l 15  -55 25)
  (let-i 30  -55 25)
  (let-n 45  -80 23)
  (let-k 75  -55 25)
  (let-e 95  -55 12)
  (let-d 110  -80 25)
  (let-l 135  -55 25)
  (let-i 150  -55 25)
  (let-s 174  -55 25)
  (let-t 178  -55 25)
  (let-s 208  -55 25)
  (let-? 218  -55 25)

  ; EECS
  (color "#083362")
  (lh 382 -170)
  (begin_fill)
  (rectangle 50 50)
  (end_fill)
  (color "#46B4E5")
  (let-e 395  -125 10)
  (let-e 410  -125 10)
  (color "#F6B142")
  (let-c 405  -155 15)
  (let-s 415  -155 15)

  (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)

