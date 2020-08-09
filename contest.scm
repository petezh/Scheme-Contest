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

  ; letter stuff
  (define (lh x y)
    (seth 0) (color "#D0D0CE")
    (penup) (setposition x y) (pendown))

  (define (let-a x y s) (lh x y) (lt 150) (fd (/ s (/ sqrt3 2))) (lt 180) (fd (/ s sqrt3)) (rt 60) (fd (* s 0.5)) (rt 60) (fd (/ s sqrt3)) (bk (/ s (/ sqrt3 2))))
  (define (let-l x y s) (lh x y) (bk s) (rt 90) (fd (/ s 2)))
  (define (let-w x y s) (lh x y) (rt 165) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))))
  (define (let-y x y s) (lh x y) (rt 150) (fd (/ s sqrt3)) (rt 30) (fd (/ s 2)) (bk (/ s 2)) (lt 150) (fd (/ s sqrt3)))
  (define (let-s x y s) (lh x y) (lt 135) (fd (/ s 2sqrt2)) (lt 90) (fd (/ s sqrt2)) (rt 90) (fd (/ s 2sqrt2)))
  ;; (define (let-s x y s) (let-c x y (/ s 2)) (lh x (- y s)) (seth 90) (circle (/ s 4) 180))
  (define (let-h x y s) (lh x y) (bk s) (fd (/ s 2)) (rt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)) (bk s))
  (define (let-b x y s) (lh x y) (bk s) (rt 45) (fd (/ s 2sqrt2)) (lt 90) (fd (/ s 2sqrt2)) (rt 90) (fd (/ s 2sqrt2)) (lt 90) (fd (/ s 2sqrt2)))
  (define (let-e x y s) (lh x y) (rt 90) (fd s) (lh x y) (bk s) (rt 90) (fd s) (lh x y) (bk (* s 2)) (rt 90) (fd s) )
  (define (let-n x y s) (lh x y) (fd s) (rt 135) (fd (* s 1.5)) (lt 135) (fd s))
  (define (let-i x y s) (lh x y) (rt 90) (fd (/ s 2)) (bk (/ s 4)) (lt 90) (bk s) (rt 90) (fd (/ s 4)) (bk (/ s 2)))
  (define (let-t x y s) (lh x y) (rt 90) (fd s) (bk (/ s 2)) (rt 90) (fd s))
  (define (let-c x y s) (lh x y) (rt 45) (bk (/ s sqrt2)) (lt 90) (bk (/ s sqrt2)))
  ;; (define (let-c x y s) (lh x y) (seth -90) (circle (/ s 2) 180))
  (define (let-m x y s) (lh x y) (rt 15) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))) (lt 150) (fd (/ s (/ sqrt3 2))) (rt 150) (fd (/ s (/ sqrt3 2))))
  (define (let-6 x y s) (lh x y) (bk s) (rt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)) (lt 90) (fd (/ s 2)))
  (define (let-1 x y s) (lh x y) (rt 45) (bk (/ s 6)) (lh x y) (bk s))
  (define (let-k x y s) (lh x y) (bk s) (fd (/ s 2)) (rt 45) (fd (/ s sqrt2)) (lt x y) (bk (/ s 2)) (rt 45) (bd (/ s sqrt2)))

  ;; Circle
  ;; (lh -95 100)
  ;; (color "#2F62F6")
  ;; (circle 125 360)


  ;; ;; SCHEME
  (let-s -296 155 25)
  (let-c -276 155 25)
  (let-h -270 155 25)
  (let-e -250 155 12)
  (let-m -235 130 25)
  (let-e -200 155 12)

  ;; ALWAYS HAS BEEN
  (let-a 0   455 25)
  (let-l 14  455 25)
  (let-w 22  455 25)
  (let-a 65  455 25)
  (let-y 75  455 25)
  (let-s 96  455 25)

  (let-h 115 455 25)
  (let-a 144 455 25)
  (let-s 159 455 25)

  (let-b 177 455 25)
  (let-e 187 455 12)
  (let-e 203 455 12)
  (let-n 219 430 23)


  ;; WAIT, IT'S ALL LINKED LISTS?
  (let-w -100 -55 25)
  (let-a -57  -55 25)
  (let-i -40  -55 25)
  (let-t -25  -55 25)

  (let-i 25  -55 25)
  (let-t 40  -55 25)
  (let-s 73  -55 25)

  (let-a 110  -55 25)
  (let-l 127  -55 25)
  (let-l 144  -55 25)

  (let-l 175  -55 25)
  (let-l 192  -55 25)
  (let-s 210  -55 25)


  (exitonclick))


;; y doesnt this work :/
;; (define (draw)
;; 	(circle 55)
;;   (exitonclick))

; Please leave this last line alone.  You may add additional procedures above
; this line.
(draw)