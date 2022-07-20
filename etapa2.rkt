#lang racket

(provide (all-defined-out))

;; Același arbore de TPP obținut în etapa 1 prin aplicarea
;; transformărilor T1, T2, T3 poate fi generat folosind 
;; tupluri GH (Gopal-Hemachandra).
;;
;; Pentru o pereche oarecare (g, e), secvența GH este:
;;    g, e, g + e, g + 2e, 2g + 3e, 3g + 5e ...
;; Pentru (g, e) = (1, 1) obținem șirul lui Fibonacci.
;;
;; Primele 4 numere din secvență formează cvartetul GH:
;;    (g, e, f, h) = (g, e, g + e, g + 2e)
;;
;; Pentru un asemenea cvartet (g, e, f, h), definim:
;;    a = gh,   b = 2ef,   c = e^2 + f^2
;; și putem demonstra că (a,b,c) este un triplet pitagoreic.
;;
;; (a,b,c) este chiar TPP, dacă adăugăm condițiile:
;;    g, e, f, h prime între ele
;;    g impar
;; însă nu veți avea nevoie să faceți asemenea verificări,
;; întrucât avem la dispoziție un algoritm care generează
;; exclusiv TPP.
;;
;; Acest algoritm este foarte asemănător cu cel din etapa
;; anterioară, cu următoarele diferențe:
;;  - nodurile din arbore sunt cvartete, nu triplete
;;    (din cvartet obținem un TPP conform formulelor)
;;    (ex: (1,1,2,3) => (1*3,2*1*2,1^2+2^2) = (3,4,5))
;;  - obținem următoarea generație de cvartete folosind 
;;    trei transformări Q1, Q2, Q3 pentru cvartete, în loc
;;    de T1, T2, T3 care lucrau cu triplete
;; 
;; Q1(g,e,f,h) = (h,e,h+e,h+2e)
;; Q2(g,e,f,h) = (h,f,h+f,h+2f) 
;; Q3(g,e,f,h) = (g,f,g+f,g+2f)
;;
;; Arborele rezultat arată astfel:
;;
;;                        (1,1,2,3)
;;              ______________|______________
;;             |              |              |
;;         (3,1,4,5)      (3,2,5,7)      (1,2,3,5)
;;       ______|______  ______|______  ______|______
;;      |      |      ||      |      ||      |      |
;;  (5,1,6,7) .........................................

;; Definim funcțiile Q1, Q2, Q3:
(define (Q1 g e f h) (list h e (+ h e) (+ h e e)))
(define (Q2 g e f h) (list h f (+ h f) (+ h f f)))
(define (Q3 g e f h) (list g f (+ g f) (+ g f f)))

;; Vom refolosi matricile T1, T2, T3:
(define T1 '((-1 2 2) (-2 1 2) (-2 2 3)))
(define T2 '( (1 2 2)  (2 1 2)  (2 2 3)))
(define T3 '((1 -2 2) (2 -1 2) (2 -2 3)))


(define (uncurry->curry f)
  (lambda (x)
    (lambda (y) (f x y)))
 )

(define (curry->uncurry f)
  (lambda (x y)
   ((f x) y))
 )


; TODO
; Reimplementați funcția care calculează produsul scalar
; a doi vectori X și Y, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că X și Y au aceeași lungime.
; Ex: (-1,2,2)·(3,4,5) = -3 + 8 + 10 = 15
(define (dot-product X Y)
  (dot-product-helper X Y 0)
 )

(define (dot-product-helper X Y acc)
  (foldl (lambda (x y acc) (+ (* x y) acc)) acc X Y)
 )

; TODO
; Reimplementați funcția care calculează produsul dintre
; o matrice M și un vector V, astfel încât să nu folosiți
; recursivitate explicită (ci funcționale).
; Memento:
; Se garantează că M și V au dimensiuni compatibile.
; Ex: |-1 2 2| |3|   |15|
;     |-2 1 2|·|4| = | 8|
;     |-2 2 3| |5|   |17|
(define (multiply M V)
  (multiply-helper M V null)
 )

(define (multiply-helper M V acc)
  (foldr (lambda (x acc) (cons (dot-product V x) acc)) acc M)
 )


; TODO
; Aduceți aici (nu sunt necesare modificări) implementarea
; funcției get-transformations de la etapa 1.
; Această funcție nu este re-punctată de checker, însă este
; necesară implementărilor ulterioare.

(define (get-transformations n)
  (reverse (get-transformation n))
 )

(define (index_min lvl)
  (cond
    [(= lvl 0) 1]
    [else (+ (power lvl) (index_min (- lvl 1)))]
    )
  )

(define (power i)
  (cond
    [(= 0 i) 1]
    [else (* 3 (power (- i 1)))]
    )
  )

(define (get_level x s lvl)
  (cond
    [(<= x s) (- lvl 1)]
    [else (get_level x (+ s (power lvl)) (+ lvl 1))]
    )
  )

(define (get-transformation n)
  (cond
    [(or (= 0 n) (= 1 n)) null]
    [(and (>= n 2) (<= n 4)) (append (list (- n 1))
                                (get-transformation 0))]
    [else (append (list (+ 1 (modulo (- n (+ 1 (index_min (- (get_level n 0 0) 1)))) 3)))
              (get-transformation (quotient (+ n 1) 3)))]
  )
)


; TODO
; În etapa anterioară ați implementat o funcție care primea
; o listă Ts de tipul celei întoarsă de get-transformations
; și un triplet de start ppt și întorcea tripletul rezultat
; în urma aplicării transformărilor din Ts asupra ppt.
; Acum dorim să generalizăm acest proces, astfel încât să
; putem reutiliza funcția atât pentru transformările de tip
; T1, T2, T3, cât și pentru cele de tip Q1, Q2, Q3.
; În acest scop operăm următoarele modificări:
;  - primul parametru este o listă de funcții Fs
;    (în loc de o listă numerică Ts)
;  - al doilea parametru reprezintă un tuplu oarecare
;    (aici modificarea este doar "cu numele", fără a schimba
;    funcționalitatea, este responsabilitatea funcțiilor din
;    Fs să primească parametri de tipul lui tuple)
; Nu folosiți recursivitate explicită (ci funcționale).
(define (apply-functional-transformations Fs tuple)
  (foldl (lambda (x tuple) (x tuple)) tuple Fs)
 )


; TODO
; Tot în spiritul abstractizării, veți defini o nouă funcție
; get-nth-tuple, care calculează al n-lea tuplu din arbore. 
; Această funcție va putea fi folosită:
;  - și pentru arborele de triplete (caz în care plecăm de la
;    (3,4,5) și avansăm via T1, T2, T3)
;  - și pentru arborele de cvartete (caz în care plecăm de la
;    (1,1,2,3) și avansăm via Q1, Q2, Q3)
; Rezultă că, în afară de parametrul n, funcția va trebui să
; primească un tuplu de start și 3 funcții de transformare a
; tuplurilor.
; Definiți get-nth-tuple astfel încât să o puteți reutiliza
; cu minim de efort pentru a defini funcțiile următoare:
;    get-nth-ppt-from-matrix-transformations
;    get-nth-quadruple
; (Hint: funcții curry)
; În define-ul de mai jos nu am precizat parametrii funcției
; get-nth-tuple pentru ca voi înșivă să decideți care este
; modul optim în care funcția să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție,
; dar asistentul va observa dacă implementarea respectă cerința.

(define (get-nth-tuple tuple)
  (lambda (Fs)
    (lambda (n) (apply-functional-transformations (get-trans-list (get-transformations n) Fs null) tuple)))
 )

;(define (get-trans-list path Fs result)
 ; (cond
 ;   [(null? path) (reverse result)]
 ;   [(= (car path) 1) (get-trans-list (cdr path) Fs (cons (car Fs) result))]
 ;   [(= (car path) 2) (get-trans-list (cdr path) Fs (cons (cadr Fs) result))]
 ;   [else (get-trans-list (cdr path) Fs (cons (caddr Fs) result))]
 ;   )
 ;)


;; this funcion transforms a path into an ordered list of functions
;; ex: (1 3 2 3) -> (G1 G3 G2 G3)
(define (get-trans-list path Fs result)
  (foldr (lambda (x result) (cond
                                    [(null? path) result]
                                    [(= 1 x) (cons (car Fs) result)]
                                    [(= 2 x) (cons (cadr Fs) result)]
                                    [else (cons (caddr Fs) result)]
                                    )) null path))

; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil
; (hint: aplicare parțială) o funcție care calculează al n-lea
; TPP din arbore, folosind transformările pe triplete.

;(define (get-nth-ppt-from-matrix-transformations n)
; 'your-code-here)

(define triple-func
  (list ((uncurry->curry multiply) T1)
        ((uncurry->curry multiply) T2)
        ((uncurry->curry multiply) T3))
 )

(define get-nth-ppt-from-matrix-transformations
  ((get-nth-tuple '(3 4 5)) triple-func)
 )


; TODO
; Din get-nth-tuple, obțineți în cel mai succint mod posibil 
; (hint: aplicare parțială) o funcție care calculează al n-lea 
; cvartet din arbore, folosind transformările pe cvartete.

;(define (apply-quadruple-func f tuple)
;  (f (car tuple) (cadr tuple) (caddr tuple) (cadddr tuple))
; )

(define quadruple-func
  (list ((uncurry->curry apply) Q1)
        ((uncurry->curry apply) Q2)
        ((uncurry->curry apply) Q3))        
 )

(define get-nth-quadruple
  ((get-nth-tuple '(1 1 2 3)) quadruple-func)
 )

; TODO
; Folosiți rezultatul întors de get-nth-quadruple pentru a 
; obține al n-lea TPP din arbore.
(define (get-nth-ppt-from-GH-quadruples n)
  (transform-quad-ppt (get-nth-quadruple n))
 )

(define (transform-quad-ppt tuple)
  (list (* (car tuple) (cadddr tuple))
        (* 2 (cadr tuple) (caddr tuple))
        (+ (* (cadr tuple) (cadr tuple)) (* (caddr tuple) (caddr tuple))))
 )
