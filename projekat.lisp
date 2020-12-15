;;;;; Vestacka Inteligencija ;;;;;
;;;;; Prva faza do 20.12.2020.
;;;;; Druga faza do ...
;;;;; Treca faza do ...
;;;;; Cetvrta faza do ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;; Projekat : 3D Four in line ;;;;;

;; Prva faza ;;

;;;; Definisati nacin predstavljanja stanja problema (igre)
;;;; Napisati funkciju za postavljanje pocetnog stanja na osnovu zadate velicine kocke
;;;; Napisati funkcije za testiranje kraja igre
;;;; Omoguciti izbor ko ce da igra prvi (covek ili racunar)
;;;; Prvi igra uvek Igrac X, a drugi O
;;;; Implementirati funkcije koje obezbedjuju prikaz proizvoljnog stanja problema(igre)
;;;; Realizovati funkcije koje na osnovu zadatog poteza, u obliku broj_stubica ili
;(vrsta, kolona), omogucavaju: proveru da li je potez valjan, ako jeste, promenu 
;prosledjenog stanja problema(igre) odigravanjem poteza


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; KOD ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Predstavljanje problema
;; ili lista listi, ili velika lista

;; ((- - - -) (- - - -) (- - - -) (- - - -) ... (- - - -))
;; (- - - - - - - - ... - - - -)
;; Zavisi kolko ima od N, N min 4, reko da napravimo iz za 6


;; definisati kako se vraca lista koja je definisana za predstavljanje problema
;u zavisnosti od N


(defun init-stanje (N) 
  
  (setq stanjeigre (init-stanje-pom N (* N N)))
  
  )

(init-stanje 4)
(init-stanje 6)
(print stanjeigre)

(defun init-stanje-pom (N pom)
  (cond 
   ((equalp 0 pom) '())
   (t (cons (stubic N) (init-stanje-pom N (1- pom))))
  )))
  
(defun stubic (N)
  (cond
   ((equalp 0 N) ())
   (t (cons '- (stubic (1- N))))
   ))


;; Testiranje kraja igre
;; treba prebroji ko ima vise od 4 spojena po dijagonale vertikale, horizontale
;u svim pravcima

(defun krajp (tstanje)
  (cond
   ((null tstanje) t)
   ((listp (car tstanje)) (and (krajp (car tstanje)) (krajp (cdr tstanje))))
   (t (and (not (equalp (car tstanje) '-)) (krajp (cdr tstanje))))
   ))


(krajp '((O O X X) (X O X X))); => T
(krajp '((O O X X) (X O - X))); => NIL


;; treba da pokrene igru, i da namesti ko igra prvi i sve to
; mozda neka globalna promenjiva..
;; ko igra prvi uvek je x, ko igra drugi uvek je o


(defun start-igra (covek) 
  
  
  )


;; treba da prikaze proizvoljno stanje

(defun print-stanje (stanje)
  
  
  )


;; proverava da li je potez validan 

(defun validanp (stanje broj-stubica)
  
  (cond
   ((null broj-stubica) '())
   ((> broj-stubica (expt 4 2)) '());pretpostavka da je N=4 jer ga nemam kao parametar
   ((< broj-stubica 0) '())         ;sad ne znam da l ce bude globalno il kako, ce resimo
   (t (stubicp (nth broj-stubica stanje)))
   )
  )

(defun stubicp(lista)
  (cond
   ((null lista) '())
   ((equalp (car lista) '-) t)
   (t (OR '() (stubicp (cdr lista))))
   )
  )


(validanp '((x o x o) (x x x x) (x - - -)) 2)
(validanp '((x o x o) (x x x x) (x - - -)) 1)

;; omogucava da igrac igra

(defun odigraj (stanje igrac broj-stubica)

  (cond
   ((validanp stanje broj-stubica) (izmeni stanje igrac broj-stubica))
   (t stanje)
   )
  )

(defun izmeni (stanje igrac broj-stubica)
  (cond
   ((= 0 broj-stubica) (cons (dodaj (car stanje) igrac) (cdr stanje)))
   (t (cons (car stanje) (izmeni (cdr stanje) igrac (- broj-stubica 1))))
   )
  )

(defun dodaj (stubic igrac)
  (cond
   ((equalp (car stubic) '-) (cons igrac (cdr stubic)))
   (t (cons (car stubic) (dodaj (cdr stubic) igrac)))
   )
  )

(odigraj '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)) 'o 1)

(odigraj '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)) 'o 3)