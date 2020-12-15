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

;;;;; =========================================================================== ;;;;;


;; definisati kako se vraca lista koja je definisana za predstavljanje problema
;u zavisnosti od N


(defun init-stanje (N)
  (let () 
    (setq Nstubica (* N N))
    (setq stanjeigre (init-stanje-pom N (* N N)))
    ))

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

(init-stanje 4)
(init-stanje 6)
(print stanjeigre)

;;;;; =========================================================================== ;;;;;


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


;;;;; =========================================================================== ;;;;;


;; treba da pokrene igru, i da namesti ko igra prvi
;; ko igra prvi uvek je x, ko igra drugi uvek je o


(defun start-igra ()
  (let* 
      ((prvi (progn (format t "unesite da li covek igra prvi ['t / ()]") (read)))
       (N (unesite-N)))
    (init-stanje N)
    
  ))



(defun unesite-N () 
  (let* 
      ((N (progn (format t "unesite koliko je velika tabla [paran broj; min 4]") (read) )))
    (cond 
     ((equalp 1 (mod N 2)) (unesite-N))
     ((< N 4) (unesite-N))
     (t N))))

(unesite-N)

;;;;; =========================================================================== ;;;;;


;; treba da prikaze proizvoljno stanje

;postavlja string kao pomoc za print
(setq chars-print "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;sklanja sa potez sa vrha
(defun remove-top (tstanje)
  (mapcar #'reverse (mapcar #'cdr (mapcar #'reverse tstanje))))

;vraca potez sa vrha
(defun get-top (tstanje)
  (mapcar #'car (mapcar #'reverse tstanje))
  )

(remove-top '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)))
(get-top '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)))


(defun get-support-chars (N)
  (cond 
   ((equalp 0 N) ())
   (t (append (get-support-chars (1- N)) (list (char chars-print (1- N)))))))


(format t "~%~A" (get-support-chars 16))


(defun print-stanje (tstanje N)
  (cond 
   ((null (caar tstanje)) (format t "~%~A" (get-support-chars N)))
   (t (let 
          ((pstanje (get-top tstanje)) 
           (sstanje (remove-top tstanje)))
        (format t "~%~A" pstanje)
        (print-stanje sstanje N)
          ))
  ))



(print-stanje stanjeigre Nstubica)

(print-stanje '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)) 6)


;;;;; =========================================================================== ;;;;;


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

;;;;; =========================================================================== ;;;;;

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

(odigraj '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)) 'x 13)
(odigraj '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)) 'o 3)