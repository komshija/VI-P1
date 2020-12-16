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

;; Globalne promenjive
; Nstubica : ukupan broj stubica
; stanjeigre : trenutno realno stanje igre

;;;;; =========================================================================== ;;;;;


;; 1. Definisati kako se vraca lista koja je definisana za predstavljanje problema u zavisnosti od N

;; Funkcija koja inicijalizuje stanje i ukuban broj stubica
;Param: 
;N : koliko poteza mogu da stanu na jednom stubicu

(defun init-stanje (N)
  (let () 
    (setq Nstubica (* N N))
    (setq stanjeigre (init-stanje-pom (* N N)))
    ))

;; Funkcija koja spaja sve stubice u jednu listu
;Param:
;N : koliko imamo ukupno stubica
;pom : pomocna koja broji do 0

(defun init-stanje-pom (pom)
  (cond 
   ((equalp 0 pom) '())
   (t (cons (stubic (sqrt Nstubica)) (init-stanje-pom (1- pom))))
  ))
  
;; Funkcija koja vraca prazan stubic visine N
; N : visina stubica

(defun stubic (N)
  (cond
   ((equalp 0 N) ())
   (t (cons '- (stubic (1- N))))
   ))


;; Test primeri
;(init-stanje 4)
;(init-stanje 6)


;;;;; =========================================================================== ;;;;;


;; 2. Testiranje kraja igre

;; Proverava da li je cela tabla popunjena
;Params:
;tstanje : trenutno stanje igre

(defun krajp (tstanje)
  (cond
   ((null tstanje) t)
   ((listp (car tstanje)) (and (krajp (car tstanje)) (krajp (cdr tstanje))))
   (t (and (not (equalp (car tstanje) '-)) (krajp (cdr tstanje))))
   ))


;; Test primeri
;(krajp '((O O X X) (X O X X))); => T
;(krajp '((O O X X) (X O - X))); => NIL


;;;;; =========================================================================== ;;;;;


;; 3. Treba da pokrene igru, i da namesti ko igra prvi
;; ko igra prvi uvek je x, ko igra drugi uvek je o


;; Pokrece igru, korisnik bira ko igra prvi, 
;takodje bira velicinu table sve dok ne unese kako treba
;Params:
;
(defun start-igra ()
  (let* 
      ((prvi (progn (format t "unesite da li covek igra prvi ['t / ()] : ") (read)))
       (N (unesite-N)))
    (init-stanje N)
    
  ))

;Unos velicine table tj visine stubica
;Params:
;
(defun unesite-N () 
  (let* 
      ((N (progn (format t "unesite koliko je velika tabla [paran broj; min 4] : ") (read) )))
    (cond 
     ((equalp 1 (mod N 2)) (unesite-N))
     ((< N 4) (unesite-N))
     (t N))))


;Test primeri
;(start-igra)
;(unesite-N)

;;;;; =========================================================================== ;;;;;


;; 4. Treba da prikaze proizvoljno stanje

;;Postavlja string kao pomoc za print
; Sluzi pri generisanju pomocnih linija pri prikazu
(setq chars-print "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;;Sklanja sve potez sa vrha svakog stubica
;Params:
;tstanje : trenutno stanje igre
(defun remove-top (tstanje)
  (mapcar #'reverse (mapcar #'cdr (mapcar #'reverse tstanje))))

;;Vraca poteze sa vrha svakog stubica
;Params:
;tstanje : trenutno stanje igre 
(defun get-top (tstanje)
  (mapcar #'car (mapcar #'reverse tstanje)))

;;Vraca listu sa pomocnim strinogivma u zavisnosti od velicine igre
;Params:
;N : ukupan broj stubica na tabli
(defun get-support-chars (N)
  (cond 
   ((equalp 0 N) ())
   (t (append (get-support-chars (1- N)) (list (char chars-print (1- N)))))))


;; Vraca listu sa spejs karakterima
;Params
;N : duzina liste koja ce biti generisana

(defun space-list (N)
  (cond 
   ((equalp 0 N) ())
   (t (cons '#\Space (space-list (1- N))))
   ))

;; Pretvara stubic u stanje sa spejsovima, pogodno za print
;Params
;lista : proizvoljno stanje u igri
;n : pomocna !!! Pozvati sa 0
(defun space-stanje (lista n)
  (cond
   ((null lista) '())
   (t (cons 
       (append (space-list n) 
               (car lista) 
               (space-list (- (- (sqrt Nstubica) 1) n)))
       (space-stanje (cdr lista) (mod (1+ n) (sqrt Nstubica))))
   )))

;; pomocna funkcija za celokupan print
; pretvara iz horizontalnog u vetrikalni format
;Params:
;tstanje : trenutno stanje
;N : ukupan broj stubica 

(defun print-stanje (tstanje N)
  (cond 
   ((null (caar tstanje)) ())
   (t (let 
          ((pstanje (get-top tstanje)) 
           (sstanje (remove-top tstanje)))
        (format t "~%~A" pstanje)
        (print-stanje sstanje N)
          ))
  ))

;; Print sa pomocnim linijama
;Params:
;tstanje : trenutno stanje igre
(defun print-glavna (tstanje)
  (let ()
    (format t "~%~A" (get-support-chars Nstubica))
    (print-stanje (space-stanje tstanje 0) Nstubica)
    (format t "~%~A" (get-support-chars Nstubica))
    ))



;;Test primeri
;(format t "~%~A" (get-support-chars 16))
;(remove-top '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)))
;(get-top '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -) (x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -) (x x x x) (x - - -) (x o - -) (x x x -)))
;(print-stanje '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)) 6)

;(print-glavna stanjeigre)


;;;;; =========================================================================== ;;;;;


;; 5. Proverava da li je potez validan 

;; Proverava da li je potez validan
;Params:
;stanje : trenutno stanje
;broj-stubica : broj stubica koji se proverava

(defun validanp (stanje broj-stubica)
  (cond
   ((null broj-stubica) '())
   ((> broj-stubica Nstubica) '())
   ((< broj-stubica 0) '())
   (t (stubicp (nth broj-stubica stanje)))))

;;Proverava da li je stubic validan
;Params:
;lista : stubic sa potezima
(defun stubicp (lista)
  (cond
   ((null lista) '())
   ((equalp (car lista) '-) t)
   (t (OR '() (stubicp (cdr lista))))))


;Test primeri
;(validanp '((x o x o) (x x x x) (x - - -)) 2)
;(validanp '((x o x o) (x x x x) (x - - -)) 1)

;;;;; =========================================================================== ;;;;;

;; 6. Omogucava da igrac igra

;; Funkcija kojom se vraca novo stanje
;Params:
;stanje : trenutno stanje igre
;igrac : x / o
;broj-stubica : broj stubica koji se igra
(defun odigraj (stanje igrac broj-stubica)
  (cond
   ((validanp stanje broj-stubica) (izmeni stanje igrac broj-stubica))
   (t stanje)))

;;
;Params:
;stanje : trenutno stanje igre
;igrac : x / o
;broj-stubica : broj stubica koji se igra
(defun izmeni (stanje igrac broj-stubica)
  (cond
   ((= 0 broj-stubica) (cons (dodaj (car stanje) igrac) (cdr stanje)))
   (t (cons (car stanje) (izmeni (cdr stanje) igrac (- broj-stubica 1))))))

;;
;Params:
;stubic : lista sa potezima za odredjeni stubic
;igrac : x / o
(defun dodaj (stubic igrac)
  (cond
   ((equalp (car stubic) '-) (cons igrac (cdr stubic)))
   (t (cons (car stubic) (dodaj (cdr stubic) igrac)))))

;Test primeri:
;(odigraj '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)) 'x 13)
;(odigraj '((x o x o) (x x x x) (x - - -) (x o - -) (x x x -) (x - - -)) 'o 2)