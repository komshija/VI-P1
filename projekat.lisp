;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; KOD ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;=====================================================================================================================;
;===================================================== PRVA FAZA =====================================================;
;=====================================================================================================================;

;; Globalne promenjive
; Nstubica : ukupan broj stubica
; prvi-igrac : igrac koji igra prvi
;;;;; =========================================================================== ;;;;;


;; 1. Definisati nacin za predstavljanje stanja problema i 
;;    napisati funkciju za postavljanje pocetnog stanja na osnovu zadate velicine kocke

;; Funkcija koja inicijalizuje stanje i ukuban broj stubica
;Param: 
;N : koliko poteza mogu da stanu na jednom stubicu

(defun init-stanje (N)
  (let () 
    (setq Nstubica (* N N))
    (init-stanje-pom (* N N))
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


;; 2. Napisati funkcije za testiranje kraja igre

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


;; 3. Omoguciti izbor ko ce igrati prvi (covek ili racunar)
;; Prvi igra uvek igrac X, a drugi igrac O


;; Pokrece igru, korisnik bira ko igra prvi, 
;takodje bira velicinu table sve dok ne unese validnu vrednost
;Params:
;
(defun start-igra ()
  (let* 
      ((prvi (progn (format t "~%Unesite da li covek igra prvi ['t / ()] : ") (read)))
       (N (unesite-N)))
    (setq prvi-igrac prvi)
    (init-stanje N)
  ))

;Unos velicine table tj visine stubica
;Params:
;
(defun unesite-N () 
  (let* 
      ((N (progn (format t "~%Unesite koliko je velika tabla [paran broj; min 4] : ") (read) )))
    (cond 
     ((equalp 1 (mod N 2)) (unesite-N))
     ((< N 4) (unesite-N))
     (t N))))


;Test primeri
;(start-igra)
;(unesite-N)


;;;;; =========================================================================== ;;;;;


;; 4. Implementirati funkcije koje obezbedjuju prikaz proizvoljnog stanja problema (igre)

;;Postavlja string kao pomoc za print
; Sluzi pri generisanju pomocnih linija pri prikazu

(setq chars-print "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")

;;Sklanja sve poteze sa vrha svakog stubica
;Params:
;tstanje : trenutno stanje igre
(defun remove-top (tstanje)
  (cond 
    ((null tstanje) ())
    (t (cons (reverse (cdr (reverse (car tstanje)))) (remove-top (cdr tstanje))))))


;;Vraca poteze sa vrha svakog stubica
;Params:
;tstanje : trenutno stanje igre 
(defun get-top (tstanje)
  (cond 
    ((null tstanje) ())
    (t (cons (car (reverse (car tstanje))) (get-top (cdr tstanje))))))


;;Vraca listu sa pomocnim strinogivma u zavisnosti od velicine igre
;Params:
;N : ukupan broj stubica na tabli

(defun get-support-chars (N)
  (cond 
   ((equalp 0 N) ())
   (t (append (get-support-chars (1- N)) (list (char chars-print (1- N)))))))


;; Vraca listu sa blanko karakterima
;Params
;N : duzina liste koja ce biti generisana

(defun space-list (N)
  (cond 
   ((equalp 0 N) ())
   (t (cons '#\Space (space-list (1- N))))
   ))

;; Pretvara stubic u stanje sa blanko znakovima, pogodno za print
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

;(print-glavna (init-stanje 4))


;;;;; =========================================================================== ;;;;;


;; 5. Realizovati funkcije koje na osnovu zadatog poteza igraca, u obliku broj-stubica omogucava:
;         - proveru da li je potez valjan
;         - ako jeste, promenu prosledjenog stanja problema (igre) odigravanjem poteza

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

;; Kreira funkcije za prevodjenje dekatnih brojeva u heksadekande i obrnuto
;Params:
;N : ukupan broj stubica 
;brStubica : broj stubica, ali heksadekadna vrednost
(defun kreiraj-associjativnu (N)
  (cond
    ((equalp N 0) '((#\0 0)))
    (t (cons (list  (char chars-print N) N) (kreiraj-associjativnu (1- N)) ))))

(defun convert-broj-stubica (brStubica) 
  (cadr (assoc brStubica (kreiraj-associjativnu (1- Nstubica)))))

;Test primeri:
;(kreiraj-associjativnu 16)
;(convert-broj-stubica (char-upcase #\0))

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


;======================================================================================================================;
;===================================================== DRUGA FAZA =====================================================;
;======================================================================================================================;


;; 6. Realizovati funkcije koje obezbedjuju odigravanje partije izmedju dva igraca

;; Rekuzivna petlja za igranje igre
;;Params
; igrac : x / o
; tstanje : trenutno stanje igre

(defun potez (igrac tstanje)
  (cond 
    ((krajp tstanje) 
      (let* ()
        (format t "~%Kraj igre.. Racunam pobednika..~%")
        (vrati-pobednika tstanje))) ;; testira na kraj, => raucna pobednika ako je kraj
    
    (t (let* (
      (temp (print-glavna tstanje)) ;;stampa
      (odigran-stubic (progn (format t "~%Unesite stubic koji hocete da odigrate [ ~A ] :" igrac ) (convert-broj-stubica (char-upcase (read-char)))));;unos poteza
    )
    (clear-input);; clearuje input, kad zove read-char unese i slovo + \n, i onda se desava da stampa dva puta..
    (cond 
      ((validanp tstanje odigran-stubic)  (format t "~%Potez vraca: ~A " (potez (if (equalp igrac 'x) 'o 'x) (odigraj tstanje igrac odigran-stubic))));; igra sledeci
      (t (potez igrac tstanje))))));; igra opet, potez nevalidan
)

;; Funkcija koja pokrece celu igru
;Params
;
(defun igraj-connect-four () 
  (let* ((tstanje (start-igra)))
    (potez 'x tstanje)))

;;Test:
;(igraj-connect-four)

;Params
;tstanje : trenutno stanje
;igrac : x/o
(defun vrati-moguca-stanja (tstanje igrac)
  (format t "~A"  (moguca-stanja tstanje igrac 0 NStubica)))

;;Pomocna funkcija koja igra potez od 0 do ukupnoStubica, tj igra virtuelene poteze

(defun moguca-stanja (tstanje igrac brStubic ukupnoStubica)
  (cond
    ((equalp ukupnoStubica brStubic) '())
    (t (cons (odigraj tstanje igrac brStubic) (moguca-stanja tstanje igrac (1+ brStubic) ukupnoStubica)))
  ))

;(format t "~A" (vrati-moguca-stanja (init-stanje 4) 'X))


;;;;; =========================================================================== ;;;;;


;; Funkcija koja stampa ko je pobednik

(defun vrati-pobednika (tstanje)
  (let* 
    ((X-spojene (+ (prebroj-horizonatalne tstanje 'X) (prebroj-vertikalne tstanje 'X) (prebroj-dijagonalne tstanje 'X)))
      (O-spojene (+ (prebroj-horizonatalne tstanje 'O) (prebroj-vertikalne tstanje 'O) (prebroj-dijagonalne tstanje 'O))))
    (cond 
      ((equalp X-spojene O-spojene) (format t "Nereseno!"))
      ((> X-spojene O-spojene) (format t "Pobednik je X."))
      (t (format t "Pobednik je O.")))))

;; Funkcija koja broji 4 ponavljanja karaktera u listi
;;Params
;lista : lista koja se prosledjuje
;karakter : karakter koji se broji
;br : 0
(defun prebroj-cetri (lista karakter br)
  (cond 
    ((null lista) 0)
    ((and (equalp br 3) (equalp karakter (car lista))) (1+ (prebroj-cetri (cdr lista) karakter br)))
    ((equalp karakter (car lista)) (prebroj-cetri (cdr lista) karakter (1+ br)))
    (t (prebroj-cetri (cdr lista) karakter 0))))


;(prebroj-cetri '(X X X X) 'X 0)


;; Broji vertikalno na stubicima koliko ima spojenih 
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji

(defun prebroj-formatiranu-listu (tstanje igrac) 
  (cond
    ((null tstanje) 0)
    (t (+ (prebroj-cetri (car tstanje) igrac 0) (prebroj-vertikalne (cdr tstanje) igrac)))))

(defun prebroj-vertikalne (tstanje igrac)
  (prebroj-formatiranu-listu tstanje igrac))


;; Transformise listu iz N*N atoma u N listi od N atoma
;;Params
;lista : lista koja se transformisa
;N : 'stranica' 

(defun transformisi (lista N)
  (cond 
    ((not (equalp (length lista) (* N N))) '())
    (t (transformisi-tops lista N '()))
))

;; Zvati preko tranformisi, vise je enkapsulirano
(defun transformisi-tops (lista N pom) 
  (cond   
    ((null lista) (list pom))
    ((equalp N (length pom)) (cons pom (transformisi-tops (cdr lista) N (list (car lista)))))
    (t (transformisi-tops (cdr lista) N (cons (car lista) pom)))
  ))

;(transformisi-tops (get-top (init-stanje 6)) 6 '())


(defun transponuj (lista)
  (cond
    ((null (caar lista)) ())
    (t (cons (get-top lista) (transponuj (remove-top lista))))
  ))

;(transponuj '((x o x o) (x x x x) (o x x o) (x o x o)))


;; Broji horizonatalno na stubicima koliko ima spojenih 
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji

;;; NISAM SIGURAN 1000% dal je ovo ispravno, tj dal radi ispravno, za neke proste slucajeve moze da se kaze da vraca tacno

(defun prebroj-horizonatalne (tstanje igrac) 
  (cond 
   ((null (caar tstanje)) 0)
   (t (let 
          ((pstanje (transformisi (get-top tstanje) 4)) ;; za sad zovem za 4 
           (sstanje (remove-top tstanje)))
          (+ 
            (prebroj-formatiranu-listu pstanje igrac)
            (prebroj-formatiranu-listu (transponuj pstanje) igrac)
            (prebroj-horizonatalne sstanje igrac)
          )
   ))))


;;(print-glavna '((x o x o) (x x x x) (o x x o) (x o x o) (x o x o) (x x x x) (o x x o) (x o x o) (x o x o) (x x x x) (o x x o) (x o x o) (x o x o) (x x x x) (o x x o) (x o x o)))

;; (prebroj-horizonatalne '((x o x o) (x o x o) (x o x o) (x o x o) 
;;                          (x o x o) (x o x o) (x o x o) (x o x o) 
;;                          (x o x o) (x o x o) (x o x o) (x o x o) 
;;                          (x o x o) (x o x o) (x o x o) (x o x o)) 'X)

                         ;;expected 16, actual 16


;; Broji dijagonalno na stubicima koliko ima spojenih 
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji

(defun prebroj-dijagonalne (tstanje igrac) 
  (+ 0 0)
)

