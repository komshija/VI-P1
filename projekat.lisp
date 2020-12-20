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


;; Print za potrebe testiranja
;; (defun print-glavna (tstanje)
;;   (let ()
;;     (format t "~%~A" (get-support-chars Nstubica))
;;     (print-stanje tstanje Nstubica)
;;     (format t "~%~A" (get-support-chars Nstubica))
;;     ))



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


;; Funkcija koja generise stanja u koja se moze doci na osnovu proizvoljnog stanja
;Params
;tstanje : trenutno stanje
;igrac : x/o
(defun vrati-moguca-stanja (tstanje igrac)
  (moguca-stanja tstanje igrac 0 NStubica))

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


;(prebroj-cetri '(X X X X X X) 'X 0)


;(x x x x) (x x x x)
(defun prebroj-formatiranu-listu (tstanje igrac) 
  (cond
    ((null tstanje) 0)
    (t (+ (prebroj-cetri (car tstanje) igrac 0) (prebroj-formatiranu-listu (cdr tstanje) igrac)))))

;; Broji vertikalno na stubicima koliko ima spojenih 
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji
(defun prebroj-vertikalne (tstanje igrac)
  (prebroj-formatiranu-listu tstanje igrac))


;; Transformise listu iz N*N atoma u N listi od N atoma
;;Params
;lista : lista koja se transformisa
;N : 'stranica' 
;(x x x x x x x x x x .. x) => ((x x x x) (x x x x))
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
(defun prebroj-horizonatalne (tstanje igrac) 
  (cond 
   ((null (caar tstanje)) 0)
   (t (let 
          ((pstanje (transformisi (get-top tstanje) 4)) ;; za sad zovem za 4 
           (sstanje (remove-top tstanje)))
          (+ 
            (prebroj-formatiranu-listu pstanje igrac)
            (prebroj-formatiranu-listu (transponuj pstanje) igrac)
            (prebroj-dijagonale-sloja pstanje igrac) ;kad vec ovde obilazimo prebroji dijagonale gornjeg sloja
            (prebroj-horizonatalne sstanje igrac)
          )
   ))))


;;(print-glavna '((x o x o) (x x x x) (o x x o) (x o x o) (x o x o) (x x x x) (o x x o) (x o x o) (x o x o) (x x x x) (o x x o) (x o x o) (x o x o) (x x x x) (o x x o) (x o x o)))

;; (prebroj-horizonatalne '((x o x o) (x o x o) (x o x o) (x o x o) 
;;                          (x o x o) (x o x o) (x o x o) (x o x o) 
;;                          (x o x o) (x o x o) (x o x o) (x o x o) 
;;                          (x o x o) (x o x o) (x o x o) (x o x o)) 'X)

                         ;;expected 16, actual 16


;; Izdvaja bocni sloj 
;;Params
;tstanje : trenutno stanje
;stub : prvi stub u tom sloju koji izdvaja 
(defun vrati-bocni-sloj (tstanje stub)
  (cond 
   ((>= stub Nstubica) '())
   (t(cons (nth stub tstanje) (vrati-bocni-sloj tstanje (+ (isqrt Nstubica) stub))))))

;; Izdvaja prednji sloj 
;;Params
;tstanje : trenutno stanje
;stub : prvi stub u tom sloju koji izdvaja
;N : pomocni brojac koji ide od (isqrt Nstubica) do 0
(defun vrati-prednji-sloj (stanje stub N)
  (cond
   ((= N 0) '())
   (t(cons (nth stub stanje) (vrati-prednji-sloj stanje (1+ stub) (1- N))))))

;; Vraca glavnu dijagonalu prosledjenog sloja
;;Params
;sloj : izdvojeni sloj
(defun vrati-dijagonalu (sloj)
  (cond
   ((null sloj) '())
   ((null (car sloj)) '())
   (t(cons (caar sloj) ( vrati-dijagonalu ( mapcar 'cdr (cdr sloj)))))))

;; Vraca sve validne dijagonale iznad glavne, ukljucujuci i glavnu
;;Params
;sloj : izdvojeni sloj
(defun vrati-dijagonale-iznad-glavne (sloj)
  (cond
   ((<(length (car sloj)) 4) '())
   (t (cons (vrati-dijagonalu sloj) (vrati-dijagonale-iznad-glavne (mapcar 'cdr sloj))))))

;; Vraca sve validne dijagonale ispod glavne
;;Params
;sloj : izdvojeni sloj
(defun Vrati-dijagonale-ispod-glavne (sloj)
  (cond
   ((< (length (mapcar 'car sloj)) 4) '())
   (t(cons (vrati-dijagonalu sloj) (Vrati-dijagonale-ispod-glavne (cdr sloj))))))

;; Broji sve dijagonale na jednom sloju 
;;Params
;sloj : izdvojeni sloj
;igrac : igrac za kog se broji
(defun prebroj-dijagonale-sloja (sloj igrac)
  (+ (prebroj-formatiranu-listu (vrati-dijagonale-iznad-glavne sloj) igrac)
     (prebroj-formatiranu-listu (vrati-dijagonale-iznad-glavne (mapcar 'reverse sloj)) igrac)
     (prebroj-formatiranu-listu (vrati-dijagonale-ispod-glavne (cdr sloj)) igrac)
     (prebroj-formatiranu-listu (vrati-dijagonale-ispod-glavne (mapcar 'reverse (cdr sloj))) igrac)
  ))

;; Broji sve dijagonale na svim bocnim slojevima
;;Params
;tstanje : trenutno stanje
;stub : prvi stub u sloju
;igrac : igrac za kog se broji
(defun prebroj-dijagonale-bocnih-slojeva (tstanje stub igrac)
  (cond
    ((= stub (isqrt Nstubica)) 0)
    (t(+ (prebroj-dijagonale-sloja (vrati-bocni-sloj tstanje stub) igrac) (prebroj-dijagonale-bocnih-slojeva tstanje (1+ stub) igrac)))
  ))

;; Broji sve dijagonale na jednom sloju 
;;Params
;tstanje : trenutno stanje
;stub : prvi stub u sloju
;igrac : igrac za kog se broji
(defun prebroj-dijagonale-prednjih-slojeva (tstanje stub igrac)
  (cond
   ((= stub Nstubica) 0)
   (t(+ (prebroj-dijagonale-sloja (vrati-prednji-sloj tstanje stub (isqrt Nstubica)) igrac) (prebroj-dijagonale-prednjih-slojeva tstanje (+ stub (isqrt Nstubica)) igrac)))))


;; Broji sve dijagonale u trenutnom stanju 
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji
(defun prebroj-dijagonalne (tstanje igrac) ;fali da se doda za dijagonalne slojeve
  (+ (prebroj-dijagonale-prednjih-slojeva tstanje 0 igrac)
     (prebroj-dijagonale-bocnih-slojeva tstanje 0 igrac)
     
  ))


;;treba se napise ovde horizontalni slojevi




;Test primeri
;(setq Nstubica 16)
;(setq sloj '((x x o o) (x x o o) (o o x x) (o o x x)))

;(setq s '((x x o o) (x x o o) (o o x x) (o o x x)
;          (x x o o) (x x o o) (o o x x) (o o x x) 
;         (x x o o) (x x o o) (o o x x) (o o x x) 
;          (x x o o) (x x o o) (o o x x) (o o x x)))

;(prebroj-dijagonale-prednjih-slojeva s 0 'X)
;(trace prebroj-dijagonale-bocnih-slojeva)
;(vrati-bocni-sloj s 0)

;(prebroj-dijagonale-sloja sloj 'X)
;(vrati-dijagonale-iznad-glavne (mapcar 'reverse sloj))
;(vrati-dijagonale-iznad-glavne sloj)
;(vrati-dijagonale-ispod-glavne (cdr sloj))
;(prebroj-dijagonalne s 'X)

(defun generisi-random (istanje pom rb) 
    (cond 
      ((equalp pom (* (isqrt Nstubica) Nstubica)) (odigraj istanje (if (equalp 0 (random 2)) 'x 'o) rb))
      ((< pom (isqrt Nstubica)) (generisi-random (odigraj istanje (if (equalp 0 (random 2)) 'x 'o) rb) (1+ pom)  rb))
      ((equalp (mod pom (isqrt Nstubica)) 0) (generisi-random (odigraj istanje (if (equalp 0 (random 2)) 'x 'o) rb) (1+ pom) (1+ rb)))
      (t (generisi-random (odigraj istanje (if (equalp 0 (random 2)) 'x 'o) rb) (1+ pom) rb))))

;(setq random-stanje (generisi-random (init-stanje 6) 0 0))
;; (setq random-stanje '((X O X X O X) (O X X X O X) (X O X X X X) (X X O O O X) (X O X O X X) (X X X X O X) (O X X X O O) (X O X X O X) (O X O O X O) (O O X X O X) (O O O O X O) (O O X O X O) (O O O X X X)
;;  (X O X O X X) (X X O X O X) (O X X O O X) (O X X O O X) (X O X O X O) (O X O O O O) (X O X X X X) (O O X O O X) (X X X X O X) (O O O O O O) (X X X X O X) (X O O X O O) (O O X O X X)
;;  (X X O O O O) (O X X X O O) (O O O O O X) (O O O O X X) (O X X X O X) (X X O O O O) (X O X X O O) (X X X X O X) (X O O O X O) (O X X X X O)))

;; (print-glavna random-stanje)
;(prebroj-dijagonalne random-stanje 'X) ;14 za bocne i prednje, prebrojano rucno!


;======================================================================================================================;
;===================================================== TRECA FAZA =====================================================;
;======================================================================================================================;


(defun max-stanje (lista-procenjenih-stanja)
  (max-stanje-i (cdr lista-procenjenih-stanja) (car lista-procenjenih-stanja)))

(defun max-stanje-i (lista-procenjenih-stanja stanje-vrednost)
  (cond ((null lista-procenjenih-stanja) stanje-vrednost)
        ((> (cadar lista-procenjenih-stanja) (cadr stanje-vrednost)) (max-stanje-i (cdr lista-procenjenih-stanja) (car lista-procenjenih-stanja)))
        (t (max-stanje-i (cdr lista-procenjenih-stanja) stanje-vrednost))))

(defun min-stanje (lista-procenjenih-stanja)
  (min-stanje-i (cdr lista-procenjenih-stanja) (car lista-procenjenih-stanja)))

(defun min-stanje-i (lista-procenjenih-stanja stanje-vrednost)
  (cond ((null lista-procenjenih-stanja) stanje-vrednost)
        ((< (cadar lista-procenjenih-stanja) (cadr stanje-vrednost)) (min-stanje-i (cdr lista-procenjenih-stanja) (car lista-procenjenih-stanja)))
        (t (min-stanje-i (cdr lista-procenjenih-stanja) stanje-vrednost))))

(defun proceni-stanje (stanje) ;; za pocetak dummy stanje
  (- 1 (random 2))
)

(defun minimax (stanje dubina moj-potez)
  (let (
        (lp (vrati-moguca-stanja stanje (if moj-potez 'X 'O)));; treba se namesti tacno ko igra
        (f (if moj-potez 'max-stanje 'min-stanje))
      )
  (cond 
    ((or (zerop dubina) (null lp)) (list stanje (proceni-stanje stanje)))
    (t (apply f (list (mapcar (lambda (x) (minimax x (1- dubina) (not moj-potez))) lp)))))))

;(trace vrati-moguca-stanja)
;(minimax (init-stanje 4) 4 t)
