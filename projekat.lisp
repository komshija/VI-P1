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
(init-stanje 4)
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

;; Generise random stanje za proveru koda
;;Params
; istanje : inicijalno stanje
; pom : pomocni brojac za kretanje kroz stubic 0
; rb : broj stubica gde se ubacuje x ili o
(defun generisi-random (istanje pom rb) 
    (cond 
      ((equalp pom (* (isqrt Nstubica) Nstubica)) (odigraj istanje (if (equalp 0 (random 2)) 'x 'o) rb))
      ((< pom (isqrt Nstubica)) (generisi-random (odigraj istanje (if (equalp 0 (random 2)) 'x 'o) rb) (1+ pom)  rb))
      ((equalp (mod pom (isqrt Nstubica)) 0) (generisi-random (odigraj istanje (if (equalp 0 (random 2)) 'x 'o) rb) (1+ pom) (1+ rb)))
      (t (generisi-random (odigraj istanje (if (equalp 0 (random 2)) 'x 'o) rb) (1+ pom) rb))))


;; 6. Realizovati funkcije koje obezbedjuju odigravanje partije izmedju dva igraca

;; Rekuzivna petlja za igranje igre
;;Params
; igrac : x / o
; tstanje : trenutno stanje igre

;; (defun potez (igrac tstanje)
;;   (cond 
;;     ((krajp tstanje) 
;;       (let* ()
;;         (format t "~%Kraj igre.. Racunam pobednika..~%")
;;         (vrati-pobednika tstanje))) ;; testira na kraj, => raucna pobednika ako je kraj
    
;;     (t (let* (
;;       (temp (print-glavna tstanje)) ;;stampa
;;       (odigran-stubic (progn (format t "~%Unesite stubic koji hocete da odigrate [ ~A ] :" igrac ) (convert-broj-stubica (char-upcase (read-char)))));;unos poteza
;;     )
;;     (clear-input);; clearuje input, kad zove read-char unese i slovo + \n, i onda se desava da stampa dva puta..
;;     (cond 
;;       ((validanp tstanje odigran-stubic)  (potez (if (equalp igrac 'x) 'o 'x) (odigraj tstanje igrac odigran-stubic)));; igra sledeci
;;       (t (potez igrac tstanje))))));; igra opet, potez nevalidan
;; )

;;Petlja u kojoj se igra protiv AI

(defun potez (igrac tstanje covek)
  (cond 
    ((krajp tstanje) 
      (let* ()
        (format t "~%Kraj igre.. Racunam pobednika..~%")
        (vrati-pobednika tstanje))) ;; testira na kraj, => raucna pobednika ako je kraj
    
    (t (let* (
      (temp (print-glavna tstanje)) ;;stampa
      (novo-stanje 
          (cond
            (covek (input-potez tstanje igrac))
            (t (car (minmax tstanje 4 -5000 5000  (not prvi-igrac)))))
      );;unos poteza
    )
    (potez (if (equalp 'x igrac) 'o 'x) novo-stanje (not covek))
))))
    
(defun input-potez (tstanje igrac)
  (let* 
    (
      (odigran-stubic (progn (format t "~%Unesite stubic koji hocete da odigrate [ ~A ] :" igrac ) (convert-broj-stubica (char-upcase (read-char)))))
    )
    (clear-input);; clearuje input, kad zove read-char unese i slovo + \n, i onda se desava da stampa dva puta..
    (cond
      ((validanp tstanje odigran-stubic)  (odigraj tstanje igrac odigran-stubic))
      (t (progn (format t "~%Nevalidan potez, odigrajte ponovo.")
                (input-potez tstanje igrac)))
    )))


;; Funkcija koja pokrece celu igru
;Params
;

;; (defun igraj-connect-four () 
;;   (let* ((tstanje (start-igra)))
;;     (potez 'x tstanje)))

;;Funkcija u kojoj se pokrece igra za igru protiv AI

(defun igraj-connect-four () 
  (let* ((tstanje (start-igra)))
    (potez 'x tstanje prvi-igrac)))

;;Test:
;; (igraj-connect-four)


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
   (t
    (let (
          (novoStanje (odigraj tstanje igrac brStubic))
          )
          (
           if (equal tstanje novoStanje) 
               (moguca-stanja tstanje igrac (1+ brStubic) ukupnoStubica)
             (cons novoStanje (moguca-stanja tstanje igrac (1+ brStubic) ukupnoStubica))
          )
         )
      )
    )
   )
 





;(format t "~A" (vrati-moguca-stanja (init-stanje 4) 'X))
;(init-stanje 2)

;;;;; =========================================================================== ;;;;;


;; Funkcija koja stampa ko je pobednik
;; Funkcija poziva sve ostale funkcije za prebrojavanje

(defun vrati-pobednika (tstanje)
  (let* 
    ((X-spojene (+ (prebroj-horizonatalne tstanje 'X) (prebroj-vertikalne tstanje 'X) (prebroj-dijagonalne tstanje 'X)))
      (O-spojene (+ (prebroj-horizonatalne tstanje 'O) (prebroj-vertikalne tstanje 'O) (prebroj-dijagonalne tstanje 'O))))
    (cond 
      ((equalp X-spojene O-spojene) (format t "Nereseno!"))
      ((> X-spojene O-spojene) (format t "Pobednik je X."))
      (t (format t "Pobednik je O.")))))

;; Funkcija koja broji 4 ponavljanja karaktera u listi, koristi se za prebrojavanje poena
; Za (x x x x x x) rezultat je 3
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

;; Za svaki element u listi poziva prebroj-cetri, gde je tstanje prava lista
;;Params
;tstanje : prava lista, lista koja se broji
;igrac : igrac za koga se broji

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

;;Transformisi tops, pomocna funkcija za transformisi
;; Napomena: Zvati preko tranformisi, vise je enkapsulirano

(defun transformisi-tops (lista N pom) 
  (cond   
    ((null lista) (list pom))
    ((equalp N (length pom)) (cons pom (transformisi-tops (cdr lista) N (list (car lista)))))
    (t (transformisi-tops (cdr lista) N (cons (car lista) pom)))
  ))

;(transformisi-tops (get-top (init-stanje 6)) 6 '())

;; Transponuje listu, tj ako listu posmatramo kao matricu, vratice transponovanu matricu
;;Params
;lista : lista koja se transponuje

(defun transponuj (lista)
  (cond
    ((null (caar lista)) ())
    (t (cons (get-top lista) (transponuj (remove-top lista))))
  ))

;(transponuj '((x o x o) (x x x x) (o x x o) (x o x o)))


;; Broji horizonatalno na stubicima koliko ima spojenih, u oba smera i dijagonalno
;; Prakticno ako se sa svakog stubica skine po jedan potez i napravi se matrica od toga,
;; na tome se broji
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji
(defun prebroj-horizonatalne (tstanje igrac) 
  (cond 
   ((null (caar tstanje)) 0)
   (t (let 
          ((pstanje (transformisi (get-top tstanje) (isqrt Nstubica)))  
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
;;                          (x o x o) (x o x o) (x o x o) (x o x o)))

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

;; Vraca sve dijagonale na jednom sloju 
;;Params
;sloj : izdvojeni sloj
;igrac : igrac za kog se broji
(defun vrati-dijagonale-sloja (sloj)
  (append (vrati-dijagonale-iznad-glavne sloj)
          (vrati-dijagonale-iznad-glavne (mapcar 'reverse sloj))
          (vrati-dijagonale-ispod-glavne (cdr sloj))
          (vrati-dijagonale-ispod-glavne (mapcar 'reverse (cdr sloj)))
          ))

;; Vraca sve dijagonale na svim bocnim slojevima
;;Params
;tstanje : trenutno stanje
;stub : prvi stub u sloju
(defun vrati-dijagonale-bocnih-slojeva (tstanje stub)
  (cond
    ((= stub (isqrt Nstubica)) '())
    (t(append (vrati-dijagonale-sloja (vrati-bocni-sloj tstanje stub)) (vrati-dijagonale-bocnih-slojeva tstanje (1+ stub))))
   ))

;; Vraca sve dijagonale na prednjih slojeva sloju 
;;Params
;tstanje : trenutno stanje
;stub : prvi stub u sloju
(defun vrati-dijagonale-prednjih-slojeva (tstanje stub)
  (cond
   ((= stub Nstubica) '())
   (t(append (vrati-dijagonale-sloja (vrati-prednji-sloj tstanje stub (isqrt Nstubica))) (vrati-dijagonale-prednjih-slojeva tstanje (+ stub (isqrt Nstubica)))))))


;; Vraca dijagonalni sloj proslednjene kocke tj stanja
;;Params
;tstanje : trenutno stanje
;N : pomocni brojac koji krece od 0 i povecava se za (isqrt Nstubica) + 1
(defun vrati-dijagonalu-kocke (tstanje N)
  (cond
   ((null (nth N tstanje)) '())
   (( and (= (mod N (isqrt Nstubica)) 0 ) (not(= N 0))) ())
   (t(cons (nth N tstanje) ( vrati-dijagonalu-kocke tstanje (+ N (1+ (isqrt Nstubica))))))))


;; Vraca dijagonalne slojeve ispod glavne dijagonale zakljucno sa glavnom dijagonalom
;;Params
;tstanje : trenutno stanje
;N : pomocni brojac koji krece od 0 i uvecava za 1 dok je 4 > ((isqrt Nstubica) - N)
(defun vrati-dijagonale-ispod-glavne-d-kocke (tstanje N)
  (cond 
   ((< (- (isqrt Nstubica) N) 4) '())
   (t(cons (vrati-dijagonalu-kocke tstanje N) (vrati-dijagonale-ispod-glavne-d-kocke tstanje (1+ N))))))


;; Okrece redosled stubova unazad na svakom prednjem sloju 
;;Params
;tstanje : trenutno stanje
;N : pomocni brojac koji se uvecava za (isqrt Nstubica)
(defun okreni-kocku (tstanje N)
  (cond
  ((or (= N Nstubica) (null (nth N tstanje))) '())
   (t(append (reverse (vrati-prednji-sloj tstanje N (isqrt Nstubica))) (okreni-kocku tstanje (+ N (isqrt Nstubica)))))))


;; Sklanja prednji sloj kocke 
;;Params
;tstanje : trenutno stanje
(defun remove-front (tstanje)
  (nthcdr (isqrt Nstubica) tstanje))


;; Vraca dijagonalne slojeve iznad glavne dijagonale 
;Params
;tstanje : trenutno stanje
;N : pomocni brojac koji krece od 0 i uvecava za 1 dok je 4 > ((isqrt Nstubica) - N - 1)
(defun vrati-dijagonale-iznad-glavne-d-kocke (tstanje N)
  (cond
   ((< (- (- (isqrt Nstubica) 1) N) 4) '())
   (t(cons (vrati-dijagonalu-kocke tstanje 0) (vrati-dijagonale-iznad-glavne-d-kocke (remove-front tstanje) (1+ N))))))


;; Vraca sve moguce dijagonalne slojeve
;Params
;tstanje : trenutno stanje
(defun vrati-sve-dijagonalne-slojeve (tstanje)
  (append (vrati-dijagonale-ispod-glavne-d-kocke tstanje 0)
          (vrati-dijagonale-ispod-glavne-d-kocke (okreni-kocku tstanje 0) 0)
          (vrati-dijagonale-iznad-glavne-d-kocke (remove-front tstanje) 0)
          (vrati-dijagonale-iznad-glavne-d-kocke (okreni-kocku (remove-front tstanje) 0) 0)))


;;Vraca sve dijagonale na svim dijagonalnim slojevima
;Params
;slojevi : dijagonalni slojevi
(defun vrati-dijagonale-dijagonalnih-slojeva (slojevi)
  (cond
   ((null slojevi) '())
   (t(append (vrati-dijagonale-sloja (car slojevi)) (vrati-dijagonale-dijagonalnih-slojeva (cdr slojevi))))))

;;Vraca dijagonale svih gornjih slojeva
;Params
;tstanje : trenutno stanje
(defun vrati-dijagonale-gornjih-slojeva (tstanje)
  (cond
   ((null (caar tstanje)) '())
   (t (append (vrati-dijagonale-sloja (transformisi (get-top random-stanje) (isqrt Nstubica))) (vrati-dijagonale-gornjih-slojeva (remove-top tstanje))))))

;;Vraca listu svih mogucih dijagonala u igri
;Params
;tstanje : trenutno stanje
(defun vrati-sve-dijagonale (tstanje)
  (append (vrati-dijagonale-prednjih-slojeva tstanje 0)
          (vrati-dijagonale-bocnih-slojeva tstanje 0)
          (vrati-dijagonale-gornjih-slojeva random-stanje)
          (vrati-dijagonale-dijagonalnih-slojeva (vrati-sve-dijagonalne-slojeve tstanje))))

;; Broji sve dijagonale u trenutnom stanju 
;;Params
;tstanje : trenutno stanje
(defun prebroj-dijagonalne (tstanje igrac)
  (prebroj-formatiranu-listu (vrati-sve-dijagonale tstanje) igrac))

;Test primeri :
;(setq Nstubica 16)
;(setq sloj '((x x o o) (x x o o) (o o x x) (o o x x)))

;(setq s '((x x o o) (x x o o) (o o x x) (o o x x)
;          (x x o o) (x x o o) (o o x x) (o o x x) 
;         (x x o o) (x x o o) (o o x x) (o o x x) 
;          (x x o o) (x x o o) (o o x x) (o o x x)))

;(vrati-bocni-sloj s 0)

;(vrati-dijagonale-iznad-glavne (mapcar 'reverse sloj))
;(vrati-dijagonale-iznad-glavne sloj)
;(vrati-dijagonale-ispod-glavne (cdr sloj))

;(vrati-dijagonalu-kocke random-stanje 0)
;(vrati-dijagonale-ispod-glavne-d-kocke random-stanje 0)
;(vrati-dijagonale-ispod-glavne-d-kocke (okreni-kocku random-stanje 0) 0)
;(vrati-dijagonale-iznad-glavne-d-kocke (remove-front random-stanje) 0)
;(vrati-dijagonale-iznad-glavne-d-kocke (okreni-kocku (remove-front random-stanje) 0) 0)
;(vrati-sve-dijagonalne-slojeve random-stanje)

 ;;(print-glavna random-stanje)
;(prebroj-dijagonalne random-stanje 'O) ;14 za bocne i prednje, prebrojano rucno!
;(vrati-pobednika random-stanje)

;(setq random-stanje (generisi-random (init-stanje 4) 0 0))
;(print-glavna random-stanje)
;(print(vrati-dijagonale-bocnih-slojeva random-stanje 0))
;(print(vrati-dijagonale-prednjih-slojeva random-stanje 0))
;(print(vrati-dijagonale-dijagonalnih-slojeva (vrati-sve-dijagonalne-slojeve random-stanje)))
;(print(vrati-dijagonale-gornjih-slojeva random-stanje))

;(print(vrati-sve-dijagonale random-stanje))
;(prebroj-dijagonalne random-stanje 'x)
;(igraj-connect-four)
;======================================================================================================================;
;===================================================== TRECA FAZA =====================================================;
;======================================================================================================================;



 

(defun minmax (stanje dubina alfa beta igrac)
  (let(
       (potezi (vrati-moguca-stanja stanje (if igrac 'X 'O)))
       )
    (cond
     ((or (zerop dubina) (null potezi)) (proceni-stanje stanje (if igrac 'X 'O)))
     ((eq igrac t) (max_igrac potezi (list (car potezi) -10000) dubina alfa beta ))
     (t (min_igrac potezi (list (car potezi) 10000) dubina alfa beta ))
     )
   )
  )


;maxPV maksimalna vrednost poteza, d dubina
(defun max_igrac (potezi maxPV d alfa beta)
  (cond
   ((null potezi) maxPV)
   (t
  (let*
        (
         (mm (minmax (car potezi) (1- d) alfa beta '()))
         (currPV (list (car potezi) (if (not (listp mm)) mm (cadr mm))))
         )
    (cond 
          ((>= alfa beta) maxPV)
          ((> (cadr currPV) alfa) (max_igrac (cdr potezi) currPV d (cadr currPV) beta))
          ((> (cadr currPV) (cadr maxPV)) (max_igrac (cdr potezi) currPV d alfa beta))
          (t (max_igrac (cdr potezi) maxPV d alfa beta))
          )
    )
    ))
 )


(defun min_igrac (potezi minPV d alfa beta)
  (cond
   ((null potezi) minPV)
   (t
  (let*
        (
         (mm (minmax (car potezi) (1- d) alfa beta t))
         (currPV (list (car potezi) (if (not (listp mm)) mm (cadr mm))))
         )
    (cond 
          ((>= alfa beta) minPV)
          ((< (cadr currPV) beta) (min_igrac (cdr potezi) currPV d alfa (cadr currPV)))
          ((< (cadr currPV) (cadr minPV)) (min_igrac (cdr potezi) currPV d alfa beta))
          (t (min_igrac (cdr potezi) minPV d alfa beta))
          )
    )
    ))
  )
;; (init-stanje 4)

;; (setq probno_stanje '((x o x o) (x o x o) (x o x o) (x o x o) (x o - -) (x o x o) (x o x o) (x o - -) (x o x o) (x o x o) (x o x o) (x o x o) (x o x o) (x o x o) (x o x o) (x o x o)))
;; (trace minmax)
;; (trace max_igrac)
;; (trace min_igrac)

;; (minmax probno_stanje 4 -5000 5000 t)


;; Procena stanja prebacena na kraj fajla

;======================================================================================================================;
;==================================================== CETVRTA FAZA ====================================================;
;======================================================================================================================;



;; Masina za zakljucivanje


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                                                ;;;;;
;;;;;                                       INFERENCE ENGINE                                         ;;;;;
;;;;;                                                                                                ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POMOCNE FUNKCIJE

;; provera da li je parametar s izvorna promenljiva (simbol koji pocinje sa ?)
(defun true-var? (s) 
  (if (symbolp s)
      (equal #\? (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s promenljiva (simbol koji pocinje sa ? ili %)
(defun var? (s) 
  (if (symbolp s)
      (let ((c (char (symbol-name s) 0)))
        (or (equal c #\?) (equal c #\%)))
    nil))

;; provera da li je parametar s funkcija (simbol koji pocinje sa =)
(defun func? (s) 
  (if (symbolp s)
      (equal #\= (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s predefinisani predikat (simbol koji pocinje sa !)
(defun predefined-predicate? (s)
  (if (symbolp s)
      (equal #\! (char (symbol-name s) 0))
    nil))

;; provera da li je parametar s konstanta (ako nije promenljiva ili funkcija onda je konstanta)
(defun const? (s)
  (not (or (var? s) (func? s))))

;; rekurzivna provera da li je parametar f funkcija od parametra x
(defun func-of (f x)
  (cond
   ((null f) ; kraj rekurzije
    t)
   ((atom f)
    (equal f x))
   (t
    (or (func-of (car f) x) (func-of (cdr f) x)))))

;; provera da li funkcija f ima promenljivih
(defun has-var (f)
  (cond
   ((null f) 
    nil)
   ((atom f)
    (var? f))
   (t
    (or (has-var (car f)) (has-var (cdr f))))))

;; funkcija koja vraca konsekvencu pravila
(defun rule-consequence (r)
  (car (last r)))

;; funkcija koja vraca premisu pravila
(defun rule-premises (r)
  (let ((p (cadr r)))
    (if (and (listp p) (equal (car p) 'and))
        (cdr p)
      (list p))))
      
;; funkcija koja vrsi prebacivanje upita u interni format (izbacuje 'and)
(defun format-query (q)
  (if (and (listp q) (equal (car q) 'and))
      (cdr q)
    (list q)))
    
;; izracunavanje istinitosne vrednosti predefinisanog predikata
(defun evaluate-predicate (p ls)
  (if (has-var p) nil  ; ako poseduje slobodne promenljive vraca nil (nije validna situacija)
    (if (eval p) 
        (list ls) ; ako predikat vazi vraca ulaznu listu smena
      nil))) ; u suprotnom vraca nil

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFEJSNE FUNKCIJE I GLOBALNE PROMENLJIVE

(defparameter *FACTS* nil)
(defparameter *RULES* nil)
(defparameter *MAXDEPTH* 10)

;; priprema *FACTS*, *RULES* i *MAXDEPTH*
(defun prepare-knowledge (lr lf maxdepth)
  (setq *FACTS* lf *RULES* (fix-rules lr) *MAXDEPTH* maxdepth))

;; vraca broj rezulata izvodjenja
(defun count-results (q)
  (length (infer- (format-query q) '(nil) 0)))

;; vraca listu lista smena
(defun infer (q)
  (filter-results (infer- (format-query q) '(nil) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE VRSE DODELU NOVIH JEDINSTVENIH PROMENLJIVIH PRAVILIMA

(defun fix-rules (lr)
  (if (null lr) nil
    (cons (fix-rule (car lr)) (fix-rules (cdr lr)))))

(defun fix-rule (r)
  (let ((ls (make-rule-ls r nil)))
    (apply-ls r ls)))

(defun make-rule-ls (r ls)
  (cond
   ((null r)
    ls)
   ((var? r)
    (let ((a (assoc r ls)))
      (if (null a)
          (cons (list r (gensym "%")) ls)
        ls)))
   ((atom r)
    ls)   
   (t
    (make-rule-ls (cdr r) 
                  (make-rule-ls (car r) ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE VRSE PRIPREMU REZULTATA (IZBACUJU SMENE KOJE SE ODNOSE NA INTERNE PROMENLJIVE)

(defun filter-results (lls)
  (if (null lls) nil
    (cons (filter-result (car lls)) (filter-results (cdr lls)))))

(defun filter-result (ls)
  (if (null ls) nil
    (if (true-var? (caar ls))
        (cons (car ls) (filter-result (cdr ls)))
      (filter-result (cdr ls)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNKCIJE KOJE SE KORISTE U IZVODJENJU

;; glavna funkcija za izvodjenje, vraca listu lista smena
;; lq - predikati upita
;; lls - lista listi smena (inicijalno lista koja sadrzi nil)
;; depth - tekuca dubina (inicijalno 0)
(defun infer- (lq lls depth)
  (if (null lq) lls
    (let ((lls-n (infer-q (car lq) lls depth)))
      (if (null lls-n) nil
        (infer- (cdr lq) lls-n depth)))))

;; izvodjenje za jedan predikat iz upita, vraca listu lista smena
(defun infer-q (q lls depth)
  (if (null lls) nil
    (let ((lls-n (infer-q-ls q (car lls) depth)))
      (if (null lls-n)
          (infer-q q (cdr lls) depth)
        (append lls-n (infer-q q (cdr lls) depth))))))

;; izvodjenje za jedan predikat sa jednom listom smena, vraca listu lista smena
(defun infer-q-ls (q ls depth)
  (if (predefined-predicate? (car q))
      (evaluate-predicate (apply-ls q ls) ls)
    (if (< depth *MAXDEPTH*)
        (append (infer-q-ls-lf q *FACTS* ls) (infer-q-ls-lr q *RULES* ls depth))
      (infer-q-ls-lf q *FACTS* ls))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; izvodjenje nad bazom cinjenica lf, vraca listu lista smena
(defun infer-q-ls-lf (q lf ls)
  (if (null lf) nil
    (let ((ls-n (infer-q-ls-f q (car lf) ls)))
      (if (null ls-n)
          (infer-q-ls-lf q (cdr lf) ls)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lf q (cdr lf) ls)))))))

;; izvodjenje sa jednom cinjenicom, vraca listu sa listom smena
(defun infer-q-ls-f (q f ls)
  (if (= (length q) (length f)) ; provera na istu duzinu
      (infer-q-ls-f- q f ls)
    nil))

;; izvodjenje sa jednom cinjenicom, vraca listu sa listom smena
(defun infer-q-ls-f- (q f ls)
  (if (null q) (list ls)
    (let ((nq (apply-and-eval (car q) ls)) (nf (car f)))
      (if (var? nq) 
          (infer-q-ls-f- (cdr q) (cdr f) (append ls (list (list nq nf))))
        (if (equal nq nf) 
            (infer-q-ls-f- (cdr q) (cdr f) ls)
          nil)))))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; izvodjenje nad bazom pravila, vraca listu lista smena
(defun infer-q-ls-lr (q lr ls depth)
  (if (null lr) nil
    (let ((ls-n (infer-q-ls-r q (car lr) ls depth)))
      (if (null ls-n)
          (infer-q-ls-lr q (cdr lr) ls depth)
        (if (null (car ls-n)) ls-n
          (append ls-n (infer-q-ls-lr q (cdr lr) ls depth)))))))

;; izvodjenje sa jednim pravilom, vraca listu sa listom smena
(defun infer-q-ls-r (q r ls depth)
  (let ((c (rule-consequence r)))
    (if (= (length q) (length c))
        (let ((lsc (unify q c nil ls)))
          (if (null lsc) nil
            (infer- (apply-ls (rule-premises r) (car lsc)) (cdr lsc) (1+ depth))))
      nil)))

;; unifikacija predikata upita q i konsekvence pravila c primenom liste smena ls, vraca listu smena
(defun unify (q c uls ls)
  (if (or (null q) (null c))
      (if (and (null q) (null c)) (list uls ls) nil)
    (let ((eq (car q)) (ec (car c)))
      (cond
       ((equal eq ec)
        (unify (cdr q) (cdr c) uls ls))
       ((var? eq)
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((a (assoc eq ls)))
            (cond
             ((null a)
              (unify (cdr q) (cdr c) uls (cons (list eq ec) ls)))
             ((equal (cadr a) ec)
              (unify (cdr q) (cdr c) uls ls))
             (t 
              nil))))))
       ((func? eq)
        (cond
         ((var? ec)
          (if (func-of eq ec) nil
            (let ((a (assoc ec uls)))
              (cond
               ((null a)              
                (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
               ((equal (cadr a) eq)
                (unify (cdr q) (cdr c) uls ls))
               (t
                nil)))))
         ((func? ec)
          nil)
         (t ;; const
          (let ((f (apply-ls eq ls)))
            (if (has-var f) nil
              (if (equal (eval f) ec)
                  (unify (cdr q) (cdr c) uls ls)
                nil))))))
       (t ;; const
        (cond
         ((var? ec)
          (let ((a (assoc ec uls)))
            (cond
             ((null a)              
              (unify (cdr q) (cdr c) (cons (list ec eq) uls) ls))
             ((equal (cadr a) eq)
              (unify (cdr q) (cdr c) uls ls))
             (t
              nil))))
         (t ;; func or const
          nil)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIMENA LISTE SMENA I IZRACUNAVANJE IZRAZA

(defun apply-and-eval (x ls)
  (if (var? x)
      (apply-ls x ls)
    (if (and (listp x) (func? (car x)))
        (eval (apply-ls x ls)) 
      x)))

;; primena liste smena ls na izraz x
(defun apply-ls (x ls)
  (cond
   ((null x)
    x)
   ((var? x)
    (let ((ax (assoc x ls)))
      (if (null ax) x
        (cadr ax))))
   ((atom x)
    x)
   (t
    (cons (apply-ls (car x) ls) (apply-ls (cdr x) ls)))))


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;                                                                                                ;;;;;
;;;;;                                 KRAJ MASINE ZA ZAKLJUCIVANJE                                   ;;;;;
;;;;;                                                                                                ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Rezime pravila za masinu za zakljucivanje

;; Funkcije koje se zovu u predikatu mora da imaju ime koje pocinje s =
;; Svaka promenjiva pocinje s ?
;; Predikati, tj funkcije koje vracaju true ili false pocinju s !

;; (prepare-knowledge lr lf maxdepth) - pripremiti znanje; lr : lista pravila, lf : lista cinjenica, maxdepth - maks dubina
;; (infer q)- zakljuci; q : upit
;; (count-results q ) - prebroji zakljucke, q : upit

;; upiti se spajaju s AND
;; (AND (voli 'marko 'milica ) ...) 

;;
 (defun proceni-stanje (tstanje igrac) ;; za pocetak dummy procena stanja
    (let* 
    ((X-spojene (+ (prebroj-horizonatalne tstanje 'X) (prebroj-vertikalne tstanje 'X) (prebroj-dijagonalne tstanje 'X)))
      (O-spojene (+ (prebroj-horizonatalne tstanje 'O) (prebroj-vertikalne tstanje 'O) (prebroj-dijagonalne tstanje 'O))))
    
    (cond 
      ((equalp igrac 'X) (- O-spojene X-spojene))
      (t (- X-spojene O-spojene))
    )))



(defun !eq (a b)
	(equal a b))
(defun !ne (a b)
	(not (equal a b)))

;; 1. Razlika broja poena, tj. ko ima vise spojenih - DONE
;; 2. Da zakljuci broj mogucih poena, npr. ako ostalo 1 polje nepopunjeno do 4 u nizu 
;; 3. Da racuna opasnost, tj. kolko protivik ima 3 spojene, ako igrac ima odredjeno npr. da gleda da sabotira nekako njega da dodbije poene
;; Mozda da svaki od ovih faktora da ima neki prioritet, tj da se mnozi s nekim koeficijentom


(setq random-stanje (generisi-random (init-stanje 4) 0 0))
;(print-glavna random-stnaje)

;; mozda zatreba
(defun get-matrix-position (stanje i j)
  (cond
    ((OR (> i Nstubica) (> j (isqrt Nstubica))) '())
    (t (nth j (nth i stanje)))
  )
)

(defun vrati-horizonatalne-stapice (tstanje) 
  (cond 
   ((null (caar tstanje)) '())
   (t (let 
          ((pstanje (transformisi (get-top tstanje) (isqrt Nstubica)))  
           (sstanje (remove-top tstanje)))

          (append 
            (list pstanje)
            (list (transponuj pstanje))
            (vrati-horizonatalne-stapice sstanje)
          )
   ))))



;; (defun vrati-cinjenice (stanje)
;;   (cond
;;     ();; neki uslov za kraj
;;     (t 
;;       (let* 
;;         (vertikalni-stapici (stanje))
;;         (horizontalni-stapici (vrati-horizontalne-stapice stanje))
;;         (dijagonalni-stapici ())
;;       )
;;     )
;;   )
;; )


