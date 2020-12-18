;;;;; Vestacka Inteligencija ;;;;;
;; Rokovi za faze projekta: 
;; 1. Formulacija problema i implementacija interfejs
;;     Rok: 20.12.2020. godine
;; 2. Implementacija operatora promene stanja
;;     Rok: 3.1.2021. godine
;; 3. Implementacija Min-Max algoritma za traženje sa alfa-beta odsecanjem
;;     Rok: 10.1.2021. godine
;; 4. Definicija heuristike (procena stanja)
;;     Rok: 24.1.2021. godine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;; Projekat : 3D Four in line ;;;;;

;; Prva faza ;;

;;;; [x] Definisati nacin predstavljanja stanja problema (igre)
;;;; [x] Napisati funkciju za postavljanje pocetnog stanja na osnovu zadate velicine kocke
;;;; [x] Napisati funkcije za testiranje kraja igre
;;;; [x] Omoguciti izbor ko ce da igra prvi (covek ili racunar)
;;;; [x] Prvi igra uvek Igrac X, a drugi O
;;;; [x] Implementirati funkcije koje obezbedjuju prikaz proizvoljnog stanja problema(igre)
;;;; [x] Realizovati funkcije koje na osnovu zadatog poteza, u obliku broj_stubica ili
;(vrsta, kolona), omogucavaju: proveru da li je potez valjan, ako jeste, promenu 
;prosledjenog stanja problema(igre) odigravanjem poteza


;; Druga faza ;; 

;;;; Napisati funkcije za operatore promene stanja problema (igre) 
;; u opštem slučaju (proizvoljno stanje u kocki)
;; Na osnovu trenutne (proizvoljne) situacije u kocki (stanja) i
;; zadatog (validnog) poteza formira novu situaciju na kocki (stanje).
;; Ne menjati postojeće stanje već napraviti novo i na njemu odigrati potez.

;;;; [x] Na osnovu trenutne (proizvoljne) situacije u kocki (stanja) i
;; igrača koji je na potezu formira listu svih mogućih situacija u kocki (stanja),
;; korišćenjem funkcije iz prethodne tačke.

;; Realizovati funkcije [x]
;; [x] koje obezbeđuju odigravanje partije između dva igrača (dva čoveka, ne računara i čoveka)
;; [x] * unos poteza i provera da li je potez moguć
;; [x] *ukoliko nije moguć zahtevati unos novog poteza
;; [x] *ukoliko je moguć odigrati ga i promeniti trenutno stanje
;; [x] *prikazati novonastalo stanje sistema
;; *proveru kraja i određivanje pobednika u igri

;; Treca faza ;;

;;;; Implementirati Min-Maxalgoritam sa alfa-beta odsecanjem za zadati problem
;;;; Obezbediti da funkcija Min-Max sa alfa-beta odsecanjem ima ulazni parametar
;; kojim se definiše dubina pretraživanja
;;;; Obezbediti da funkcija Min-Max sa alfa-beta odsecanjem vrati potez koji treba
;; odigrati ili stanje u koje treba preći
;;;; **** Funkciju za određivanje heuristike ne treba implementirati 
;; **** Napraviti funkciju koja za odgovarajuća stanja vraća karakteristične
;; **** vrednosti samo u svrhu testiranja ispravnosti napravljenog Min-Maxalgoritma

;; Cetvrta faza ;;

;;;; U implementaciju Min-Max-a sa alfa-beta odsecanjem dodati funkciju 
;;za procenu stanja koja se poziva kada se dostigne zadata dubina traženja.
;;;; Implementirati funkciju koja vrši procenu stanja na osnovu pravila zaključivanja
;;;; Funkcija za procenu stanja kao parametre treba da ima oznaku igrača za kojeg računa valjanost stanja,
;;kao i samo stanje za koju se računa procena.
;;;; Procena stanja se mora vršiti isključivo korišćenjem mehanizma zaključivanja nad prethodno definisanim skupom pravila.
;; Zadatak je formulisati skup pravila i iskoristiti ih na adekvatan način za izračunavanje heuristike.
;;;; Za izvođenje potrebnih zaključaka (izvršavanje upita nad skupom činjenica kojima se opisuje stanje) 
;;koristiti mašinu za zaključivanje.
;;Implementirati funkciju koja prevodi stanje u listu činjenica ...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; KOD ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Globalne promenjive
; Nstubica : ukupan broj stubica
; prvi-igrac : igrac koji igra prvi
;;;;; =========================================================================== ;;;;;


;; 1. Definisati kako se vraca lista koja je definisana za predstavljanje problema u zavisnosti od N

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


;; 4. Treba da prikaze proizvoljno stanje

;;Postavlja string kao pomoc za print
; Sluzi pri generisanju pomocnih linija pri prikazu
(setq chars-print '(0 1 2 3 4 5 6 7 8 9 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

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
   (t (append (get-support-chars (1- N)) (list (nth (1- N) chars-print))))))


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

;;;;; =========================================================================== ;;;;;

;; Rekuzivna petlja za igranje igre
;;Params
; igrac : x / o
; tstanje : trenutno stanje igre

(defun potez (igrac tstanje)
  (cond 
    ((krajp tstanje) 
      (let* ()
        (format t "~%Kraj igre.. Racunam pobednika..~%")
        (vrati-pobednika tstanje)
      )
    
    ) ;; testira na kraj, => raucna pobednika ako je kraj

    (t (let* ((temp (print-glavna tstanje)) ;;stampa
          (odigran-stubic (progn (format t "~%Unesite stubic koji hocete da odigrate [ ~A ] :" igrac ) (read))));;unos poteza
    (cond ((validanp tstanje odigran-stubic)  (potez (if (equalp igrac 'x) 'o 'x) (odigraj tstanje igrac odigran-stubic)));; igra sledeci
      (t (potez igrac tstanje)))))));; igra opet, potez nevalidan
   

;; Funkcija koja pokrece celu igru
;Params
;
(defun igraj-connect-four () 
  (let* ((tstanje (start-igra)))
    (potez 'x tstanje)))


 (igraj-connect-four)

;;;;; =========================================================================== ;;;;;

;;Funkcija koja generise sva stanja za trenutno stnaje i igraca koji je prosledjen
;Params
;tstanje : trenutno stanje
;igrac : x/o
(defun vrati-moguca-stanja (tstanje igrac)
  (format t "~A"  (moguca-stanja tstanje igrac 0 NStubica))

;;Pomocna funkcija koja igra potez od 0 do ukupnoStubica, tj igra virtuelene poteze

(defun moguca-stanja (tstanje igrac brStubic ukupnoStubica)
  (cond
    ((equalp ukupnoStubica brStubic) '())
    (t (cons (odigraj tstanje igrac brStubic) (moguca-stanja tstanje igrac (1+ brStubic) ukupnoStubica)))
  ))

(format t "~A" (vrati-moguca-stanja (init-stanje 4) 'X))

;;;;; =========================================================================== ;;;;;

;; Funkcija koja stampa ko je pobednik

(defun vrati-pobednika (tstanje)
  (let* 
    ((X-spojene (+ (prebroj-horizonatalne tstanje 'X) (prebroj-vertikalne tstanje 'X) (prebroj-dijagonalne tstanje 'X)))
      (O-spojene (+ (prebroj-horizonatalne tstanje 'O) (prebroj-vertikalne tstanje 'O) (prebroj-dijagonalne tstanje 'O))))
    (cond ((> X-spojene O-spojene) (format t "Pobednik je X."))
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


;; Broji vertikalno na stubicima koliko ima spojenih 
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji

(defun prebroj-vertikalne (tstanje igrac) 
  (cond
    ((null tstanje) 0)
    (t (+ (prebroji-cetri (car tstanje) igrac) (prebroj-vertikalne (cdr tstanje) igrac)))))


;; Transformise listu iz N*N atoma u N listi od N atoma
;;Params
;lista : lista koja se transformisa
;N : 'stranica' 

;; OVO NE RADI JOS
;; OVO NE RADI JOS
;; OVO NE RADI JOS
;; OVO NE RADI JOS
;; OVO NE RADI JOS

(defun transformisi (lista N)
  (cond 
    ((equalp (length lista) (* N N)) '())  
    (t (transformisi-tops lista N 0))
    ))

;; OVO NE RADI JOS
;; OVO NE RADI JOS
;; OVO NE RADI JOS
;; OVO NE RADI JOS
;; OVO NE RADI JOS

(defun transformisi-tops (lista N pom) 
  (cond   
    ((null lista) '())
    ((equalp pom N) (cons (car lista) (transformisi-tops (cdr lista) N 0)))
    (t (transformisi-tops (cons (append (car lista) (cadr lista)) (cddr lista)) N (1+ pom)))))


;; Broji horizonatalno na stubicima koliko ima spojenih 
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji
(defun prebroj-horizonatalne (tstanje igrac) 

)

;; Broji dijagonalno na stubicima koliko ima spojenih 
;;Params
;tstanje : trenutno stanje
;igrac : igrac za kog se broji

(defun prebroj-dijagonalne (tstanje igrac) 

)


;(prebroj-cetri '(O O O O O X X X O X X X O O O O) 'O 0)
