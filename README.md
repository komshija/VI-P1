# Projekat : 3D Four in line 
``` Projekat iz predmeta Veštačka Inteligencija ``` 



## Rokovi za faze projekta: 
1. Formulacija problema i implementacija interfejs 
    * Rok: 20.12.2020. godine
2. Implementacija operatora promene stanja
    * Rok: 3.1.2021. godine
3. Implementacija Min-Max algoritma za traženje sa alfa-beta odsecanjem
    * Rok: 10.1.2021. godine
4. Definicija heuristike (procena stanja)
    * Rok: 24.1.2021. godine



## Prva faza 

- [x] Definisati nacin predstavljanja stanja problema (igre)
- [x] Napisati funkciju za postavljanje pocetnog stanja na osnovu zadate velicine kocke
- [x] Napisati funkcije za testiranje kraja igre
- [x] Omoguciti izbor ko ce da igra prvi (covek ili racunar)
- [x] Prvi igra uvek Igrac X, a drugi O
- [x] Implementirati funkcije koje obezbedjuju prikaz proizvoljnog stanja problema(igre)
- [x] Realizovati funkcije koje na osnovu zadatog poteza, u obliku broj_stubica ili (vrsta, kolona), omogucavaju: proveru da li je potez valjan, ako jeste, promenu prosledjenog stanja problema(igre) odigravanjem poteza


## Druga faza 

* Napisati funkcije za operatore promene stanja problema (igre) u opštem slučaju (proizvoljno stanje u kocki). 
* Na osnovu trenutne (proizvoljne) situacije u kocki (stanja) i zadatog (validnog) poteza formira novu situaciju na kocki (stanje).
* Ne menjati postojeće stanje već napraviti novo i na njemu odigrati potez.
* Na osnovu trenutne (proizvoljne) situacije u kocki (stanja) i  igrača koji je na potezu formira listu svih mogućih situacija u kocki (stanja), korišćenjem funkcije iz prethodne tačke
* Realizovati funkcije:
* - koje obezbeđuju odigravanje partije između dva igrača (dva čoveka, ne računara i čoveka)
* - unos poteza i provera da li je potez moguć
* - ukoliko nije moguć zahtevati unos novog poteza
* - ukoliko je moguć odigrati ga i promeniti trenutno stanje
* - prikazati novonastalo stanje sistema
* - proveru kraja i određivanje pobednika u igri

## Treca faza 

* Implementirati Min-Maxalgoritam sa alfa-beta odsecanjem za zadati problem
* Obezbediti da funkcija Min-Max sa alfa-beta odsecanjem ima ulazni parametar kojim se definiše dubina pretraživanja
* Obezbediti da funkcija Min-Max sa alfa-beta odsecanjem vrati potez koji trebaodigrati ili stanje u koje treba preći
* - Funkciju za određivanje heuristike ne treba implementirati 
* - Napraviti funkciju koja za odgovarajuća stanja vraća karakteristične
* - vrednosti samo u svrhu testiranja ispravnosti napravljenog Min-Maxalgoritma

## Cetvrta faza 

* U implementaciju Min-Max-a sa alfa-beta odsecanjem dodati funkciju  za procenu stanja koja se poziva kada se dostigne zadata dubina traženja.
* Implementirati funkciju koja vrši procenu stanja na osnovu pravila zaključivanja
* Funkcija za procenu stanja kao parametre treba da ima oznaku igrača za kojeg računa valjanost stanja, kao i samo stanje za koju se računa procena.
* Procena stanja se mora vršiti isključivo korišćenjem mehanizma zaključivanja nad prethodno definisanim skupom pravila.
* Zadatak je formulisati skup pravila i iskoristiti ih na adekvatan način za izračunavanje heuristike.
* Za izvođenje potrebnih zaključaka (izvršavanje upita nad skupom činjenica kojima se opisuje stanje) koristiti mašinu za zaključivanje.
* Implementirati funkciju koja prevodi stanje u listu činjenica ...
