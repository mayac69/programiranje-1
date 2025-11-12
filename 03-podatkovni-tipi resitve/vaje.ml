(*----------------------------------------------------------------------------*
 # Ponovitev vrst tipov
[*----------------------------------------------------------------------------*)
let niz: string = "to je niz" (* OCaml ga takoj prepozna. *)

let pet: float = 2. +. 3.
let pet_int: int = 2 + 3

let resnica: bool = true

let seznam: int list = [1; 2] (* Moramo mu povedati, katerega tipa je seznam - v tem primeru s celimi števili. *)


(* OKRAJŠAVA *)
type r3 = float * float * float
let tocka: r3 = (1., 2., 3.)
let tocka2: r3 = (2., 2., 3.)


(* ZAPISNI TIP *)
type complex = {re: float; im: float}
let x: complex = {re = 1.; im = 0.}

(* Če želimo dostopati do katerega izmed polj, ga pokličemo s piko. *)
let realni_del_x = x.re


(* NAŠTEVNI TIP *)
type dostava =
  | Osebno
  | PoPosti of string

(* Možne so TOČNO stvari, ki so zgoraj naštete pod type dostava. *)
let osebno: dostava = Osebno
let doma: dostava = PoPosti ("FMF")


(* ALGEBRAJSKI TIP *)
(* definira se rekurzivno *)

type naravno_stevilo =
  | Nic
  | Naslednik of naravno_stevilo

let dva: naravno_stevilo = Naslednik (Naslednik Nic)


(*----------------------------------------------------------------------------*
 # Podatkovni tipi
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Valute

 Pri modeliranju denarja ponavadi uporabljamo racionalna števila. Problemi se
 pojavijo, ko uvedemo različne valute. Oglejmo si dva pristopa k izboljšavi
 varnosti pri uporabi valut.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte tipa `euro` in `dollar`, kjer ima vsak od tipov zgolj en
 konstruktor, ki sprejme racionalno število. Nato napišite funkciji
 `euro_to_dollar` in `dollar_to_euro`, ki primerno pretvarjata valuti (točne
 vrednosti pridobite na internetu ali pa si jih izmislite).

 Namig: Občudujte informativnost tipov funkcij.
[*----------------------------------------------------------------------------*)

type euro = Euro of float

type dollar = Dollar of float

let dollar_to_euro (Dollar dolar) = 
  Euro (0.95 *. dolar)

let euro_to_dollar (Euro evro) = 
  Dollar (1.05 *. evro)

let primer_valute_1 = dollar_to_euro (Dollar 0.5) 
(* val primer_valute_1 : euro = Euro 0.4305 *)

(*----------------------------------------------------------------------------*
 Definirajte tip `currency` kot en vsotni tip z konstruktorji za jen, funt in
 švedsko krono. Nato napišite funkcijo `to_pound`, ki primerno pretvori valuto
 tipa `currency` v funte.

 Namig: V tip dodajte še švicarske franke in se navdušite nad dejstvom, da vas
 Ocaml sam opozori, da je potrebno popraviti funkcijo `to_pound`.
[*----------------------------------------------------------------------------*)

type currency =
  | Jen of float
  | Funt of float
  | Krona of float

let to_pound x = 
  match x with 
  | Jen j -> Funt (j *. 0.007)
  | Funt f -> Funt f
  | Krona k -> Funt (k *. 0.08)

let primer_valute_2 = to_pound (Jen 100.) 
(* val primer_valute_2 : currency = Pound 0.700000000000000067 *)

(*----------------------------------------------------------------------------*
 ## Mešani seznami

 Želimo uporabljati sezname, ki hranijo tako cela števila kot tudi logične
 vrednosti. To bi lahko rešili tako da uvedemo nov tip, ki predstavlja celo
 število ali logično vrednost, v nadaljevanju pa bomo raje konstruirali nov tip
 seznamov.

 Spomnimo se, da lahko tip `list` predstavimo s konstruktorjem za prazen seznam
 `Nil`(oz. `[]` v Ocamlu) in pa konstruktorjem za člen `Cons(x, xs)` (oz. `x ::
 xs` v Ocamlu).
[*----------------------------------------------------------------------------*)
type seznam = 
  | Nil 
  | Element of int * seznam

(*----------------------------------------------------------------------------*
 Definirajte tip `intbool_list` z konstruktorji za:

 - prazen seznam,
 - člen s celoštevilsko vrednostjo,
 - člen z logično vrednostjo.

 Nato napišite testni primer, ki bi predstavljal `[5; true; false; 7]`.
[*----------------------------------------------------------------------------*)

type intbool_list =
  | Nil
  | Bool of bool * intbool_list (* dodamo še preostanek seznama *)
  | Int of int * intbool_list (* -||- *)

let test : intbool_list = Int (5, Bool (true, Bool (false, Int (7, Nil))))

(*----------------------------------------------------------------------------*
 Funkcija `intbool_map f_int f_bool ib_list` preslika vrednosti `ib_list` v nov
 `intbool_list` seznam, kjer na elementih uporabi primerno od funkcij `f_int`
 oz. `f_bool`.
[*----------------------------------------------------------------------------*)

let rec intbool_map f_int f_bool ib_list =
  match ib_list with
  | Nil -> Nil
  | Int (x, ibs) -> Int (f_int x, intbool_map f_int f_bool ibs)
  | Bool (b, ibs) -> Bool (f_bool x, intbool_map f_int f_bool ibs)


(*----------------------------------------------------------------------------*
 Funkcija `intbool_reverse` obrne vrstni red elementov `intbool_list` seznama.
 Funkcija je repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec intbool_reverse intbool_list = 
  let rec intbool_reverse_aux acc drugi_intbool_list =
    match drugi_intbool_list with
      | Nil -> acc
      | Int (i, dibs) -> intbool_reverse_aux (Int (i, acc)) dibs
      | Bool (b, dibs) -> intbool_reverse_aux (Bool (b, acc)) dibs
  in
  intbool_reverse_aux Nil intbool_list
(*----------------------------------------------------------------------------*
 Funkcija `intbool_separate ib_list` loči vrednosti `ib_list` v par `list`
 seznamov, kjer prvi vsebuje vse celoštevilske vrednosti, drugi pa vse logične
 vrednosti. Funkcija je repno rekurzivna in ohranja vrstni red elementov.
[*----------------------------------------------------------------------------*)

let rec intbool_separate ib_list = 
  let rec intbool_separate_aux int_acc bool_acc drugi_ib_list =
    match drugi_ib_list with
    | Nil -> (List.rev int_acc, List.rev bool_acc)
    | Int (i, dibs) -> intbool_separate_aux (i :: int_acc) bool_acc dibs
    | Bool (b, dibs) -> intbool_separate_aux int_acc (b :: bool_acc) dibs
  in
  intbool_separate_aux [] [] ib_list  (* Začetna argumenta int lista in bool lista sta kar []. *) 
                                      (* Reverse bi lahko uporavili tudi na koncu: intbool_reverse ib_list. *)

(*----------------------------------------------------------------------------*
 ## Čarodeji

 Določeni ste bili za vzdrževalca baze podatkov za svetovno priznano čarodejsko
 akademijo "Effemef". Vaša naloga je konstruirati sistem, ki bo omogočil
 pregledno hranjenje podatkov.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Čarodeje razvrščamo glede na vrsto magije, ki se ji posvečajo. Definirajte tip
 `magic`, ki loči med magijo ognja, magijo ledu in magijo arkane oz. fire, frost
 in arcane. Ko se čarodej zaposli na akademiji, se usmeri v zgodovino,
 poučevanje ali raziskovanje oz. historian, teacher in researcher. Definirajte
 tip `specialisation`, ki loči med temi zaposlitvami.
[*----------------------------------------------------------------------------*)

type magic = 
 | Fire
 | Frost
 | Arcane

type specialisation 

(*----------------------------------------------------------------------------*
 Vsak od čarodejev začne kot začetnik, nato na neki točki postane študent, na
 koncu pa SE lahko tudi zaposli. Definirajte tip `status`, ki določa ali je
 čarodej:

 - začetnik `Newbie`,
 - študent `Student` (in kateri vrsti magije pripada in koliko časa študira),
 - zaposlen `Employed` (in vrsto magije in specializacijo).

 Nato definirajte zapisni tip `wizard` z poljem za ime in poljem za trenuten
 status ter dodajte primer `professor`, ki je zaposlen učitelj magije ognja, in
 `jaina`, ki je četrto leto študentka magije ledu.
[*----------------------------------------------------------------------------*)

type status 

type wizard 

let professor  = ()

let jaina  = ()

(*----------------------------------------------------------------------------*
 Želimo prešteti koliko uporabnikov posamezne od vrst magije imamo na akademiji.
 Definirajte zapisni tip `magic_counter`, ki v posameznem polju hrani število
 uporabnikov magije. Nato definirajte funkcijo `update counter magic`, ki vrne
 nov števec s posodobljenim poljem glede na vrednost `magic`.
[*----------------------------------------------------------------------------*)

type magic_counter = {fire: int; frost: int; arcane: int}

let update {fire = fi; frost = fr; arcane = ar} magic = 
  match magic with
  | Fire -> {fire = fi + 1; frost = fr; arcane = ar}
  | Frost -> {fire = fi; frost = fr + 1; arcane = ar}
  | Arcane -> {fire = fi; frost = fr; arcane = ar + 1}


let update counter magic = 
  (* let {fire = fi; frost = fr; arcane = ar} = counter in *)
  match magic with
  | Fire -> {counter with fire = counter.fire + 1} (* Beseda 'with' nam omogoča, da nam ni potrebno razbiti celega objekta. *)
  | Frost -> {counter with frost = counter.frost + 1}
  | Arcane -> {counter with arcane = counter.arcane + 1}

(* let primer_carovniki_1 = update {fire = 1; frost = 1; arcane = 1} Arcane *)
(* val primer_carovniki_1 : magic_counter = {fire = 1; frost = 1; arcane = 2} *)

(*----------------------------------------------------------------------------*
 Funkcija `count_magic` sprejme seznam čarodejev in vrne števec uporabnikov
 različnih vrst magij.
[*----------------------------------------------------------------------------*)

let count_magic _ = ()

(* let primer_carovniki_2 = count_magic [professor; professor; professor] *)
(* val primer_carovniki_2 : magic_counter = {fire = 3; frost = 0; arcane = 0} *)

(*----------------------------------------------------------------------------*
 Želimo poiskati primernega kandidata za delovni razpis. Študent lahko postane
 zgodovinar po vsaj treh letih študija, raziskovalec po vsaj štirih letih
 študija in učitelj po vsaj petih letih študija. Funkcija `find_candidate magic
 specialisation wizard_list` poišče prvega primernega kandidata na seznamu
 čarodejev in vrne njegovo ime, čim ustreza zahtevam za `specialisation` in
 študira vrsto `magic`. V primeru, da ni primernega kandidata, funkcija vrne
 `None`.
[*----------------------------------------------------------------------------*)

let find_candidate _ _ _ = ()

(* let primer_carovniki_3 =
  find_candidate Frost Researcher [professor; jaina] *)
(* val primer_carovniki_3 : string option = Some "Jaina" *)
