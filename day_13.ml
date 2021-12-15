#load "str.cma";;
let prepogibi = [("x", 655); ("y",447); ("x",327); ("y",223); ("x",163); ("y",111); ("x",81); ("y",55); ("x",40); ("y",27); ("y",13); ("y",6)]

let pretvori list =
  match list with
  | [a;b] -> (a, b)
  | _ -> failwith "Napaka"

let koordinate_v_stevila list =
  let rec aux l acc =
    match l with
    | [] -> acc
    | x :: xs -> 
    let tocka_list = String.split_on_char ',' x in
    let tocka_int = List.map int_of_string tocka_list in
    let tocka = pretvori tocka_int in
    aux xs (tocka :: acc)
  in aux list []

let prestavi_tocko_levo tocka x_f =
  let (x, y) = tocka in
  let razlika = (x - x_f) in
  (x_f - razlika, y)

let prepogni_levo xf list = 
  let rec prestavi l acc =
    match l with
    | [] -> acc
    | (x, y) :: xs -> if x <= xf then prestavi xs ((x, y) :: acc) else prestavi xs ((prestavi_tocko_levo (x, y) xf) :: acc)
  in prestavi list []

let prestavi_tocko_gor tocka y_f =
  let (x, y) = tocka in
  let razlika = (y - y_f) in
  (x, y_f - razlika)

let prepogni_gor yf list = 
  let rec prestavi l acc =
    match l with
    | [] -> acc
    | (x, y) :: xs -> if y <= yf then prestavi xs ((x, y) :: acc) else prestavi xs ((prestavi_tocko_gor (x, y) yf) :: acc)
  in prestavi list []

let rec ali_vsebuje x list =
  match list with
  | [] -> false
  | y :: ys -> if x = y then true else ali_vsebuje x ys

let izbrisi_ponovljene list =
  let rec samo_razlicne l acc =
    match l with 
    | [] -> acc
    | x :: xs -> if ali_vsebuje x acc then samo_razlicne xs acc else samo_razlicne xs (x :: acc)
  in samo_razlicne list []

let rec prestavi tocke prepogibi =
  let rec prepogni ostali_prepogibi acc =
  match ostali_prepogibi with
  | [] -> acc
  | (smer, vrednost) :: rep -> 
  if smer = "x" then prepogni rep (prepogni_levo vrednost acc) else prepogni rep (prepogni_gor vrednost acc)
  in prepogni prepogibi tocke

let naloga1 vsebina = 
  let prestavljene = prepogni_levo 655 vsebina in
  let brez_ponovitev = izbrisi_ponovljene prestavljene in
  string_of_int (List.length brez_ponovitev)

let naloga2 vsebina = 
  let prestavljene = prestavi vsebina prepogibi in
  let brez_ponovitev = izbrisi_ponovljene prestavljene in
  string_of_int (List.length brez_ponovitev)

let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "day_13.in" in
        let no_spaces = String.trim vsebina_datoteke in
        let str_list = String.split_on_char '\n' no_spaces in
        let koordinate = koordinate_v_stevila str_list in
    let odgovor1 = naloga1 koordinate
    and odgovor2 = naloga2 koordinate 
    in
    izpisi_datoteko "day_13_1.out" odgovor1;
    izpisi_datoteko "day_13_2.out" odgovor2