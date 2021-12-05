let izzrebani = List.map int_of_string (String.split_on_char ',' "13,79,74,35,76,12,43,71,87,72,23,91,31,67,58,61,96,16,81,92,41,6,32,86,77,42,0,55,68,14,53,26,25,11,45,94,75,1,93,83,52,7,4,22,34,64,69,88,65,66,39,97,27,29,78,5,49,82,54,46,51,28,98,36,48,15,2,50,38,24,89,59,8,3,18,47,10,90,21,80,73,33,85,62,19,37,57,95,60,20,99,17,63,56,84,44,40,70,9,30")

let odstrani_presledke list =
    let rec odstrani_aux l acc =
        match l with
        | [] -> acc
        | x :: xs -> odstrani_aux xs ((String.trim x) :: acc)
        in odstrani_aux list []

let odstrani_prazne list =
    let rec pomozna l acc =
        match l with
        | [] -> acc
        | x :: xs -> if x = "" then pomozna xs acc else pomozna xs (x :: acc)
        in pomozna list []

let loci_na_tabele vsebina = 
    let rec loci_aux list acc acc_cel =
    match list with
    | [] -> (acc :: acc_cel)
    | x :: xs -> if x = "" then loci_aux xs [] (acc :: acc_cel) else 
    let loceno = String.split_on_char ' ' x in
    let brez_presledkov = odstrani_prazne loceno in
    let v_int = List.map int_of_string brez_presledkov in
    loci_aux xs (v_int :: acc) acc_cel
    in loci_aux vsebina [] []


let transponiraj matrika =
    let velikost = List.length matrika in 
    let rec pomozna list acc acc_cel i = 
        if i >= velikost then acc_cel else
        match list with
        | [] -> pomozna matrika [] (acc :: acc_cel) (i + 1)
        | x :: xs -> pomozna xs ((List.nth x i) :: acc) acc_cel i
        in pomozna matrika [] [] 0

let poisci_index a = 
    let rec iskalnik l i =
    match l with
    | [] -> List.length izzrebani
    | x :: xs -> if x = a then i else iskalnik xs (i + 1)
    in iskalnik izzrebani 0

let izracunaj_max vrstica = 
    let rec pomozna l acc = 
        match l with
        | [] -> acc
        | x :: xs -> if x > acc then pomozna xs x else pomozna xs acc
    in pomozna vrstica 0

let zmaga_vrstica vrstica = 
    let rec vrstica_aux l acc = 
    match l with
    | [] -> izracunaj_max acc
    | x :: xs -> vrstica_aux xs ((poisci_index x) :: acc)
    in vrstica_aux vrstica []

let izracunaj_min vrstica = 
    let rec pomozna l acc = 
        match l with
        | [] -> acc
        | x :: xs -> if x < acc then pomozna xs x else pomozna xs acc
    in pomozna vrstica 100

let zmaga_matrika matrika = 
    let rec zmaga matrika acc =
    match matrika with
    | [] -> izracunaj_min acc
    | x :: xs -> zmaga xs ((zmaga_vrstica x) :: acc)
    in izracunaj_min [zmaga matrika []; zmaga (transponiraj matrika)[]]

let zmage_vseh_matrik matrike = 
    let rec zmage list acc = 
    match list with
    | [] -> acc
    | x :: xs -> zmage xs ((zmaga_matrika x) :: acc)
    in zmage matrike []

let poisci_zmagovalno zmage =
    let zmagovalno_stevilo = izracunaj_min zmage in
    let rec zmagovalni_indeks list i =
    match list with
    | x :: xs -> if (x = zmagovalno_stevilo) then i else zmagovalni_indeks xs (i + 1)
    | _ -> failwith "Napaka"
    in zmagovalni_indeks zmage 0

let vrstica_to_str vrstica = String.concat " " (List.map string_of_int vrstica)

let sestej_vrstico vrstica indeks=
    let rec sestej list acc =
        match list with
        | [] -> acc
        | x :: xs -> if poisci_index x < indeks then sestej xs (acc + x) else sestej xs acc
        in sestej vrstica 0

let rezultat matrika i zadnja =
    let rec sestej mat acc =
        match mat with
        | [] -> acc
        | x :: xs -> sestej xs (acc + sestej_vrstico x i)
        in ((sestej matrika 0) * zadnja)


let naloga1 matrike izzrebani = 
    let seznam_matrik = loci_na_tabele matrike in
    let zmage = zmage_vseh_matrik seznam_matrik in
    let zmagovalni_i = poisci_zmagovalno zmage in
    let zadnja = List.nth izzrebani zmagovalni_i in
    let zmagovalna = List.nth seznam_matrik (zmagovalni_i + 1) in
    let rez = rezultat zmagovalna (zmagovalni_i + 1) zadnja in 
    string_of_int rez


let naloga2 vsebina = string_of_int 10

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
    let vsebina_datoteke = preberi_datoteko "day_3.in" in
        let no_spaces = String.trim vsebina_datoteke in
        let str_list = String.split_on_char '\n' no_spaces in
        let izzrebani = List.map int_of_string (String.split_on_char ',' (List.hd str_list)) in 
        let matrike_str = List.tl (List.tl str_list) in
    let odgovor1 = naloga1 matrike_str izzrebani
    and odgovor2 = naloga2 str_list
    in
    izpisi_datoteko "day_3_1.out" odgovor1;
    izpisi_datoteko "day_3_2.out" odgovor2
