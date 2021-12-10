let set = [2; 4; 3; 7]

let razdeli_na_input_output list =
  let rec razdeli_aux l acc =
    match l with 
    | [] -> acc
    | x :: xs -> 
    let output = String.split_on_char '|' x in
    razdeli_aux xs ((String.split_on_char ' ' (List.nth output 1)) :: acc)
  in razdeli_aux list []

let prestej_list list =
  let rec prestej_aux l acc =
    match l with 
    | [] -> acc
    | x :: xs -> if List.mem (String.length x) set then prestej_aux xs (acc + 1) else prestej_aux xs acc
  in prestej_aux list 0

let prestej_vse output = 
  let rec prestej_aux l acc =
    match l with
    | [] -> acc
    | x :: xs -> prestej_aux xs ((prestej_list x) + acc)
  in prestej_aux output 0


let naloga1 vsebina = string_of_int (prestej_vse (razdeli_na_input_output vsebina))

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
    let vsebina_datoteke = preberi_datoteko "day_8.in" in
        let no_spaces = String.trim vsebina_datoteke in
        let str_list = String.split_on_char '\n' no_spaces in
    let odgovor1 = naloga1 str_list
    and odgovor2 = naloga2 str_list
    in
    izpisi_datoteko "day_8_1.out" odgovor1;
    izpisi_datoteko "day_8_2.out" odgovor2

