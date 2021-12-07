let rec naslednji_dan l acc =
  match l with 
  | [] -> acc
  | x :: xs -> if x = 0 then naslednji_dan xs (6 :: (8 :: acc)) else naslednji_dan xs ((x - 1) :: acc)



let naloga1 vsebina = 
    let rec stevilo_rib list n = 
        if n = 0 then string_of_int (List.length list) else stevilo_rib (naslednji_dan list []) (n - 1)
    in stevilo_rib vsebina 80

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
    let vsebina_datoteke = preberi_datoteko "day_6.in" in
        let no_spaces = String.trim vsebina_datoteke in
        let str_list = String.split_on_char ',' no_spaces in
        let int_list = List.map int_of_string str_list in
    let odgovor1 = naloga1 int_list
    and odgovor2 = naloga2 int_list
    in
    izpisi_datoteko "day_6_1.out" odgovor1;
    izpisi_datoteko "day_6_2.out" odgovor2

