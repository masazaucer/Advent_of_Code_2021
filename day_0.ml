
let naloga1 vsebina =
    let rec increase_aux l acc =
        match l with
        | [] -> string_of_int acc
        | x :: [] -> string_of_int acc
        | x :: (y :: xs) -> if (x < y) then increase_aux (y :: xs) (acc + 1) else increase_aux (y :: xs) acc
    in 
    increase_aux vsebina 0



let naloga2 vsebina = 
    let rec measurement_windows l acc =
        match l with
        | [] -> List.rev acc
        | x :: [] -> List.rev acc
        | x :: (y :: []) -> List.rev acc
        | x :: (y :: (z :: rep)) -> measurement_windows (y :: (z :: rep)) ((x + y + z) :: acc)
    in
    naloga1 (measurement_windows vsebina [])

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
    let vsebina_datoteke = preberi_datoteko "day_0.in" in
        let no_spaces = String.trim vsebina_datoteke in
        let str_list = String.split_on_char '\n' no_spaces in
        let int_list = List.map int_of_string str_list in
    let odgovor1 = naloga1 int_list
    and odgovor2 = naloga2 int_list
    in
    izpisi_datoteko "day_0_1.out" odgovor1;
    izpisi_datoteko "day_0_2.out" odgovor2




