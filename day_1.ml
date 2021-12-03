type command =
    | Forward of int
    | Up of int
    | Down of int

let string_to_command str = 
    let str_l = String.split_on_char ' ' str in
    match str_l with 
    | x :: (y :: []) ->
        let int = int_of_string y in
        if x = "forward" then Forward int else
            if x = "up" then Up int else Down int
    | _ -> failwith "Napaka"


let naloga1 vsebina = 
    let rec counter_aux list acc_forward acc_depth = 
        match list with
        | [] -> string_of_int (acc_forward * acc_depth)
        | x :: xs -> 
            match x with
            | Forward int-> counter_aux xs (acc_forward + int) acc_depth
            | Up int -> counter_aux xs acc_forward (acc_depth - int) 
            | Down int -> counter_aux xs acc_forward (acc_depth + int)
    in counter_aux vsebina 0 0


let naloga2 vsebina = 
    let rec counter_aux list acc_forward acc_depth acc_aim= 
        match list with
        | [] -> string_of_int (acc_forward * acc_depth)
        | x :: xs -> 
            match x with
            | Forward int-> counter_aux xs (acc_forward + int) (acc_depth + (int * acc_aim)) acc_aim
            | Up int -> counter_aux xs acc_forward acc_depth (acc_aim - int)
            | Down int -> counter_aux xs acc_forward acc_depth (acc_aim + int)
    in counter_aux vsebina 0 0 0

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
    let vsebina_datoteke = preberi_datoteko "day_1.in" in
        let no_spaces = String.trim vsebina_datoteke in
        let str_list = String.split_on_char '\n' no_spaces in
        let com_list = List.map string_to_command str_list in
    let odgovor1 = naloga1 com_list
    and odgovor2 = naloga2 com_list
    in
    izpisi_datoteko "day_1_1.out" odgovor1;
    izpisi_datoteko "day_1_2.out" odgovor2