let poraba_goriva list pozicija =
    let rec pomozna l acc =
        match l with 
        | [] -> acc
        | x :: xs -> 
        let n = Int.abs (x - pozicija) in
        pomozna xs (((n * (n + 1)) / 2) + acc)
    in pomozna list 0

let poraba_goriva_nova list pozicija = 
    let rec pomozna l acc = 
        match l with 
        | [] -> acc
        | x :: xs -> pomozna xs ((Int.abs (x - pozicija)) + acc)
    in pomozna list 0

let vse_porabe list =
    let rec poraba l acc =
        match l with 
        | [] -> acc
        | x :: xs -> poraba xs ((poraba_goriva list x) :: acc)
    in poraba list []

let vse_porabe_nova list =
    let rec poraba l acc =
        match l with 
        | [] -> acc
        | x :: xs -> poraba xs ((poraba_goriva_nova list x) :: acc)
    in poraba list []

let izracunaj_min list =
    let rec pomozna l acc =
        match l with
        | [] -> acc
        | x :: xs -> if x < acc then pomozna xs x else pomozna xs acc
    in pomozna list 1000000000

let naloga1 vsebina = string_of_int (izracunaj_min (vse_porabe vsebina))

let naloga2 vsebina = string_of_int (izracunaj_min (vse_porabe_nova vsebina))

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
    let vsebina_datoteke = preberi_datoteko "day_7.in" in
        let no_spaces = String.trim vsebina_datoteke in
        let str_list = String.split_on_char ',' no_spaces in
        let int_list = List.map int_of_string str_list in
    let odgovor1 = naloga1 int_list
    and odgovor2 = naloga2 int_list
    in
    izpisi_datoteko "day_7_1.out" odgovor1;
    izpisi_datoteko "day_7_2.out" odgovor2