let prestej_na_i_1 list i =
    let rec prestej_aux l acc_0 acc_1 = 
    match l with
    | [] -> if acc_0 > acc_1 then '0' else '1'
    | x :: xs -> if x.[i] = '0' then prestej_aux xs (acc_0 + 1) acc_1 else prestej_aux xs acc_0 (acc_1 + 1)
    in prestej_aux list 0 0

let prestej_na_i_0 list i =
    let rec prestej_aux l acc_0 acc_1 = 
    match l with
    | [] -> if acc_0 >= acc_1 then '0' else '1'
    | x :: xs -> if x.[i] = '0' then prestej_aux xs (acc_0 + 1) acc_1 else prestej_aux xs acc_0 (acc_1 + 1)
    in prestej_aux list 0 0

let get_gama vsebina = 
    let rec prestej_aux acc i =
        if i >= 12 then acc else prestej_aux ((prestej_na_i_1 vsebina i) :: acc) (i + 1)
        in List.rev (prestej_aux [] 0)

let get_epsilon gamma = 
    let rec epsilon g e =
    match g with 
    | [] -> e 
    | x :: xs -> if x = '0' then epsilon xs ('1' :: e) else epsilon xs ('0' :: e)
    in List.rev (epsilon gamma [])

let binary_to_decimal bin = 
    let rec pretvori_aux list acc = 
    match list with
    | [] -> acc
    | x :: xs -> if x = '0' then pretvori_aux xs acc else pretvori_aux xs (acc +. (2. ** float_of_int (List.length xs)))
    in pretvori_aux bin 0.

let izberi list i filter = 
    let rec pomozna l acc =
    match l with
    | [] -> acc
    | x :: xs -> if x.[i] = filter then pomozna xs (x :: acc) else pomozna xs acc
    in pomozna list []

let filter_by_i_1 list i = 
    let bolj_pogost = prestej_na_i_1 list i in
    match bolj_pogost with
    | '0' -> izberi list i '0'
    | '1' -> izberi list i '1'
    
let filter_by_i_0 list i = 
    let bolj_pogost = prestej_na_i_1 list i in
    match bolj_pogost with
    | '0' -> izberi list i '1'
    | '1' -> izberi list i '0'

let naloga1 vsebina = 
    let gama = get_gama vsebina in
    let epsilon = get_epsilon gama in
    let gama_dec = binary_to_decimal gama in
    let epsilon_dec = binary_to_decimal epsilon in
    string_of_float (gama_dec *. epsilon_dec)

let oxygen vsebina = 
    let rec pomozna list i =
    match list with 
    | [] -> failwith "Napaka"
    | x :: [] -> x
    | _ -> 
    let nov_seznam = filter_by_i_1 list i in
    pomozna nov_seznam (i + 1)
    in pomozna vsebina 0

let co2 vsebina = 
    let rec pomozna list i =
    match list with 
    | [] -> failwith "Napaka"
    | x :: [] -> x
    | _ -> 
    let nov_seznam = filter_by_i_0 list i in
    pomozna nov_seznam (i + 1)
    in pomozna vsebina 0

let naloga2 vsebina = 
    let oxygen = oxygen vsebina in
    let co2 = co2 vsebina in
    let oxygen_char = List.init (String.length oxygen) (String.get oxygen) in
    let co2_char = List.init (String.length co2) (String.get co2) in
    let oxygen_dec = binary_to_decimal oxygen_char in
    let co2_dec = binary_to_decimal co2_char in
    string_of_float (oxygen_dec *. co2_dec)


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
    let vsebina_datoteke = preberi_datoteko "day_2.in" in
        let no_spaces = String.trim vsebina_datoteke in
        let str_list = String.split_on_char '\n' no_spaces in
    let odgovor1 = naloga1 str_list
    and odgovor2 = naloga2 str_list
    in
    izpisi_datoteko "day_2_1.out" odgovor1;
    izpisi_datoteko "day_2_2.out" odgovor2