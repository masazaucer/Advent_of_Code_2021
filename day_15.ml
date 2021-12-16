let int_value c = int_of_char c - int_of_char '0'

let razdeli list =
  let visina = List.length list in
  let sirina = String.length (List.nth list 0) in
  let matrika = Array.make_matrix sirina visina 0 in
  for i = 0 to (visina - 1) do 
    let vrstica = List.nth list i in
    for j = 0 to (sirina - 1) do 
      let element = String.get vrstica j in
      let el_int = int_value element in
      matrika.(i).(j) <- el_int
    done;
  done;
  matrika

let minimum (x, y) = if x < y then x else y

let minimalna matrika =
  let visina = Array.length matrika in
  let sirina = Array.length matrika.(0) in
  let prazna = Array.make_matrix sirina visina 0 in
  for i = 0 to visina - 1 do 
    for j = 0 to sirina - 1 do
      if (i, j) = (0, 0) then 
      prazna.(i).(j) <- matrika.(i).(j)
      else
        (if i = 0 then 
        prazna.(i).(j) <- (prazna.(i).(j - 1) + matrika.(i).(j))
        else 
          (if j = 0 then
          prazna.(i).(j) <- (prazna.(i - 1).(j) + matrika.(i).(j))
          else 
          prazna.(i).(j) <- ((minimum (prazna.(i - 1).(j), prazna.(i).(j - 1))) + matrika.(i).(j))))
    done;
  done;
  prazna.(0).(0) - matrika.(0).(0)


let naloga1 matrika = string_of_int (minimalna matrika)

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
        let matrika = razdeli str_list in
    let odgovor1 = naloga1 matrika
    and odgovor2 = naloga2 str_list
    in
    izpisi_datoteko "day_8_1.out" odgovor1;
    izpisi_datoteko "day_8_2.out" odgovor2