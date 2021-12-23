let zacetno = (9, 4);;

let krog = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|];;

let met lega kocka =
  let nova = kocka + 1 in
  if nova <= 100 then 
    if lega + nova > 10 then
      let nova_lega = (lega + nova) mod 10 in
      (nova_lega, nova)
    else (lega + nova, nova)
  else 
    let nova = 1 in
      if lega + nova > 10 then
        let lega = lega mod 10 in
        (lega, nova)
      else (lega + nova, nova)

let trije_meti lega kocka =
  let (l, k) = met lega kocka in
  let (l1, k1) = met l k in
  let (l2, k2) = met l1 k1 in
  (l2, k2)

let korak (l1, l2) =
  let meti = ref 0 in
  let kocka = ref 0 in 
  let v1 = ref 0 in
  let v2 = ref 0 in
  let l1 = ref l1 in
  let l2 = ref l2 in
  let zmaga = ref 1000 in
  while !v1 < !zmaga && !v2 < !zmaga do 
    let (lega1, k1) = trije_meti !l1 !kocka in
      meti := (3 + !meti);
      kocka := k1;
      v1 := !v1 + lega1;
      l1 := lega1;
    let (lega2, k2) = trije_meti !l2 !kocka in
      meti := !meti + 3;
      kocka := k2;
      v2 := !v2 + lega2;
      l2 := lega2;
  done;
  if !v1 >= !zmaga then !v2 * !meti else !v1 * !meti




let naloga1 vsebina = string_of_int (korak vsebina)
  
let naloga2 vsebina = string_of_int 10

let _ =
    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let odgovor1 = naloga1 zacetno
    and odgovor2 = naloga2 zacetno
    in
    izpisi_datoteko "day_21_1.out" odgovor1;
    izpisi_datoteko "day_21_2.out" odgovor2
