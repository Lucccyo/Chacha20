(*
initial_state (Array 16 integer): state without any mix
tmp_state     (Array 16 integer): state during the mix
final_state   (Array 16 integer): state after all mix and xor with initial_state
keystream     (Array 64 integer): final_state splited in 64
*)

let c = ref ""
let k = ref ""
let n = ref ""

let extract_file f = 
  let c1 = open_in f in
  try
    c := (input_line c1);
    k := (input_line c1);
    n := (input_line c1)
  with End_of_file ->
    close_in c1


let rotate_l number n = 
  (*take a 32b number*)
  let suffix = number lsr (32 - n) in
  ((number lsl n) lor suffix) land 0xffffffff


let mix w x y z = 
  let a = ref w in
  let b = ref x in
  let c = ref y in
  let d = ref z in

  a := (!a + !b) land 0xffffffff;
  d := rotate_l (!a lxor !d) 16;
  c := (!c + !d) land 0xffffffff;
  b := rotate_l (!b lxor !c) 12;
  a := (!a + !b) land 0xffffffff;
  d := rotate_l (!a lxor !d) 8;
  c := (!c + !d) land 0xffffffff;
  b := rotate_l (!b lxor !c) 7;
  !a land 0xffffffff, !b land 0xffffffff, !c land 0xffffffff, !d land 0xffffffff


let serialize mot = 
  (*take 32bit integer as parameter named mot*)
  let b0 = (mot land 0x000000ff) lsl 24 in
  let b1 = (mot land 0x0000ff00) lsl 8 in
  let b2 = (mot land 0x00ff0000) lsr 8 in
  let b3 = (mot land 0xff000000) lsr 24 in
  let res = b0 + b1 + b2 + b3 in
  res


let suffix_int_in_char s i = 
  s ^ (String.make 1 (char_of_int i))


let ascii_of_array array = 
  (*array has the same length as msg*)
  let res = ref "" in
  for i = 0 to ((Array.length array)-1) do
    res := suffix_int_in_char !res array.(i)
  done;
  !res


let display_tab tab = 
  for i = 0 to ((Array.length tab) - 1) do
    Printf.printf "%08x " tab.(i)
  done;
  Printf.printf "\n"


let ascii_to_array msg = 
  let tab = Array.make (String.length msg) 0 in
  for i = 0 to ((String.length msg) -1) do
    tab.(i) <- (Char.code (String.get msg i));
  done;
  tab


let initial_state_maker constant key counter nonce = 
  (*construct the initial state/keystream with a key and a nonce. counter get +1 if the message is bigger than 512b*)
  let initial_state = Array.make 16 0 in 
  let cpt_k = ref 0 in
  let cpt_n = ref 0 in
  let cpt_c = ref 0 in

  for i = 0 to 3 do
    initial_state.(i) <- (serialize (int_of_string ("0x" ^ (String.sub constant !cpt_c 8))));
    cpt_c := !cpt_c + 8;
  done;

  for i = 4 to 11 do
    initial_state.(i) <- (serialize (int_of_string ("0x" ^ (String.sub key !cpt_k 8))));
    cpt_k := !cpt_k + 8;
  done;

  initial_state.(12) <- counter;

  for i = 13 to 14 do
    initial_state.(i) <- (serialize (int_of_string ("0x" ^ (String.sub nonce !cpt_n 8))));
    cpt_n := !cpt_n + 8;
  done;
  initial_state



let tab_xor tab1 tab2 = 
  (*xor between two tab of same size (16x32b)*)
  let xored_state = Array.make 16 0 in
  for i = 0 to Array.length tab1 - 1 do
    xored_state.(i) <- (tab1.(i) + tab2.(i)) land 0xffffffff
  done;
  xored_state

let final_state_maker count = 
  let initial_state = initial_state_maker !c !k count !n in
  let tmp_state = Array.copy initial_state in

  let remix i j k l =
    let a,b,c,d = mix tmp_state.(i) tmp_state.(j) tmp_state.(k) tmp_state.(l) in
    tmp_state.(i) <- a; tmp_state.(j) <- b; tmp_state.(k) <- c; tmp_state.(l) <- d
  in

  for i = 0 to 9 do
    (*column mix*)
    remix 0 4 8 12;
    remix 1 5 9 13;
    remix 2 6 10 14;
    remix 3 7 11 15;
    (*diagonal mix*)
    remix 0 5 10 15;
    remix 1 6 11 12;
    remix 2 7 8 13;
    remix 3 4 9 14
  done;
  let final_state = (tab_xor tmp_state initial_state) in
  final_state


let keystream_maker state =
  let state = Array.copy state in
  let keystream = Array.make 64 0 in
  let cpt = ref 0 in
  for i = 0 to 15 do
    for j = 0 to 3 do
      keystream.(!cpt) <- (state.(i) land 0xff);
      state.(i) <- state.(i) lsr 8;
      cpt := !cpt + 1
    done
  done;
  keystream

let key_length_calc txt_len = 
  let key_len = ref 0 in
  let msg_l = ref txt_len in
  while !msg_l >= 0 do
    key_len := !key_len + 64;
    msg_l := !msg_l - 64
  done;
  !key_len


let encrypt txt = 
  let input_text_tab = (ascii_to_array txt) in
  let txt_len = Array.length input_text_tab in
  let keystream = Array.make (key_length_calc txt_len) 0 in

  for k = 0 to ((Array.length keystream)/64)-1 do
    let tmp_keystream = keystream_maker (final_state_maker k) in
    for m = 0 to 63 do
      keystream.(64*k + m) <- tmp_keystream.(m);
    done
  done;
  
  let encrypt_tab = Array.make txt_len 0 in
  for k = 0 to txt_len-1 do
    encrypt_tab.(k) <- (keystream.(k) lxor input_text_tab.(k))
  done;

  let s = ascii_of_array encrypt_tab in
  Format.printf "%s\n" s;
  s


(* let _ = extract_file Sys.argv.(1); *)
(*   encrypt "hello world" *)
