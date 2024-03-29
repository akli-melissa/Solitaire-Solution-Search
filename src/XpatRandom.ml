(** In Xpat2, the index of the game is a seed used to shuffle
    pseudo-randomly the cards.
    The shuffle function emulates this permutation generator.
    The input number is the seed (between 1 and 999_999_999).
    The output list is of size 52, and contains all numbers in 0..51
    (hence without duplicates).

*)

open Fifo

(* The numbers manipulated below will be in [0..randmax[ *)
let randmax = 1_000_000_000

(* Converting an integer n in [0..randmax[ to an integer in [0..limit[ *)
let reduce n limit =
  Int.(of_float (to_float n /. to_float randmax *. to_float limit))



(* For now, we provide a shuffle function that can handle a few examples.
   This can be kept later for testing your implementation. *)

let shuffle_test = function
  | 1 ->
     [13;32;33;35;30;46;7;29;9;48;38;36;51;41;26;20;23;43;27;
      42;4;21;37;39;2;15;34;28;25;17;16;18;31;3;0;10;50;49;
      14;6;24;1;22;5;40;44;11;8;45;19;12;47]
  | 12 ->
     [44;9;28;35;8;5;3;4;11;25;43;2;27;1;24;40;17;41;47;18;
      10;34;39;7;36;29;15;19;30;37;48;45;0;21;12;46;22;13;16;
      33;31;38;23;6;14;49;26;50;20;32;42;51]
  | 123 ->
     [16;51;44;27;11;37;33;50;48;13;17;38;7;28;39;15;4;5;3;6;
      42;25;19;34;20;49;23;0;8;26;30;29;47;36;9;24;40;45;14;
      22;32;10;1;18;12;31;35;2;21;43;46;41]
  | 1234 ->
     [36;37;44;26;9;10;23;30;29;18;4;35;15;50;33;43;28;2;45;
      6;3;31;27;20;7;51;39;5;14;8;38;17;49;0;40;42;13;19;34;
      1;46;22;25;24;12;48;16;21;32;11;41;47]
  | 12345 ->
     [10;12;6;23;50;29;28;24;7;37;49;32;38;30;31;18;13;2;15;4;
      5;47;16;1;0;35;43;40;42;44;46;39;48;20;36;34;8;14;33;11;
      25;45;41;19;3;17;21;51;26;22;27;9]
  | 123456 ->
     [1;7;39;47;5;15;50;49;37;44;29;10;4;23;17;20;0;11;24;14;
      28;35;3;48;8;41;19;46;13;12;36;34;27;9;33;22;43;32;25;30;
      38;6;31;16;51;21;26;18;45;40;42;2]
  | 1234567 ->
     [19;17;31;6;4;14;9;36;35;30;39;40;50;48;42;37;12;3;25;1;
      43;27;5;20;10;51;11;44;46;38;16;22;26;23;21;28;15;7;47;
      13;18;29;32;0;49;34;8;45;24;33;2;41]
  | 22222 ->
     [43;17;21;40;42;47;0;35;23;18;11;29;41;10;45;7;15;25;13;
      51;6;12;33;24;8;34;50;2;30;28;37;3;4;39;49;31;32;14;44;
      22;46;48;9;1;36;5;27;26;38;20;16;19]
  | 222222 ->
     [42;48;16;9;22;21;45;12;40;44;29;31;24;27;33;38;14;15;49;
      37;0;26;10;1;47;4;50;34;23;8;3;2;19;32;13;43;51;6;39;35;
      18;30;11;7;46;17;20;5;41;36;25;28]
  | 2222222 ->
     [17;45;5;4;33;23;10;42;39;3;24;46;6;29;44;27;0;43;2;7;20;
      14;34;8;11;18;15;28;25;49;40;47;48;21;41;9;31;30;36;12;
      51;1;35;26;50;38;32;19;13;37;22;16]
  | 999_999_999 ->
     [22;1;0;21;20;44;23;43;38;11;4;2;19;27;36;9;49;7;18;14;
      46;10;25;35;39;48;51;40;33;13;42;16;32;50;24;47;26;6;34;
      45;5;3;41;15;12;31;17;28;8;29;30;37]
  | _ -> failwith "shuffle : unsupported number (TODO)"


let max a b = if (b <= a) then a - b else a - b + randmax
let diviseur = 21 mod 55
  
let perm graine =
      let rec perm_aux n avant graine_1 graine_2=
          if (n = 55) then []
          else
              if (n = 0) then
                  (0, graine_1)::(perm_aux (n+1) 0 (graine_1+1) graine_1)
              else
                  let acc = (avant + diviseur) mod 55 in
                  let nv = max graine_1 graine_2 in
                  (acc, nv) :: (perm_aux (n+1) (acc) (graine_2) (nv))
      in perm_aux 0 0 graine 0
;;
  
let rec sous_liste liste debut fin =
      if (debut > fin) then []
      else let l = List.nth liste debut
      in l :: (sous_liste liste (debut+1) fin)
;;
  
let f = (fun a b -> let (a1,_) = a in let (b1,_)=b in if (a1<b1) then -1 else if (a1=b1) then 0 else 1)
  
let reduire liste =
   let liste_trie =
          let liste_trie_1 = List.sort f liste
          in List.map (fun x -> let _,b = x in b) liste_trie_1
      in let i1 = Fifo.of_list (sous_liste liste_trie 24 54)
      in let i2 = Fifo.of_list (sous_liste liste_trie 0 23)
      in i1, i2
;;
  
let rec tirage file_1 file_2 n =
      if (n <= 0) then [], file_1, file_2
      else let n1, update_file_1 = Fifo.pop file_1 in
          let n2, update_file_2 = Fifo.pop file_2 in
          let d = max n1 n2
          in let l, f1, f2 = (tirage (Fifo.push n2 update_file_1) (Fifo.push d update_file_2) (n-1))
          in (d :: l), f1, f2 
;;
  
let reduire_liste liste limit =
      let rec reduire_liste_aux liste limit n size=
          if (n >= size) then []
          else let res = reduce (List.nth liste n) limit in
          res :: (reduire_liste_aux liste (limit-1) (n+1) size) in
      reduire_liste_aux liste limit 0 (List.length liste)
    
;;
  
let permutation_reduite liste =
      let rec permutation_reduite_aux liste sous_liste =
          match liste with
              | [] -> []
              | b::bs -> let res = List.nth sous_liste b in
                  (permutation_reduite_aux bs (List.filter (fun x->x!=res) sous_liste))@[res]
      in let rec gen_list a b =
          if (a >= b) then []
          else a :: gen_list (a+1) b
      in permutation_reduite_aux liste (gen_list 0 52)
;;
  
let shuffle n =
     let paires = perm n in
     let f1_init, f2_init = reduire paires in
     let _, f1_165, f2_165 = tirage f1_init f2_init 165 in
     let tirages_52, _, _ = tirage f1_165 f2_165 52 in
     let reduced_52 = reduire_liste tirages_52 52 in
     permutation_reduite reduced_52 
;;
