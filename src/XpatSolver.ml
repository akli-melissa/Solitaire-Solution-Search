
open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)

type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }

(*
let configFreeCell = { game = Freecell; seed = 1; mode = Search "" }
let configSeahaven = { game = Seahaven; seed = 1; mode = Search "" }
let configMidnight = { game = Midnight; seed = 1; mode = Search "" }
let configBaker    = { game = Baker;    seed = 1; mode = Search "" }
*)
type etat = { 
  mutable registres : Card.card option FArray.t  ;
  mutable colonnes : Card.card list FArray.t  ;
  mutable depots : int FArray.t;
}

let etat = {
  registres = FArray.make 4 None;
  colonnes  = FArray.make 8 [];
  depots    = FArray.make 4 0
}

let getgame = function
  | "FreeCell"|"fc" -> Freecell
  | "Seahaven"|"st" -> Seahaven
  | "MidnightOil"|"mo" -> Midnight
  | "BakersDozen"|"bd" -> Baker
  | _ -> raise Not_found

let split_on_dot name =
  match String.split_on_char '.' name with
  | [string1;string2] -> (string1,string2)
  | _ -> raise Not_found

let set_game_seed name =
  try
    let (sname,snum) = split_on_dot name in
    config.game <- getgame sname;
    config.seed <- int_of_string snum
  with _ -> failwith ("Error: <game>.<number> expected, with <game> in "^
                      "FreeCell Seahaven MidnightOil BakersDozen")

(* Fonctions auxiliaires ajoutées *)

let initialisation_colonnes etat permutation = 
  match etat.game with 
    | Freecell -> ()(*partion_liste_freecell (etat.colonnes) (permutation)*)
    | Seahaven -> ()
    | Midnight -> ()
    | Baker    -> ()


let setEtat game = match game with
  | Seahaven -> etat.colonnes <- FArray.make 10 []
  | Midnight -> etat.colonnes <- FArray.make 18 []
  | Baker    -> etat.colonnes <- FArray.make 13 []
  | Freecell -> ()

let partion_liste_freecell permutation  = 
  let rec partion_liste_freecell_aux perm res acc cpt ss = (*ss: six ou sept*)
    match perm with
    | [] -> acc::res
    | x::perm' -> 
      if (ss = 7) then 
        if cpt<7 then partion_liste_freecell_aux (perm') (res) (x::acc) (cpt+1) (ss)
        else partion_liste_freecell_aux (perm') (acc::res) (x::[]) (1) (6)
      else (* ss = 6 *)
        if cpt<6 then partion_liste_freecell_aux (perm') (res) (x::acc) (cpt+1) (ss)
        else partion_liste_freecell_aux (perm') (acc::res) (x::[]) (1) (7)

  in partion_liste_freecell_aux (permutation) ([]) ([]) (0) (7)

let partition_liste_seahaven permutation = 
  let rec partition_liste_seahaven_aux perm res acc cpt =
    match perm with
    | []   -> acc::res
    | [c1;c2]  ->  partition_liste_seahaven_aux ([]) (res) (acc) (cpt)
    | x::perm' -> if (cpt < 5) then partition_liste_seahaven_aux (perm') (res) (x::acc) (cpt+1)
      else partition_liste_seahaven_aux (x::perm') (acc::res) ([]) (0)
  in partition_liste_seahaven_aux (permutation) ([]) ([]) (0)


  let rec affiche_colonnes ll = 
  match ll with
   | [] -> ()
   | x::l -> 
    begin 
      List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n))) x; 
      print_newline ();
      affiche_colonnes l
    end

let partion_liste_midnight = ()
let partion_liste_baker = ()

(* TODO : La fonction suivante est à adapter et continuer *)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();
  (*print_string "C'est tout pour l'instant. TODO: continuer...\n";*)
  (* Selon la variante jouée, initialer *)

  setEtat conf.game;
  print_newline ();
  let res = partition_liste_seahaven (permut) in affiche_colonnes res;
  exit 0

let main () =
  Arg.parse
    [("-check", String (fun filename -> config.mode <- Check filename),
        "<filename>:\tValidate a solution file");
     ("-search", String (fun filename -> config.mode <- Search filename),
        "<filename>:\tSearch a solution and write it to a solution file")]
    set_game_seed (* pour les arguments seuls, sans option devant *)
    "XpatSolver <game>.<number> : search solution for Xpat2 game <number>";
  treat_game config

let _ = if not !Sys.interactive then main () else ()
