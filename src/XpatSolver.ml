
open XpatLib

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)


type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }


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

type deplacement = Carte of int | T | V


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

(* Initialisation de l'état selon la configuration choisie *)
let setEtat game = match game with
  | Seahaven -> etat.colonnes <- FArray.make 10 []
  | Midnight -> etat.colonnes <- FArray.make 18 []
  | Baker    -> etat.colonnes <- FArray.make 13 []
  | Freecell -> ()



(* Distribution des cartes suivant chaque configuration  *)
let partition_liste_freecell permutation  = 
  let rec partition_liste_freecell_aux perm res acc cpt ss = (*ss: six ou sept*)
    match perm with
    | [] -> acc::res
    | x::perm' -> 
      if (ss = 7) then 
        if cpt<7 then partition_liste_freecell_aux (perm') (res) ((Card.of_num x)::acc) (cpt+1) (ss)
        else partition_liste_freecell_aux (perm') (acc::res) ((Card.of_num x)::[]) (1) (6)
      else (* ss = 6 *)
        if cpt<6 then partition_liste_freecell_aux (perm') (res) ((Card.of_num x)::acc) (cpt+1) (ss)
        else partition_liste_freecell_aux (perm') (acc::res) ((Card.of_num x)::[]) (1) (7)

  in partition_liste_freecell_aux (permutation) ([]) ([]) (0) (7)

let partition_liste_seahaven permutation = 
  let rec partition_liste_seahaven_aux perm res acc cpt =
    match perm with
    | []   -> acc::res
    | [c1;c2]  -> begin
      etat.registres <- FArray.set (etat.registres) (0) (Some (Card.of_num(c1)));
      etat.registres <- FArray.set (etat.registres) (1) (Some (Card.of_num(c2)));
      partition_liste_seahaven_aux ([]) (res) (acc) (cpt)
    end
    | x::perm' -> if (cpt < 5) then partition_liste_seahaven_aux (perm') (res) ((Card.of_num x)::acc) (cpt+1)
      else partition_liste_seahaven_aux ( x::perm') (acc::res) ([]) (0)
  in partition_liste_seahaven_aux (permutation) ([]) ([]) (0)



let partition_liste_midnight permutation = 
  let rec partition_liste_midnight_aux perm res acc cpt = 
    match perm with
    | [] -> acc::res
    | x::perm' -> if(cpt < 3) then partition_liste_midnight_aux (perm') (res) ((Card.of_num x)::acc) (cpt+1)
      else partition_liste_midnight_aux (x::perm') (acc::res) ([]) (0)

  in partition_liste_midnight_aux (permutation) ([]) ([]) (0)


let partition_liste_baker permutation = 
  let rec partition_liste_baker_aux perm res acc cpt = 
    match perm with
    | [] -> acc::res
    | x::perm' -> match (Card.of_num x) with 
      | (13,_) -> if(cpt < 4) then partition_liste_baker_aux (perm') (res) (acc@((Card.of_num x)::[])) (cpt+1) 
                  else partition_liste_baker_aux (x::perm') (acc::res) ([]) (0)
      | ( _,_) -> if(cpt < 4) then partition_liste_baker_aux (perm') (res) ((Card.of_num x)::acc) (cpt+1) 
                  else partition_liste_baker_aux (x::perm') (acc::res) ([]) (0)
  in partition_liste_baker_aux permutation [] [] 0

let partition_des_cartes permutation = 
match config.game with 
| Seahaven -> partition_liste_seahaven permutation
| Freecell -> partition_liste_freecell permutation
| Midnight -> partition_liste_midnight permutation
| Baker    -> partition_liste_baker    permutation


let initialisation_colonnes permutation_partitionee = 
  let rec initialisation_colonnes_aux partitions ind = 
    match partitions with
    | [] -> ()
    | x::part -> begin
      etat.colonnes <- FArray.set (etat.colonnes) (ind) (x);
      initialisation_colonnes_aux part (ind+1) 
    end
  in initialisation_colonnes_aux permutation_partitionee (0)
              


let rec mise_au_depot colonne pos = match colonne with
  | [] -> ()
  | carte::sub_l -> match carte with
    |(r,s) ->
      begin
        if (r = (FArray.get (etat.depots) (Card.num_of_suit s))+1 ) then 
        begin
          etat.depots <- FArray.set (etat.depots) (Card.num_of_suit s) (r+1);
          etat.colonnes <- FArray.set (etat.colonnes) (pos) (sub_l);
          mise_au_depot sub_l pos; (* Faire tant qu'une normalisation est possible*)
        end
      end

(* Normalisation *)
let normalisation()  = 
  let rec normalisation_aux pos = 
    match FArray.get (etat.colonnes) (pos) with
    | exception Not_found -> ()
    | colonne -> begin
        mise_au_depot (colonne) (pos);
        normalisation_aux (pos+1);
    end
  in normalisation_aux 0

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
  let res = partition_des_cartes (permut) in
  initialisation_colonnes res;

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
