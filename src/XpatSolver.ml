
open XpatLib

exception Deplacement_impossible;;

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



let setEtat game = match game with
  | Seahaven -> begin 
                  etat.colonnes <- FArray.make 10 [];
                  etat.registres <- FArray.make 4 None;
                end
  | Midnight -> etat.colonnes <- FArray.make 18 []
  | Baker    -> etat.colonnes <- FArray.make 13 []
  | Freecell -> ()


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


let initialisation_colonnes permutation_partitionee = 
  let rec initialisation_colonnes_aux partitions ind = 
    match partitions with
    | [] -> ()
    | x::part -> begin
      etat.colonnes <- FArray.set (etat.colonnes) (ind) (x);
      initialisation_colonnes_aux part (ind+1) 
    end
  in initialisation_colonnes_aux permutation_partitionee (0)
              

let partition_des_cartes permutation= 
    match config.game with 
    | Seahaven -> partition_liste_seahaven permutation
    | Freecell -> partition_liste_freecell permutation
    | Midnight -> partition_liste_midnight permutation
    | Baker    -> partition_liste_baker    permutation

let rec mise_au_depot colonne pos = match colonne with
  | [] -> ()
  | carte::sub_l -> match carte with
    |(r,s) ->
      begin
        if (r = (FArray.get (etat.depots) (Card.num_of_suit s))+1 ) then 
        begin
          etat.depots <- FArray.set (etat.depots) (Card.num_of_suit s) (r+1);
          etat.colonnes <- FArray.set (etat.colonnes) (pos) (sub_l);
          mise_au_depot sub_l pos;
        end
      end

let normalisation()  = 
  let rec normalisation_aux pos = 
    match FArray.get (etat.colonnes) (pos) with
    | exception Not_found -> ()
    | colonne -> begin
        mise_au_depot (colonne) (pos);
        normalisation_aux (pos+1);
    end
  in normalisation_aux 0

  (*Source: Accessible, Destination: Vide *)

let depose_carte_colonne carte pos_colonne =   match (FArray.get etat.colonnes pos_colonne) with 
    | col -> FArray.set (etat.colonnes) (pos_colonne) (carte::(FArray.get etat.colonnes pos_colonne))

let retire_carte_colonne pos_colonne = match (FArray.get etat.colonnes pos_colonne) with
  | [] -> failwith ("Aucune carte à retirer pour cette colonne!")
  | carte::col ->  FArray.set (etat.colonnes) (pos_colonne) (col)

let depose_carte_registre carte = 
  let rec depose_carte_registre_aux pos carte res posee = match (FArray.get etat.registres pos) with
  | exception Not_found -> FArray.of_list res (*A revoir *)
  | None -> if(posee = false) then depose_carte_registre_aux (pos+1) (carte) (carte::res) (true) else depose_carte_registre_aux (pos+1) (carte) (None::res) (posee)
  | x -> depose_carte_registre_aux  (pos+1) carte (x::res)  (posee)
  in depose_carte_registre_aux (0) (carte) ([]) (false) (*Renvoie le res à l'envers mais pas trop grave*)

let retire_carte_registre carte = 
  let rec retire_carte_registre_aux  res (pos) = match (FArray.get etat.registres (pos)) with
  | exception Not_found -> FArray.of_list res (* A Revoir !!! *)
  | None ->  retire_carte_registre_aux (None::res) (pos+1)
  | Some(x) -> if(x = carte) then (retire_carte_registre_aux (None::res) (pos+1) ) else (retire_carte_registre_aux (Some(x)::res) (pos+1))
  in retire_carte_registre_aux [] 0

let existe_colonne_vide  = 
  let rec existe_colonne_vide_aux pos = match (FArray.get etat.colonnes pos) with
    | exception Not_found -> failwith ("Pas de colonne libre")
    | [] -> pos
    | _::_ -> existe_colonne_vide_aux (pos+1)
  in existe_colonne_vide_aux (0)

let existe_registre_vide = 
  let rec existe_registre_vide_aux pos = match (FArray.get etat.registres pos) with
    | exception Not_found -> failwith ("Pas de registre vide")
    | None -> true
    | _ -> existe_registre_vide_aux (pos+1)
  in existe_registre_vide_aux (0)

  let pos_source_colonne src = 
  let rec pos_source_aux source pos =
    match FArray.get (etat.colonnes) (pos) with
    | exception Not_found  -> failwith ("Position non accessible de la carte source")
    | [] -> pos_source_aux source (pos+1)
    | x::col -> if (Card.to_num x =  src) then pos else pos_source_aux (source) (pos+1)
  in (pos_source_aux (src) (0)) 

let pos_source_reg src =
  let rec pos_source_reg_aux source pos = match (FArray.get etat.registres pos) with
  | exception Not_found  -> failwith ("Position source invalide: Col")
  | Some(x) -> if (Card.to_num x =  source) then pos else pos_source_reg_aux (source) (pos+1)
  | _ -> pos_source_reg_aux (source) (pos+1)
in pos_source_reg_aux (src) (0)

let pos_destination dest = 
  let rec pos_destination_aux dest pos = match FArray.get (etat.colonnes) (pos) with
    | exception Not_found -> failwith ("Position destination incorrecte: Reg")
    | [] -> pos_destination_aux dest (pos+1)
    | x::col -> if (Card.to_num x =  dest) then pos else pos_destination_aux dest (pos+1)
  in pos_destination_aux (dest) (0)


let rec source_valide_aux_colonne source pos = match FArray.get (etat.colonnes) (pos) with
  | exception Not_found -> false
  | [] -> false
  | x::col -> if(Card.to_num x =source) then true
  else source_valide_aux_colonne source (pos+1)

let rec source_valide_aux_registre source pos = match FArray.get (etat.registres) (pos) with
  | exception Not_found -> false
  | Some(x) -> if (source = Card.to_num x) then true else source_valide_aux_registre source (pos+1)
  | _ -> source_valide_aux_registre source (pos+1)

let source_valide source = (source_valide_aux_colonne source 0) || (source_valide_aux_registre source 0)


let  destination_colonne_valide destination =
  let rec destination_valide_aux destination (pos) = match FArray.get (etat.colonnes) (pos) with
    | exception Not_found -> false
    | [] -> false
    | x::col -> if(Card.to_num x =destination) then true
    else destination_valide_aux destination (pos+1)
  in destination_valide_aux destination 0

let destination_valide dest = 
  let rec destination_valide_aux dest (pos) = match FArray.get (etat.registres) (pos) with
    | exception Not_found -> false
    | None -> true
    | _ -> destination_valide_aux dest (pos+1)
  in destination_valide_aux dest (0)

let est_rouge c = match Card.of_num c with
  | (r,s) -> if(s = Coeur || s = Carreau) then true else false
let est_noire c = match Card.of_num c  with
| (r,s) -> if(s = Trefle || s = Pique) then true else false
  
let couleur_diff c1 c2 = (est_rouge c1 && est_noire c2) || 
                          (est_noire c1 && est_rouge c2)
let get_rank c = match Card.of_num c with
  | (r,s) -> r

let get_suit c = match Card.of_num c with
  | (r,s) -> s


let deplace_freecell source destination =
  if(source_valide source) then
    match destination with
      | Carte n -> if(destination_colonne_valide n) then match (FArray.get (etat.colonnes) (pos_destination n) ) with
        | _::_ -> if (couleur_diff source n && ((get_rank source - 1) = (get_rank n)) ) then 
          begin
            match (pos_source_colonne source) with
            | p -> begin
                  etat.colonnes <- retire_carte_colonne (pos_source_colonne source); 
                  etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
                  end
            |exception Not_found -> begin (*cas ou la carte est dans un registre*)
                etat.registres <- retire_carte_registre (Card.of_num source);
                etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
              end
            end
        | _ -> raise Deplacement_impossible
        else raise  Deplacement_impossible
      
      | T -> if (existe_registre_vide) then 
        begin 
          etat.colonnes <- retire_carte_colonne (pos_source_colonne (source));
          etat.registres <- depose_carte_registre (Some (Card.of_num source));
        end
      else raise Deplacement_impossible
      | V -> match (existe_colonne_vide) with
        | pos ->  etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos)
  else raise Deplacement_impossible

(*mm couleur inf , col vide roi 4 Reg*)
let deplace_seahaven source destination = 
  if (source_valide source) then 
    match destination with
    | Carte n -> if(destination_colonne_valide n) then match (FArray.get (etat.colonnes) (pos_destination n)) with
      | c1::l -> if (((get_suit source) = (get_suit n)) && ((get_rank source) = ((get_rank n) - 1)) ) then 
        begin
          match (pos_source_colonne source) with
          | p -> begin
                etat.colonnes <- retire_carte_colonne (pos_source_colonne source); 
                etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
                end
          |exception Not_found -> begin (*cas ou la carte est dans un registre*)
              etat.registres <- retire_carte_registre (Card.of_num source);
              etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
            end
          end
      | _ -> raise Deplacement_impossible
      else raise  Deplacement_impossible
    
      | T -> if(existe_registre_vide) then
      begin
        etat.colonnes <- retire_carte_colonne (pos_source_colonne (source));
        etat.registres <- depose_carte_registre (Some (Card.of_num source));
      end
    else raise Deplacement_impossible
    | V -> if(get_rank source = 13) then 
      match (existe_colonne_vide) with
    | pos -> etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos)
    else failwith ("Seahaven: Colonne vide reçoit uniquement un roi!")

  else raise Deplacement_impossible


let deplace_midnight source destination = 
  if (source_valide source) then
    match destination with
    | Carte n -> if(destination_colonne_valide n) then match (FArray.get (etat.colonnes) (pos_destination n)) with
      | _ -> if (((get_suit source) = (get_suit n)) && ((get_rank source) = ((get_rank n) - 1)) ) then
        begin
          match (pos_source_colonne source) with
          | p -> begin
                etat.colonnes <- retire_carte_colonne (pos_source_colonne source); 
                etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
                end
          end
      else raise Deplacement_impossible
      else raise Deplacement_impossible
    | T -> failwith ("Midnight: Destination = Carte")
    | V -> failwith ("Midnight: Destination = Carte")
  else raise Deplacement_impossible


  (*inf, col vide rien 0 Reg*)
let deplace_baker   source destination= 
if (source_valide source) then
  match destination with
  | Carte n -> if(destination_colonne_valide n) then match (FArray.get (etat.colonnes) (pos_destination n)) with
    | _ -> if (get_rank source) = ((get_rank n) - 1)  then 
      begin
        match (pos_source_colonne source) with
        | p -> begin
              etat.colonnes <- retire_carte_colonne (pos_source_colonne source); 
              etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
              end
        end
      else raise Deplacement_impossible
  else raise Deplacement_impossible
  | T -> failwith ("BakerDozen: Destination = Carte")
  | V -> failwith ("BakerDozen: Destination = Carte")
else raise Deplacement_impossible 


let deplace_carte source destination = match config.game with
| Freecell -> deplace_freecell source destination
| Midnight -> deplace_midnight source destination
| Baker -> deplace_baker source destination
| Seahaven -> deplace_seahaven source destination



(*
Def variable fichier: let file = "fichierSol..."
Ouvrir un fichier lecture: let ic = open_in file in
Lecture ligne let line = input_line ic in ....
Fermer le fichier: close_in ic 
Exception: End_of_file

*)

let lire_fichier f =
  let (*rec*) lecture_rec () = match (input_line f) with
    | ligne -> match (String.split_on_char (' ') (ligne)) with
    | [a;b] ->  
    begin 
    match b with
     | "T" -> deplace_carte (int_of_string a) (T)
     | "V" -> deplace_carte (int_of_string a) (V)
     | _ -> deplace_carte (int_of_string a) (Carte(int_of_string b));
     normalisation ();
    end
     | _ -> begin
        close_in f;
        raise End_of_file;
    end
  in lecture_rec()

(* Fonctions d'affichage pour les tests*)
let rec affiche_colonnes ind = 
  match FArray.get etat.colonnes ind with
   |  exception Not_found -> ()
   | x -> 
    begin 
      List.iter (fun n -> Printf.printf "%s " (Card.to_string (n)) ) x; 
      print_newline ();
      affiche_colonnes (ind+1);
    end

  let rec affiche_colonnes_int ind = 
    match FArray.get etat.colonnes ind with
      |  exception Not_found -> ()
      | x -> 
      begin 
        List.iter (fun n -> Printf.printf " %d " (Card.to_num n) )  (x); 
        print_newline ();
        affiche_colonnes_int (ind+1);
      end

  let affiche_registres () =
    let rec affiche_registres_aux pos = 
      match (FArray.get etat.registres pos) with
      | exception Not_found -> ()
      | None -> begin print_string " _ "; affiche_registres_aux (pos+1) ; end
      | Some(x) ->begin print_string ((Card.to_string(x))^" ");  affiche_registres_aux (pos+1); end
    in affiche_registres_aux 0

(* TODO : La fonction suivante est à adapter et continuer *)

let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  print_newline ();
  (*Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;*)
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  (*List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;
  print_newline ();*)
  (*print_string "C'est tout pour l'instant. TODO: continuer...\n";*)
  (*  Change dans etat.config: Freecel......    *)
    setEtat conf.game;
    let res = partition_des_cartes (permut) in
    initialisation_colonnes res;
    let fichier = match config.mode with
    | Check s -> s
    | Search s -> s in
    let fd = open_in fichier in
    lire_fichier fd;

(*
  affiche_colonnes 0;
  print_newline ();
  affiche_colonnes_int 0;
  print_newline ();
  affiche_registres ();
  print_newline ();
 
  deplace_seahaven (19) T;
  affiche_colonnes 0;
  print_newline ();
  affiche_colonnes_int 0;
  print_newline ();
  affiche_registres ();
  print_newline ();

*)

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

