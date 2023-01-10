open XpatLib

exception Deplacement_impossible;;

type game = Freecell | Seahaven | Midnight | Baker

type mode =
  | Check of string (* filename of a solution file to check *)
  | Search of string (* filename where to write the solution *)


type config = { mutable game : game; mutable seed: int; mutable mode: mode }
let config = { game = Freecell; seed = 1; mode = Search "" }


(*Structures de données*)
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

(*Déplacements possibles dans une ligne d'un fichier: Int, V (Colonne vide), T (Registre)*)
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


let nb_colonnes game = match game with
  | Freecell -> 8
  | Seahaven -> 10
  | Midnight -> 18
  | Baker -> 13
  
let nb_registres game = match game with
  | Freecell -> 4
  | Seahaven -> 4
  | Midnight -> 0
  | Baker -> 4


(*Partition des cartes selon les conditions de chaque variante du jeu*)

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
    | Seahaven -> etat.colonnes <- FArray.of_list (partition_liste_seahaven permutation)
    | Freecell -> etat.colonnes <- FArray.of_list (partition_liste_freecell permutation)
    | Midnight -> etat.colonnes <- FArray.of_list (partition_liste_midnight permutation)
    | Baker    -> etat.colonnes <- FArray.of_list (partition_liste_baker    permutation)




    (*Source: Accessible, Destination: Vide *)
let depose_carte_colonne carte pos_colonne = FArray.set (etat.colonnes) (pos_colonne) (carte::(FArray.get etat.colonnes pos_colonne))

let retire_carte_colonne etat pos =
  if ( pos >= nb_colonnes config.game ) then raise Not_found
  else match (FArray.get etat.colonnes pos) with
    | [] -> failwith("Pas de carte à retirer sur cette colonne")
    | x::col -> FArray.set etat.colonnes pos col 

let depose_carte_registre c = 
  let rec depose_carte_registre_aux pos c res posee = 
    if (pos >= nb_registres config.game) then FArray.of_list (List.rev res)
    else match (FArray.get etat.registres pos) with
      | None -> if(posee) then depose_carte_registre_aux (pos+1) (c) (None::res) (posee) 
                else depose_carte_registre_aux (pos+1) (c) (c::res) (true)
      | Some(x) -> depose_carte_registre_aux (pos+1) c (Some(x)::res) (posee)
  in depose_carte_registre_aux 0 c [] false

let retire_carte_registre carte = 
  let rec retire_carte_registre_aux c res pos =
    if (pos >= nb_registres config.game) then (List.rev res)
    else match (FArray.get etat.registres pos) with
    | None ->  retire_carte_registre_aux  (c) (None::res) (pos+1)
    | Some(x) -> if(x = carte) then (retire_carte_registre_aux (c) (None::res) (pos+1) ) else (retire_carte_registre_aux (c) (Some(x)::res) (pos+1))
  in retire_carte_registre_aux carte [] 0



let retire_carte_registre carte = 
  let rec retire_carte_registre_aux  res (pos) = match (FArray.get etat.registres (pos)) with
  | exception Not_found -> FArray.of_list res (* A Revoir !!! *)
  | None ->  retire_carte_registre_aux (None::res) (pos+1)
  | Some(x) -> if(x = carte) then (retire_carte_registre_aux (None::res) (pos+1) ) else (retire_carte_registre_aux (Some(x)::res) (pos+1))
  in retire_carte_registre_aux [] 0

let depose_carte_depot carte = 
  let (r,s) = carte in FArray.set etat.depots (Card.num_of_suit s) (r)

  let existe_colonne_vide etat =
    let rec existe_colonne_vide_aux etat  pos = 
      if (pos >= nb_colonnes config.game) then raise Not_found
      else match FArray.get etat.colonnes pos with
      | [] -> pos
      | _ -> existe_colonne_vide_aux etat  (pos+1)
    in existe_colonne_vide_aux etat (0)

let existe_registre_vide = 
  let rec existe_registre_vide_aux pos = match (FArray.get etat.registres pos) with
    | exception Not_found -> raise Deplacement_impossible
    | None -> true
    | _ -> existe_registre_vide_aux (pos+1)
  in existe_registre_vide_aux (0)
  
  let pos_source_colonne src = 
  let rec pos_source_aux source pos =
    match FArray.get (etat.colonnes) (pos) with
    | x::col -> if (Card.to_num x =  source) then pos else pos_source_aux (source) (pos+1)
    | [] -> pos_source_aux source (pos+1)
    | exception Not_found  -> raise Not_found
  in (pos_source_aux (src) (0)) 

let pos_source_reg src =
  let rec pos_source_reg_aux source pos = match (FArray.get etat.registres pos) with
  | exception Not_found  ->raise Deplacement_impossible
  | Some(x) -> if (Card.to_num x =  source) then pos else pos_source_reg_aux (source) (pos+1)
  | _ -> pos_source_reg_aux (source) (pos+1)
in pos_source_reg_aux (src) (0)

let pos_destination destination = 
  let rec pos_destination_aux dest pos = match FArray.get (etat.colonnes) (pos) with
    | x::col -> if (Card.to_num x =  dest) then pos else pos_destination_aux dest (pos+1)
    | [] -> pos_destination_aux dest (pos+1)
    | exception Not_found -> raise Deplacement_impossible
  in pos_destination_aux (destination) (0)


let rec source_valide_aux_colonne source pos = match FArray.get (etat.colonnes) (pos) with
  | exception Not_found -> false
  | x::col ->if(Card.to_num x = source) then true else source_valide_aux_colonne source (pos+1)
  | [] -> true

let rec source_valide_aux_registre source pos = match FArray.get (etat.registres) (pos) with
  | exception Not_found -> false
  | Some(x) -> if (source = Card.to_num x) then true else source_valide_aux_registre source (pos+1)
  | _ -> source_valide_aux_registre source (pos+1)

let source_valide source = (source_valide_aux_colonne source 0) || (source_valide_aux_registre source 0)

let  destination_colonne_valide destination = 
let rec destination_colonne_valide_aux destination pos = match FArray.get (etat.colonnes) (pos) with
  | exception Not_found -> false
  | x::col ->if(Card.to_num x = destination) then true else destination_colonne_valide_aux destination (pos+1)
  | [] -> true
in destination_colonne_valide_aux destination 0


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


let rec mise_au_depot_colonne pos etat bo =
  if (pos >= nb_colonnes config.game ) then ()
  else match (FArray.get etat.colonnes pos) with
  | [] -> mise_au_depot_colonne (pos+1) etat false
  | c::l -> let (r,s) = c in 
  if r = (FArray.get etat.depots (Card.num_of_suit s)) + 1 then
    begin
      etat.colonnes <- FArray.set (etat.colonnes) (pos) (l);
      etat.depots <-   depose_carte_depot c;
    mise_au_depot_colonne (0) (etat) true;
    end
  else mise_au_depot_colonne (pos+1) etat false


let rec mise_au_depot_registre pos etat  b= 
if (pos >= nb_registres config.game) then ()
else let carte_registre = FArray.get etat.registres pos in 
  match carte_registre with
  | None -> mise_au_depot_registre (pos+1) etat false
  | Some(c) -> let (r,s) = c in 
  if r = FArray.get etat.depots ((Card.num_of_suit s)) + 1 then
    begin
      etat.registres <- FArray.set etat.registres pos None; 
      etat.depots <- depose_carte_depot c;
      mise_au_depot_registre (pos+1) (etat) true; 
   end
  else mise_au_depot_registre (pos+1) (etat) false 

let rec normalisation etat b = 
  begin
        mise_au_depot_colonne 0 etat b;
        mise_au_depot_registre 0 etat b;
      if b = false then () else normalisation etat b;
  end


(* Un coup (déplacement) pour chacune des variantes demandées  *)
let deplace_freecell source destination =
  if(source_valide source) then
    match destination with
      | Carte n -> if(destination_colonne_valide n) then match (FArray.get (etat.colonnes) (pos_destination n) ) with
        | _::_ -> if ((couleur_diff source n) && ((get_rank source) = (get_rank n)-1) ) then 
          begin
            match (pos_source_colonne source) with
            | p -> begin
                  etat.colonnes <- retire_carte_colonne etat p; 
                  etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
                  normalisation etat false;
                  end
            |exception Not_found -> 
              begin (*cas ou la carte est dans un registre*)
                etat.registres <- retire_carte_registre (Card.of_num source);
                etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
                normalisation etat false;
              end
            end
          else raise Deplacement_impossible
        | _ -> raise Deplacement_impossible
        else raise  Deplacement_impossible
      
      | T -> if (existe_registre_vide ) then 
        begin 
          etat.colonnes <- retire_carte_colonne etat (pos_source_colonne (source));
          etat.registres <- depose_carte_registre (Some (Card.of_num source));
          normalisation etat false;
        end
        else raise Deplacement_impossible
      | V -> (match (existe_colonne_vide etat) with
        | pos -> 
            begin 
              etat.colonnes <- retire_carte_colonne etat (pos_source_colonne source);
              etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos);
              normalisation etat false;
              end
        | exception Not_found ->  raise Deplacement_impossible
        )
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
                etat.colonnes <- retire_carte_colonne etat (pos_source_colonne source); 
                etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
                normalisation etat false;
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
        normalisation etat false;
        etat.colonnes <- retire_carte_colonne etat (pos_source_colonne (source));
        etat.registres <- depose_carte_registre (Some (Card.of_num source));
        normalisation etat false;
      end
    else raise Deplacement_impossible
    | V -> if(get_rank source = 13) then 
      match (existe_colonne_vide etat) with
      | pos -> begin
        normalisation etat false;
        etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos);
        etat.registres <- retire_carte_registre (Card.of_num source);
        normalisation etat false;
        end
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
                etat.colonnes <- retire_carte_colonne etat (pos_source_colonne source); 
                etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
                normalisation etat false;
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
              etat.colonnes <- retire_carte_colonne etat (pos_source_colonne source); 
              etat.colonnes <- depose_carte_colonne (Card.of_num source) (pos_destination n) ;
              normalisation etat false;
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


(* Si tous les dépôts sont à 13 *)
let solution_complete etat = 
  let rec solution_complete_aux pos etat = 
    if pos = (FArray.length etat.depots) then true
    else if (FArray.get etat.depots pos) = 13 then solution_complete_aux (pos+1) etat
    else false
in  solution_complete_aux 0 etat



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
      | None -> begin print_string "  _  "; affiche_registres_aux (pos+1) ; end
      | Some(x) ->begin print_string (" "^(Card.to_string(x))^" ");  affiche_registres_aux (pos+1); end
    in affiche_registres_aux 0

    let affiche_registres_int () =
      let rec affiche_registres_int_aux pos = 
        match (FArray.get etat.registres pos) with
        | exception Not_found -> ()
        | None -> begin print_string "  _  "; affiche_registres_int_aux (pos+1) ; end
        | Some(x) ->begin print_string (" "^(Int.to_string(Card.to_num(x)))^" ");  affiche_registres_int_aux (pos+1); end
      in affiche_registres_int_aux 0
  
let affiche_depots () = 
  let rec affiche_depot_aux pos = 
    match (FArray.get etat.depots pos) with
    | exception Not_found -> ()
    | _ -> begin print_string(" "^ Int.to_string (FArray.get etat.depots pos) ^" "); affiche_depot_aux (pos+1) end
  in affiche_depot_aux 0



(* Fonction de lecture dans le fichier solution, traitement de la ligne  *)
let lire_fichier f =
  let rec lecture_rec n = begin
    normalisation etat false ;
    match (input_line f) with
    | exception End_of_file -> 
      normalisation etat false;
      begin
        normalisation etat false;
        if (solution_complete etat) then begin
          print_string "SUCCES";
          exit 0;
        end
        else begin 
          print_string ("ECHEC "^Int.to_string(n));
          exit 1;
        end
      end
    | ligne -> match (String.split_on_char (' ') (ligne)) with
      | [a;b] ->
        begin
          normalisation etat false;
          match b with
            | "T" ->
              begin
                match deplace_carte (int_of_string a) (T) with
                | exception Deplacement_impossible -> print_string ("ECHEC "^Int.to_string(n));
                | _ -> 
                  begin
                  normalisation etat false;
                  lecture_rec (n+1);
                end
              end
            | "V" ->
                begin
                  match deplace_carte (int_of_string a) (V) with
                  | exception Deplacement_impossible -> print_string ("ECHEC "^Int.to_string(n));
                  | _ -> 
                    begin
                    normalisation etat false;
                    lecture_rec (n+1);
                  end
                end
            | _   ->(match (deplace_carte (int_of_string a) (Carte(int_of_string b))) with
              | exception Deplacement_impossible -> 
                    begin
                      print_string ("ECHEC "^Int.to_string(n));
                      exit 1;
                    end
              | _ -> 
                begin
                  normalisation etat false;
                  lecture_rec (n+1);
                end);
        end
      | _ -> normalisation etat false
      end
  
  in lecture_rec 1


let treat_game conf =
  let permut = XpatRandom.shuffle conf.seed in
  print_newline ();
  Printf.printf "Voici juste la permutation de graine %d:\n" conf.seed;
  List.iter (fun n -> print_int n; print_string " ") permut;
  print_newline ();
  (*List.iter (fun n -> Printf.printf "%s " (Card.to_string (Card.of_num n)))
    permut;*)
  (*print_string "C'est tout pour l'instant. TODO: continuer...\n";*)    
  setEtat conf.game;
  let res = partition_des_cartes (permut) in
  let fichier = match config.mode with
    | Check s -> s
    | Search s -> s in
    let fd = open_in fichier in
    lire_fichier fd;
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

