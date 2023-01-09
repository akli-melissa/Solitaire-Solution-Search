                                                     Rapport de projet 
                                                    ===================

Project ID : 5614

Membres du binome :

     -AKLI Melissa 
     -YAHMI Yanis 


### Partie I :
1.Représentation des etats 
Pour définir un etat, nous devons d'abord commencer par ses composantes qui sont les colonnes,les registres et les depots.Donc pour ces dernieres nous avons opté pour ces structures:
 - On a choisi de  représenter les Colonnes comme Un FArray de liste de cartes car c'est plus facile de manipuler un tableau que de faire liste de liste, et puis
 en utilisant les Farray ça nous permet de rester toujours dans du fonctionnel.
 - Pour les Registres on les a représenté par Un Farray d'options de cartes, on aurait pu faire juste Farray ou Parray de cartes mais ça nous a paru plus logique 
 d'utiliser options pour éviter la création d'une carte fastoche lors des vérifications si le registre est vide.
 - Et enfin pour le depot on a juste fait un tableau d'entiers où à chaque fois qu'on met une carte ça va s'incrementer et le but est d'arriver à [13;13;13;13].
Et pour notre état initial on a tout initialisé selon FreeCell de sorte que si on spécifie pas à l'entrée quelle variante jouer, le programme lancera par défaut
le jeu FreeCell.

2.Validation d'un fichier solution 

 .Les coups :
   Pour modeliser les coups, il faut se concentrer sur les différents déplacements qu'on peut avoir (et bien sur vérifier les conditions selon chaque variante).
   - Deplacement colonne vers colonne
   - Deplacement colonne vers registre 
   - Deplacement registre vers colonne 
   - 
 .La normalisation :

   Par normalisation on veut dire le deplacement qui permet de mettre une carte dans le depot.
   - Deplacement colonne vers depot 
   - Deplacement registre vers depot
 
 .Lecture d'un fichier solution :


3.Création de permutations 











### Partie II :