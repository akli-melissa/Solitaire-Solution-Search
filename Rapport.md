                                                          RAPPORT DE PROJET 
                                                    ===========================

Project ID : 5614

Membres du binome :
    - AKLI Melissa 
    - YAHMI Yanis 



### Introduction :
Dans le cadre du cours PF5 de notre troisième année à l’Université de 
Paris-Cité, il nous est proposé un projet de 6 semaines nous permettant de mettre en pratique nos connaissances et nos compétences en programmation fonctionnelle au travers d’un cahier des charges ayant pour finalité la réalisation d’un programme de recherche automatique de solutions de différentes variantes de jeux de solitaire en particulier : FreeCell, Seahaven, Midnight Oil et Baker’s Dozen.


### Méthodologie :
  **Cahier de charges**
Décrivant l’ensemble des conditions attachés à l’exécution du projet, le cahier des charges nous a permis dans un premier temps, de définir le contexte, les enjeux, les objectifs techniques ainsi que l’exécutable et les axes de développement envisagés. En organisant nos idées, nous avons ainsi pu vérifier la concordance et la faisabilité de notre projet.

  **Méthodes de gestion**
Pour mieux organiser notre temps, on se fixait des réunions à la bibliothèque pour avancer ensemble sur le projet.
On a pris beaucoup de temps pour se mettre d’accord sur les structures, mais une fois que la modélisation de l’architecture globale a été faite, on a chacun pris des parties du projet à réaliser.


### Conception :
  **Architecture globale**
Pour les structures de notre programme on a opté pour ce choix, on a défini notre type état de sorte qu’on représente :
-	Les colonnes par un tableau de liste de cartes 
-	Les registres par un tableau d’options de cartes 
-	Les dépôts par un tableau d’entiers 


### Réalisation :
  **Représentation de l'etat**
Pour définir un état, nous devons d’abord commencer par ses composantes qui sont les colonnes, les registres et les dépôts.   
On a choisi de représenter les colonnes comme un FArray de liste de cartes car c’est plus facile de manipuler un tableau que de manipuler une liste de liste, et puis en utilisant cette structure ça nous permet de rester toujours dans du fonctionnel. 
Pour les registres on les a représentés par un FArray d’options de cartes, on aurait pu faire juste FArray ou PArray de cartes mais ça nous a paru plus logique pour éviter la création d’une carte fastoche à chaque vérification si le registre est vide.
Et enfin pour le dépôt on a juste fait un tableau d’entiers où à chaque fois qu’on met une carte ça va s’incrémenter et le but est d’arriver à [13 ;13 ;13 ;13].
Et on a tout initialisé selon les critères de FreeCell de sorte que si on ne spécifie pas à l’entrée quelle variante jouer, le programme lancera par défaut le jeu FreeCell.
   
  **Les Coups**
Pour modéliser les coups, il suffit de se concentrer sur les différents déplacements qu’on peut avoir (et bien sûr vérifier les conditions selon chaque variante de jeu)
-	Déplacement colonne vers colonne :
On teste si la position est valide puis on ajoute la carte à la tête de la colonne concernée (ajout d’élément au début d’une liste).
-	Déplacement colonne vers registre :
Vu que l’ordre des registres n’est pas important donc on parcourt récursivement, si Some(carte) (case occupée) on passe à l’indice suivant et si None on place la carte dans cette case.
Exemple :
Chaque variante de jeu a sa propre fonction de déplacement qui vérifie les différentes conditions du jeu :
Pour FreeCell on vérifie :
Si y’a 8 colonnes et 4 registres 
Une colonne non vide ne peut recevoir qu’une seule carte 
On déplace selon carte n reçois carte n-1 
On déplace de couleur alternée  
Une colonne vide peut recevoir n’importe quelle carte (resp pour le registre)

  **La normalisation**
Par normalisation on veut dire le déplacement qui permet de mettre une carte dans le dépôt, et le but est d’arriver à les déposer toutes pour gagner. 
-	Déplacement colonne vers dépôt : On parcourt la liste des colonnes et on teste à chaque fois la premiere carte en tete, on teste son rang et son suit 
de sorte que si son rank = au rank+1 de celle qui est déja dans le depot, on la deplace vers le depot qui correspond à sa suit et on remet la normalisation à zero.
-	Déplacement registre vers dépôt : meme principe avec le deplacement colonne vers depot juste que dans celle ci on manipule des options.


  **Shuffle**
Pour cette fonction on a juste suivi l’algorithme donné par le prof, de sorte que chaque point est représenté par une fonction auxiliaire.

  **Lecture du fichier solution**
on traite chaque ligne selonles trois cas possibles: entier entier | entier V | entier T 
Et selon le cas, on fait appel aux fonctions auxiliaires concernées  et on teste en parcourant toutes les lignes du fichier 
si tout est bien fait on renvoie succes sinon echec avec le numero du coup illegal .



### Conclusion :
Le projet avait cette particularité de rassembler divers disciplines:Programmation fonctionnelle, Logique, algorithmique et bien d'autres.
Mais les temps impartis à la réalisation de ce dernier furent bref et il a fallu faire preuve de perseverance pour respecter les contraintes technologiques imposées par le projet.
Somme toute, nous avons retrouvé lors de ce projet une experience, des contraintes mais aussi l'excitation d'un projet avec un nouveau langage.




