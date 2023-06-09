--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO, CInterfaceTicTacToe, Ada.Characters.Latin_1;
USE CInterfaceTicTacToe;
PROCEDURE CTicTacToeBin IS
-- Jeu de Tic Tac Toe interactif. Vous (O) jouez contre l'ordinateur (X).
-- Programmeurs:  Philippe Gabrini et Denis Mouraux

Profondeur: CONSTANT Natural := 3;        -- nombre de niveaux de projection

SUBTYPE Type_Joueur IS Type_Case RANGE X..O;
Ordinateur: CONSTANT Type_Joueur := X;
Humain: CONSTANT Type_Joueur := O;
SUBTYPE Indice_Vecteur IS Natural RANGE 0..Côté;
TYPE Vecteur IS ARRAY(Indice_Vecteur) OF Natural;

PACKAGE Arbres RENAMES CinterfaceTicTacToe.Arbres;

PROCEDURE Positions_Possibles(Carré: IN Type_Carré;
                              Horizontal, Vertical: IN OUT Vecteur;
                              Joueur: IN Type_Joueur) IS
-- Engendrer les vecteurs représentant les positions de Joueur sur le Carré
Valeur: Natural;
BEGIN
  FOR Indice IN Indice_Vecteur LOOP
    Horizontal(Indice) := 0;
    Vertical(Indice) := 0;
  END LOOP;
  FOR Rangée IN Indice_Carré LOOP
    FOR Colonne IN Indice_Carré LOOP
      IF Carré(Rangée, Colonne) = Joueur THEN
        Valeur := 1;
      ELSIF Carré(Rangée, Colonne) = Libre THEN
        Valeur := 0;
      ELSE -- c'est l'adversaire de Joueur
        Valeur := Côté+1;
      END IF;
      Horizontal(Rangée) := Horizontal(Rangée) + Valeur;
      Vertical(Colonne) := Vertical(Colonne) + Valeur;
      IF Rangée = Colonne THEN
        Vertical(0) := Vertical(0) + Valeur;     -- première diagonale
      END IF;
      IF (Rangée + Colonne) = (Côté + 1) THEN
        Horizontal(0) := Horizontal(0) + Valeur; -- seconde diagonale
      END IF;
    END LOOP;
  END LOOP;
END Positions_Possibles;

FUNCTION Adversaire(Joueur: Type_Joueur) RETURN Type_Joueur IS
-- Donne l'adversaire de Joueur
BEGIN
  IF Joueur = X THEN
    RETURN O;
  ELSE
    RETURN X;
  END IF;
END Adversaire;

FUNCTION Valeur_Carré(Carré: Type_Carré;
                      Joueur: Type_Joueur) RETURN Integer IS
-- Evaluation de la force d'une situation pour Joueur
Horizontal, Vertical: Vecteur;
Points_Joueur, Points_Adversaire: Integer := 0;
BEGIN
  Positions_Possibles(Carré, Horizontal, Vertical, Joueur);
  FOR Indice IN Indice_Vecteur LOOP
    -- on compte les points de Joueur:
    IF Horizontal(Indice) < Côté THEN
      -- Joueur peut encore "gagner" la ligne no Indice
      Points_Joueur := Points_Joueur + Horizontal(Indice);
    END IF;
    IF Vertical(Indice) < Côté THEN
      -- Joueur peut encore "gagner" la colonne no Indice
      Points_Joueur := Points_Joueur + Vertical(Indice);
    END IF;
    IF (Horizontal(Indice) = Côté) OR (Vertical(Indice) = Côté) THEN
      -- Joueur a rempli la ligne ou la colonne no Indice, donc il gagne
      Points_Joueur := Points_Joueur + Nombre_Lignes+1;
      -- le nombre de lignes dans le jeu est Nombre_Lignes, soit le nombre
      -- de lignes horizontales et verticales, plus deux diagonales; on veut
      -- ici dépasser ce nombre, pour différencier un jeu normal d'un jeu
      -- gagnant.
    END IF;
      -- l'adversaire gagne-t-il?
    IF (Horizontal(Indice) = (Côté+1)*Côté)
       OR (Vertical(Indice) = (Côté+1)*Côté) THEN
      -- l'adversaire a rempli la ligne ou la colonne no Indice, donc il gagne
      Points_Adversaire := Points_Adversaire + Nombre_Lignes+1;
    END IF;
  END LOOP;
  -- dans ce qui suit, par convention, si les points du joueur
  -- dépassent le nombre total de lignes du jeu, c'est qu'il a gagné.
  IF Points_Joueur > Nombre_Lignes THEN
    Points_Joueur := Nombre_Lignes+1; -- par convention
  ELSIF Points_Adversaire > Nombre_Lignes THEN
    -- l'adversaire gagne
    Points_Joueur := -(Nombre_Lignes+1);
  END IF;
  IF Joueur = Humain THEN
    Points_Joueur := -Points_Joueur; -- l'ordinateur minimise son adversaire
  END IF;
  RETURN Points_Joueur;
END Valeur_Carré;

PROCEDURE Déterminer_Prochain_Choix(Carré: IN OUT Type_Carré;
                                    Profondeur: IN Natural;
                                    Joueur: IN Type_Joueur) IS
-- Rechercher le meilleur jeu de Joueur à un niveau de projection Profondeur
  
  PROCEDURE Trouver_Meilleur_Choix(Arbre: IN OUT Arbres.Arbre_Binaire;
                                   Joueur: IN Type_Joueur;
                                   Meilleur: IN OUT Situation;
                                   Valeur: IN OUT Integer) IS
  -- Trouver le meilleur endroit où jouer à partir de l'arbre étendu
  Val_Temp: Integer;
  Élément, Meilleur_Temp: Situation;
  Noeud: CInterfaceTicTacToe.Noeuds.Noeud_Binaire;
  BEGIN
    Élément := Arbres.Valeur_Courante(Arbre);
    IF NOT Arbres.Noeud_Existe(Arbre, Arbres.Gauche) THEN
      Valeur   := Valeur_Carré(Élément.Carré, Joueur);
      Meilleur := Élément;
    ELSE
      Arbres.Déplacer_Courant(Arbre, Arbres.Gauche);
      Noeud := Arbres.Noeud_Courant(Arbre);
      Trouver_Meilleur_Choix(Arbre, Adversaire(Joueur), Meilleur, Valeur);
      Arbres.Positionner_Noeud_Courant(Arbre, Noeud);
      Meilleur := Arbres.Valeur_Courante(Arbre);
      IF Élément.Tour = Moins THEN
        Valeur := -Valeur;
      END IF;
      WHILE Arbres.Noeud_Existe(Arbre, Arbres.Droite) LOOP
        Arbres.Déplacer_Courant(Arbre, Arbres.Droite);
        Noeud := Arbres.Noeud_Courant(Arbre);
        Trouver_Meilleur_Choix(Arbre, Adversaire(Joueur), Meilleur_Temp, Val_Temp);
        Arbres.Positionner_Noeud_Courant(Arbre, Noeud);
        IF Élément.Tour = Moins THEN
          Val_Temp := -Val_Temp;
        END IF;
        IF Val_Temp > Valeur THEN
          Valeur := Val_Temp;
          Meilleur := Arbres.Valeur_Courante(Arbre);
        END IF;
      END LOOP;
      IF Élément.Tour = Moins THEN  -- Min(a, b) = -Max(-a, -b)
        Valeur := -Valeur;
      END IF;
    END IF;
  END Trouver_Meilleur_Choix;

  PROCEDURE Engendrer_Arbre(Arbre: IN OUT Arbres.Arbre_Binaire;
                            Niveau: IN Natural;
                            Profondeur: IN Natural;
                            Joueur: IN Type_Joueur) IS
  -- Expansion de l'arbre à partir d'un carré: génération de tous les carrés 
  -- possibles puis répétition pour chacun d'eux jusqu'à la Profondeur désirée

    PROCEDURE Engendrer_Carrés(Arbre: IN OUT Arbres.Arbre_Binaire;
                               Joueur: IN Type_Joueur) IS
    -- Génération des carrés obtenus à partir du carré courant où Joueur
    -- va jouer

    Première_Fois: Boolean;
    Élément: Situation;
    Carré: Type_Carré;
    BEGIN
      Première_Fois := True;
      Élément := Arbres.Valeur_Courante(Arbre);
      Carré := Élément.Carré;
      FOR Rangée IN Indice_Carré LOOP
        FOR Colonne IN Indice_Carré LOOP
          IF Carré(Rangée, Colonne) = Libre THEN
            Élément.Carré := Carré;
            Élément.Carré(Rangée, Colonne) := Joueur;
            IF Première_Fois THEN -- premier descendant à gauche
              Arbres.Insérer_Noeud(Arbre, Élément, Arbres.Gauche);
              Première_Fois := False;
            ELSE -- on insère un frère à droite
              Arbres.Insérer_Noeud(Arbre, Élément, Arbres.Droite);
            END IF;
          END IF;
        END LOOP;
      END LOOP;
    END Engendrer_Carrés;
  
  Parent: CInterfaceTicTacToe.Noeuds.Noeud_Binaire;
  Élément: Situation;
  Tour_Parent: Type_Niveau;

  BEGIN -- Engendrer_Arbre
    IF Niveau < Profondeur THEN
      Élément := Arbres.Valeur_Courante(Arbre);
      Tour_Parent := Élément.Tour;
      Parent := Arbres.Noeud_Courant(Arbre);
      Engendrer_Carrés(Arbre, Joueur);
      Arbres.Positionner_Noeud_Courant(Arbre, Parent);
      IF Arbres.Noeud_Existe(Arbre, Arbres.Gauche) THEN
        Arbres.Déplacer_Courant(Arbre, Arbres.Gauche);
        LOOP
          Élément := Arbres.Valeur_Courante(Arbre);
          IF Tour_Parent = Plus THEN
            Élément.Tour := Moins;
          ELSE
            Élément.Tour := Plus;
          END IF;
          Arbres.Modifier_Courant(Arbre, Élément);
          Parent := Arbres.Noeud_Courant(Arbre);
          Engendrer_Arbre(Arbre, Niveau+1, Profondeur, Adversaire(Joueur));
          Arbres.Positionner_Noeud_Courant(Arbre, Parent);
          EXIT WHEN NOT Arbres.Noeud_Existe(Arbre, Arbres.Droite);
          Arbres.Déplacer_Courant(Arbre, Arbres.Droite);
        END LOOP;
      END IF;
    END IF;
  END Engendrer_Arbre;

Arbre: Arbres.Arbre_Binaire;
Meilleur: Situation;
Valeur: Integer;

BEGIN -- Déterminer_Prochain_Choix
  Meilleur.Carré := Carré;  -- O vient de jouer, au tour de X
  Meilleur.Tour := Plus;
  Arbres.Insérer_Noeud(Arbre, Meilleur, Arbres.Racine);
  Engendrer_Arbre(Arbre, 0, Profondeur, Joueur);
  Arbres.Déplacer_Courant(Arbre, Arbres.Racine);
  --Arbres.Afficher_Arbre(Arbre);
  Trouver_Meilleur_Choix(Arbre, Joueur, Meilleur, Valeur);
  Carré := Meilleur.Carré;
END Déterminer_Prochain_Choix;

FUNCTION Gagne(Carré: Type_Carré; Joueur: Type_Joueur) RETURN Boolean IS
-- Vérification si Joueur gagne
Indice: Indice_Vecteur := Indice_Vecteur'First;
Horizontal, Vertical: Vecteur;
Fini: Boolean := False;
BEGIN
  Positions_Possibles(Carré, Horizontal, Vertical, Joueur);
  LOOP
    Fini := (Horizontal(Indice) = Côté) OR (Vertical(Indice) = Côté);
    EXIT WHEN Fini OR Indice = Indice_Vecteur'Last;
    Indice := Indice+1;
  END LOOP;
  RETURN Fini;
END Gagne;

FUNCTION Partie_Terminée(Carré: Type_Carré) RETURN Boolean IS
-- Vérification si le carré est plein
Case_Libre: Boolean := False;
BEGIN
  FOR Rangée IN Indice_Carré LOOP
    FOR Colonne IN Indice_Carré LOOP
      IF Carré(Rangée, Colonne) = Libre THEN
        Case_Libre := True;
      END IF;
    END LOOP;
  END LOOP;
  RETURN NOT Case_Libre;
END Partie_Terminée;

PROCEDURE Demander_Coordonnées(Rangée: OUT Indice_Carré;
                               Colonne: OUT Indice_Carré) IS
-- Lecture des coordonnées de la position utilisée
Coordonnées_Ok: Boolean := False;
BEGIN
  LOOP
    Bloc_Coordonnées:
    BEGIN
      Ada.Text_IO.Put("Où jouez-vous (m n)?  ");
      Ada.Integer_Text_IO.Get(Rangée);
      Ada.Integer_Text_IO.Get(Colonne);
      Ada.Text_IO.Skip_Line;
      Coordonnées_Ok := True;
    EXCEPTION 
      WHEN Constraint_Error =>
        Ada.Text_IO.Skip_Line;
        Ada.Text_IO.Put_Line
                  ("Coordonnées hors des limites du carré, veuillez recommencer...");
      WHEN Ada.Text_IO.Data_Error =>
        Ada.Text_IO.Skip_Line;
        Ada.Text_IO.Put_Line ("Coordonnées non numériques, veuillez recommencer...");
    END Bloc_Coordonnées;
    EXIT WHEN Coordonnées_Ok;
    Ada.Text_IO.Put (Ada.Characters.Latin_1.BEL);
  END LOOP;
  Ada.Text_IO.New_Line;
END Demander_Coordonnées;

PROCEDURE Afficher_Carré(Carré: IN Type_Carré) IS
-- Afficher un carré sur l'écran
  PROCEDURE Afficher_Ligne IS
  --
  BEGIN
    FOR Colonne IN Indice_Carré LOOP
      Ada.Text_IO.Put("+---");
    END LOOP;
    Ada.Text_IO.Put_Line("+");
  END Afficher_Ligne;

BEGIN -- Afficher_Carré
  Afficher_Ligne;
  FOR Rangée IN Indice_Carré LOOP
    FOR Colonne IN Indice_Carré LOOP
      IF Carré(Rangée, Colonne) = X THEN
        Ada.Text_IO.Put("| X ");
      ELSIF Carré(Rangée, Colonne) = O THEN
        Ada.Text_IO.Put("| O ");
      ELSE
        Ada.Text_IO.Put("|   ");
      END IF;
    END LOOP;
    Ada.Text_IO.Put_Line("|");
    Afficher_Ligne;
  END LOOP;
END Afficher_Carré;

Jeu: Type_Carré;
Rangée, Colonne: Indice_Carré;
Réponse: Character;
Ordinateur_A_Commencé: Boolean;

BEGIN -- TicTacToe
  LOOP
    FOR Rangée IN Indice_Carré LOOP
      FOR Colonne IN Indice_Carré LOOP
        Jeu(Rangée, Colonne) := Libre;
      END LOOP;
    END LOOP;
    Ada.Text_IO.Put_Line("Jeu de Tic Tac Toe");
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put("Voulez-vous jouer en premier (O/N)?  ");
    Ada.Text_IO.Get(Item => Réponse);
    Ada.Text_IO.Skip_Line;
    IF Réponse = 'O' OR Réponse = 'o' THEN
      Demander_Coordonnées(Rangée, Colonne);
      Jeu(Rangée, Colonne) := Humain;
      Ordinateur_A_Commencé := False;
    ELSE
      IF Côté MOD 2 = 0 THEN -- Côté est pair
        Rangée := Côté/2;
      ELSE
        Rangée := 1 + Côté/2;
      END IF;
      Colonne := 1 + Côté/2;
      Jeu(Rangée, Colonne) := Ordinateur;
      Ordinateur_A_Commencé := True;
    END IF;
    LOOP
      IF Ordinateur_A_Commencé THEN
        Ordinateur_A_Commencé := False;
      ELSE
        Déterminer_Prochain_Choix(Jeu, Profondeur, Ordinateur);
      END IF;
      Afficher_Carré(Jeu);
      IF NOT Gagne(Jeu, Ordinateur)
         AND NOT Partie_Terminée(Jeu) THEN
        LOOP
          Demander_Coordonnées(Rangée, Colonne);
          EXIT WHEN Jeu(Rangée, Colonne) = Libre;
          Ada.Text_IO.Put_Line("Cette case n'est pas libre, veuillez recommencer...");
          Ada.Text_IO.Put(Ada.Characters.Latin_1.BEL);
        END LOOP;
        Jeu(Rangée, Colonne) := Humain;
        Afficher_Carré(Jeu);
      END IF;
      EXIT WHEN Gagne(Jeu, Ordinateur)
                OR Gagne(Jeu, Humain)
                OR Partie_Terminée(Jeu);
    END LOOP;
    IF Gagne(Jeu, Humain) THEN
      Ada.Text_IO.Put_Line("Félicitations!!!");
    ELSIF Gagne(Jeu, Ordinateur) THEN
      Ada.Text_IO.Put_Line("J'ai gagné!!!");
    ELSE
      Ada.Text_IO.Put_Line("Ex aequo...");
    END IF;
    Ada.Text_IO.Put("Voulez-vous rejouer (O/N)?  ");
    Ada.Text_IO.Get(Réponse);
    EXIT WHEN Réponse /= 'O' AND Réponse /= 'o';
  END LOOP;
END CTicTacToeBin;
