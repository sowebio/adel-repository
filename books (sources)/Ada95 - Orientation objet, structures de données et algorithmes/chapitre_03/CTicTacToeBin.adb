--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO, CInterfaceTicTacToe, Ada.Characters.Latin_1;
USE CInterfaceTicTacToe;
PROCEDURE CTicTacToeBin IS
-- Jeu de Tic Tac Toe interactif. Vous (O) jouez contre l'ordinateur (X).
-- Programmeurs:  Philippe Gabrini et Denis Mouraux

Profondeur: CONSTANT Natural := 3;        -- nombre de niveaux de projection

SUBTYPE Type_Joueur IS Type_Case RANGE X..O;
Ordinateur: CONSTANT Type_Joueur := X;
Humain: CONSTANT Type_Joueur := O;
SUBTYPE Indice_Vecteur IS Natural RANGE 0..C�t�;
TYPE Vecteur IS ARRAY(Indice_Vecteur) OF Natural;

PACKAGE Arbres RENAMES CinterfaceTicTacToe.Arbres;

PROCEDURE Positions_Possibles(Carr�: IN Type_Carr�;
                              Horizontal, Vertical: IN OUT Vecteur;
                              Joueur: IN Type_Joueur) IS
-- Engendrer les vecteurs repr�sentant les positions de Joueur sur le Carr�
Valeur: Natural;
BEGIN
  FOR Indice IN Indice_Vecteur LOOP
    Horizontal(Indice) := 0;
    Vertical(Indice) := 0;
  END LOOP;
  FOR Rang�e IN Indice_Carr� LOOP
    FOR Colonne IN Indice_Carr� LOOP
      IF Carr�(Rang�e, Colonne) = Joueur THEN
        Valeur := 1;
      ELSIF Carr�(Rang�e, Colonne) = Libre THEN
        Valeur := 0;
      ELSE -- c'est l'adversaire de Joueur
        Valeur := C�t�+1;
      END IF;
      Horizontal(Rang�e) := Horizontal(Rang�e) + Valeur;
      Vertical(Colonne) := Vertical(Colonne) + Valeur;
      IF Rang�e = Colonne THEN
        Vertical(0) := Vertical(0) + Valeur;     -- premi�re diagonale
      END IF;
      IF (Rang�e + Colonne) = (C�t� + 1) THEN
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

FUNCTION Valeur_Carr�(Carr�: Type_Carr�;
                      Joueur: Type_Joueur) RETURN Integer IS
-- Evaluation de la force d'une situation pour Joueur
Horizontal, Vertical: Vecteur;
Points_Joueur, Points_Adversaire: Integer := 0;
BEGIN
  Positions_Possibles(Carr�, Horizontal, Vertical, Joueur);
  FOR Indice IN Indice_Vecteur LOOP
    -- on compte les points de Joueur:
    IF Horizontal(Indice) < C�t� THEN
      -- Joueur peut encore "gagner" la ligne no Indice
      Points_Joueur := Points_Joueur + Horizontal(Indice);
    END IF;
    IF Vertical(Indice) < C�t� THEN
      -- Joueur peut encore "gagner" la colonne no Indice
      Points_Joueur := Points_Joueur + Vertical(Indice);
    END IF;
    IF (Horizontal(Indice) = C�t�) OR (Vertical(Indice) = C�t�) THEN
      -- Joueur a rempli la ligne ou la colonne no Indice, donc il gagne
      Points_Joueur := Points_Joueur + Nombre_Lignes+1;
      -- le nombre de lignes dans le jeu est Nombre_Lignes, soit le nombre
      -- de lignes horizontales et verticales, plus deux diagonales; on veut
      -- ici d�passer ce nombre, pour diff�rencier un jeu normal d'un jeu
      -- gagnant.
    END IF;
      -- l'adversaire gagne-t-il?
    IF (Horizontal(Indice) = (C�t�+1)*C�t�)
       OR (Vertical(Indice) = (C�t�+1)*C�t�) THEN
      -- l'adversaire a rempli la ligne ou la colonne no Indice, donc il gagne
      Points_Adversaire := Points_Adversaire + Nombre_Lignes+1;
    END IF;
  END LOOP;
  -- dans ce qui suit, par convention, si les points du joueur
  -- d�passent le nombre total de lignes du jeu, c'est qu'il a gagn�.
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
END Valeur_Carr�;

PROCEDURE D�terminer_Prochain_Choix(Carr�: IN OUT Type_Carr�;
                                    Profondeur: IN Natural;
                                    Joueur: IN Type_Joueur) IS
-- Rechercher le meilleur jeu de Joueur � un niveau de projection Profondeur
  
  PROCEDURE Trouver_Meilleur_Choix(Arbre: IN OUT Arbres.Arbre_Binaire;
                                   Joueur: IN Type_Joueur;
                                   Meilleur: IN OUT Situation;
                                   Valeur: IN OUT Integer) IS
  -- Trouver le meilleur endroit o� jouer � partir de l'arbre �tendu
  Val_Temp: Integer;
  �l�ment, Meilleur_Temp: Situation;
  Noeud: CInterfaceTicTacToe.Noeuds.Noeud_Binaire;
  BEGIN
    �l�ment := Arbres.Valeur_Courante(Arbre);
    IF NOT Arbres.Noeud_Existe(Arbre, Arbres.Gauche) THEN
      Valeur   := Valeur_Carr�(�l�ment.Carr�, Joueur);
      Meilleur := �l�ment;
    ELSE
      Arbres.D�placer_Courant(Arbre, Arbres.Gauche);
      Noeud := Arbres.Noeud_Courant(Arbre);
      Trouver_Meilleur_Choix(Arbre, Adversaire(Joueur), Meilleur, Valeur);
      Arbres.Positionner_Noeud_Courant(Arbre, Noeud);
      Meilleur := Arbres.Valeur_Courante(Arbre);
      IF �l�ment.Tour = Moins THEN
        Valeur := -Valeur;
      END IF;
      WHILE Arbres.Noeud_Existe(Arbre, Arbres.Droite) LOOP
        Arbres.D�placer_Courant(Arbre, Arbres.Droite);
        Noeud := Arbres.Noeud_Courant(Arbre);
        Trouver_Meilleur_Choix(Arbre, Adversaire(Joueur), Meilleur_Temp, Val_Temp);
        Arbres.Positionner_Noeud_Courant(Arbre, Noeud);
        IF �l�ment.Tour = Moins THEN
          Val_Temp := -Val_Temp;
        END IF;
        IF Val_Temp > Valeur THEN
          Valeur := Val_Temp;
          Meilleur := Arbres.Valeur_Courante(Arbre);
        END IF;
      END LOOP;
      IF �l�ment.Tour = Moins THEN  -- Min(a, b) = -Max(-a, -b)
        Valeur := -Valeur;
      END IF;
    END IF;
  END Trouver_Meilleur_Choix;

  PROCEDURE Engendrer_Arbre(Arbre: IN OUT Arbres.Arbre_Binaire;
                            Niveau: IN Natural;
                            Profondeur: IN Natural;
                            Joueur: IN Type_Joueur) IS
  -- Expansion de l'arbre � partir d'un carr�: g�n�ration de tous les carr�s 
  -- possibles puis r�p�tition pour chacun d'eux jusqu'� la Profondeur d�sir�e

    PROCEDURE Engendrer_Carr�s(Arbre: IN OUT Arbres.Arbre_Binaire;
                               Joueur: IN Type_Joueur) IS
    -- G�n�ration des carr�s obtenus � partir du carr� courant o� Joueur
    -- va jouer

    Premi�re_Fois: Boolean;
    �l�ment: Situation;
    Carr�: Type_Carr�;
    BEGIN
      Premi�re_Fois := True;
      �l�ment := Arbres.Valeur_Courante(Arbre);
      Carr� := �l�ment.Carr�;
      FOR Rang�e IN Indice_Carr� LOOP
        FOR Colonne IN Indice_Carr� LOOP
          IF Carr�(Rang�e, Colonne) = Libre THEN
            �l�ment.Carr� := Carr�;
            �l�ment.Carr�(Rang�e, Colonne) := Joueur;
            IF Premi�re_Fois THEN -- premier descendant � gauche
              Arbres.Ins�rer_Noeud(Arbre, �l�ment, Arbres.Gauche);
              Premi�re_Fois := False;
            ELSE -- on ins�re un fr�re � droite
              Arbres.Ins�rer_Noeud(Arbre, �l�ment, Arbres.Droite);
            END IF;
          END IF;
        END LOOP;
      END LOOP;
    END Engendrer_Carr�s;
  
  Parent: CInterfaceTicTacToe.Noeuds.Noeud_Binaire;
  �l�ment: Situation;
  Tour_Parent: Type_Niveau;

  BEGIN -- Engendrer_Arbre
    IF Niveau < Profondeur THEN
      �l�ment := Arbres.Valeur_Courante(Arbre);
      Tour_Parent := �l�ment.Tour;
      Parent := Arbres.Noeud_Courant(Arbre);
      Engendrer_Carr�s(Arbre, Joueur);
      Arbres.Positionner_Noeud_Courant(Arbre, Parent);
      IF Arbres.Noeud_Existe(Arbre, Arbres.Gauche) THEN
        Arbres.D�placer_Courant(Arbre, Arbres.Gauche);
        LOOP
          �l�ment := Arbres.Valeur_Courante(Arbre);
          IF Tour_Parent = Plus THEN
            �l�ment.Tour := Moins;
          ELSE
            �l�ment.Tour := Plus;
          END IF;
          Arbres.Modifier_Courant(Arbre, �l�ment);
          Parent := Arbres.Noeud_Courant(Arbre);
          Engendrer_Arbre(Arbre, Niveau+1, Profondeur, Adversaire(Joueur));
          Arbres.Positionner_Noeud_Courant(Arbre, Parent);
          EXIT WHEN NOT Arbres.Noeud_Existe(Arbre, Arbres.Droite);
          Arbres.D�placer_Courant(Arbre, Arbres.Droite);
        END LOOP;
      END IF;
    END IF;
  END Engendrer_Arbre;

Arbre: Arbres.Arbre_Binaire;
Meilleur: Situation;
Valeur: Integer;

BEGIN -- D�terminer_Prochain_Choix
  Meilleur.Carr� := Carr�;  -- O vient de jouer, au tour de X
  Meilleur.Tour := Plus;
  Arbres.Ins�rer_Noeud(Arbre, Meilleur, Arbres.Racine);
  Engendrer_Arbre(Arbre, 0, Profondeur, Joueur);
  Arbres.D�placer_Courant(Arbre, Arbres.Racine);
  --Arbres.Afficher_Arbre(Arbre);
  Trouver_Meilleur_Choix(Arbre, Joueur, Meilleur, Valeur);
  Carr� := Meilleur.Carr�;
END D�terminer_Prochain_Choix;

FUNCTION Gagne(Carr�: Type_Carr�; Joueur: Type_Joueur) RETURN Boolean IS
-- V�rification si Joueur gagne
Indice: Indice_Vecteur := Indice_Vecteur'First;
Horizontal, Vertical: Vecteur;
Fini: Boolean := False;
BEGIN
  Positions_Possibles(Carr�, Horizontal, Vertical, Joueur);
  LOOP
    Fini := (Horizontal(Indice) = C�t�) OR (Vertical(Indice) = C�t�);
    EXIT WHEN Fini OR Indice = Indice_Vecteur'Last;
    Indice := Indice+1;
  END LOOP;
  RETURN Fini;
END Gagne;

FUNCTION Partie_Termin�e(Carr�: Type_Carr�) RETURN Boolean IS
-- V�rification si le carr� est plein
Case_Libre: Boolean := False;
BEGIN
  FOR Rang�e IN Indice_Carr� LOOP
    FOR Colonne IN Indice_Carr� LOOP
      IF Carr�(Rang�e, Colonne) = Libre THEN
        Case_Libre := True;
      END IF;
    END LOOP;
  END LOOP;
  RETURN NOT Case_Libre;
END Partie_Termin�e;

PROCEDURE Demander_Coordonn�es(Rang�e: OUT Indice_Carr�;
                               Colonne: OUT Indice_Carr�) IS
-- Lecture des coordonn�es de la position utilis�e
Coordonn�es_Ok: Boolean := False;
BEGIN
  LOOP
    Bloc_Coordonn�es:
    BEGIN
      Ada.Text_IO.Put("O� jouez-vous (m n)?  ");
      Ada.Integer_Text_IO.Get(Rang�e);
      Ada.Integer_Text_IO.Get(Colonne);
      Ada.Text_IO.Skip_Line;
      Coordonn�es_Ok := True;
    EXCEPTION 
      WHEN Constraint_Error =>
        Ada.Text_IO.Skip_Line;
        Ada.Text_IO.Put_Line
                  ("Coordonn�es hors des limites du carr�, veuillez recommencer...");
      WHEN Ada.Text_IO.Data_Error =>
        Ada.Text_IO.Skip_Line;
        Ada.Text_IO.Put_Line ("Coordonn�es non num�riques, veuillez recommencer...");
    END Bloc_Coordonn�es;
    EXIT WHEN Coordonn�es_Ok;
    Ada.Text_IO.Put (Ada.Characters.Latin_1.BEL);
  END LOOP;
  Ada.Text_IO.New_Line;
END Demander_Coordonn�es;

PROCEDURE Afficher_Carr�(Carr�: IN Type_Carr�) IS
-- Afficher un carr� sur l'�cran
  PROCEDURE Afficher_Ligne IS
  --
  BEGIN
    FOR Colonne IN Indice_Carr� LOOP
      Ada.Text_IO.Put("+---");
    END LOOP;
    Ada.Text_IO.Put_Line("+");
  END Afficher_Ligne;

BEGIN -- Afficher_Carr�
  Afficher_Ligne;
  FOR Rang�e IN Indice_Carr� LOOP
    FOR Colonne IN Indice_Carr� LOOP
      IF Carr�(Rang�e, Colonne) = X THEN
        Ada.Text_IO.Put("| X ");
      ELSIF Carr�(Rang�e, Colonne) = O THEN
        Ada.Text_IO.Put("| O ");
      ELSE
        Ada.Text_IO.Put("|   ");
      END IF;
    END LOOP;
    Ada.Text_IO.Put_Line("|");
    Afficher_Ligne;
  END LOOP;
END Afficher_Carr�;

Jeu: Type_Carr�;
Rang�e, Colonne: Indice_Carr�;
R�ponse: Character;
Ordinateur_A_Commenc�: Boolean;

BEGIN -- TicTacToe
  LOOP
    FOR Rang�e IN Indice_Carr� LOOP
      FOR Colonne IN Indice_Carr� LOOP
        Jeu(Rang�e, Colonne) := Libre;
      END LOOP;
    END LOOP;
    Ada.Text_IO.Put_Line("Jeu de Tic Tac Toe");
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put("Voulez-vous jouer en premier (O/N)?  ");
    Ada.Text_IO.Get(Item => R�ponse);
    Ada.Text_IO.Skip_Line;
    IF R�ponse = 'O' OR R�ponse = 'o' THEN
      Demander_Coordonn�es(Rang�e, Colonne);
      Jeu(Rang�e, Colonne) := Humain;
      Ordinateur_A_Commenc� := False;
    ELSE
      IF C�t� MOD 2 = 0 THEN -- C�t� est pair
        Rang�e := C�t�/2;
      ELSE
        Rang�e := 1 + C�t�/2;
      END IF;
      Colonne := 1 + C�t�/2;
      Jeu(Rang�e, Colonne) := Ordinateur;
      Ordinateur_A_Commenc� := True;
    END IF;
    LOOP
      IF Ordinateur_A_Commenc� THEN
        Ordinateur_A_Commenc� := False;
      ELSE
        D�terminer_Prochain_Choix(Jeu, Profondeur, Ordinateur);
      END IF;
      Afficher_Carr�(Jeu);
      IF NOT Gagne(Jeu, Ordinateur)
         AND NOT Partie_Termin�e(Jeu) THEN
        LOOP
          Demander_Coordonn�es(Rang�e, Colonne);
          EXIT WHEN Jeu(Rang�e, Colonne) = Libre;
          Ada.Text_IO.Put_Line("Cette case n'est pas libre, veuillez recommencer...");
          Ada.Text_IO.Put(Ada.Characters.Latin_1.BEL);
        END LOOP;
        Jeu(Rang�e, Colonne) := Humain;
        Afficher_Carr�(Jeu);
      END IF;
      EXIT WHEN Gagne(Jeu, Ordinateur)
                OR Gagne(Jeu, Humain)
                OR Partie_Termin�e(Jeu);
    END LOOP;
    IF Gagne(Jeu, Humain) THEN
      Ada.Text_IO.Put_Line("F�licitations!!!");
    ELSIF Gagne(Jeu, Ordinateur) THEN
      Ada.Text_IO.Put_Line("J'ai gagn�!!!");
    ELSE
      Ada.Text_IO.Put_Line("Ex aequo...");
    END IF;
    Ada.Text_IO.Put("Voulez-vous rejouer (O/N)?  ");
    Ada.Text_IO.Get(R�ponse);
    EXIT WHEN R�ponse /= 'O' AND R�ponse /= 'o';
  END LOOP;
END CTicTacToeBin;
