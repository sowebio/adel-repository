--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Unchecked_Deallocation, Ada.Text_IO;
PACKAGE BODY ArbresBinRechFil�s IS
-- Ce module pr�sente la r�alisation d'arbres binaires de recherche qui
-- utilise un arbre doublement "fil�".  L'utilisation de "fils" permet
-- non seulement de traverser l'arbre de fa�on it�rative dans toutes
-- les directions, mais aussi de faire des travers�es incr�mentales,
-- un noeud � la fois.

PROCEDURE Lib�rer IS NEW
             Ada.Unchecked_Deallocation(NoeudArbre, Arbre_Bin_Rech_Fil�);

FUNCTION ArbreVide(Arbre: Arbre_Bin_Rech_Fil�) RETURN Boolean IS
-- Retourne Vrai si l'Arbre est vide, et Faux autrement.
BEGIN
  RETURN Arbre = NULL;
END ArbreVide;

PROCEDURE D�truireArbre(Arbre: IN OUT Arbre_Bin_Rech_Fil�)IS
-- D�truit toute la structure d'Arbre.
BEGIN
  IF Arbre /= NULL THEN
    IF NOT Arbre.FilGauche THEN
      D�truireArbre(Arbre.Gauche);
    END IF;
    IF NOT Arbre.FilDroit THEN
      D�truireArbre(Arbre.Droite);
    END IF;
    Lib�rer(Arbre);
  END IF;
END D�truireArbre;

FUNCTION Pr�d�cesseur(Arbre: Arbre_Bin_Rech_Fil�)
                     RETURN Arbre_Bin_Rech_Fil� IS
-- Retourne le pr�d�cesseur d'un noeud donn� d'Arbre.
Noeud: Arbre_Bin_Rech_Fil�;
BEGIN
  IF Arbre = NULL THEN
    RETURN NULL;
  ELSE
    Noeud := Arbre.Gauche;
    IF NOT Arbre.FilGauche THEN
      WHILE NOT Noeud.FilDroit LOOP
        Noeud := Noeud.Droite;
      END LOOP;
    END IF;
    RETURN Noeud;
  END IF;
END Pr�d�cesseur;

FUNCTION Successeur(Arbre: IN Arbre_Bin_Rech_Fil�)
                   RETURN Arbre_Bin_Rech_Fil� IS
-- Retourne le successeur d'un noeud donn� d'Arbre.
Noeud: Arbre_Bin_Rech_Fil�;
BEGIN
  IF Arbre = NULL THEN
    RETURN NULL;
  ELSE
    Noeud := Arbre.Droite;
    IF NOT Arbre.FilDroit THEN
      WHILE NOT Noeud.FilGauche LOOP
        Noeud := Noeud.Gauche;
      END LOOP;
    END IF;
    RETURN Noeud;
  END IF;
END Successeur;

FUNCTION Noeud(Arbre: IN Arbre_Bin_Rech_Fil�; Clef: IN Type�l�ment)
               RETURN Arbre_Bin_Rech_Fil� IS
-- Retourne un pointeur au noeud qui poss�de la Clef donn�e; si la Clef n'est
-- pas dans Arbre, on retourne NULL.
O�: Arbre_Bin_Rech_Fil�;
BEGIN
  O� := Arbre;
  IF (O� = NULL) OR ELSE �l�ments�gaux(O�.�l�ment, Clef) THEN
    RETURN O�;
  ELSIF Inf�rieur(O�.�l�ment, Clef) THEN
    IF O�.FilDroit THEN
      RETURN NULL;
    ELSE
      RETURN Noeud(O�.Droite, Clef);
    END IF;
  ELSE
    IF O�.FilGauche THEN
      RETURN NULL;
    ELSE
      RETURN Noeud(O�.Gauche, Clef);
    END IF;
  END IF;
END Noeud;

PROCEDURE Ins�rerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Fil�;
                       �l�ment: IN Type�l�ment) IS
-- Ins�re �l�ment dans Arbre; si �l�ment existe d�j� dans l'Arbre, ce dernier
-- n'est pas modifi�. 
Parent: Arbre_Bin_Rech_Fil�;
Trouv�: Boolean;

  PROCEDURE Cr�erNoeud(Arbre: IN OUT Arbre_Bin_Rech_Fil�;
                       �lt: IN Type�l�ment;
                       Gauche, Droite: IN Arbre_Bin_Rech_Fil�) IS
  -- Cr�e le noeud Arbre avec les fils mis � Vrai
  BEGIN
    Arbre := NEW NoeudArbre;
    Arbre.�l�ment := �lt;
    Arbre.Gauche := Gauche;
    Arbre.Droite := Droite;
    Arbre.FilGauche := True;
    Arbre.FilDroit := True;
  END Cr�erNoeud;

BEGIN
  IF Arbre = NULL THEN
    Cr�erNoeud(Arbre, �l�ment, NULL, NULL);
  ELSE
    Trouv� := False;
    Parent := Arbre;
    WHILE NOT Trouv� LOOP 
      IF Inf�rieur(�l�ment, Parent.�l�ment) THEN -- regarder � gauche
        IF Parent.FilGauche  THEN
          -- ins�rer comme fils gauche de Parent
          Parent.FilGauche := False;
          Cr�erNoeud(Parent.Gauche, �l�ment, Parent.Gauche, Parent);
          Trouv� := True;
        ELSE -- continuer � chercher
          Parent := Parent.Gauche;
        END IF;
      ELSIF �l�ments�gaux(�l�ment, Parent.�l�ment) THEN
        -- d�j� dans Arbre
        Trouv� := True;
      ELSE       -- regarder � droite
        IF Parent.FilDroit THEN
          -- ins�rer comme fils droit de Parent
          Parent.FilDroit := False;
          Cr�erNoeud(Parent.Droite, �l�ment, Parent, Parent.Droite);
          Trouv� := True;
        ELSE
          Parent := Parent.Droite;
        END IF;
      END IF;
    END LOOP;
  END IF;
END Ins�rerNoeud;

PROCEDURE SupprimerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Fil�;
                         �l�ment: IN Type�l�ment) IS
-- Supprime le noeud de l'Arbre ayant la m�me clef qu'�l�ment; s'il n'y a pas
-- de noeud poss�dant la m�me clef, Arbre n'est pas modifi�.
 
  PROCEDURE SupprimerDescDroit(Parent: IN Arbre_Bin_Rech_Fil�) IS
  -- Supprime le descendant droit du Parent.
  Courant, Droit, Pr�c�dent: Arbre_Bin_Rech_Fil�;
  BEGIN
    NULL;
  END SupprimerDescDroit;
  
  PROCEDURE SupprimerDescGauche(Parent: IN Arbre_Bin_Rech_Fil�) IS
  -- Supprime le descendant gauche du Parent. 
  Courant, Gauche, Pr�c�dent: Arbre_Bin_Rech_Fil�;
  BEGIN
    NULL;
  END SupprimerDescGauche;
  
  PROCEDURE SupprimerRacine(Arbre: IN OUT Arbre_Bin_Rech_Fil�) IS
  -- Supprime la racine de l'Arbre. 
  Courant, Pr�c�dent: Arbre_Bin_Rech_Fil�;
  BEGIN
    NULL;
  END SupprimerRacine;
  
Parent, Noeud: Arbre_Bin_Rech_Fil�;
Fait: Boolean := False;

BEGIN
  NULL;
END SupprimerNoeud;

PROCEDURE TraverserAvant(Arbre: IN Arbre_Bin_Rech_Fil�) IS
-- Effectue une travers�e infixe de l'Arbre et affiche les �l�ments en ordre  
-- ascendant des clefs; on utilise Afficher�l�ment et on s�pare les valeurs 
-- par un espace.
Courant: Arbre_Bin_Rech_Fil�;
BEGIN
  IF Arbre /= NULL THEN
    Courant := Arbre;
    WHILE NOT Courant.FilGauche LOOP
     -- trouver le premier �l�ment de la travers�e
      Courant := Courant.Gauche;
    END LOOP;
    WHILE Courant /= NULL LOOP
      -- trouver les �l�ments suivants au moyen de Successeur
      Afficher�l�ment(Courant.�l�ment); Ada.Text_IO.Put(Item => ' ');
      Courant := Successeur(Courant);
    END LOOP;
  ELSE
    Ada.Text_IO.Put(Item => "arbre vide"); Ada.Text_IO.New_Line;
  END IF;
END TraverserAvant;

PROCEDURE TraverserArri�re(Arbre: IN Arbre_Bin_Rech_Fil�) IS
-- Effectue une travers�e infixe de l'Arbre et affiche les �l�ments en ordre  
-- descendant des clefs; on utilise Afficher�l�ment et on s�pare les valeurs 
-- par un espace.
Courant: Arbre_Bin_Rech_Fil�;
BEGIN
  IF Arbre /= NULL THEN
    Courant := Arbre;
    WHILE NOT Courant.FilDroit LOOP
      Courant := Courant.Droite;
    END LOOP;
    WHILE Courant /= NULL LOOP
      Afficher�l�ment(Courant.�l�ment); Ada.Text_IO.Put(Item => ' ');
      Courant := Pr�d�cesseur(Courant);
    END LOOP;
  ELSE
    Ada.Text_IO.Put(Item =>" arbre vide "); Ada.Text_IO.New_Line;
  END IF;
END TraverserArri�re;

PROCEDURE AfficherNoeud(Arbre: IN Arbre_Bin_Rech_Fil�) IS
-- Affiche la valeur de l'�l�ment au noeud Arbre.
BEGIN
  Afficher�l�ment(Arbre.�l�ment);
END AfficherNoeud;

PROCEDURE AfficherArbre(Arbre: IN Arbre_Bin_Rech_Fil�) IS
-- Affiche l'Arbre de gauche � droite avec la racine � gauche.

  PROCEDURE Afficher(Arbre: IN Arbre_Bin_Rech_Fil�;
                     D�calage: IN Natural) IS
  -- Fait l'affichage de l'Arbre avec d�calages.
  BEGIN
    IF Arbre /= NULL THEN
      IF NOT Arbre.FilDroit THEN
        Afficher(Arbre.Droite, D�calage+1);
      END IF;
      FOR D�cal IN 1..D�calage LOOP
        Ada.Text_IO.Put(Item => "     ");
      END LOOP;
      Afficher�l�ment(Arbre.�l�ment); Ada.Text_IO.New_Line;
      IF NOT Arbre.FilGauche THEN
        Afficher(Arbre.Gauche, D�calage+1);
      END IF;
    END IF;
  END Afficher;

BEGIN
  Afficher(Arbre, 0);
END AfficherArbre;

END ArbresBinRechFil�s;

