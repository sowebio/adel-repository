--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PACKAGE BODY Reines IS

FUNCTION ReineCr��e(Rang: IN Integer; Voisine: IN Reine) RETURN Reine IS
-- Une reine par rang�e, avec une voisine � sa gauche.
Majest�: Reine;
BEGIN
  Majest� := NEW UneReine;   -- objet cr��
  Majest�.Colonne := 0;      -- valeur qui change au cours du traitement
  Majest�.Rang�e := Rang;    -- valeur fix�e une fois pour toutes
  Majest�.Voisine := Voisine;-- voisine de gauche
  RETURN Majest�;
END ReineCr��e;

FUNCTION SolutionSuivante(Majest�: IN Reine) RETURN Boolean; -- � venir
  
FUNCTION AttaquePossible(Majest�: IN Reine; Rang, Col: IN Integer) RETURN Boolean IS
-- Sa majest� peut-elle attaquer la position?
DiffRangs: Integer;
BEGIN
  IF Majest�.Colonne = Col THEN   -- m�me colonne
    RETURN True;
  ELSE   -- diagonale: diff�rence des colonnes = diff�rence des rang�es
    DiffRangs := Rang - Majest�.Rang�e;
    IF (Majest�.Colonne + DiffRangs = Col) OR (Majest�.Colonne - DiffRangs = Col) THEN
      RETURN True;
    ELSIF Majest�.Voisine = NULL THEN
      RETURN False;
    ELSE
      RETURN AttaquePossible(Majest�.Voisine, Rang, Col);
    END IF;
  END IF;
END AttaquePossible;

FUNCTION OKOuAvance(Majest�: IN Reine) RETURN Boolean IS
-- Position OK? Si non avancer.
BEGIN
  IF Majest�.Voisine = NULL THEN
    RETURN True;
  ELSIF AttaquePossible(Majest�.Voisine, Majest�.Rang�e, Majest�.Colonne) THEN
    RETURN SolutionSuivante(Majest�);
  ELSE
    RETURN True;
  END IF;
END OKOuAvance;

FUNCTION Premi�reSolution(Majest�: IN Reine) RETURN Boolean IS
-- Initialiser la colonne de la reine, puis trouver la premi�re
-- solution acceptable pour sa majest� et ses voisines.
BEGIN
  Majest�.Colonne := 1;
  IF Majest�.Voisine = NULL THEN   -- pas de voisine dangereuse
    RETURN True;
  ELSIF NOT Premi�reSolution(Majest�.Voisine) THEN
    RETURN False;
  ELSE
    RETURN OKouAvance(Majest�);    -- position s�re
  END IF;
END Premi�reSolution;

FUNCTION SolutionSuivante(Majest�: IN Reine) RETURN Boolean IS
-- Avancer la reine � la colonne suivante, puis trouver la prochaine
-- solution acceptable.
BEGIN
  IF Majest�.Colonne = 8 THEN      -- on sort du damier
    IF Majest�.Voisine = NULL THEN -- demander aux voisines
      RETURN False;
    ELSIF NOT SolutionSuivante(Majest�.Voisine) THEN
      RETURN False;
    ELSE
      Majest�.Colonne := 1; -- relancer la recherche de la premi�re colonne
       RETURN OKouAvance(Majest�);
    END IF;
  ELSE                      -- avancer la colonne et v�rifier avec voisines
    Majest�.Colonne := Majest�.Colonne + 1;
    RETURN OKouAvance(Majest�);
  END IF;
END SolutionSuivante;

PROCEDURE Afficher(Majest�: IN Reine) IS
-- L'affichage se propage d'abord aux voisines.
BEGIN
  IF Majest�.Voisine /= NULL THEN
    Afficher(Majest�.Voisine);
  END IF;
  Ada.Text_IO.Put(" Rang�e ");
  Ada.Integer_Text_IO.Put(Majest�.Rang�e);
  Ada.Text_IO.Put(" Colonne ");
  Ada.Integer_Text_IO.Put(Majest�.Colonne);
  Ada.Text_IO.New_Line;
END Afficher;

END Reines; 
