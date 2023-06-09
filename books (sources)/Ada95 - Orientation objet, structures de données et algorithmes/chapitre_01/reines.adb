--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PACKAGE BODY Reines IS

FUNCTION ReineCréée(Rang: IN Integer; Voisine: IN Reine) RETURN Reine IS
-- Une reine par rangée, avec une voisine à sa gauche.
Majesté: Reine;
BEGIN
  Majesté := NEW UneReine;   -- objet créé
  Majesté.Colonne := 0;      -- valeur qui change au cours du traitement
  Majesté.Rangée := Rang;    -- valeur fixée une fois pour toutes
  Majesté.Voisine := Voisine;-- voisine de gauche
  RETURN Majesté;
END ReineCréée;

FUNCTION SolutionSuivante(Majesté: IN Reine) RETURN Boolean; -- à venir
  
FUNCTION AttaquePossible(Majesté: IN Reine; Rang, Col: IN Integer) RETURN Boolean IS
-- Sa majesté peut-elle attaquer la position?
DiffRangs: Integer;
BEGIN
  IF Majesté.Colonne = Col THEN   -- même colonne
    RETURN True;
  ELSE   -- diagonale: différence des colonnes = différence des rangées
    DiffRangs := Rang - Majesté.Rangée;
    IF (Majesté.Colonne + DiffRangs = Col) OR (Majesté.Colonne - DiffRangs = Col) THEN
      RETURN True;
    ELSIF Majesté.Voisine = NULL THEN
      RETURN False;
    ELSE
      RETURN AttaquePossible(Majesté.Voisine, Rang, Col);
    END IF;
  END IF;
END AttaquePossible;

FUNCTION OKOuAvance(Majesté: IN Reine) RETURN Boolean IS
-- Position OK? Si non avancer.
BEGIN
  IF Majesté.Voisine = NULL THEN
    RETURN True;
  ELSIF AttaquePossible(Majesté.Voisine, Majesté.Rangée, Majesté.Colonne) THEN
    RETURN SolutionSuivante(Majesté);
  ELSE
    RETURN True;
  END IF;
END OKOuAvance;

FUNCTION PremièreSolution(Majesté: IN Reine) RETURN Boolean IS
-- Initialiser la colonne de la reine, puis trouver la première
-- solution acceptable pour sa majesté et ses voisines.
BEGIN
  Majesté.Colonne := 1;
  IF Majesté.Voisine = NULL THEN   -- pas de voisine dangereuse
    RETURN True;
  ELSIF NOT PremièreSolution(Majesté.Voisine) THEN
    RETURN False;
  ELSE
    RETURN OKouAvance(Majesté);    -- position sûre
  END IF;
END PremièreSolution;

FUNCTION SolutionSuivante(Majesté: IN Reine) RETURN Boolean IS
-- Avancer la reine à la colonne suivante, puis trouver la prochaine
-- solution acceptable.
BEGIN
  IF Majesté.Colonne = 8 THEN      -- on sort du damier
    IF Majesté.Voisine = NULL THEN -- demander aux voisines
      RETURN False;
    ELSIF NOT SolutionSuivante(Majesté.Voisine) THEN
      RETURN False;
    ELSE
      Majesté.Colonne := 1; -- relancer la recherche de la première colonne
       RETURN OKouAvance(Majesté);
    END IF;
  ELSE                      -- avancer la colonne et vérifier avec voisines
    Majesté.Colonne := Majesté.Colonne + 1;
    RETURN OKouAvance(Majesté);
  END IF;
END SolutionSuivante;

PROCEDURE Afficher(Majesté: IN Reine) IS
-- L'affichage se propage d'abord aux voisines.
BEGIN
  IF Majesté.Voisine /= NULL THEN
    Afficher(Majesté.Voisine);
  END IF;
  Ada.Text_IO.Put(" Rangée ");
  Ada.Integer_Text_IO.Put(Majesté.Rangée);
  Ada.Text_IO.Put(" Colonne ");
  Ada.Integer_Text_IO.Put(Majesté.Colonne);
  Ada.Text_IO.New_Line;
END Afficher;

END Reines; 
