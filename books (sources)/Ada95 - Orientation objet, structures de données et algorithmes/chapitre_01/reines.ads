--          Copyright © 1998 Philippe J. Gabrini
PACKAGE Reines IS
TYPE Reine IS PRIVATE; -- la reine a une vie privée!
FUNCTION ReineCréée(Rang: IN Integer; Voisine: IN Reine) RETURN Reine;
-- une reine a été créée et rattachée à sa voisine
FUNCTION PremièreSolution(Majesté: IN Reine) RETURN Boolean;
-- à partir de la colonne initiale donne la première solution acceptable 
-- pour la reine et sa voisine
PROCEDURE Afficher(Majesté: IN Reine);
-- donner les coordonnées de la reine après sa voisine
PRIVATE
  TYPE UneReine IS               -- ce qui caractérise une reine
    RECORD
      Colonne, Rangée: Integer :=0;
      Voisine: Reine;
    END RECORD;
  TYPE Reine IS ACCESS UneReine; -- les reines sont toutes reliées
END Reines; 
