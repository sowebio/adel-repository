--          Copyright � 1998 Philippe J. Gabrini
PACKAGE Reines IS
TYPE Reine IS PRIVATE; -- la reine a une vie priv�e!
FUNCTION ReineCr��e(Rang: IN Integer; Voisine: IN Reine) RETURN Reine;
-- une reine a �t� cr��e et rattach�e � sa voisine
FUNCTION Premi�reSolution(Majest�: IN Reine) RETURN Boolean;
-- � partir de la colonne initiale donne la premi�re solution acceptable 
-- pour la reine et sa voisine
PROCEDURE Afficher(Majest�: IN Reine);
-- donner les coordonn�es de la reine apr�s sa voisine
PRIVATE
  TYPE UneReine IS               -- ce qui caract�rise une reine
    RECORD
      Colonne, Rang�e: Integer :=0;
      Voisine: Reine;
    END RECORD;
  TYPE Reine IS ACCESS UneReine; -- les reines sont toutes reli�es
END Reines; 
