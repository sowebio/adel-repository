--          Copyright � 1998 Philippe J. Gabrini
PACKAGE BODY Cha�nesS IS

FUNCTION Longueur(Cha�ne: TypCha�ne) RETURN Natural IS
-- Retourne la longueur d'une cha�ne. 
BEGIN
 RETURN Cha�ne.Longueur;
END Longueur;

FUNCTION Tranche(Source: TypCha�ne; Bas: Positive;
                 Haut: Natural) RETURN TypCha�ne IS
-- Retourne la partie de Source allant de Bas � Haut.
R�sultat: TypCha�ne;
BEGIN
  IF Haut <= MaxCha�ne THEN
    R�sultat.Longueur := Haut-Bas+1;
    R�sultat.Caract�res(1..Haut-Bas+1) := Source.Caract�res(Bas..Haut);
  ELSE
    R�sultat.Longueur := 0;
  END IF;
  RETURN R�sultat;
END Tranche;

PROCEDURE Supprimer(Source: IN OUT TypCha�ne; De, �: IN Natural) IS
-- Supprimer une partie d'une cha�ne Source.
BEGIN
  Source.Caract�res(De..Source.Longueur-(�-De+1)) :=
                            Source.Caract�res(�+1..Source.Longueur);
  Source.Longueur := Source.Longueur - (�-De+1);
END Supprimer;

PROCEDURE Ins�rer(Source: IN OUT TypCha�ne; Avant: IN Positive;
                  Nouveau: IN TypCha�ne) IS
-- Ins�rer Nouveau dans Source avant Avant.
AncienneLongueur: Natural;
BEGIN
  AncienneLongueur := Source.Longueur;
  IF AncienneLongueur >= 0 THEN
    IF Avant = AncienneLongueur + 1 THEN  -- concat�ner
      Source.Longueur := Nouveau.Longueur + AncienneLongueur;
      Source.Caract�res(Avant..Avant+Nouveau.Longueur-1) :=
                             Nouveau.Caract�res(1..Nouveau.Longueur);
	  ELSIF Avant <= AncienneLongueur THEN  -- cas g�n�ral
      IF AncienneLongueur /= 0 THEN
        -- Source non vide, d�placer caract�res
        Source.Longueur := Nouveau.Longueur + AncienneLongueur;
        Source.Caract�res(Avant+Nouveau.Longueur..Source.Longueur) :=
                          Source.Caract�res(Avant..AncienneLongueur);
	    END IF;
      Source.Caract�res(Avant..Avant+Nouveau.Longueur-1) :=
                             Nouveau.Caract�res(1..Nouveau.Longueur);
	  END IF;
  END IF;
END Ins�rer;
            
FUNCTION "&" (Gauche: TypCha�ne; Droite: TypCha�ne) RETURN TypCha�ne IS
-- Concat�ner cha�nes Gauche et Droite. 
R�sultat: TypCha�ne;
BEGIN
  R�sultat.Longueur := Gauche.Longueur + Droite.Longueur;
  R�sultat.Caract�res(1..Gauche.Longueur) :=
                            Gauche.Caract�res(1..Gauche.Longueur);
  R�sultat.Caract�res(Gauche.Longueur+1..R�sultat.Longueur) :=
                            Droite.Caract�res(1..Droite.Longueur);
  RETURN R�sultat;
END "&";

FUNCTION "&" (Gauche: TypCha�ne; Droite: Character) RETURN TypCha�ne IS
-- Concat�ner cha�ne Gauche et caract�re Droite.
R�sultat: TypCha�ne;
BEGIN
  R�sultat.Longueur := Gauche.Longueur + 1;
  R�sultat.Caract�res(1..Gauche.Longueur) :=
                            Gauche.Caract�res(1..Gauche.Longueur);
  R�sultat.Caract�res(R�sultat.Longueur) := Droite;
  RETURN R�sultat;
END "&";

FUNCTION "&" (Gauche: Character; Droite: TypCha�ne) RETURN TypCha�ne IS
-- Concat�ner caract�re Gauche et cha�ne Droite.
R�sultat: TypCha�ne;
BEGIN
  R�sultat.Longueur  := Droite.Longueur + 1;
  R�sultat.Caract�res(1) := Gauche;
  R�sultat.Caract�res(2..R�sultat.Longueur) :=
                            Droite.Caract�res(1..Droite.Longueur);
  RETURN R�sultat;
END "&";

FUNCTION "&" (Gauche: TypCha�ne; Droite: String) RETURN TypCha�ne IS
-- Concat�ner cha�ne Gauche et cah�ne statique Droite
R�sultat: TypCha�ne;
LongueurDroite: Integer;  -- nombre de caract�res dans Droite
BEGIN
  LongueurDroite  := Droite'Last - Droite'First + 1;
  R�sultat.Longueur := Gauche.Longueur + LongueurDroite;
  R�sultat.Caract�res(1..Gauche.Longueur) :=
                                Gauche.Caract�res(1..Gauche.Longueur);
  R�sultat.Caract�res(Gauche.Longueur+1..R�sultat.Longueur) := Droite;
  RETURN R�sultat;
END "&";

FUNCTION "&" (Gauche: String; Droite: TypCha�ne) RETURN TypCha�ne IS
-- Concat�ner cha�ne statique Gauche et cha�ne Droite.
R�sultat: TypCha�ne;
LongueurGauche: Integer;  -- nombre de caract�res dans Gauche
BEGIN
  LongueurGauche   := Gauche'Last - Gauche'First + 1;
  R�sultat.Longueur := LongueurGauche + Droite.Longueur;
  R�sultat.Caract�res(1..LongueurGauche) := Gauche;
  RETURN R�sultat;
END "&";

FUNCTION ">"(Gauche, Droite: TypCha�ne) RETURN Boolean IS
-- Comparaison lexicale selon l'ensemble de caract�res.
BEGIN
  RETURN  Gauche.Caract�res(1..Gauche.Longueur) 
                         > Droite.Caract�res(1..Droite.Longueur);
END ">";

FUNCTION "="(Gauche, Droite: TypCha�ne) RETURN Boolean IS
-- Comparaison de l'�galit� de deux cha�nes.
BEGIN
  RETURN  Gauche.Caract�res(1..Gauche.Longueur) 
                         = Droite.Caract�res(1..Droite.Longueur);
END "=";

FUNCTION �l�ment(Source: TypCha�ne; Index: IN Natural) RETURN Character IS
-- Extrait un caract�re d'une cha�ne.
BEGIN
  IF Index <= Source.Longueur THEN
    RETURN Source.Caract�res(Index);
  ELSE
    RETURN Character'Val(0);
  END IF;
END �l�ment;

PROCEDURE Remplacer�l�ment(Car: IN Character; Source: IN OUT TypCha�ne;
                           Index: IN Natural) IS
-- Range un caract�re dans une cha�ne.
BEGIN
 IF Index <= Source.Longueur THEN
   Source.Caract�res(Index) := Car;
 ELSIF (Index = Source.Longueur+1) AND (Index <= MaxCha�ne) THEN
   -- ajouter � la fin
   Source.Caract�res(Index) := Car;
   Source.Longueur := Source.Longueur + 1;
 END IF;
END Remplacer�l�ment;

FUNCTION Position(Source, Patron: TypCha�ne) RETURN Natural IS
-- Recherche sous-cha�ne Patron dans cha�ne Source.
Premier: Positive;       -- Patron est compar� �
Dernier: Positive;       -- Source(Premier..Dernier)
Pos: Natural;
BEGIN
  Premier := 1;          -- commencer au d�but de la cha�ne
  Dernier  := Patron.Longueur;
  BoucleRecherche:
  LOOP  -- Sortir quand il reste moins de caract�res dans Source 
        -- qu'il n'y en a dans Patron ou si Patron trouv�.
    EXIT WHEN  Dernier > Source.Longueur
             OR ELSE Source.Caract�res(Premier..Dernier) 
                     = Patron.Caract�res(1..Patron.Longueur); 
    Premier := Premier + 1;
    Dernier := Dernier + 1;
  END LOOP BoucleRecherche;
  IF Dernier <= Source.Longueur THEN
    Pos := Premier;
  ELSE
    Pos := 0;
  END IF;
  RETURN Pos;
END Position;

FUNCTION �_Statique(Cha�ne: IN TypCha�ne) RETURN String IS
-- Conversion d'une cha�ne dynamique � une cha�ne statique de type String.
BEGIN
  RETURN Cha�ne.Caract�res(1..Cha�ne.Longueur);  -- retourne la tranche
END �_Statique;
  
FUNCTION �_Dynamique(Source: IN String) RETURN TypCha�ne IS
-- Conversion d'une cha�ne statique de type String � une cha�ne dynamique.
R�sultat : TypCha�ne;
BEGIN
  R�sultat.Longueur := Source'Last - Source'First + 1;
  R�sultat.Caract�res(1..R�sultat.Longueur) := Source;
  RETURN R�sultat;
END �_Dynamique;

END Cha�nesS;

