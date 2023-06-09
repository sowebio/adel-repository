--          Copyright © 1998 Philippe J. Gabrini
PACKAGE BODY ChaînesS IS

FUNCTION Longueur(Chaîne: TypChaîne) RETURN Natural IS
-- Retourne la longueur d'une chaîne. 
BEGIN
 RETURN Chaîne.Longueur;
END Longueur;

FUNCTION Tranche(Source: TypChaîne; Bas: Positive;
                 Haut: Natural) RETURN TypChaîne IS
-- Retourne la partie de Source allant de Bas à Haut.
Résultat: TypChaîne;
BEGIN
  IF Haut <= MaxChaîne THEN
    Résultat.Longueur := Haut-Bas+1;
    Résultat.Caractères(1..Haut-Bas+1) := Source.Caractères(Bas..Haut);
  ELSE
    Résultat.Longueur := 0;
  END IF;
  RETURN Résultat;
END Tranche;

PROCEDURE Supprimer(Source: IN OUT TypChaîne; De, À: IN Natural) IS
-- Supprimer une partie d'une chaîne Source.
BEGIN
  Source.Caractères(De..Source.Longueur-(À-De+1)) :=
                            Source.Caractères(À+1..Source.Longueur);
  Source.Longueur := Source.Longueur - (À-De+1);
END Supprimer;

PROCEDURE Insérer(Source: IN OUT TypChaîne; Avant: IN Positive;
                  Nouveau: IN TypChaîne) IS
-- Insérer Nouveau dans Source avant Avant.
AncienneLongueur: Natural;
BEGIN
  AncienneLongueur := Source.Longueur;
  IF AncienneLongueur >= 0 THEN
    IF Avant = AncienneLongueur + 1 THEN  -- concaténer
      Source.Longueur := Nouveau.Longueur + AncienneLongueur;
      Source.Caractères(Avant..Avant+Nouveau.Longueur-1) :=
                             Nouveau.Caractères(1..Nouveau.Longueur);
	  ELSIF Avant <= AncienneLongueur THEN  -- cas général
      IF AncienneLongueur /= 0 THEN
        -- Source non vide, déplacer caractères
        Source.Longueur := Nouveau.Longueur + AncienneLongueur;
        Source.Caractères(Avant+Nouveau.Longueur..Source.Longueur) :=
                          Source.Caractères(Avant..AncienneLongueur);
	    END IF;
      Source.Caractères(Avant..Avant+Nouveau.Longueur-1) :=
                             Nouveau.Caractères(1..Nouveau.Longueur);
	  END IF;
  END IF;
END Insérer;
            
FUNCTION "&" (Gauche: TypChaîne; Droite: TypChaîne) RETURN TypChaîne IS
-- Concaténer chaînes Gauche et Droite. 
Résultat: TypChaîne;
BEGIN
  Résultat.Longueur := Gauche.Longueur + Droite.Longueur;
  Résultat.Caractères(1..Gauche.Longueur) :=
                            Gauche.Caractères(1..Gauche.Longueur);
  Résultat.Caractères(Gauche.Longueur+1..Résultat.Longueur) :=
                            Droite.Caractères(1..Droite.Longueur);
  RETURN Résultat;
END "&";

FUNCTION "&" (Gauche: TypChaîne; Droite: Character) RETURN TypChaîne IS
-- Concaténer chaîne Gauche et caractère Droite.
Résultat: TypChaîne;
BEGIN
  Résultat.Longueur := Gauche.Longueur + 1;
  Résultat.Caractères(1..Gauche.Longueur) :=
                            Gauche.Caractères(1..Gauche.Longueur);
  Résultat.Caractères(Résultat.Longueur) := Droite;
  RETURN Résultat;
END "&";

FUNCTION "&" (Gauche: Character; Droite: TypChaîne) RETURN TypChaîne IS
-- Concaténer caractère Gauche et chaîne Droite.
Résultat: TypChaîne;
BEGIN
  Résultat.Longueur  := Droite.Longueur + 1;
  Résultat.Caractères(1) := Gauche;
  Résultat.Caractères(2..Résultat.Longueur) :=
                            Droite.Caractères(1..Droite.Longueur);
  RETURN Résultat;
END "&";

FUNCTION "&" (Gauche: TypChaîne; Droite: String) RETURN TypChaîne IS
-- Concaténer chaîne Gauche et cahîne statique Droite
Résultat: TypChaîne;
LongueurDroite: Integer;  -- nombre de caractères dans Droite
BEGIN
  LongueurDroite  := Droite'Last - Droite'First + 1;
  Résultat.Longueur := Gauche.Longueur + LongueurDroite;
  Résultat.Caractères(1..Gauche.Longueur) :=
                                Gauche.Caractères(1..Gauche.Longueur);
  Résultat.Caractères(Gauche.Longueur+1..Résultat.Longueur) := Droite;
  RETURN Résultat;
END "&";

FUNCTION "&" (Gauche: String; Droite: TypChaîne) RETURN TypChaîne IS
-- Concaténer chaîne statique Gauche et chaîne Droite.
Résultat: TypChaîne;
LongueurGauche: Integer;  -- nombre de caractères dans Gauche
BEGIN
  LongueurGauche   := Gauche'Last - Gauche'First + 1;
  Résultat.Longueur := LongueurGauche + Droite.Longueur;
  Résultat.Caractères(1..LongueurGauche) := Gauche;
  RETURN Résultat;
END "&";

FUNCTION ">"(Gauche, Droite: TypChaîne) RETURN Boolean IS
-- Comparaison lexicale selon l'ensemble de caractères.
BEGIN
  RETURN  Gauche.Caractères(1..Gauche.Longueur) 
                         > Droite.Caractères(1..Droite.Longueur);
END ">";

FUNCTION "="(Gauche, Droite: TypChaîne) RETURN Boolean IS
-- Comparaison de l'égalité de deux chaînes.
BEGIN
  RETURN  Gauche.Caractères(1..Gauche.Longueur) 
                         = Droite.Caractères(1..Droite.Longueur);
END "=";

FUNCTION Élément(Source: TypChaîne; Index: IN Natural) RETURN Character IS
-- Extrait un caractère d'une chaîne.
BEGIN
  IF Index <= Source.Longueur THEN
    RETURN Source.Caractères(Index);
  ELSE
    RETURN Character'Val(0);
  END IF;
END Élément;

PROCEDURE RemplacerÉlément(Car: IN Character; Source: IN OUT TypChaîne;
                           Index: IN Natural) IS
-- Range un caractère dans une chaîne.
BEGIN
 IF Index <= Source.Longueur THEN
   Source.Caractères(Index) := Car;
 ELSIF (Index = Source.Longueur+1) AND (Index <= MaxChaîne) THEN
   -- ajouter à la fin
   Source.Caractères(Index) := Car;
   Source.Longueur := Source.Longueur + 1;
 END IF;
END RemplacerÉlément;

FUNCTION Position(Source, Patron: TypChaîne) RETURN Natural IS
-- Recherche sous-chaîne Patron dans chaîne Source.
Premier: Positive;       -- Patron est comparé à
Dernier: Positive;       -- Source(Premier..Dernier)
Pos: Natural;
BEGIN
  Premier := 1;          -- commencer au début de la chaîne
  Dernier  := Patron.Longueur;
  BoucleRecherche:
  LOOP  -- Sortir quand il reste moins de caractères dans Source 
        -- qu'il n'y en a dans Patron ou si Patron trouvé.
    EXIT WHEN  Dernier > Source.Longueur
             OR ELSE Source.Caractères(Premier..Dernier) 
                     = Patron.Caractères(1..Patron.Longueur); 
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

FUNCTION À_Statique(Chaîne: IN TypChaîne) RETURN String IS
-- Conversion d'une chaîne dynamique à une chaîne statique de type String.
BEGIN
  RETURN Chaîne.Caractères(1..Chaîne.Longueur);  -- retourne la tranche
END À_Statique;
  
FUNCTION À_Dynamique(Source: IN String) RETURN TypChaîne IS
-- Conversion d'une chaîne statique de type String à une chaîne dynamique.
Résultat : TypChaîne;
BEGIN
  Résultat.Longueur := Source'Last - Source'First + 1;
  Résultat.Caractères(1..Résultat.Longueur) := Source;
  RETURN Résultat;
END À_Dynamique;

END ChaînesS;

