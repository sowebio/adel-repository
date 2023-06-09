--          Copyright © 1997 Philippe J. Gabrini
WITH Ada.Unchecked_Deallocation, Ada.Text_IO, Ada.Integer_Text_IO;
PACKAGE BODY RéÉquilibrage IS

PROCEDURE Libérer IS NEW Ada.Unchecked_Deallocation(NoeudArbre, ArbreBinaireRecherche);

TYPE Élément IS RECORD
                  Carac: Character;
                  Num: Natural;
                END RECORD;
TYPE Tampon IS ARRAY (0..NoeudsMax) OF Élément;

PROCEDURE Rééquilibrer(Arbre: IN OUT ArbreBinaireRecherche) IS
-- Étant donné un Arbre binaire de recherche, retourner un arbre binaire 
-- de recherche équilibré.

  PROCEDURE DétruireArbre(Arbre: IN OUT ArbreBinaireRecherche) IS
  -- Détruire Arbre et libérer la mémoire.
  BEGIN
    IF Arbre /= NULL THEN
      DétruireArbre(Arbre.Gauche);
      DétruireArbre(Arbre.Droite);
      Libérer(Arbre);
    END IF;
  END DétruireArbre;

  PROCEDURE AplatirArbre(Arbre: IN ArbreBinaireRecherche; Table: IN OUT Tampon;
                         Compteur: IN OUT Natural) IS
  -- Placer tous les éléments d'Arbre dans la Table ordonnée.
  BEGIN
    IF Arbre.Gauche /= NULL THEN
      AplatirArbre(Arbre.Gauche, Table, Compteur);
    END IF;
    Table(Compteur).Carac := Arbre.Caractère;
    Table(Compteur).Num := Arbre.Numéro;
    Compteur := Compteur + 1;
    IF Arbre.Droite /= NULL THEN
      AplatirArbre(Arbre.Droite, Table, Compteur);
    END IF;
  END AplatirArbre;

  PROCEDURE Reconstruire(Table: IN Tampon; Bas, Haut: IN OUT Natural;
                         NouvelArbre: IN OUT ArbreBinaireRecherche) IS
  -- À partir de la Table ordonnée, retourner un arbre binaire de recherche équilibré.
  NouveauBas, Milieu, NouveauHaut: Natural;
  BEGIN
    NouvelArbre := NEW NoeudArbre;  -- construire racine
    Milieu := Bas + (Haut - Bas) / 2;
    NouvelArbre.Caractère := Table(Milieu).Carac;
    NouvelArbre.Numéro := Table(Milieu).Num;
    IF Bas <= (Milieu - 1) THEN     -- construire sous-arbre gauche
      NouveauHaut := Milieu - 1;
      Reconstruire(Table, Bas, NouveauHaut, NouvelArbre.Gauche);
    ELSE
      NouvelArbre.Gauche := NULL;
    END IF;
    IF (Milieu + 1) <= Haut THEN    -- construire sous-arbre droit
      NouveauBas := Milieu + 1;
      Reconstruire(Table, NouveauBas, Haut, NouvelArbre.Droite);
    ELSE
      NouvelArbre.Droite := NULL;
    END IF;
  END Reconstruire;

Table: Tampon;
Compteur: Natural;
Bas, Haut: Integer;

BEGIN -- Rééquilibrer
  Compteur := 0;   -- conserver nombre de noeuds dans l'arbre
  IF Arbre /= NULL THEN -- rééquilibrer
    AplatirArbre(Arbre, Table, Compteur);
    DétruireArbre(Arbre);
    Bas := 0;
    Haut := Compteur - 1;
    Reconstruire(Table, Bas, Haut, Arbre);
  END IF;
END Rééquilibrer;

PROCEDURE ConstruireArbre(Clefs: IN String; OrdreNum: IN Boolean;
                          NumMax: IN Natural;  Arbre: IN OUT ArbreBinaireRecherche) IS
-- Construire un arbre binaire de recherche avec une clef en deux parties: 
-- un caractère pris séquentiellement dans Clefs et un numéro allant de 0 à NumMax.

  FUNCTION Inférieur(Car1: Character; Num1: Natural;
                     Car2: Character; Num2: Natural) RETURN Boolean IS
  -- Comparaison de clefs.
  BEGIN
    IF Car1 < Car2 THEN
      RETURN True;
    ELSIF Car1 > Car2 THEN
      RETURN False;
    ELSE
      RETURN Num1 < Num2;
    END IF;
  END Inférieur;

  FUNCTION Pos(Car: Character; Chaîne: String) RETURN Natural IS
  -- Trouver la position de Car dans Chaîne.
  BEGIN
    FOR Index IN 1..Chaîne'Last LOOP
      IF Car = Chaîne(Index) THEN
	    RETURN Index;
	  END IF;
    END LOOP;
	RETURN 0;
  END Pos;

  PROCEDURE ProchaineClef(Clefs: IN String; OrdreNum: IN Boolean;
                          Car: IN OUT Character; Num: IN OUT Natural) IS
  -- Construire prochaine clef.
  BEGIN
    IF OrdreNum THEN      -- A0, A1, A2, ...
      IF Num < NumMax THEN
        Num := Num + 1;
      ELSE
        Num := 0;
        Car := Clefs(Pos(Car, Clefs)+1);
      END IF;
    ELSE                 -- A0, B0, C0, ...
      IF Pos(Car, Clefs) < Clefs'Last THEN
        Car := Clefs(Pos(Car, Clefs)+1);
      ELSE
        Car := Clefs(1);
        Num := Num + 1;
      END IF;
    END IF;
  END ProchaineClef;

  PROCEDURE Insérer(Car: IN Character; Num: IN Natural;
                    Arbre: IN OUT ArbreBinaireRecherche) IS
  -- Insérer un noeud avec la valeur Car, Num dans Arbre
  BEGIN
    IF Arbre = NULL THEN    -- construire noeud
      Arbre := NEW NoeudArbre;
      Arbre.Caractère := Car;
      Arbre.Numéro := Num;
      Arbre.Gauche := NULL;
      Arbre.Droite := NULL;
    ELSIF Inférieur(Car, Num, Arbre.Caractère, Arbre.Numéro) THEN -- gauche
      Insérer(Car, Num, Arbre.Gauche);
    ELSIF Inférieur(Arbre.Caractère, Arbre.Numéro, Car, Num) THEN -- droite
      Insérer(Car, Num, Arbre.Droite);
    END IF;
  END Insérer;

Car: Character;
Num: Natural;
    
BEGIN	-- ConstruireArbre
  Arbre := NULL;
  Car := Clefs(1); Num := 0;
  LOOP
    Insérer(Car, Num, Arbre);
    IF (Car = Clefs(Clefs'Last))
       AND (Num = NumMax) THEN
	      EXIT;
    END IF;
    ProchaineClef(Clefs, OrdreNum, Car, Num);
  END LOOP;
END ConstruireArbre;

PROCEDURE TraverserInfixe(Arbre: IN ArbreBinaireRecherche) IS
-- Effectuer une traversée en ordre infixe de Arbre et afficher la clef de chaque noeud.
BEGIN
  IF Arbre /= NULL THEN
    TraverserInfixe(Arbre.Gauche);
    Ada.Text_IO.Put(Item => Arbre.Caractère);
	Ada.Integer_Text_IO.Put(Item => Arbre.Numéro, Width => 1);
	Ada.Text_IO.Put(Item => ' ');
    TraverserInfixe(Arbre.Droite);
  END IF;
END TraverserInfixe;

PROCEDURE Afficher(Arbre: IN ArbreBinaireRecherche; Décalage: IN Natural) IS
-- Afficher l'arbre au complet de gauche à droite, avec la racine à gauche.
BEGIN
  IF Arbre /= NULL THEN
    IF Arbre.Droite /= NULL THEN
      Afficher(Arbre.Droite, Décalage+1);
    END IF;
    FOR Décal IN 1..Décalage LOOP
      Ada.Text_IO.Put(Item => "   ");
    END LOOP;
    Ada.Text_IO.Put(Item => Arbre.Caractère);
	Ada.Integer_Text_IO.Put(Item => Arbre.Numéro, Width => 1); Ada.Text_IO.New_Line;
    IF Arbre.Gauche /= NULL THEN
      Afficher(Arbre.Gauche, Décalage+1);
    END IF;
  END IF;
END Afficher;

PROCEDURE AfficherArbre(Arbre: IN ArbreBinaireRecherche) IS
-- Afficher l'arbre au complet de gauche à droite, avec la racine à gauche.
BEGIN
  Afficher(Arbre, 0);
END AfficherArbre;

END RéÉquilibrage;

