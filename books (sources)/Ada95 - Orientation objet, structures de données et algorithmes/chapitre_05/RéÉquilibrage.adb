--          Copyright � 1997 Philippe J. Gabrini
WITH Ada.Unchecked_Deallocation, Ada.Text_IO, Ada.Integer_Text_IO;
PACKAGE BODY R��quilibrage IS

PROCEDURE Lib�rer IS NEW Ada.Unchecked_Deallocation(NoeudArbre, ArbreBinaireRecherche);

TYPE �l�ment IS RECORD
                  Carac: Character;
                  Num: Natural;
                END RECORD;
TYPE Tampon IS ARRAY (0..NoeudsMax) OF �l�ment;

PROCEDURE R��quilibrer(Arbre: IN OUT ArbreBinaireRecherche) IS
-- �tant donn� un Arbre binaire de recherche, retourner un arbre binaire 
-- de recherche �quilibr�.

  PROCEDURE D�truireArbre(Arbre: IN OUT ArbreBinaireRecherche) IS
  -- D�truire Arbre et lib�rer la m�moire.
  BEGIN
    IF Arbre /= NULL THEN
      D�truireArbre(Arbre.Gauche);
      D�truireArbre(Arbre.Droite);
      Lib�rer(Arbre);
    END IF;
  END D�truireArbre;

  PROCEDURE AplatirArbre(Arbre: IN ArbreBinaireRecherche; Table: IN OUT Tampon;
                         Compteur: IN OUT Natural) IS
  -- Placer tous les �l�ments d'Arbre dans la Table ordonn�e.
  BEGIN
    IF Arbre.Gauche /= NULL THEN
      AplatirArbre(Arbre.Gauche, Table, Compteur);
    END IF;
    Table(Compteur).Carac := Arbre.Caract�re;
    Table(Compteur).Num := Arbre.Num�ro;
    Compteur := Compteur + 1;
    IF Arbre.Droite /= NULL THEN
      AplatirArbre(Arbre.Droite, Table, Compteur);
    END IF;
  END AplatirArbre;

  PROCEDURE Reconstruire(Table: IN Tampon; Bas, Haut: IN OUT Natural;
                         NouvelArbre: IN OUT ArbreBinaireRecherche) IS
  -- � partir de la Table ordonn�e, retourner un arbre binaire de recherche �quilibr�.
  NouveauBas, Milieu, NouveauHaut: Natural;
  BEGIN
    NouvelArbre := NEW NoeudArbre;  -- construire racine
    Milieu := Bas + (Haut - Bas) / 2;
    NouvelArbre.Caract�re := Table(Milieu).Carac;
    NouvelArbre.Num�ro := Table(Milieu).Num;
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

BEGIN -- R��quilibrer
  Compteur := 0;   -- conserver nombre de noeuds dans l'arbre
  IF Arbre /= NULL THEN -- r��quilibrer
    AplatirArbre(Arbre, Table, Compteur);
    D�truireArbre(Arbre);
    Bas := 0;
    Haut := Compteur - 1;
    Reconstruire(Table, Bas, Haut, Arbre);
  END IF;
END R��quilibrer;

PROCEDURE ConstruireArbre(Clefs: IN String; OrdreNum: IN Boolean;
                          NumMax: IN Natural;  Arbre: IN OUT ArbreBinaireRecherche) IS
-- Construire un arbre binaire de recherche avec une clef en deux parties: 
-- un caract�re pris s�quentiellement dans Clefs et un num�ro allant de 0 � NumMax.

  FUNCTION Inf�rieur(Car1: Character; Num1: Natural;
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
  END Inf�rieur;

  FUNCTION Pos(Car: Character; Cha�ne: String) RETURN Natural IS
  -- Trouver la position de Car dans Cha�ne.
  BEGIN
    FOR Index IN 1..Cha�ne'Last LOOP
      IF Car = Cha�ne(Index) THEN
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

  PROCEDURE Ins�rer(Car: IN Character; Num: IN Natural;
                    Arbre: IN OUT ArbreBinaireRecherche) IS
  -- Ins�rer un noeud avec la valeur Car, Num dans Arbre
  BEGIN
    IF Arbre = NULL THEN    -- construire noeud
      Arbre := NEW NoeudArbre;
      Arbre.Caract�re := Car;
      Arbre.Num�ro := Num;
      Arbre.Gauche := NULL;
      Arbre.Droite := NULL;
    ELSIF Inf�rieur(Car, Num, Arbre.Caract�re, Arbre.Num�ro) THEN -- gauche
      Ins�rer(Car, Num, Arbre.Gauche);
    ELSIF Inf�rieur(Arbre.Caract�re, Arbre.Num�ro, Car, Num) THEN -- droite
      Ins�rer(Car, Num, Arbre.Droite);
    END IF;
  END Ins�rer;

Car: Character;
Num: Natural;
    
BEGIN	-- ConstruireArbre
  Arbre := NULL;
  Car := Clefs(1); Num := 0;
  LOOP
    Ins�rer(Car, Num, Arbre);
    IF (Car = Clefs(Clefs'Last))
       AND (Num = NumMax) THEN
	      EXIT;
    END IF;
    ProchaineClef(Clefs, OrdreNum, Car, Num);
  END LOOP;
END ConstruireArbre;

PROCEDURE TraverserInfixe(Arbre: IN ArbreBinaireRecherche) IS
-- Effectuer une travers�e en ordre infixe de Arbre et afficher la clef de chaque noeud.
BEGIN
  IF Arbre /= NULL THEN
    TraverserInfixe(Arbre.Gauche);
    Ada.Text_IO.Put(Item => Arbre.Caract�re);
	Ada.Integer_Text_IO.Put(Item => Arbre.Num�ro, Width => 1);
	Ada.Text_IO.Put(Item => ' ');
    TraverserInfixe(Arbre.Droite);
  END IF;
END TraverserInfixe;

PROCEDURE Afficher(Arbre: IN ArbreBinaireRecherche; D�calage: IN Natural) IS
-- Afficher l'arbre au complet de gauche � droite, avec la racine � gauche.
BEGIN
  IF Arbre /= NULL THEN
    IF Arbre.Droite /= NULL THEN
      Afficher(Arbre.Droite, D�calage+1);
    END IF;
    FOR D�cal IN 1..D�calage LOOP
      Ada.Text_IO.Put(Item => "   ");
    END LOOP;
    Ada.Text_IO.Put(Item => Arbre.Caract�re);
	Ada.Integer_Text_IO.Put(Item => Arbre.Num�ro, Width => 1); Ada.Text_IO.New_Line;
    IF Arbre.Gauche /= NULL THEN
      Afficher(Arbre.Gauche, D�calage+1);
    END IF;
  END IF;
END Afficher;

PROCEDURE AfficherArbre(Arbre: IN ArbreBinaireRecherche) IS
-- Afficher l'arbre au complet de gauche � droite, avec la racine � gauche.
BEGIN
  Afficher(Arbre, 0);
END AfficherArbre;

END R��quilibrage;

