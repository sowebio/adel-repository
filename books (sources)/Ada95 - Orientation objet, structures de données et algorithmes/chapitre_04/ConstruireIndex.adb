WITH Ada.Text_IO, Ada.Integer_Text_IO, Ada.Characters.Latin_1;
WITH ArbresBinRecherche, TDAFile;
PROCEDURE ConstruireIndex IS

LongueurTampon: CONSTANT Natural := 10000;
LongueurNum�ro: CONSTANT Natural := 6;
LongueurMot: CONSTANT Natural := 3 * LongueurNum�ro;
FinCha�ne: CONSTANT Character := Ada.Characters.Latin_1.NUL;
FinPage: CONSTANT Character := '\';
Espace: CONSTANT Character := ' ';
�l�mentsParLigne: CONSTANT Natural := 8;

PACKAGE Files IS NEW TDAFile(Type�l�ment => Natural);
TYPE Type�l�ment IS RECORD
                      Clef: Natural;
                      Derni�rePage: Natural := 0;
                      Pages: Files.File;
                    END RECORD;

FUNCTION Comparaison(E1, E2: Type�l�ment) RETURN Integer;
PROCEDURE Afficher�l�ment(�l�ment: IN Type�l�ment);
PACKAGE TDAArbre IS NEW ArbresBinRecherche(Type�l�ment => Type�l�ment,
                              Comparaison => Comparaison,
								              Afficher�l�ment => Afficher�l�ment);

Tampon: ARRAY (1..LongueurTampon) OF Character; -- Tampon de mots global

PROCEDURE AfficherMot(Index: IN Natural; NombresAffich�s: OUT Natural) IS
-- Afficher un mot du tampon global dans une largeur LongueurMot.  Si le mot
-- est plus long que LongueurMot, il est affich� au complet et on d�cale les
-- colonnes subs�quentes en comptant leur nombre dans NombresAffich�s.
Limite, IndiceCarac, Compte: Natural;
BEGIN
  IndiceCarac := Index;
  Limite := Index + LongueurMot;
  WHILE Tampon(IndiceCarac) > FinCha�ne LOOP     -- affiche mot
    Ada.Text_IO.Put(Item => Tampon(IndiceCarac));
    IndiceCarac := IndiceCarac + 1;
  END LOOP;
  IF IndiceCarac > Limite THEN
    NombresAffich�s := (IndiceCarac - Limite) / LongueurNum�ro; -- au del�
    Compte := (IndiceCarac - Limite) MOD LongueurNum�ro;
    IF Compte > 0 THEN
      NombresAffich�s := NombresAffich�s + 1;
      Compte := LongueurNum�ro - Compte; -- nombre d'espaces
    END IF;
  ELSE
    NombresAffich�s := 0;                -- on ne d�borde pas
    Compte := Limite - Indicecarac;
  END IF;
  FOR Indice IN 1..Compte LOOP           -- remplir avec espaces
    Ada.Text_IO.Put(Item => Espace);     -- pour aligner sur la colonne
  END LOOP;                              -- des num�ros de page
END AfficherMot;

PROCEDURE Afficher�l�ment(�l�ment: IN Type�l�ment) IS
-- Afficher un �l�ment de l'index: mot et r�f�rences de page
NombresAffich�s, Num�ro: Natural;
Copie: Type�l�ment;
BEGIN
  Copie := �l�ment;
  AfficherMot(Copie.Clef, NombresAffich�s);
  WHILE Files.Longueur(Copie.Pages) /= 0 LOOP
    IF NombresAffich�s = �l�mentsParLigne THEN    -- ligne pleine
      Ada.Text_IO.New_Line;
      NombresAffich�s := 0;
      FOR Index IN 1..LongueurMot LOOP            -- saute espace sous mot
        Ada.Text_IO.Put(Item => Espace);
      END LOOP;
    END IF;
    Files.D�filer(Copie.Pages, Num�ro);
    Ada.Integer_Text_IO.Put(Item => Num�ro, Width => LongueurNum�ro);
    -- affiche num�ro de page
    NombresAffich�s := NombresAffich�s + 1;
  END LOOP;
  Ada.Text_IO.New_Line;
END Afficher�l�ment;

FUNCTION Diff�rence(Index1, Index2: Natural) RETURN Integer IS
-- Calculer la diff�rence entre deux mots du tampon global
i: Natural := Index1;
j: Natural := Index2;
BEGIN
  LOOP
    IF Tampon(i) /= Tampon(j) THEN    -- non identiques
      RETURN Character'Pos(Tampon(i)) - Character'Pos(Tampon(j));
    ELSIF Tampon(i) = FinCha�ne THEN  -- identiques
      RETURN 0;
    END IF;
    i := i + 1; j := j + 1;
  END LOOP;
END Diff�rence;

FUNCTION Comparaison(E1, E2: Type�l�ment) RETURN Integer IS
BEGIN
  RETURN Diff�rence(E1.Clef, E2.Clef);
END Comparaison;

PROCEDURE FinDeLigne(Num�roLigne: IN OUT Natural) IS
BEGIN
  Ada.Text_IO.New_Line;
  Num�roLigne := Num�roLigne + 1;
  Ada.Integer_Text_IO.Put(Item => Num�roLigne, Width => LongueurNum�ro);
  Ada.Text_IO.Put(Item => Espace);
  Ada.Text_IO.Skip_Line;
END FinDeLigne;

PROCEDURE LireMot(Carac: IN OUT Character;
                  Suivant, Ancien: IN OUT Natural;
                  Num�roLigne: IN OUT Natural) IS
-- Lire un mot et le ranger dans le tampon global.
BEGIN
  Suivant := Ancien;
  LOOP                 -- pour toutes les lettres y compris les accents
    Ada.Text_IO.Put(Item => Carac);
    Tampon(Suivant) := Carac;
    Suivant := Suivant + 1;
	  IF Ada.Text_IO.End_Of_Line THEN      -- sauter
	    LOOP
		    FinDeLigne(Num�roLigne);
		    EXIT WHEN NOT Ada.Text_IO.End_Of_Line OR Ada.Text_IO.End_Of_File;
      END LOOP;
      IF NOT Ada.Text_IO.End_Of_File THEN
	      Ada.Text_IO.Get(Item => Carac);  -- prochain caract�re
	      EXIT;
      ELSE
        RETURN;
      END IF;
	  END IF;
    Ada.Text_IO.Get(Item => Carac);
    EXIT WHEN (Carac<'0') OR ((Carac>'9') AND (Carac<'A'))
     OR ((Carac>'Z') AND (Carac<'a'))
     OR ((Carac>'z') AND (Carac < '�'))
	   OR (Carac = Ada.Characters.Latin_1.Multiplication_Sign)
	   OR (Carac = Ada.Characters.Latin_1.Division_Sign)
     OR Ada.Text_IO.End_Of_File;
  END LOOP;
  Tampon(Suivant) := FinCha�ne;
  Suivant := Suivant + 1;
  IF Ada.Text_IO.End_Of_File THEN  -- ne pas perdre le dernier caract�re
	Ada.Text_IO.Put(Item => Carac);
	Ada.Text_IO.New_Line;
  END IF;
END LireMot;

PROCEDURE Ins�rerMot(Racine: IN OUT TDAArbre.ArbreBinaireRecherche;
                     Page: IN Natural;
                     Suivant, Ancien: IN OUT Natural) IS
-- Ins�rer mot dans arbre index si pas d�j� l�. Ajouter nouvelle r�f�rence
-- � sa file de num�ros de page
Mot: Type�l�ment;
D�j�L�: Boolean;
BEGIN
  Mot.Clef := Ancien;
  TDAArbre.Rechercher(Racine, Mot, D�j�L�);
  IF D�j�L� THEN
    IF Mot.Derni�rePage /= Page THEN
      -- n'ajouter que les nouvelles r�f�rences de page
      Files.Enfiler(Mot.Pages, Page);
      Mot.Derni�rePage := Page;
      TDAArbre.Ins�rerNoeud(Racine, Mot); -- met � jour le noeud existant
    END IF;
  ELSE                    -- nouveau mot
    Files.Vider(Mot.Pages);
    IF Page /= 0 THEN     -- page z�ro pour dictionnaire
      Files.Enfiler(Mot.Pages, Page);
      Mot.Derni�rePage := Page;
    END IF;
    TDAArbre.Ins�rerNoeud(Racine, Mot);
    Ancien := Suivant;    -- conserver mot dans tampon global
  END IF;
END Ins�rerMot;

Mots, Dictionnaire: TDAArbre.ArbreBinaireRecherche;
VieilIndex, NouvelIndex, Num�roPage, Num�roLigne, Longueur: Natural;
Carac: Character;
Pr�sent: Boolean;
Mot: Type�l�ment;
F1, F2: Ada.Text_IO.File_Type;
Nom: String(1..25);

BEGIN
  VieilIndex := 1;
  NouvelIndex := 1;
  Num�roLigne := 1;
  Ada.Text_IO.Put(Item => "Donnez le nom du fichier dictionnaire: ");
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Get_Line(Item => Nom, Last => Longueur);
  Ada.Text_IO.Open(File => F1, Mode => Ada.Text_IO.IN_FILE, Name => Nom(1..Longueur));
  Ada.Text_IO.Set_Input(File => F1);
  Ada.Text_IO.Get(Item => Carac);
  WHILE NOT Ada.Text_IO.End_Of_File(F1) LOOP
    CASE Carac IS                    -- une lettre accentu�e ou non
      WHEN 'A'..'Z'|'a'..'z'|'�'..'�'|'�'..'�'|'�'..'�' =>
             LireMot(Carac, NouvelIndex, VieilIndex, Num�roLigne);
             Ins�rerMot(Dictionnaire, 0, NouvelIndex, VieilIndex);
      WHEN OTHERS => Ada.Text_IO.Put(Item => Carac); Ada.Text_IO.Get(Item => Carac);
    END CASE;
  END LOOP;
  Ada.Text_IO.Set_Input(File => Ada.Text_IO.Standard_Input);
  Ada.Text_IO.Close(F1);
  Ada.Text_IO.New_Line;
  TDAArbre.AfficherArbre(Dictionnaire, 0);
  Ada.Text_IO.Put_Line(Item => "Donnez le nom du fichier texte: ");
  Ada.Text_IO.Get_Line(Item => Nom, Last => Longueur);
  Ada.Text_IO.Open(File => F1, Mode => Ada.Text_IO.In_File, Name => Nom(1..Longueur));
  Ada.Text_IO.Put_Line(Item => "Donnez le nom du fichier de sortie: ");
  Ada.Text_IO.Get_Line(Item => Nom, Last => Longueur);
  Ada.Text_IO.Create(File => F2, Name => Nom(1..Longueur));
  Ada.Text_IO.Set_Input(File => F1);
  Ada.Text_IO.Set_Output(File => F2);
  Num�roPage := 1;
  Num�roLigne := 1;                  -- replacer au d�part
  Ada.Integer_Text_IO.Put(Item => Num�roLigne, Width => 6);
  Ada.Text_IO.Put(Item => Espace);
  Ada.Text_IO.Get(Item => Carac);
  WHILE NOT Ada.Text_IO.End_Of_File LOOP
    CASE Carac IS                    -- une lettre accentu�e ou non
      WHEN 'A'..'Z'|'a'..'z'|'�'..'�'|'�'..'�'|'�'..'�' =>
           LireMot(Carac, NouvelIndex, VieilIndex, Num�roLigne);
		   Mot.Clef := VieilIndex;
		   TDAArbre.Rechercher(Dictionnaire, Mot, Pr�sent);
           IF NOT Pr�sent THEN
             Ins�rerMot(Mots, Num�roPage, NouvelIndex, VieilIndex);
           END IF;
      WHEN FinPage => Num�roPage := Num�roPage + 1;
                      Ada.Text_IO.Put(Item => Carac); 
                      WHILE Ada.Text_IO.End_Of_Line LOOP
                        FinDeLigne(Num�roLigne);
                      END LOOP;
                      Ada.Text_IO.Get(Item => Carac);
      WHEN OTHERS => Ada.Text_IO.Put(Item => Carac); 
                     WHILE Ada.Text_IO.End_Of_Line LOOP
                       FinDeLigne(Num�roLigne);
                     END LOOP;
                     Ada.Text_IO.Get(Item => Carac);
    END CASE;
  END LOOP;
  Ada.Text_IO.New_Line; Ada.Text_IO.New_Line; 
  Ada.Text_IO.Set_Input(File => Ada.Text_IO.Standard_Input);
  Ada.Text_IO.Close(F1);
  TDAArbre.TraverserArbre(Mots, Afficher�l�ment'ACCESS);
  Ada.Text_IO.Set_Output(File => Ada.Text_IO.Standard_Output);
  Ada.Text_IO.Close(F2);
  Ada.Text_IO.Put_Line(Item => "Index termin�");
END ConstruireIndex;
