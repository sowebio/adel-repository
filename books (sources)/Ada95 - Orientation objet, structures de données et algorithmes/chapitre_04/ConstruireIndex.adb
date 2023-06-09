WITH Ada.Text_IO, Ada.Integer_Text_IO, Ada.Characters.Latin_1;
WITH ArbresBinRecherche, TDAFile;
PROCEDURE ConstruireIndex IS

LongueurTampon: CONSTANT Natural := 10000;
LongueurNuméro: CONSTANT Natural := 6;
LongueurMot: CONSTANT Natural := 3 * LongueurNuméro;
FinChaîne: CONSTANT Character := Ada.Characters.Latin_1.NUL;
FinPage: CONSTANT Character := '\';
Espace: CONSTANT Character := ' ';
ÉlémentsParLigne: CONSTANT Natural := 8;

PACKAGE Files IS NEW TDAFile(TypeÉlément => Natural);
TYPE TypeÉlément IS RECORD
                      Clef: Natural;
                      DernièrePage: Natural := 0;
                      Pages: Files.File;
                    END RECORD;

FUNCTION Comparaison(E1, E2: TypeÉlément) RETURN Integer;
PROCEDURE AfficherÉlément(Élément: IN TypeÉlément);
PACKAGE TDAArbre IS NEW ArbresBinRecherche(TypeÉlément => TypeÉlément,
                              Comparaison => Comparaison,
								              AfficherÉlément => AfficherÉlément);

Tampon: ARRAY (1..LongueurTampon) OF Character; -- Tampon de mots global

PROCEDURE AfficherMot(Index: IN Natural; NombresAffichés: OUT Natural) IS
-- Afficher un mot du tampon global dans une largeur LongueurMot.  Si le mot
-- est plus long que LongueurMot, il est affiché au complet et on décale les
-- colonnes subséquentes en comptant leur nombre dans NombresAffichés.
Limite, IndiceCarac, Compte: Natural;
BEGIN
  IndiceCarac := Index;
  Limite := Index + LongueurMot;
  WHILE Tampon(IndiceCarac) > FinChaîne LOOP     -- affiche mot
    Ada.Text_IO.Put(Item => Tampon(IndiceCarac));
    IndiceCarac := IndiceCarac + 1;
  END LOOP;
  IF IndiceCarac > Limite THEN
    NombresAffichés := (IndiceCarac - Limite) / LongueurNuméro; -- au delà
    Compte := (IndiceCarac - Limite) MOD LongueurNuméro;
    IF Compte > 0 THEN
      NombresAffichés := NombresAffichés + 1;
      Compte := LongueurNuméro - Compte; -- nombre d'espaces
    END IF;
  ELSE
    NombresAffichés := 0;                -- on ne déborde pas
    Compte := Limite - Indicecarac;
  END IF;
  FOR Indice IN 1..Compte LOOP           -- remplir avec espaces
    Ada.Text_IO.Put(Item => Espace);     -- pour aligner sur la colonne
  END LOOP;                              -- des numéros de page
END AfficherMot;

PROCEDURE AfficherÉlément(Élément: IN TypeÉlément) IS
-- Afficher un élément de l'index: mot et références de page
NombresAffichés, Numéro: Natural;
Copie: TypeÉlément;
BEGIN
  Copie := Élément;
  AfficherMot(Copie.Clef, NombresAffichés);
  WHILE Files.Longueur(Copie.Pages) /= 0 LOOP
    IF NombresAffichés = ÉlémentsParLigne THEN    -- ligne pleine
      Ada.Text_IO.New_Line;
      NombresAffichés := 0;
      FOR Index IN 1..LongueurMot LOOP            -- saute espace sous mot
        Ada.Text_IO.Put(Item => Espace);
      END LOOP;
    END IF;
    Files.Défiler(Copie.Pages, Numéro);
    Ada.Integer_Text_IO.Put(Item => Numéro, Width => LongueurNuméro);
    -- affiche numéro de page
    NombresAffichés := NombresAffichés + 1;
  END LOOP;
  Ada.Text_IO.New_Line;
END AfficherÉlément;

FUNCTION Différence(Index1, Index2: Natural) RETURN Integer IS
-- Calculer la différence entre deux mots du tampon global
i: Natural := Index1;
j: Natural := Index2;
BEGIN
  LOOP
    IF Tampon(i) /= Tampon(j) THEN    -- non identiques
      RETURN Character'Pos(Tampon(i)) - Character'Pos(Tampon(j));
    ELSIF Tampon(i) = FinChaîne THEN  -- identiques
      RETURN 0;
    END IF;
    i := i + 1; j := j + 1;
  END LOOP;
END Différence;

FUNCTION Comparaison(E1, E2: TypeÉlément) RETURN Integer IS
BEGIN
  RETURN Différence(E1.Clef, E2.Clef);
END Comparaison;

PROCEDURE FinDeLigne(NuméroLigne: IN OUT Natural) IS
BEGIN
  Ada.Text_IO.New_Line;
  NuméroLigne := NuméroLigne + 1;
  Ada.Integer_Text_IO.Put(Item => NuméroLigne, Width => LongueurNuméro);
  Ada.Text_IO.Put(Item => Espace);
  Ada.Text_IO.Skip_Line;
END FinDeLigne;

PROCEDURE LireMot(Carac: IN OUT Character;
                  Suivant, Ancien: IN OUT Natural;
                  NuméroLigne: IN OUT Natural) IS
-- Lire un mot et le ranger dans le tampon global.
BEGIN
  Suivant := Ancien;
  LOOP                 -- pour toutes les lettres y compris les accents
    Ada.Text_IO.Put(Item => Carac);
    Tampon(Suivant) := Carac;
    Suivant := Suivant + 1;
	  IF Ada.Text_IO.End_Of_Line THEN      -- sauter
	    LOOP
		    FinDeLigne(NuméroLigne);
		    EXIT WHEN NOT Ada.Text_IO.End_Of_Line OR Ada.Text_IO.End_Of_File;
      END LOOP;
      IF NOT Ada.Text_IO.End_Of_File THEN
	      Ada.Text_IO.Get(Item => Carac);  -- prochain caractère
	      EXIT;
      ELSE
        RETURN;
      END IF;
	  END IF;
    Ada.Text_IO.Get(Item => Carac);
    EXIT WHEN (Carac<'0') OR ((Carac>'9') AND (Carac<'A'))
     OR ((Carac>'Z') AND (Carac<'a'))
     OR ((Carac>'z') AND (Carac < 'À'))
	   OR (Carac = Ada.Characters.Latin_1.Multiplication_Sign)
	   OR (Carac = Ada.Characters.Latin_1.Division_Sign)
     OR Ada.Text_IO.End_Of_File;
  END LOOP;
  Tampon(Suivant) := FinChaîne;
  Suivant := Suivant + 1;
  IF Ada.Text_IO.End_Of_File THEN  -- ne pas perdre le dernier caractère
	Ada.Text_IO.Put(Item => Carac);
	Ada.Text_IO.New_Line;
  END IF;
END LireMot;

PROCEDURE InsérerMot(Racine: IN OUT TDAArbre.ArbreBinaireRecherche;
                     Page: IN Natural;
                     Suivant, Ancien: IN OUT Natural) IS
-- Insérer mot dans arbre index si pas déjà là. Ajouter nouvelle référence
-- à sa file de numéros de page
Mot: TypeÉlément;
DéjàLà: Boolean;
BEGIN
  Mot.Clef := Ancien;
  TDAArbre.Rechercher(Racine, Mot, DéjàLà);
  IF DéjàLà THEN
    IF Mot.DernièrePage /= Page THEN
      -- n'ajouter que les nouvelles références de page
      Files.Enfiler(Mot.Pages, Page);
      Mot.DernièrePage := Page;
      TDAArbre.InsérerNoeud(Racine, Mot); -- met à jour le noeud existant
    END IF;
  ELSE                    -- nouveau mot
    Files.Vider(Mot.Pages);
    IF Page /= 0 THEN     -- page zéro pour dictionnaire
      Files.Enfiler(Mot.Pages, Page);
      Mot.DernièrePage := Page;
    END IF;
    TDAArbre.InsérerNoeud(Racine, Mot);
    Ancien := Suivant;    -- conserver mot dans tampon global
  END IF;
END InsérerMot;

Mots, Dictionnaire: TDAArbre.ArbreBinaireRecherche;
VieilIndex, NouvelIndex, NuméroPage, NuméroLigne, Longueur: Natural;
Carac: Character;
Présent: Boolean;
Mot: TypeÉlément;
F1, F2: Ada.Text_IO.File_Type;
Nom: String(1..25);

BEGIN
  VieilIndex := 1;
  NouvelIndex := 1;
  NuméroLigne := 1;
  Ada.Text_IO.Put(Item => "Donnez le nom du fichier dictionnaire: ");
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Get_Line(Item => Nom, Last => Longueur);
  Ada.Text_IO.Open(File => F1, Mode => Ada.Text_IO.IN_FILE, Name => Nom(1..Longueur));
  Ada.Text_IO.Set_Input(File => F1);
  Ada.Text_IO.Get(Item => Carac);
  WHILE NOT Ada.Text_IO.End_Of_File(F1) LOOP
    CASE Carac IS                    -- une lettre accentuée ou non
      WHEN 'A'..'Z'|'a'..'z'|'À'..'Ö'|'Ù'..'ö'|'ù'..'ÿ' =>
             LireMot(Carac, NouvelIndex, VieilIndex, NuméroLigne);
             InsérerMot(Dictionnaire, 0, NouvelIndex, VieilIndex);
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
  NuméroPage := 1;
  NuméroLigne := 1;                  -- replacer au départ
  Ada.Integer_Text_IO.Put(Item => NuméroLigne, Width => 6);
  Ada.Text_IO.Put(Item => Espace);
  Ada.Text_IO.Get(Item => Carac);
  WHILE NOT Ada.Text_IO.End_Of_File LOOP
    CASE Carac IS                    -- une lettre accentuée ou non
      WHEN 'A'..'Z'|'a'..'z'|'À'..'Ö'|'Ù'..'ö'|'ù'..'ÿ' =>
           LireMot(Carac, NouvelIndex, VieilIndex, NuméroLigne);
		   Mot.Clef := VieilIndex;
		   TDAArbre.Rechercher(Dictionnaire, Mot, Présent);
           IF NOT Présent THEN
             InsérerMot(Mots, NuméroPage, NouvelIndex, VieilIndex);
           END IF;
      WHEN FinPage => NuméroPage := NuméroPage + 1;
                      Ada.Text_IO.Put(Item => Carac); 
                      WHILE Ada.Text_IO.End_Of_Line LOOP
                        FinDeLigne(NuméroLigne);
                      END LOOP;
                      Ada.Text_IO.Get(Item => Carac);
      WHEN OTHERS => Ada.Text_IO.Put(Item => Carac); 
                     WHILE Ada.Text_IO.End_Of_Line LOOP
                       FinDeLigne(NuméroLigne);
                     END LOOP;
                     Ada.Text_IO.Get(Item => Carac);
    END CASE;
  END LOOP;
  Ada.Text_IO.New_Line; Ada.Text_IO.New_Line; 
  Ada.Text_IO.Set_Input(File => Ada.Text_IO.Standard_Input);
  Ada.Text_IO.Close(F1);
  TDAArbre.TraverserArbre(Mots, AfficherÉlément'ACCESS);
  Ada.Text_IO.Set_Output(File => Ada.Text_IO.Standard_Output);
  Ada.Text_IO.Close(F2);
  Ada.Text_IO.Put_Line(Item => "Index terminé");
END ConstruireIndex;
