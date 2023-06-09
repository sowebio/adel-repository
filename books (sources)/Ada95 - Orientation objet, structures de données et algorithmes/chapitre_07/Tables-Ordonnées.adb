--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Integer_Text_IO;
WITH Ada.Text_IO;
PACKAGE BODY Tables.Ordonnées IS

FUNCTION Nb_Éléments(T: IN Table) RETURN Natural IS
-- Donne le nombre d'éléments contenus dans la table T
BEGIN
  RETURN T.Nombre_d_Éléments;
END Nb_Éléments;

PROCEDURE Chercher(T: IN Table; Élément: IN OUT Type_Élément;
                   Trouvé: OUT Boolean) IS
-- Cherche un élément dans la table T qui possède la clef de Élément.
-- Si on trouve un élément, on le retourne dans Élément et Trouvé est vrai,
-- sinon Trouvé est faux.
Index: TypeIndex := 1;
BEGIN
  WHILE (Index <= T.Nombre_d_Éléments) 
	   AND (Comparaison(Élément, T.Éléments(Index)) = 1) LOOP
    Index := Index + 1;     -- Clef pas dans positions 1..Index
  END LOOP;
  IF (Index <= T.Nombre_d_Éléments) AND 
		(Comparaison(Élément, T.Éléments(Index)) = 0) THEN  -- trouvé
    Élément := T.Éléments(Index);
    Trouvé := True;
  ELSE        -- absent
    Trouvé := False;
  END IF;
END Chercher;

PROCEDURE Insérer(T: IN OUT Table; Élt: IN Type_Élément) IS
-- Insère l'élément E dans la Table
Index: TypeIndex;
BEGIN   -- conserver T en ordre
  IF T.Nombre_d_Éléments < Taille_Table THEN
    Index := 1;
    WHILE (Index <= T.Nombre_d_Éléments) AND
	    (Comparaison(Élt, T.Éléments(Index)) = 1) LOOP
      Index := Index + 1;        -- Élt.Clef pas dans positions 1..Index
    END LOOP;
    -- Index indique où Élt.Clef doit être placé
    IF (Comparaison(Élt, T.Éléments(Index)) /= 0) THEN     -- insérer
      FOR Place IN REVERSE Index..T.Nombre_d_Éléments LOOP -- faire place
	      T.Éléments(Place+1) := T.Éléments(Place);
      END LOOP;
      T.Nombre_d_Éléments := T.Nombre_d_Éléments + 1;
    END IF;
    T.Éléments(Index) := Élt;                            -- mettre à jour
  ELSE
    RAISE Table_Pleine;
  END IF;
END Insérer;

PROCEDURE Supprimer(T: IN OUT Table; Élément: IN Type_Élément;
                    Succès: OUT Boolean) IS
-- Supprime l'élément ayant la même clef que Élément dans la table T.
-- Si la suppression a réussi Succès est mis à True, sinon à False.
Index: TypeIndex;
BEGIN
  Index := 1;
  WHILE (Index <= T.Nombre_d_Éléments) AND
	  (Comparaison(Élément, T.Éléments(Index)) = 1) LOOP
    Index := Index + 1;       -- trouver position de Clef dans table
  END LOOP;
  IF (Comparaison(Élément, T.Éléments(Index)) = 0) THEN
    FOR Place IN Index..T.Nombre_d_Éléments-1 LOOP
    -- supprimer en tassant la table
  	  T.Éléments(Place) := T.Éléments(Place+1);
    END LOOP;
    T.Nombre_d_Éléments := T.Nombre_d_Éléments - 1;
    Succès := True;
  ELSE
    Succès := False;  -- élément absent
  END IF;
END Supprimer;

PROCEDURE AfficherTable(T: IN Table) IS
-- Afficher tous les éléments de la table T.
BEGIN
  FOR Index IN 1..T.Nombre_d_Éléments LOOP   -- afficher clefs
    AfficherClef(T.Éléments(Index));
  END LOOP;
END AfficherTable;

PROCEDURE Rangertable(T: IN Table) IS
Longueur: Natural;
F1: Ada.Text_IO.File_Type;
Nom: String(1..25);
BEGIN
  Ada.Text_IO.Put(Item => "Sauvegarde de la table.  Donnez un nom de fichier. ");
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Get_Line(Item => Nom, Last => Longueur);
  Ada.Text_IO.Create(File => F1, Name => Nom(1..Longueur));
  Ada.Text_IO.Set_Output(File => F1);
  Nom(1..7) := "Chiffré";
  Ada.Text_IO.Put_Line(Item => Nom(1..7)); -- indicateur pour mots de passe
  FOR Index IN 1..T.Nombre_d_Éléments LOOP
    AfficherÉlément(T.Éléments(Index));
  END LOOP;
  Ada.Text_IO.Set_Output(File => Ada.Text_IO.Standard_Output);
  Ada.Text_IO.Close(F1);
  Ada.Text_IO.Put(Item => "Sauvegarde terminée."); Ada.Text_IO.New_Line;
END Rangertable;

END Tables.Ordonnées;
