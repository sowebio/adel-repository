--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Integer_Text_IO;
WITH Ada.Text_IO;
PACKAGE BODY Tables.Ordonn�es IS

FUNCTION Nb_�l�ments(T: IN Table) RETURN Natural IS
-- Donne le nombre d'�l�ments contenus dans la table T
BEGIN
  RETURN T.Nombre_d_�l�ments;
END Nb_�l�ments;

PROCEDURE Chercher(T: IN Table; �l�ment: IN OUT Type_�l�ment;
                   Trouv�: OUT Boolean) IS
-- Cherche un �l�ment dans la table T qui poss�de la clef de �l�ment.
-- Si on trouve un �l�ment, on le retourne dans �l�ment et Trouv� est vrai,
-- sinon Trouv� est faux.
Index: TypeIndex := 1;
BEGIN
  WHILE (Index <= T.Nombre_d_�l�ments) 
	   AND (Comparaison(�l�ment, T.�l�ments(Index)) = 1) LOOP
    Index := Index + 1;     -- Clef pas dans positions 1..Index
  END LOOP;
  IF (Index <= T.Nombre_d_�l�ments) AND 
		(Comparaison(�l�ment, T.�l�ments(Index)) = 0) THEN  -- trouv�
    �l�ment := T.�l�ments(Index);
    Trouv� := True;
  ELSE        -- absent
    Trouv� := False;
  END IF;
END Chercher;

PROCEDURE Ins�rer(T: IN OUT Table; �lt: IN Type_�l�ment) IS
-- Ins�re l'�l�ment E dans la Table
Index: TypeIndex;
BEGIN   -- conserver T en ordre
  IF T.Nombre_d_�l�ments < Taille_Table THEN
    Index := 1;
    WHILE (Index <= T.Nombre_d_�l�ments) AND
	    (Comparaison(�lt, T.�l�ments(Index)) = 1) LOOP
      Index := Index + 1;        -- �lt.Clef pas dans positions 1..Index
    END LOOP;
    -- Index indique o� �lt.Clef doit �tre plac�
    IF (Comparaison(�lt, T.�l�ments(Index)) /= 0) THEN     -- ins�rer
      FOR Place IN REVERSE Index..T.Nombre_d_�l�ments LOOP -- faire place
	      T.�l�ments(Place+1) := T.�l�ments(Place);
      END LOOP;
      T.Nombre_d_�l�ments := T.Nombre_d_�l�ments + 1;
    END IF;
    T.�l�ments(Index) := �lt;                            -- mettre � jour
  ELSE
    RAISE Table_Pleine;
  END IF;
END Ins�rer;

PROCEDURE Supprimer(T: IN OUT Table; �l�ment: IN Type_�l�ment;
                    Succ�s: OUT Boolean) IS
-- Supprime l'�l�ment ayant la m�me clef que �l�ment dans la table T.
-- Si la suppression a r�ussi Succ�s est mis � True, sinon � False.
Index: TypeIndex;
BEGIN
  Index := 1;
  WHILE (Index <= T.Nombre_d_�l�ments) AND
	  (Comparaison(�l�ment, T.�l�ments(Index)) = 1) LOOP
    Index := Index + 1;       -- trouver position de Clef dans table
  END LOOP;
  IF (Comparaison(�l�ment, T.�l�ments(Index)) = 0) THEN
    FOR Place IN Index..T.Nombre_d_�l�ments-1 LOOP
    -- supprimer en tassant la table
  	  T.�l�ments(Place) := T.�l�ments(Place+1);
    END LOOP;
    T.Nombre_d_�l�ments := T.Nombre_d_�l�ments - 1;
    Succ�s := True;
  ELSE
    Succ�s := False;  -- �l�ment absent
  END IF;
END Supprimer;

PROCEDURE AfficherTable(T: IN Table) IS
-- Afficher tous les �l�ments de la table T.
BEGIN
  FOR Index IN 1..T.Nombre_d_�l�ments LOOP   -- afficher clefs
    AfficherClef(T.�l�ments(Index));
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
  Nom(1..7) := "Chiffr�";
  Ada.Text_IO.Put_Line(Item => Nom(1..7)); -- indicateur pour mots de passe
  FOR Index IN 1..T.Nombre_d_�l�ments LOOP
    Afficher�l�ment(T.�l�ments(Index));
  END LOOP;
  Ada.Text_IO.Set_Output(File => Ada.Text_IO.Standard_Output);
  Ada.Text_IO.Close(F1);
  Ada.Text_IO.Put(Item => "Sauvegarde termin�e."); Ada.Text_IO.New_Line;
END Rangertable;

END Tables.Ordonn�es;
