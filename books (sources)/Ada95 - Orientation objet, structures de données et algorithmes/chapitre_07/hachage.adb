--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PACKAGE BODY Hachage IS
 
--**************************************************************************
--* Module        Hachage
--* But:
--* Ce module contient des proc�dures pour la cr�ation de tables de hachage,
--* la cr�ation de noeuds al�atoires, et l'insertion de noeuds dans les
--* tables de hachage en utilisant les m�thodes de r�solution des collisions
--* lin�aire, par cha�nage fusionn� ou par d�calage.
--**************************************************************************
 
FUNCTION Hachage (Noeud: TypeNoeud) RETURN Natural IS
--  Cette proc�dure simpliste prend un noeud et donne une 
--  valeur dans 1..NoeudsMax en retour.
Valeur:Natural;
�lt:�l�ment;
BEGIN -- Hachage
  �lt:=Noeud.Identificateur;
  Valeur:=((Character'Pos(�lt(1)) * 26 * 26
          + Character'Pos(�lt(2)) * 26
          + Character'Pos (�lt(3))) MOD NoeudsMax)+1;
  RETURN Valeur;
END Hachage;
 
PROCEDURE HacherFusionn�(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                         Essais: IN OUT Natural) IS
-- Cette proc�dure ins�re un Noeud dans une table de hachage
-- en utilisant la m�thode de r�solution des collisions cha�nage fusionn� 
-- et retourne le nombre de collision produites. 
-- NB: si un Noeud identique est ins�r�, la proc�dure
-- cr�era un nouveau Noeud, l'ins�rera, et retournera sa valeur.
Index, Position, Ant�rieur: Natural;
Trouv�: Boolean;
BEGIN
  IF Table.Taille < NoeudsMax THEN
    Essais := 0;
    Position := Hachage(Noeud);
    Trouv� := False;
    IF Table.�l�ments(Position).Identificateur(1) /= '+' THEN
      -- occupied
      Index := Position;
      LOOP
        Essais := Essais + 1;
        IF Table.�l�ments(Index).Identificateur =
                                 Noeud.Identificateur THEN
          Trouv� := True;	-- noeud d�j� l�, essayer un autre
          EngendrerNoeud(Noeud);
          HacherFusionn�(Noeud,Table,Essais);
        ELSE
          Ant�rieur := Index;			-- pour lien futur
          Index := Table.�l�ments(Index).Suivant;
        END IF;
        EXIT WHEN Trouv� OR (Index = 0);
	    END LOOP;
    END IF;
    IF NOT Trouv� THEN
      IF Table.�l�ments(Position).Identificateur(1) = '+' THEN
        -- le ranger dans endroit vide
        Table.�l�ments(Position) := Noeud;
        Essais := Essais + 1;
        Table.Taille := Table.Taille + 1;
      ELSE
        LOOP	-- trouver endroit vide
          Essais := Essais + 1;
          Table.Libre := Table.Libre - 1;
          EXIT WHEN Table.�l�ments(Table.Libre).Identificateur(1) = '+';
		    END LOOP;
        IF Table.Libre /= 0 THEN
          Table.�l�ments(Ant�rieur).Suivant := Table.Libre;
          Table.�l�ments(Table.Libre) := Noeud;
          Table.�l�ments(Table.Libre).Suivant := 0;
          Table.Taille := Table.Taille + 1;
        END IF;
      END IF;
    END IF;
  ELSE
    Ada.Text_IO.Put(Item => "D�bordement de table fusionn�e, pas d'op�ration effectu�e");
    Ada.Text_IO.New_Line;
  END IF;
END HacherFusionn�;
 
PROCEDURE HacherD�cal�(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                       Essais: IN OUT Natural; Magique: IN Modulaire) IS
-- Cette proc�dure ins�re un Noeud dans une table de hachage
-- en utilisant la m�thode de r�solution des collisions al�atoire
-- et retourne le nombre de collision produites. 
-- NB: si un Noeud identique est ins�r�, la proc�dure
-- cr�era un nouveau Noeud, l'ins�rera, et retournera sa valeur.
Position: Natural;
Modula: Modulaire;
BEGIN
  IF Table.Taille < NoeudsMax THEN
    Essais := 0;
    Position := Hachage(Noeud);
    LOOP
      Essais := Essais + 1;
      IF Table.�l�ments(Position).Identificateur(1) = '+' THEN
        -- endroit vide
        Table.�l�ments(Position) := Noeud;
        Table.Taille := Table.Taille + 1;
        EXIT;
      ELSIF Table.�l�ments(Position).Identificateur =
                               Noeud.Identificateur THEN
         -- d�j� l�, essayer un autre
         EngendrerNoeud(Noeud);
         HacherD�cal�(Noeud, Table, Essais, Magique);
         EXIT;
      ELSE
         Position := Position * 2;	       -- d�caler � gauche d'un bit
         IF Position > NoeudsMax THEN      -- supprimer premier bit et XOR
           Position := Position - (NoeudsMax + 1);
           Modula := Modulaire(Position);
           Position := Natural(Modula XOR Magique); -- Magique d�j� calcul�
        END IF;
      END IF;
    END LOOP;
  ELSE
    Ada.Text_IO.Put(Item => "D�bordement table d�cal�e, pas d'op�ration effectu�e");
    Ada.Text_IO.New_Line;
  END IF;
END HacherD�cal�;          
  
END Hachage;
