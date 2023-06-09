--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PACKAGE BODY Hachage IS
 
--**************************************************************************
--* Module        Hachage
--* But:
--* Ce module contient des procédures pour la création de tables de hachage,
--* la création de noeuds aléatoires, et l'insertion de noeuds dans les
--* tables de hachage en utilisant les méthodes de résolution des collisions
--* linéaire, par chaînage fusionné ou par décalage.
--**************************************************************************
 
FUNCTION Hachage (Noeud: TypeNoeud) RETURN Natural IS
--  Cette procédure simpliste prend un noeud et donne une 
--  valeur dans 1..NoeudsMax en retour.
Valeur:Natural;
Élt:Élément;
BEGIN -- Hachage
  Élt:=Noeud.Identificateur;
  Valeur:=((Character'Pos(Élt(1)) * 26 * 26
          + Character'Pos(Élt(2)) * 26
          + Character'Pos (Élt(3))) MOD NoeudsMax)+1;
  RETURN Valeur;
END Hachage;
 
PROCEDURE HacherFusionné(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                         Essais: IN OUT Natural) IS
-- Cette procédure insère un Noeud dans une table de hachage
-- en utilisant la méthode de résolution des collisions chaînage fusionné 
-- et retourne le nombre de collision produites. 
-- NB: si un Noeud identique est inséré, la procédure
-- créera un nouveau Noeud, l'insèrera, et retournera sa valeur.
Index, Position, Antérieur: Natural;
Trouvé: Boolean;
BEGIN
  IF Table.Taille < NoeudsMax THEN
    Essais := 0;
    Position := Hachage(Noeud);
    Trouvé := False;
    IF Table.Éléments(Position).Identificateur(1) /= '+' THEN
      -- occupied
      Index := Position;
      LOOP
        Essais := Essais + 1;
        IF Table.Éléments(Index).Identificateur =
                                 Noeud.Identificateur THEN
          Trouvé := True;	-- noeud déjà là, essayer un autre
          EngendrerNoeud(Noeud);
          HacherFusionné(Noeud,Table,Essais);
        ELSE
          Antérieur := Index;			-- pour lien futur
          Index := Table.Éléments(Index).Suivant;
        END IF;
        EXIT WHEN Trouvé OR (Index = 0);
	    END LOOP;
    END IF;
    IF NOT Trouvé THEN
      IF Table.Éléments(Position).Identificateur(1) = '+' THEN
        -- le ranger dans endroit vide
        Table.Éléments(Position) := Noeud;
        Essais := Essais + 1;
        Table.Taille := Table.Taille + 1;
      ELSE
        LOOP	-- trouver endroit vide
          Essais := Essais + 1;
          Table.Libre := Table.Libre - 1;
          EXIT WHEN Table.Éléments(Table.Libre).Identificateur(1) = '+';
		    END LOOP;
        IF Table.Libre /= 0 THEN
          Table.Éléments(Antérieur).Suivant := Table.Libre;
          Table.Éléments(Table.Libre) := Noeud;
          Table.Éléments(Table.Libre).Suivant := 0;
          Table.Taille := Table.Taille + 1;
        END IF;
      END IF;
    END IF;
  ELSE
    Ada.Text_IO.Put(Item => "Débordement de table fusionnée, pas d'opération effectuée");
    Ada.Text_IO.New_Line;
  END IF;
END HacherFusionné;
 
PROCEDURE HacherDécalé(Noeud: IN OUT TypeNoeud; Table: IN OUT TableHachage;
                       Essais: IN OUT Natural; Magique: IN Modulaire) IS
-- Cette procédure insère un Noeud dans une table de hachage
-- en utilisant la méthode de résolution des collisions aléatoire
-- et retourne le nombre de collision produites. 
-- NB: si un Noeud identique est inséré, la procédure
-- créera un nouveau Noeud, l'insèrera, et retournera sa valeur.
Position: Natural;
Modula: Modulaire;
BEGIN
  IF Table.Taille < NoeudsMax THEN
    Essais := 0;
    Position := Hachage(Noeud);
    LOOP
      Essais := Essais + 1;
      IF Table.Éléments(Position).Identificateur(1) = '+' THEN
        -- endroit vide
        Table.Éléments(Position) := Noeud;
        Table.Taille := Table.Taille + 1;
        EXIT;
      ELSIF Table.Éléments(Position).Identificateur =
                               Noeud.Identificateur THEN
         -- déjà là, essayer un autre
         EngendrerNoeud(Noeud);
         HacherDécalé(Noeud, Table, Essais, Magique);
         EXIT;
      ELSE
         Position := Position * 2;	       -- décaler à gauche d'un bit
         IF Position > NoeudsMax THEN      -- supprimer premier bit et XOR
           Position := Position - (NoeudsMax + 1);
           Modula := Modulaire(Position);
           Position := Natural(Modula XOR Magique); -- Magique déjà calculé
        END IF;
      END IF;
    END LOOP;
  ELSE
    Ada.Text_IO.Put(Item => "Débordement table décalée, pas d'opération effectuée");
    Ada.Text_IO.New_Line;
  END IF;
END HacherDécalé;          
  
END Hachage;
