--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PROCEDURE FilesAvecPriorit� IS

Max: CONSTANT Natural := 100;

TYPE Type�l�ment IS RECORD
					  Clef: Character;
					  Donn�es: Integer;
				    END RECORD;
TYPE Noeud IS RECORD
                Priorit�: Natural;
                �l�ment: Type�l�ment;
              END RECORD;
SUBTYPE Position IS Natural RANGE 0..Max;
TYPE Vecteur IS ARRAY (Position) OF Noeud;
TYPE FileAvecPriorit� IS RECORD
                           Taille: Position;
                           �l�mentsFile: Vecteur;
                         END RECORD;

PROCEDURE Enfiler(Q: IN OUT FileAvecPriorit�; �lt: IN Type�l�ment;
                  Prior: IN Natural) IS
-- Ajouter �l�ment avec Priorit� � la file d'attente Q.
Parent, Descendant: Position;
BEGIN
  IF Q.Taille < Max THEN
    Q.Taille := Q.Taille + 1;
    Q.�l�mentsFile(Q.Taille).Priorit� := Prior;    -- ajouter � la fin
    Q.�l�mentsFile(Q.Taille).�l�ment := �lt;
    -- remonter �l�ment � sa place dans le monceau
    Q.�l�mentsFile(0) := Q.�l�mentsFile(Q.Taille); -- sentinelle
    Descendant := Q.Taille;
    Parent := Descendant / 2;
    WHILE Q.�l�mentsFile(Parent).Priorit� < Prior LOOP
      -- trouver position pour dernier �l�ment
      Q.�l�mentsFile(Descendant) := Q.�l�mentsFile(Parent);
      Descendant := Parent;
      Parent := Parent / 2;
    END LOOP;
    -- copier �l�ment � sa place
    Q.�l�mentsFile(Descendant) := Q.�l�mentsFile(0);
  END IF;
END Enfiler;

PROCEDURE D�filer(Q: IN OUT FileAvecPriorit�; �lt: IN OUT Type�l�ment;
                  Prior: IN OUT Natural) IS
-- R�cup�rer l'�l�ment avec la plus grande priorit�.
Parent, Descendant: Position;
Fini: Boolean;
Copie: Noeud;
BEGIN
  IF Q.Taille > 0 THEN
    Prior := Q.�l�mentsFile(1).Priorit�; -- copier premier �l�ment
    �lt := Q.�l�mentsFile(1).�l�ment;
    Q.�l�mentsFile(1) := Q.�l�mentsFile(Q.Taille); -- le remplacer
    Q.Taille := Q.Taille - 1;
    Parent := 1;                 -- reconstruire monceau en d�pla�ant le premier
    Descendant := 2;             -- �l�ment vers le bas � sa place dans monceau
    Copie := Q.�l�mentsFile(1);
    Fini := False;
    WHILE (Descendant <= Q.Taille) AND NOT Fini LOOP -- inspecter descendants
      IF Descendant < Q.Taille THEN
        IF Q.�l�mentsFile(Descendant).Priorit� < Q.�l�mentsFile(Descendant+1).Priorit� THEN
          Descendant := Descendant + 1; -- selectionner le plus grand des deux descendants
        END IF;
      END IF;
      IF Copie.Priorit� > Q.�l�mentsFile(Descendant).Priorit� THEN
        Fini := True;		         -- position trouv�e
      ELSE
        Q.�l�mentsFile(Parent) := Q.�l�mentsFile(Descendant); -- remonter un niveau
        Parent := Descendant;
        Descendant := 2 * Parent;
      END IF;
    END LOOP;
    Q.�l�mentsFile(Parent) := Copie;
  END IF;
END D�filer;

QueuePrior: FileAvecPriorit�;
�lt: Type�l�ment;
Priorit�: Natural;

BEGIN
  QueuePrior.Taille := 0;
END FilesAvecPriorit�;
