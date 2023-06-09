--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PROCEDURE FilesAvecPriorité IS

Max: CONSTANT Natural := 100;

TYPE TypeÉlément IS RECORD
					  Clef: Character;
					  Données: Integer;
				    END RECORD;
TYPE Noeud IS RECORD
                Priorité: Natural;
                Élément: TypeÉlément;
              END RECORD;
SUBTYPE Position IS Natural RANGE 0..Max;
TYPE Vecteur IS ARRAY (Position) OF Noeud;
TYPE FileAvecPriorité IS RECORD
                           Taille: Position;
                           ÉlémentsFile: Vecteur;
                         END RECORD;

PROCEDURE Enfiler(Q: IN OUT FileAvecPriorité; Élt: IN TypeÉlément;
                  Prior: IN Natural) IS
-- Ajouter élément avec Priorité à la file d'attente Q.
Parent, Descendant: Position;
BEGIN
  IF Q.Taille < Max THEN
    Q.Taille := Q.Taille + 1;
    Q.ÉlémentsFile(Q.Taille).Priorité := Prior;    -- ajouter à la fin
    Q.ÉlémentsFile(Q.Taille).Élément := Élt;
    -- remonter élément à sa place dans le monceau
    Q.ÉlémentsFile(0) := Q.ÉlémentsFile(Q.Taille); -- sentinelle
    Descendant := Q.Taille;
    Parent := Descendant / 2;
    WHILE Q.ÉlémentsFile(Parent).Priorité < Prior LOOP
      -- trouver position pour dernier élément
      Q.ÉlémentsFile(Descendant) := Q.ÉlémentsFile(Parent);
      Descendant := Parent;
      Parent := Parent / 2;
    END LOOP;
    -- copier élément à sa place
    Q.ÉlémentsFile(Descendant) := Q.ÉlémentsFile(0);
  END IF;
END Enfiler;

PROCEDURE Défiler(Q: IN OUT FileAvecPriorité; Élt: IN OUT TypeÉlément;
                  Prior: IN OUT Natural) IS
-- Récupérer l'élément avec la plus grande priorité.
Parent, Descendant: Position;
Fini: Boolean;
Copie: Noeud;
BEGIN
  IF Q.Taille > 0 THEN
    Prior := Q.ÉlémentsFile(1).Priorité; -- copier premier élément
    Élt := Q.ÉlémentsFile(1).Élément;
    Q.ÉlémentsFile(1) := Q.ÉlémentsFile(Q.Taille); -- le remplacer
    Q.Taille := Q.Taille - 1;
    Parent := 1;                 -- reconstruire monceau en déplaçant le premier
    Descendant := 2;             -- élément vers le bas à sa place dans monceau
    Copie := Q.ÉlémentsFile(1);
    Fini := False;
    WHILE (Descendant <= Q.Taille) AND NOT Fini LOOP -- inspecter descendants
      IF Descendant < Q.Taille THEN
        IF Q.ÉlémentsFile(Descendant).Priorité < Q.ÉlémentsFile(Descendant+1).Priorité THEN
          Descendant := Descendant + 1; -- selectionner le plus grand des deux descendants
        END IF;
      END IF;
      IF Copie.Priorité > Q.ÉlémentsFile(Descendant).Priorité THEN
        Fini := True;		         -- position trouvée
      ELSE
        Q.ÉlémentsFile(Parent) := Q.ÉlémentsFile(Descendant); -- remonter un niveau
        Parent := Descendant;
        Descendant := 2 * Parent;
      END IF;
    END LOOP;
    Q.ÉlémentsFile(Parent) := Copie;
  END IF;
END Défiler;

QueuePrior: FileAvecPriorité;
Élt: TypeÉlément;
Priorité: Natural;

BEGIN
  QueuePrior.Taille := 0;
END FilesAvecPriorité;
