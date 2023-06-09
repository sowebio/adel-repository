--          Copyright © 1998 Philippe J. Gabrini
GENERIC
PACKAGE Tables.Ordonnées IS

TYPE Table IS NEW Table_Abstraite WITH PRIVATE;

Table_Pleine: EXCEPTION; -- levée si on tente d'insérer dans une table pleine
   
PROCEDURE Insérer(T: IN OUT Table; Élt: IN Type_Élément);
-- Insère l'élément Élt dans la Table T.
-- Antécédents:  La table n'est pas pleine.
-- Conséquents:  Si Élt faisait déjà partie de la table, il a été mis à jour,
--               sinon, Élt a été inséré dans T.
-- Exceptions:   Table_Pleine si la table est pleine.

FUNCTION Nb_Éléments(T: Table) RETURN Natural;
-- Donne le nombre d'éléments contenus dans la table T.
-- Antécédents:  Aucun.
-- Conséquents:  Retourne le nombre d'éléments de la table T.

PROCEDURE Chercher(T: IN Table; Élément: IN OUT Type_Élément;
                   Trouvé: OUT Boolean);
-- Cherche un élément dans la table T qui possède la clef de Élément.
-- Si on trouve un élément, on le retourne dans Élément et Trouvé est vrai,
-- sinon Trouvé est faux.
-- Antécédents: Aucun.
-- Conséquents: Trouvé' est faux si la clef de Élément n'est pas dans T
--              ou Trouvé' est vrai et Élément' = élément de la table T.
   
PROCEDURE Supprimer(T: IN OUT Table; Élément: IN Type_Élément;
                    Succès: OUT Boolean);
-- Supprime l'élément ayant la même clef que Élément dans la table T.
-- Si la suppression a réussi Succès est mis à True, sinon à False.
-- Antécédents: la table T existe.
-- Conséquents: Succès' est vrai et la table T ne contient pas Élément
--              ou Succès' est faux et la table T est inchangée.
   
PROCEDURE AfficherTable(T: IN Table);
-- Afficher tous les éléments de la table T.

PROCEDURE RangerTable(T: IN Table);
-- Copier les éléments de la table T dans un fichier.

PRIVATE
  SUBTYPE TypeIndex IS Natural RANGE 1..Taille_Table;
  TYPE Vecteur IS ARRAY (TypeIndex) OF Type_Élément;
  TYPE Table IS NEW Table_Abstraite WITH
              RECORD
                Nombre_d_Éléments: Natural := 0;
                Éléments: Vecteur;
              END RECORD;

END Tables.Ordonnées;
