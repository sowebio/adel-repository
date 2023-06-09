--          Copyright © 1998 Philippe J. Gabrini
GENERIC
  Taille_Table: Positive := 50;
  TYPE Type_Élément IS PRIVATE;  
  WITH FUNCTION Comparaison(E1, E2: Type_Élément) RETURN Integer;
  WITH PROCEDURE AfficherÉlément(E: IN Type_Élément);
  WITH PROCEDURE AfficherClef(E: IN Type_Élément);
PACKAGE Tables IS
-- Ce module définit le type abstrait Table_Abstraite.  Ce type servira à
-- dériver d'autres types de tables pour différentes méthodes de réalisation.

TYPE Table_Abstraite IS ABSTRACT TAGGED LIMITED NULL RECORD;
-- Le type de table, dérivable et extensible pour créer d'autres
-- types de tables de la même classe.

PROCEDURE Insérer(T: IN OUT Table_Abstraite; Élt: IN Type_Élément) IS ABSTRACT;
-- Insère l'élément Élt dans la Table T.
-- Antécédents:  La table n'est pas pleine.
-- Conséquents:  Si Élt faisait déjà partie de la table, il a été mis à jour,
--               sinon, Élt a été inséré dans T.
-- Exceptions:   Table_Pleine si la table est pleine

FUNCTION Nb_Éléments(T: Table_Abstraite) RETURN Natural IS ABSTRACT;
-- Donne le nombre d'éléments contenus dans la table T.
-- Antécédents:  Aucun.
-- Conséquents:  Retourne le nombre d'éléments de la table T.

PROCEDURE Chercher(T: IN Table_Abstraite; Élément: IN OUT Type_Élément;
                   Trouvé: OUT Boolean) IS ABSTRACT;
-- Cherche un élément dans la table T qui possède la clef de Élément.
-- Si on trouve un élément, on le retourne dans Élément et Trouvé est vrai,
-- sinon Trouvé est faux.
-- Antécédents: Aucun.
-- Conséquents: Trouvé' est faux si la clef de Élément n'est pas dans T
--              ou Trouvé' est vrai et Élément' = élément de la table T.
   
PROCEDURE Supprimer(T: IN OUT Table_Abstraite; Élément: IN Type_Élément;
                    Succès: OUT Boolean) IS ABSTRACT;
-- Supprime l'élément ayant la même clef que Élément dans la table T.
-- Si la suppression a réussi, Succès est mis à True, sinon à False.
-- Antécédents: La table T existe.
-- Conséquents: Succès' est vrai et la table T ne contient pas Élément
--              ou Succès' est faux et la table T est inchangée.
   
END Tables;
