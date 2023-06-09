--          Copyright � 1998 Philippe J. Gabrini
GENERIC
PACKAGE Tables.Ordonn�es IS

TYPE Table IS NEW Table_Abstraite WITH PRIVATE;

Table_Pleine: EXCEPTION; -- lev�e si on tente d'ins�rer dans une table pleine
   
PROCEDURE Ins�rer(T: IN OUT Table; �lt: IN Type_�l�ment);
-- Ins�re l'�l�ment �lt dans la Table T.
-- Ant�c�dents:  La table n'est pas pleine.
-- Cons�quents:  Si �lt faisait d�j� partie de la table, il a �t� mis � jour,
--               sinon, �lt a �t� ins�r� dans T.
-- Exceptions:   Table_Pleine si la table est pleine.

FUNCTION Nb_�l�ments(T: Table) RETURN Natural;
-- Donne le nombre d'�l�ments contenus dans la table T.
-- Ant�c�dents:  Aucun.
-- Cons�quents:  Retourne le nombre d'�l�ments de la table T.

PROCEDURE Chercher(T: IN Table; �l�ment: IN OUT Type_�l�ment;
                   Trouv�: OUT Boolean);
-- Cherche un �l�ment dans la table T qui poss�de la clef de �l�ment.
-- Si on trouve un �l�ment, on le retourne dans �l�ment et Trouv� est vrai,
-- sinon Trouv� est faux.
-- Ant�c�dents: Aucun.
-- Cons�quents: Trouv�' est faux si la clef de �l�ment n'est pas dans T
--              ou Trouv�' est vrai et �l�ment' = �l�ment de la table T.
   
PROCEDURE Supprimer(T: IN OUT Table; �l�ment: IN Type_�l�ment;
                    Succ�s: OUT Boolean);
-- Supprime l'�l�ment ayant la m�me clef que �l�ment dans la table T.
-- Si la suppression a r�ussi Succ�s est mis � True, sinon � False.
-- Ant�c�dents: la table T existe.
-- Cons�quents: Succ�s' est vrai et la table T ne contient pas �l�ment
--              ou Succ�s' est faux et la table T est inchang�e.
   
PROCEDURE AfficherTable(T: IN Table);
-- Afficher tous les �l�ments de la table T.

PROCEDURE RangerTable(T: IN Table);
-- Copier les �l�ments de la table T dans un fichier.

PRIVATE
  SUBTYPE TypeIndex IS Natural RANGE 1..Taille_Table;
  TYPE Vecteur IS ARRAY (TypeIndex) OF Type_�l�ment;
  TYPE Table IS NEW Table_Abstraite WITH
              RECORD
                Nombre_d_�l�ments: Natural := 0;
                �l�ments: Vecteur;
              END RECORD;

END Tables.Ordonn�es;
