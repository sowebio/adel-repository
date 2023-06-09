--          Copyright � 1998 Philippe J. Gabrini
GENERIC
  Taille_Table: Positive := 50;
  TYPE Type_�l�ment IS PRIVATE;  
  WITH FUNCTION Comparaison(E1, E2: Type_�l�ment) RETURN Integer;
  WITH PROCEDURE Afficher�l�ment(E: IN Type_�l�ment);
  WITH PROCEDURE AfficherClef(E: IN Type_�l�ment);
PACKAGE Tables IS
-- Ce module d�finit le type abstrait Table_Abstraite.  Ce type servira �
-- d�river d'autres types de tables pour diff�rentes m�thodes de r�alisation.

TYPE Table_Abstraite IS ABSTRACT TAGGED LIMITED NULL RECORD;
-- Le type de table, d�rivable et extensible pour cr�er d'autres
-- types de tables de la m�me classe.

PROCEDURE Ins�rer(T: IN OUT Table_Abstraite; �lt: IN Type_�l�ment) IS ABSTRACT;
-- Ins�re l'�l�ment �lt dans la Table T.
-- Ant�c�dents:  La table n'est pas pleine.
-- Cons�quents:  Si �lt faisait d�j� partie de la table, il a �t� mis � jour,
--               sinon, �lt a �t� ins�r� dans T.
-- Exceptions:   Table_Pleine si la table est pleine

FUNCTION Nb_�l�ments(T: Table_Abstraite) RETURN Natural IS ABSTRACT;
-- Donne le nombre d'�l�ments contenus dans la table T.
-- Ant�c�dents:  Aucun.
-- Cons�quents:  Retourne le nombre d'�l�ments de la table T.

PROCEDURE Chercher(T: IN Table_Abstraite; �l�ment: IN OUT Type_�l�ment;
                   Trouv�: OUT Boolean) IS ABSTRACT;
-- Cherche un �l�ment dans la table T qui poss�de la clef de �l�ment.
-- Si on trouve un �l�ment, on le retourne dans �l�ment et Trouv� est vrai,
-- sinon Trouv� est faux.
-- Ant�c�dents: Aucun.
-- Cons�quents: Trouv�' est faux si la clef de �l�ment n'est pas dans T
--              ou Trouv�' est vrai et �l�ment' = �l�ment de la table T.
   
PROCEDURE Supprimer(T: IN OUT Table_Abstraite; �l�ment: IN Type_�l�ment;
                    Succ�s: OUT Boolean) IS ABSTRACT;
-- Supprime l'�l�ment ayant la m�me clef que �l�ment dans la table T.
-- Si la suppression a r�ussi, Succ�s est mis � True, sinon � False.
-- Ant�c�dents: La table T existe.
-- Cons�quents: Succ�s' est vrai et la table T ne contient pas �l�ment
--              ou Succ�s' est faux et la table T est inchang�e.
   
END Tables;
