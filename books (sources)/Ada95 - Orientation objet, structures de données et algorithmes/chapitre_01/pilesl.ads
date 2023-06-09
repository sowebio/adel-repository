----------------------------------------------------------------------------
--   Fichier:  Piles.ads
--   Objectif: Sp�cification des piles bas�es sur les listes cha�n�es.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Listes;
GENERIC
  TYPE Type_�l�ment IS PRIVATE;
PACKAGE Piles IS

TYPE Type_Pile IS LIMITED PRIVATE;

Pile_Pleine, Pile_Vide: EXCEPTION;

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  �l�ment: IN Type_�l�ment);
-- Ant�c�dent: aucun.
-- Cons�quent: �l�ment est empil� sur la Pile.
-- Exception:  Pile_Pleine est lev�e s'il n'y a plus de m�moire.

PROCEDURE D�sempiler(Pile: IN OUT Type_Pile;
                     �l�ment: OUT Type_�l�ment);
-- Ant�c�dent: la Pile n'est pas vide.
-- Cons�quent: l'�l�ment du sommet de la pile est plac� dans �l�ment.
-- Exception:  Pile_Vide.

FUNCTION Sommet(Pile: Type_Pile) RETURN Type_�l�ment;
-- Ant�c�dent: la Pile n'est pas vide.
-- Cons�quent: l'�l�ment au sommet de la pile est retourn�.
-- Exception:  Pile_Vide.

FUNCTION Vide(Pile: Type_Pile) RETURN Boolean;
-- Ant�c�dent: aucun.
-- Cons�quent: indique si la Pile est vide.

PRIVATE
  PACKAGE Listes_Piles IS NEW Listes(Type_�l�ment); -- instanciation automatique
  TYPE Type_Pile IS RECORD
                      L: Listes_Piles.Type_Liste;
                    END RECORD;

END Piles;
