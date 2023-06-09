----------------------------------------------------------------------------
--   Fichier:  Piles.ads
--   Objectif: Spécification des piles basées sur les listes chaînées.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Listes;
GENERIC
  TYPE Type_Élément IS PRIVATE;
PACKAGE Piles IS

TYPE Type_Pile IS LIMITED PRIVATE;

Pile_Pleine, Pile_Vide: EXCEPTION;

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  Élément: IN Type_Élément);
-- Antécédent: aucun.
-- Conséquent: Élément est empilé sur la Pile.
-- Exception:  Pile_Pleine est levée s'il n'y a plus de mémoire.

PROCEDURE Désempiler(Pile: IN OUT Type_Pile;
                     Élément: OUT Type_Élément);
-- Antécédent: la Pile n'est pas vide.
-- Conséquent: l'élément du sommet de la pile est placé dans Élément.
-- Exception:  Pile_Vide.

FUNCTION Sommet(Pile: Type_Pile) RETURN Type_Élément;
-- Antécédent: la Pile n'est pas vide.
-- Conséquent: l'élément au sommet de la pile est retourné.
-- Exception:  Pile_Vide.

FUNCTION Vide(Pile: Type_Pile) RETURN Boolean;
-- Antécédent: aucun.
-- Conséquent: indique si la Pile est vide.

PRIVATE
  PACKAGE Listes_Piles IS NEW Listes(Type_Élément); -- instanciation automatique
  TYPE Type_Pile IS RECORD
                      L: Listes_Piles.Type_Liste;
                    END RECORD;

END Piles;
