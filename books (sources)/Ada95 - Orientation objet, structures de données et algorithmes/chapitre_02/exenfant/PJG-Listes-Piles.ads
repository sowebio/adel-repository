----------------------------------------------------------------------------
--   Objectif: Spécification des piles basées sur les listes chaînées.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
GENERIC  -- un paquetage enfant d'un paquetage générique doit être générique
PACKAGE PJG.Listes.Piles IS

SUBTYPE Type_Pile IS Type_Liste; -- équivalent à Type_Liste

Pile_Pleine, Pile_Vide: EXCEPTION;

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  Élément: IN Type_Valeur);
PROCEDURE Désempiler(Pile: IN OUT Type_Pile;
                     Élément: OUT Type_Valeur);
FUNCTION Sommet(Pile: Type_Pile) RETURN Type_Valeur;
FUNCTION Vide(Pile: Type_Pile) RETURN Boolean;

END PJG.Listes.Piles;
