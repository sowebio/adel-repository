----------------------------------------------------------------------------
--   Objectif: Sp�cification des piles bas�es sur les listes cha�n�es.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
GENERIC  -- un paquetage enfant d'un paquetage g�n�rique doit �tre g�n�rique
PACKAGE PJG.Listes.Piles IS

SUBTYPE Type_Pile IS Type_Liste; -- �quivalent � Type_Liste

Pile_Pleine, Pile_Vide: EXCEPTION;

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  �l�ment: IN Type_Valeur);
PROCEDURE D�sempiler(Pile: IN OUT Type_Pile;
                     �l�ment: OUT Type_Valeur);
FUNCTION Sommet(Pile: Type_Pile) RETURN Type_Valeur;
FUNCTION Vide(Pile: Type_Pile) RETURN Boolean;

END PJG.Listes.Piles;
