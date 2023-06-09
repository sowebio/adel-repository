----------------------------------------------------------------------------
--   Fichier:  PJG-Piles.ads
--   Objectif: Sp�cification des piles bas�es sur les listes cha�n�es.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH PJG.Listes;
GENERIC
  TYPE Type_�l�ment IS PRIVATE;
PACKAGE PJG.Piles IS

PACKAGE Listes_Piles IS NEW PJG.Listes(Type_�l�ment);

TYPE Type_Pile IS NEW Listes_Piles.Type_Liste WITH PRIVATE;

Pile_Pleine, Pile_Vide: EXCEPTION;

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  �l�ment: IN Type_�l�ment);
PROCEDURE D�sempiler(Pile: IN OUT Type_Pile;
                     �l�ment: OUT Type_�l�ment);
FUNCTION Sommet(Pile: Type_Pile) RETURN Type_�l�ment;
FUNCTION Vide(Pile: Type_Pile) RETURN Boolean;

PRIVATE
  TYPE Type_Pile IS NEW Listes_Piles.Type_Liste WITH NULL RECORD;
  -- Tous les champs d�j� pr�sents sont suffisants.
END PJG.Piles;
