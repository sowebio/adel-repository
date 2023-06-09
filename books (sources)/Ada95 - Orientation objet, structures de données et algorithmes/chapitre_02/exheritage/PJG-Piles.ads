----------------------------------------------------------------------------
--   Fichier:  PJG-Piles.ads
--   Objectif: Spécification des piles basées sur les listes chaînées.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH PJG.Listes;
GENERIC
  TYPE Type_Élément IS PRIVATE;
PACKAGE PJG.Piles IS

PACKAGE Listes_Piles IS NEW PJG.Listes(Type_Élément);

TYPE Type_Pile IS NEW Listes_Piles.Type_Liste WITH PRIVATE;

Pile_Pleine, Pile_Vide: EXCEPTION;

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  Élément: IN Type_Élément);
PROCEDURE Désempiler(Pile: IN OUT Type_Pile;
                     Élément: OUT Type_Élément);
FUNCTION Sommet(Pile: Type_Pile) RETURN Type_Élément;
FUNCTION Vide(Pile: Type_Pile) RETURN Boolean;

PRIVATE
  TYPE Type_Pile IS NEW Listes_Piles.Type_Liste WITH NULL RECORD;
  -- Tous les champs déjà présents sont suffisants.
END PJG.Piles;
