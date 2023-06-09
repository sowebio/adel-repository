----------------------------------------------------------------------------
--   Fichier:  Piles.adb
--   Objectif: D�finition des piles bas�es sur les listes cha�n�es.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
PACKAGE BODY Piles IS

PACKAGE LIP RENAMES Listes_Piles; -- d�j� instanci� dans la partie priv�e

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  �l�ment: IN Type_�l�ment) IS
BEGIN
  LIP.Ins�rer(LIP.Premier(Pile.L), �l�ment); -- premier �l�ment liste
EXCEPTION
  WHEN LIP.Erreur_Liste => RAISE Pile_Pleine;
END Empiler;

PROCEDURE D�sempiler(Pile: IN OUT Type_Pile;
                     �l�ment: OUT Type_�l�ment) IS
BEGIN
  �l�ment := Sommet(Pile);                -- copier valeur
  LIP.Supprimer(LIP.Premier(Pile.L));     -- premier �l�ment de liste
EXCEPTION
  WHEN LIP.Erreur_Liste => RAISE Pile_Vide;
END D�sempiler;

FUNCTION Sommet(Pile: Type_Pile) RETURN Type_�l�ment IS
BEGIN
  RETURN LIP.Valeur(LIP.Premier(Pile.L)); -- valeur premier �l�ment
EXCEPTION
  WHEN LIP.Erreur_Liste => RAISE Pile_Vide;
END Sommet;

FUNCTION Vide(Pile: Type_Pile) RETURN Boolean IS
BEGIN
  RETURN LIP.Taille(Pile.L) = 0;
END Vide;

END Piles;
