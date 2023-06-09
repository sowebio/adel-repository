----------------------------------------------------------------------------
--   Fichier:  Piles.adb
--   Objectif: Définition des piles basées sur les listes chaînées.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
PACKAGE BODY Piles IS

PACKAGE LIP RENAMES Listes_Piles; -- déjà instancié dans la partie privée

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  Élément: IN Type_Élément) IS
BEGIN
  LIP.Insérer(LIP.Premier(Pile.L), Élément); -- premier élément liste
EXCEPTION
  WHEN LIP.Erreur_Liste => RAISE Pile_Pleine;
END Empiler;

PROCEDURE Désempiler(Pile: IN OUT Type_Pile;
                     Élément: OUT Type_Élément) IS
BEGIN
  Élément := Sommet(Pile);                -- copier valeur
  LIP.Supprimer(LIP.Premier(Pile.L));     -- premier élément de liste
EXCEPTION
  WHEN LIP.Erreur_Liste => RAISE Pile_Vide;
END Désempiler;

FUNCTION Sommet(Pile: Type_Pile) RETURN Type_Élément IS
BEGIN
  RETURN LIP.Valeur(LIP.Premier(Pile.L)); -- valeur premier élément
EXCEPTION
  WHEN LIP.Erreur_Liste => RAISE Pile_Vide;
END Sommet;

FUNCTION Vide(Pile: Type_Pile) RETURN Boolean IS
BEGIN
  RETURN LIP.Taille(Pile.L) = 0;
END Vide;

END Piles;
