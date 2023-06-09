----------------------------------------------------------------------------
--   Fichier:  PJG-Piles.adb
--   Objectif: Définition des piles basées sur les listes chaînées.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
PACKAGE BODY PJG.Piles IS

PACKAGE LIP RENAMES Listes_Piles;

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  Élément: IN Type_Élément) IS
BEGIN
  -- il est toujours possible de convertir d'un type dérivé à un type de base
  LIP.Insérer(LIP.Premier(LIP.Type_Liste(Pile)), Élément);
EXCEPTION
  WHEN Storage_Error => RAISE Pile_Pleine;
END Empiler;

PROCEDURE Désempiler(Pile: IN OUT Type_Pile;
                     Élément: OUT Type_Élément) IS
BEGIN
  Élément := Sommet(Pile);
  LIP.Supprimer(LIP.Premier(LIP.Type_Liste(Pile)));
EXCEPTION
  WHEN LIP.Erreur_Liste => RAISE Pile_Vide;
END Désempiler;

FUNCTION Sommet(Pile: Type_Pile) RETURN Type_Élément IS
BEGIN
  RETURN LIP.Valeur(LIP.Premier(LIP.Type_Liste(Pile)));
EXCEPTION
  WHEN LIP.Erreur_Liste => RAISE Pile_Vide;
END Sommet;

FUNCTION Vide(Pile: Type_Pile) RETURN Boolean IS
BEGIN
  RETURN Taille(Pile) = 0;
END Vide;

END PJG.Piles;
