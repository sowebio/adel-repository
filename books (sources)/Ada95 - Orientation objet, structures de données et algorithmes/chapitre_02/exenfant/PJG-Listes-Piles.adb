----------------------------------------------------------------------------
--   Objectif: Définition des piles basées sur les listes chaînées
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
PACKAGE BODY PJG.Listes.Piles IS

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  Élément: IN Type_Valeur) IS
BEGIN
  Insérer(Premier(Pile), Élément);
EXCEPTION
  WHEN Erreur_Liste => RAISE Pile_Pleine;
END Empiler;

PROCEDURE Désempiler(Pile: IN OUT Type_Pile;
                     Élément: OUT Type_Valeur) IS
BEGIN
  Élément := Sommet(Pile);
  Supprimer(Premier(Pile));
EXCEPTION
  WHEN Erreur_Liste => RAISE Pile_Vide;
END Désempiler;

FUNCTION Sommet(Pile: Type_Pile) RETURN Type_Valeur IS
BEGIN
  RETURN Valeur(Premier(Pile));
EXCEPTION
  WHEN Erreur_Liste => RAISE Pile_Vide;
END Sommet;

FUNCTION Vide(Pile: Type_Pile) RETURN Boolean IS
BEGIN
  RETURN Taille(Pile) = 0;
END Vide;

END PJG.Listes.Piles;
