----------------------------------------------------------------------------
--   Objectif: D�finition des piles bas�es sur les listes cha�n�es
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
PACKAGE BODY PJG.Listes.Piles IS

PROCEDURE Empiler(Pile: IN OUT Type_Pile;
                  �l�ment: IN Type_Valeur) IS
BEGIN
  Ins�rer(Premier(Pile), �l�ment);
EXCEPTION
  WHEN Erreur_Liste => RAISE Pile_Pleine;
END Empiler;

PROCEDURE D�sempiler(Pile: IN OUT Type_Pile;
                     �l�ment: OUT Type_Valeur) IS
BEGIN
  �l�ment := Sommet(Pile);
  Supprimer(Premier(Pile));
EXCEPTION
  WHEN Erreur_Liste => RAISE Pile_Vide;
END D�sempiler;

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
