----------------------------------------------------------------------------
--   Objectif: Listes doublement cha�n�es
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Ada.Unchecked_Deallocation;
PACKAGE BODY PJG.Listes IS

PROCEDURE Supprimer_Composant IS 
       NEW Ada.Unchecked_Deallocation(Composant, Pointeur_Composant);

FUNCTION Taille(Liste: Type_Liste) RETURN Natural IS
BEGIN
  RETURN Liste.Liste.Compteur;
END Taille;

FUNCTION Premier(Liste: Type_Liste) RETURN �l�ment_Liste IS
BEGIN
  RETURN (Liste => Liste.Liste, Courant => Liste.Liste.Premier);
END Premier;

FUNCTION Dernier(Liste: Type_Liste) RETURN �l�ment_Liste IS
BEGIN
  RETURN (Liste => Liste.Liste, Courant => Liste.Liste.Dernier);
END Dernier;

PROCEDURE Vider_Liste(Liste: IN OUT Type_Liste) IS
BEGIN
  WHILE Liste.Liste.Compteur /= 0 LOOP
    Supprimer(Premier(Liste));
  END LOOP;
END Vider_Liste;

FUNCTION Successeur(�l�ment : �l�ment_Liste) RETURN �l�ment_Liste IS
BEGIN
  IF �l�ment.Liste = NULL OR ELSE �l�ment.Courant = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    RETURN (Liste => �l�ment.Liste, Courant => �l�ment.Courant.Suivant);
  END IF;
END Successeur;

FUNCTION Pr�d�cesseur(�l�ment : �l�ment_Liste) RETURN �l�ment_Liste IS
BEGIN
  IF �l�ment.Liste = NULL OR ELSE �l�ment.Courant = �l�ment.Liste.Premier THEN
    RAISE Erreur_Liste;
  ELSIF �l�ment.Courant = NULL THEN
    RETURN (Liste => �l�ment.Liste, Courant => �l�ment.Liste.Dernier);
  ELSE
    RETURN (Liste => �l�ment.Liste, Courant => �l�ment.Courant.Pr�c�dent);
  END IF;
END Pr�d�cesseur;

FUNCTION Valeur(�l�ment : �l�ment_Liste) RETURN Type_Valeur IS
BEGIN
  IF �l�ment.Liste = NULL OR ELSE �l�ment.Courant = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    RETURN �l�ment.Courant.Item;
  END IF;
END Valeur;

PROCEDURE Supprimer(�l�ment: IN �l�ment_Liste) IS
Item : Pointeur_Composant := �l�ment.Courant;
BEGIN
  IF �l�ment.Liste = NULL OR ELSE �l�ment.Courant = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    IF �l�ment.Courant.Suivant = NULL THEN
      �l�ment.Liste.Dernier := �l�ment.Courant.Pr�c�dent;
    ELSE
      �l�ment.Courant.Suivant.Pr�c�dent := �l�ment.Courant.Pr�c�dent;
    END IF;

    IF �l�ment.Courant.Pr�c�dent = NULL THEN
      �l�ment.Liste.Premier := �l�ment.Courant.Suivant;
    ELSE
      �l�ment.Courant.Pr�c�dent.Suivant := �l�ment.Courant.Suivant;
    END IF;

    Supprimer_Composant(Item);
    �l�ment.Liste.Compteur := �l�ment.Liste.Compteur - 1;
  END IF;
END Supprimer;

PROCEDURE Ins�rer(�l�ment: IN �l�ment_Liste;
                  Item: IN Type_Valeur) IS
Nouveau: Pointeur_Composant;
BEGIN
  IF �l�ment.Liste = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    Nouveau := NEW Composant;
    Nouveau.Suivant := �l�ment.Courant;
    Nouveau.Item := Item;

    IF �l�ment.Courant = NULL THEN
      Nouveau.Pr�c�dent := �l�ment.Liste.Dernier;
      �l�ment.Liste.Dernier := Nouveau;
    ELSE
      Nouveau.Pr�c�dent := �l�ment.Courant.Pr�c�dent;
      �l�ment.Courant.Pr�c�dent := Nouveau;
    END IF;

    IF �l�ment.Courant = �l�ment.Liste.Premier THEN
      �l�ment.Liste.Premier := Nouveau;
    ELSE
      Nouveau.Pr�c�dent.Suivant := Nouveau;
    END IF;

    �l�ment.Liste.Compteur := �l�ment.Liste.Compteur + 1;
  END IF;
EXCEPTION
  WHEN Storage_Error => RAISE Erreur_Liste; -- plus de m�moire
END Ins�rer;

END PJG.Listes;
