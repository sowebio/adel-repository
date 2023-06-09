----------------------------------------------------------------------------
--   Objectif: Listes doublement chaînées
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Ada.Unchecked_Deallocation;
PACKAGE BODY PJG.Listes IS

PROCEDURE Supprimer_Composant IS 
       NEW Ada.Unchecked_Deallocation(Composant, Pointeur_Composant);

FUNCTION Taille(Liste: Type_Liste) RETURN Natural IS
BEGIN
  RETURN Liste.Liste.Compteur;
END Taille;

FUNCTION Premier(Liste: Type_Liste) RETURN Élément_Liste IS
BEGIN
  RETURN (Liste => Liste.Liste, Courant => Liste.Liste.Premier);
END Premier;

FUNCTION Dernier(Liste: Type_Liste) RETURN Élément_Liste IS
BEGIN
  RETURN (Liste => Liste.Liste, Courant => Liste.Liste.Dernier);
END Dernier;

PROCEDURE Vider_Liste(Liste: IN OUT Type_Liste) IS
BEGIN
  WHILE Liste.Liste.Compteur /= 0 LOOP
    Supprimer(Premier(Liste));
  END LOOP;
END Vider_Liste;

FUNCTION Successeur(Élément : Élément_Liste) RETURN Élément_Liste IS
BEGIN
  IF Élément.Liste = NULL OR ELSE Élément.Courant = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    RETURN (Liste => Élément.Liste, Courant => Élément.Courant.Suivant);
  END IF;
END Successeur;

FUNCTION Prédécesseur(Élément : Élément_Liste) RETURN Élément_Liste IS
BEGIN
  IF Élément.Liste = NULL OR ELSE Élément.Courant = Élément.Liste.Premier THEN
    RAISE Erreur_Liste;
  ELSIF Élément.Courant = NULL THEN
    RETURN (Liste => Élément.Liste, Courant => Élément.Liste.Dernier);
  ELSE
    RETURN (Liste => Élément.Liste, Courant => Élément.Courant.Précédent);
  END IF;
END Prédécesseur;

FUNCTION Valeur(Élément : Élément_Liste) RETURN Type_Valeur IS
BEGIN
  IF Élément.Liste = NULL OR ELSE Élément.Courant = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    RETURN Élément.Courant.Item;
  END IF;
END Valeur;

PROCEDURE Supprimer(Élément: IN Élément_Liste) IS
Item : Pointeur_Composant := Élément.Courant;
BEGIN
  IF Élément.Liste = NULL OR ELSE Élément.Courant = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    IF Élément.Courant.Suivant = NULL THEN
      Élément.Liste.Dernier := Élément.Courant.Précédent;
    ELSE
      Élément.Courant.Suivant.Précédent := Élément.Courant.Précédent;
    END IF;

    IF Élément.Courant.Précédent = NULL THEN
      Élément.Liste.Premier := Élément.Courant.Suivant;
    ELSE
      Élément.Courant.Précédent.Suivant := Élément.Courant.Suivant;
    END IF;

    Supprimer_Composant(Item);
    Élément.Liste.Compteur := Élément.Liste.Compteur - 1;
  END IF;
END Supprimer;

PROCEDURE Insérer(Élément: IN Élément_Liste;
                  Item: IN Type_Valeur) IS
Nouveau: Pointeur_Composant;
BEGIN
  IF Élément.Liste = NULL THEN
    RAISE Erreur_Liste;
  ELSE
    Nouveau := NEW Composant;
    Nouveau.Suivant := Élément.Courant;
    Nouveau.Item := Item;

    IF Élément.Courant = NULL THEN
      Nouveau.Précédent := Élément.Liste.Dernier;
      Élément.Liste.Dernier := Nouveau;
    ELSE
      Nouveau.Précédent := Élément.Courant.Précédent;
      Élément.Courant.Précédent := Nouveau;
    END IF;

    IF Élément.Courant = Élément.Liste.Premier THEN
      Élément.Liste.Premier := Nouveau;
    ELSE
      Nouveau.Précédent.Suivant := Nouveau;
    END IF;

    Élément.Liste.Compteur := Élément.Liste.Compteur + 1;
  END IF;
EXCEPTION
  WHEN Storage_Error => RAISE Erreur_Liste; -- plus de mémoire
END Insérer;

END PJG.Listes;
