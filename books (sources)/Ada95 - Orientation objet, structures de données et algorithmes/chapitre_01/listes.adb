----------------------------------------------------------------------------
--   Fichier:  Listes.adb
--   Objectif: Listes doublement chaînées.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Ada.Unchecked_Deallocation;
PACKAGE BODY Listes IS

PROCEDURE Relâcher_Composant IS 
       NEW Ada.Unchecked_Deallocation(Un_Composant, Pointeur_Composant);

FUNCTION Taille(Liste: Type_Liste) RETURN Natural IS
BEGIN
  RETURN Liste.Tête.Compteur;  -- nombre d'éléments
END Taille;

FUNCTION Premier(Liste: Type_Liste) RETURN Élément_Liste IS
BEGIN  -- repère au premier élément
  RETURN (Liste => Liste.Tête, Élément => Liste.Tête.Premier);
END Premier;

FUNCTION Dernier(Liste: Type_Liste) RETURN Élément_Liste IS
BEGIN  -- repère au dernier élément
  RETURN (Liste => Liste.Tête, Élément => Liste.Tête.Dernier);
END Dernier;

PROCEDURE Vider_Liste(Liste: IN OUT Type_Liste) IS
BEGIN
  WHILE Liste.Tête.Compteur /= 0 LOOP
    Supprimer(Premier(Liste));
  END LOOP;
END Vider_Liste;

FUNCTION Successeur(Composant: Élément_Liste) RETURN Élément_Liste IS
BEGIN
  IF Composant.Liste = NULL OR ELSE Composant.Élément = NULL THEN
    RAISE Erreur_Liste;  -- pas de liste ou pas d'élément
  ELSE   -- successeur (si dernier -> NULL)
    RETURN (Liste => Composant.Liste, Élément => Composant.Élément.Suivant);
  END IF;
END Successeur;

FUNCTION Prédécesseur(Composant : Élément_Liste) RETURN Élément_Liste IS
BEGIN
  IF Composant.Liste = NULL 
     OR ELSE Composant.Élément = Composant.Liste.Premier THEN
    RAISE Erreur_Liste;  -- pas de liste ou premier élément
  ELSIF Composant.Élément = NULL THEN -- dernier élément
    RETURN (Liste => Composant.Liste, Élément => Composant.Liste.Dernier);
  ELSE
    RETURN (Liste => Composant.Liste, Élément => Composant.Élément.Précédent);
  END IF;
END Prédécesseur;

FUNCTION Valeur(Composant : Élément_Liste) RETURN Type_Valeur IS
BEGIN
  IF Composant.Liste = NULL OR ELSE Composant.Élément = NULL THEN
    RAISE Erreur_Liste;   -- pas de liste ou pas d'élément
  ELSE
    RETURN Composant.Élément.Item;
  END IF;
END Valeur;

PROCEDURE Supprimer(Composant: IN Élément_Liste) IS
Item : Pointeur_Composant := Composant.Élément;
BEGIN
  IF Composant.Liste = NULL OR ELSE Composant.Élément = NULL THEN
    RAISE Erreur_Liste;  -- pas de liste ou pas d'élément
  ELSE
    IF Composant.Élément.Suivant = NULL THEN   -- dernier élément
      Composant.Liste.Dernier := Composant.Élément.Précédent;
    ELSE
      Composant.Élément.Suivant.Précédent := Composant.Élément.Précédent;
    END IF;
    IF Composant.Élément.Précédent = NULL THEN -- premier élément
      Composant.Liste.Premier := Composant.Élément.Suivant;
    ELSE
      Composant.Élément.Précédent.Suivant := Composant.Élément.Suivant;
    END IF;
    Relâcher_Composant(Item);
    Composant.Liste.Compteur := Composant.Liste.Compteur - 1;
  END IF;
END Supprimer;

PROCEDURE Insérer(Composant: IN Élément_Liste;
                  Item: IN Type_Valeur) IS
Nouveau: Pointeur_Composant;
BEGIN
  IF Composant.Liste = NULL THEN        -- liste inexistante
    RAISE Erreur_Liste;
  ELSE
    Nouveau := NEW Un_Composant;        -- nouvel élément
    Nouveau.Suivant := Composant.Élément;
    Nouveau.Item := Item;
    IF Composant.Élément = NULL THEN    -- insérer en dernier
      Nouveau.Précédent := Composant.Liste.Dernier;
      Composant.Liste.Dernier := Nouveau;
    ELSE                                -- insérer avant Composant
      Nouveau.Précédent := Composant.Élément.Précédent;
      Composant.Élément.Précédent := Nouveau;
    END IF;
    IF Composant.Élément = Composant.Liste.Premier THEN -- en premier
      Composant.Liste.Premier := Nouveau;
    ELSE
      Nouveau.Précédent.Suivant := Nouveau;
    END IF;
    Composant.Liste.Compteur := Composant.Liste.Compteur + 1;
  END IF;
EXCEPTION
  WHEN Storage_Error => RAISE Erreur_Liste; -- plus de mémoire
END Insérer;

END Listes;
