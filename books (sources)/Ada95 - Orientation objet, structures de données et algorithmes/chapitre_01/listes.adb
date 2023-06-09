----------------------------------------------------------------------------
--   Fichier:  Listes.adb
--   Objectif: Listes doublement cha�n�es.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Ada.Unchecked_Deallocation;
PACKAGE BODY Listes IS

PROCEDURE Rel�cher_Composant IS 
       NEW Ada.Unchecked_Deallocation(Un_Composant, Pointeur_Composant);

FUNCTION Taille(Liste: Type_Liste) RETURN Natural IS
BEGIN
  RETURN Liste.T�te.Compteur;  -- nombre d'�l�ments
END Taille;

FUNCTION Premier(Liste: Type_Liste) RETURN �l�ment_Liste IS
BEGIN  -- rep�re au premier �l�ment
  RETURN (Liste => Liste.T�te, �l�ment => Liste.T�te.Premier);
END Premier;

FUNCTION Dernier(Liste: Type_Liste) RETURN �l�ment_Liste IS
BEGIN  -- rep�re au dernier �l�ment
  RETURN (Liste => Liste.T�te, �l�ment => Liste.T�te.Dernier);
END Dernier;

PROCEDURE Vider_Liste(Liste: IN OUT Type_Liste) IS
BEGIN
  WHILE Liste.T�te.Compteur /= 0 LOOP
    Supprimer(Premier(Liste));
  END LOOP;
END Vider_Liste;

FUNCTION Successeur(Composant: �l�ment_Liste) RETURN �l�ment_Liste IS
BEGIN
  IF Composant.Liste = NULL OR ELSE Composant.�l�ment = NULL THEN
    RAISE Erreur_Liste;  -- pas de liste ou pas d'�l�ment
  ELSE   -- successeur (si dernier -> NULL)
    RETURN (Liste => Composant.Liste, �l�ment => Composant.�l�ment.Suivant);
  END IF;
END Successeur;

FUNCTION Pr�d�cesseur(Composant : �l�ment_Liste) RETURN �l�ment_Liste IS
BEGIN
  IF Composant.Liste = NULL 
     OR ELSE Composant.�l�ment = Composant.Liste.Premier THEN
    RAISE Erreur_Liste;  -- pas de liste ou premier �l�ment
  ELSIF Composant.�l�ment = NULL THEN -- dernier �l�ment
    RETURN (Liste => Composant.Liste, �l�ment => Composant.Liste.Dernier);
  ELSE
    RETURN (Liste => Composant.Liste, �l�ment => Composant.�l�ment.Pr�c�dent);
  END IF;
END Pr�d�cesseur;

FUNCTION Valeur(Composant : �l�ment_Liste) RETURN Type_Valeur IS
BEGIN
  IF Composant.Liste = NULL OR ELSE Composant.�l�ment = NULL THEN
    RAISE Erreur_Liste;   -- pas de liste ou pas d'�l�ment
  ELSE
    RETURN Composant.�l�ment.Item;
  END IF;
END Valeur;

PROCEDURE Supprimer(Composant: IN �l�ment_Liste) IS
Item : Pointeur_Composant := Composant.�l�ment;
BEGIN
  IF Composant.Liste = NULL OR ELSE Composant.�l�ment = NULL THEN
    RAISE Erreur_Liste;  -- pas de liste ou pas d'�l�ment
  ELSE
    IF Composant.�l�ment.Suivant = NULL THEN   -- dernier �l�ment
      Composant.Liste.Dernier := Composant.�l�ment.Pr�c�dent;
    ELSE
      Composant.�l�ment.Suivant.Pr�c�dent := Composant.�l�ment.Pr�c�dent;
    END IF;
    IF Composant.�l�ment.Pr�c�dent = NULL THEN -- premier �l�ment
      Composant.Liste.Premier := Composant.�l�ment.Suivant;
    ELSE
      Composant.�l�ment.Pr�c�dent.Suivant := Composant.�l�ment.Suivant;
    END IF;
    Rel�cher_Composant(Item);
    Composant.Liste.Compteur := Composant.Liste.Compteur - 1;
  END IF;
END Supprimer;

PROCEDURE Ins�rer(Composant: IN �l�ment_Liste;
                  Item: IN Type_Valeur) IS
Nouveau: Pointeur_Composant;
BEGIN
  IF Composant.Liste = NULL THEN        -- liste inexistante
    RAISE Erreur_Liste;
  ELSE
    Nouveau := NEW Un_Composant;        -- nouvel �l�ment
    Nouveau.Suivant := Composant.�l�ment;
    Nouveau.Item := Item;
    IF Composant.�l�ment = NULL THEN    -- ins�rer en dernier
      Nouveau.Pr�c�dent := Composant.Liste.Dernier;
      Composant.Liste.Dernier := Nouveau;
    ELSE                                -- ins�rer avant Composant
      Nouveau.Pr�c�dent := Composant.�l�ment.Pr�c�dent;
      Composant.�l�ment.Pr�c�dent := Nouveau;
    END IF;
    IF Composant.�l�ment = Composant.Liste.Premier THEN -- en premier
      Composant.Liste.Premier := Nouveau;
    ELSE
      Nouveau.Pr�c�dent.Suivant := Nouveau;
    END IF;
    Composant.Liste.Compteur := Composant.Liste.Compteur + 1;
  END IF;
EXCEPTION
  WHEN Storage_Error => RAISE Erreur_Liste; -- plus de m�moire
END Ins�rer;

END Listes;
