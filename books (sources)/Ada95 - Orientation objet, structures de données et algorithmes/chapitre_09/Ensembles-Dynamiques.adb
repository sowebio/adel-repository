--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO, Ada.Unchecked_Deallocation;
PACKAGE BODY Ensembles.Dynamiques IS
-- Réalisation dynamique des ensembles
PROCEDURE Libérer IS NEW Ada.Unchecked_Deallocation(Élément, PtrÉlément);

PROCEDURE Vider(Ens: IN OUT Ensemble) IS
-- Donne à Ens la valeur de l'ensemble vide.
Présent, Précédent: PtrÉlément;
BEGIN
  Présent := Ens.Éléments; Précédent := NULL;
  WHILE Présent /= NULL LOOP -- libérer espace
    Précédent := Présent;
    Présent := Présent.Suivant;
    Libérer(Précédent);
  END LOOP;
  Ens.Éléments := NULL;
  Ens.Compte := 0;
END Vider;

PROCEDURE Inclure(Ens: IN OUT Ensemble; Élt: IN Type_Élément) IS
-- Inclure Élt dans Ens.
Nouveau, Présent, Précédent: PtrÉlément;
BEGIN
  Nouveau := NEW Élément'(Élt, NULL);  -- créer nouvel élément
  IF Ens.Éléments = NULL THEN          -- premier et seul élément
    Ens.Éléments := Nouveau;
    Ens.Compte := 1;
  ELSE
    Présent := Ens.Éléments; Précédent := NULL;
    WHILE (Présent /= NULL) AND THEN 
                   (Comparaison(Présent.ValeurÉlément, Élt) = -1) LOOP
      -- chercher dans liste ordonnée la position où insérer
      Précédent := Présent;
      Présent := Présent.Suivant;
    END LOOP;
    IF (Précédent = NULL) 
       AND THEN (Comparaison(Présent.ValeurÉlément, Élt) /= 0) THEN
      -- insérer en première position
      Nouveau.Suivant := Ens.Éléments;
      Ens.Éléments := Nouveau;
    ELSIF (Présent = NULL) 
            OR ELSE ((Présent /= NULL)  -- dernier ou milieu
            AND THEN (Comparaison(Présent.ValeurÉlément, Élt) /= 0)) THEN
      Précédent.Suivant := Nouveau;
      Nouveau.Suivant := Présent;
    END IF;
    Ens.Compte := Ens.Compte + 1;
  END IF;
END Inclure;

FUNCTION Membre(Élt: Type_Élément; Ens: Ensemble) RETURN Boolean IS
-- Retourne True, si  Élt est dans Ens, False autrement.
Présent: PtrÉlément;
BEGIN
  Présent := Ens.Éléments;
  WHILE (Présent /= NULL)  
          AND THEN (Comparaison(Présent.ValeurÉlément, Élt) = -1) LOOP
    Présent := Présent.Suivant;
  END LOOP;
  RETURN (Présent /= NULL) 
          AND THEN (Comparaison(Présent.ValeurÉlément, Élt) = 0);
END Membre;

PROCEDURE FaireIntersection(Gauche, Droite: IN Ensemble;
                            Résultat: OUT Ensemble) IS
-- Place dans Résultat l'ensemble contenant tous les éléments à la fois 
-- dans Gauche et Droite.
Présent1, Présent2, ÉltRésultat, Nouveau: PtrÉlément; 
BEGIN
  Présent1 := Gauche.Éléments;
  Présent2 := Droite.Éléments;
  Résultat.Éléments := NULL;
  Résultat.Compte := 0;
  ÉltRésultat := NULL;
  WHILE (Présent1 /= NULL) AND (Présent2 /= NULL) LOOP
    IF (Comparaison(Présent1.ValeurÉlément, Présent2.ValeurÉlément) = 0) THEN
      -- ajouter à intersection
      Nouveau := NEW Élément'(Présent1.ValeurÉlément, NULL);
      IF ÉltRésultat /= NULL THEN
        ÉltRésultat.Suivant := Nouveau;
        Résultat.Compte := Résultat.Compte + 1;
      ELSE -- premier élément
        Résultat.Éléments := Nouveau;
        Résultat.Compte := 1;
      END IF;
      ÉltRésultat := Nouveau;
      Présent1 := Présent1.Suivant;
      Présent2 := Présent2.Suivant;
    ELSIF Comparaison(Présent1.ValeurÉlément, Présent2.ValeurÉlément) = -1 THEN
      -- sauter élément gauche
      Présent1 := Présent1.Suivant;
    ELSE -- sauter élément droite
      Présent2 := Présent2.Suivant;
    END IF;
  END LOOP;
END FaireIntersection;

FUNCTION "="(Gauche, Droite: Ensemble) RETURN Boolean IS
-- Retourne True, si les deux ensembles contiennent les mêmes éléments,
-- False autrement.
Présent1, Présent2: PtrÉlément;
BEGIN
  Présent1 := Gauche.Éléments;
  Présent2 := Droite.Éléments;
  WHILE (Présent1 /= NULL) AND (Présent2 /= NULL) LOOP -- vérifier égalité
    IF (Comparaison(Présent1.ValeurÉlément, Présent2.ValeurÉlément) = 0) THEN
      Présent1 := Présent1.Suivant;
      Présent2 := Présent2.Suivant;
    ELSE
      RETURN False;
    END IF;
  END LOOP;
  RETURN Présent1 = Présent2;
END "=";

-- etc.

END Ensembles.Dynamiques;
