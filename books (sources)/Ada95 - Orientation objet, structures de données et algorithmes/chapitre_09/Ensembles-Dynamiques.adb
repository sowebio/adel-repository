--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO, Ada.Unchecked_Deallocation;
PACKAGE BODY Ensembles.Dynamiques IS
-- R�alisation dynamique des ensembles
PROCEDURE Lib�rer IS NEW Ada.Unchecked_Deallocation(�l�ment, Ptr�l�ment);

PROCEDURE Vider(Ens: IN OUT Ensemble) IS
-- Donne � Ens la valeur de l'ensemble vide.
Pr�sent, Pr�c�dent: Ptr�l�ment;
BEGIN
  Pr�sent := Ens.�l�ments; Pr�c�dent := NULL;
  WHILE Pr�sent /= NULL LOOP -- lib�rer espace
    Pr�c�dent := Pr�sent;
    Pr�sent := Pr�sent.Suivant;
    Lib�rer(Pr�c�dent);
  END LOOP;
  Ens.�l�ments := NULL;
  Ens.Compte := 0;
END Vider;

PROCEDURE Inclure(Ens: IN OUT Ensemble; �lt: IN Type_�l�ment) IS
-- Inclure �lt dans Ens.
Nouveau, Pr�sent, Pr�c�dent: Ptr�l�ment;
BEGIN
  Nouveau := NEW �l�ment'(�lt, NULL);  -- cr�er nouvel �l�ment
  IF Ens.�l�ments = NULL THEN          -- premier et seul �l�ment
    Ens.�l�ments := Nouveau;
    Ens.Compte := 1;
  ELSE
    Pr�sent := Ens.�l�ments; Pr�c�dent := NULL;
    WHILE (Pr�sent /= NULL) AND THEN 
                   (Comparaison(Pr�sent.Valeur�l�ment, �lt) = -1) LOOP
      -- chercher dans liste ordonn�e la position o� ins�rer
      Pr�c�dent := Pr�sent;
      Pr�sent := Pr�sent.Suivant;
    END LOOP;
    IF (Pr�c�dent = NULL) 
       AND THEN (Comparaison(Pr�sent.Valeur�l�ment, �lt) /= 0) THEN
      -- ins�rer en premi�re position
      Nouveau.Suivant := Ens.�l�ments;
      Ens.�l�ments := Nouveau;
    ELSIF (Pr�sent = NULL) 
            OR ELSE ((Pr�sent /= NULL)  -- dernier ou milieu
            AND THEN (Comparaison(Pr�sent.Valeur�l�ment, �lt) /= 0)) THEN
      Pr�c�dent.Suivant := Nouveau;
      Nouveau.Suivant := Pr�sent;
    END IF;
    Ens.Compte := Ens.Compte + 1;
  END IF;
END Inclure;

FUNCTION Membre(�lt: Type_�l�ment; Ens: Ensemble) RETURN Boolean IS
-- Retourne True, si  �lt est dans Ens, False autrement.
Pr�sent: Ptr�l�ment;
BEGIN
  Pr�sent := Ens.�l�ments;
  WHILE (Pr�sent /= NULL)  
          AND THEN (Comparaison(Pr�sent.Valeur�l�ment, �lt) = -1) LOOP
    Pr�sent := Pr�sent.Suivant;
  END LOOP;
  RETURN (Pr�sent /= NULL) 
          AND THEN (Comparaison(Pr�sent.Valeur�l�ment, �lt) = 0);
END Membre;

PROCEDURE FaireIntersection(Gauche, Droite: IN Ensemble;
                            R�sultat: OUT Ensemble) IS
-- Place dans R�sultat l'ensemble contenant tous les �l�ments � la fois 
-- dans Gauche et Droite.
Pr�sent1, Pr�sent2, �ltR�sultat, Nouveau: Ptr�l�ment; 
BEGIN
  Pr�sent1 := Gauche.�l�ments;
  Pr�sent2 := Droite.�l�ments;
  R�sultat.�l�ments := NULL;
  R�sultat.Compte := 0;
  �ltR�sultat := NULL;
  WHILE (Pr�sent1 /= NULL) AND (Pr�sent2 /= NULL) LOOP
    IF (Comparaison(Pr�sent1.Valeur�l�ment, Pr�sent2.Valeur�l�ment) = 0) THEN
      -- ajouter � intersection
      Nouveau := NEW �l�ment'(Pr�sent1.Valeur�l�ment, NULL);
      IF �ltR�sultat /= NULL THEN
        �ltR�sultat.Suivant := Nouveau;
        R�sultat.Compte := R�sultat.Compte + 1;
      ELSE -- premier �l�ment
        R�sultat.�l�ments := Nouveau;
        R�sultat.Compte := 1;
      END IF;
      �ltR�sultat := Nouveau;
      Pr�sent1 := Pr�sent1.Suivant;
      Pr�sent2 := Pr�sent2.Suivant;
    ELSIF Comparaison(Pr�sent1.Valeur�l�ment, Pr�sent2.Valeur�l�ment) = -1 THEN
      -- sauter �l�ment gauche
      Pr�sent1 := Pr�sent1.Suivant;
    ELSE -- sauter �l�ment droite
      Pr�sent2 := Pr�sent2.Suivant;
    END IF;
  END LOOP;
END FaireIntersection;

FUNCTION "="(Gauche, Droite: Ensemble) RETURN Boolean IS
-- Retourne True, si les deux ensembles contiennent les m�mes �l�ments,
-- False autrement.
Pr�sent1, Pr�sent2: Ptr�l�ment;
BEGIN
  Pr�sent1 := Gauche.�l�ments;
  Pr�sent2 := Droite.�l�ments;
  WHILE (Pr�sent1 /= NULL) AND (Pr�sent2 /= NULL) LOOP -- v�rifier �galit�
    IF (Comparaison(Pr�sent1.Valeur�l�ment, Pr�sent2.Valeur�l�ment) = 0) THEN
      Pr�sent1 := Pr�sent1.Suivant;
      Pr�sent2 := Pr�sent2.Suivant;
    ELSE
      RETURN False;
    END IF;
  END LOOP;
  RETURN Pr�sent1 = Pr�sent2;
END "=";

-- etc.

END Ensembles.Dynamiques;
