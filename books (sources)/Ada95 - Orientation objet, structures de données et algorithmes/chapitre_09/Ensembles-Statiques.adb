--          Copyright © 1998 Philippe J. Gabrini
PACKAGE BODY Ensembles.Statiques IS
PRAGMA PURE(Ensembles.Statiques);

PROCEDURE Vider(Ens: IN OUT Ensemble) IS
-- Affecte l'ensemble vide à Ens.
BEGIN
  Ens.Compte := 0;
END Vider;

PROCEDURE Affecter(Destination: OUT Ensemble;
                   Source: IN Ensemble) IS
-- Donne à Destination une copie de la valeur de Source.
BEGIN
  Destination.Éléments := Source.Éléments;
  Destination.Compte := Source.Compte;
END Affecter;

PROCEDURE Inclure(Ens: IN OUT Ensemble; Élément: IN Type_Élément) IS
-- Inclure Élément dans Ens.
BEGIN
  IF NOT Ens.Éléments(Clef(Élément)).Présent THEN
    Ens.Éléments(Clef(Élément)).Présent := True;
    Ens.Éléments(Clef(Élément)).Valeur := Élément;
    Ens.Compte := Ens.Compte + 1;
  END IF;
END Inclure;

PROCEDURE Exclure(Ens: IN OUT Ensemble; Élément: IN OUT Type_Élément) IS
-- Exclure Élément de Ens et en copier la valeur.
BEGIN
  IF Ens.Éléments(Clef(Élément)).Présent THEN
    Ens.Compte := Ens.Compte - 1;
    Ens.Éléments(Clef(Élément)).Présent := False;
    Élément := Ens.Éléments(Clef(Élément)).Valeur;
  END IF;
END Exclure;

FUNCTION Membre(Élément: IN Type_Élément; Ens: IN Ensemble) RETURN Boolean IS
-- Retourne True, si  Élément est dans Ens, False autrement.
BEGIN
  RETURN Ens.Éléments(Clef(Élément)).Présent;
END Membre;

PROCEDURE FaireUnion(Gauche, Droite: IN Ensemble;
                     Résultat: OUT Ensemble) IS
-- Retourner l'ensemble contenant tous les éléments de Gauche et Droite.
BEGIN
  FOR Index IN Type_Clef LOOP
    IF Gauche.Éléments(Index).Présent THEN
      Résultat.Éléments(Index).Valeur := Gauche.Éléments(Index).Valeur;
      Résultat.Compte := Résultat.Compte + 1;
    ELSIF Droite.Éléments(Index).Présent THEN
      Résultat.Éléments(Index).Valeur := Droite.Éléments(Index).Valeur;
      Résultat.Compte := Résultat.Compte + 1;
    END IF;
    Résultat.Éléments(Index).Présent := Gauche.Éléments(Index).Présent
                                     OR Droite.Éléments(Index).Présent;
  END LOOP;
END FaireUnion;

PROCEDURE FaireIntersection(Gauche, Droite: IN Ensemble;
                            Résultat: OUT Ensemble) IS
-- Retourner l'ensemble contenant tous les éléments à la fois dans Gauche
-- et Droite.
BEGIN
  FOR Index IN Type_Clef LOOP
    IF Gauche.Éléments(Index).Présent AND Droite.Éléments(Index).Présent THEN
      Résultat.Éléments(Index).Valeur := Droite.Éléments(Index).Valeur;
      Résultat.Compte := Résultat.Compte + 1;
    END IF;
    Résultat.Éléments(Index).Présent := Gauche.Éléments(Index).Présent
                                    AND Droite.Éléments(Index).Présent;
  END LOOP;
END FaireIntersection;

PROCEDURE FaireDifférence(Gauche, Droite: IN Ensemble;
                          Résultat: OUT Ensemble) IS
-- Retourner l'ensemble contenant tous les éléments de Gauche
-- qui ne sont pas dans Droite.
BEGIN
  FOR Index IN Type_Clef LOOP
    IF Gauche.Éléments(Index).Présent
                AND NOT Droite.Éléments(Index).Présent THEN
      Résultat.Éléments(Index).Valeur := Gauche.Éléments(Index).Valeur;
      Résultat.Compte := Résultat.Compte + 1;
    END IF;
    Résultat.Éléments(Index).Présent := Gauche.Éléments(Index).Présent 
                                AND NOT Droite.Éléments(Index).Présent;
  END LOOP;
END FaireDifférence;

FUNCTION "="(Gauche, Droite: IN Ensemble) RETURN Boolean IS
-- Retourne True, si les deux ensembles contiennent les mêmes éléments,
-- False autrement.
BEGIN
  FOR Index IN Type_Clef LOOP
    IF Gauche.Éléments(Index).Présent /= Droite.Éléments(Index).Présent THEN
      RETURN False;
    END IF;
  END LOOP;
  RETURN True;
END "=";

FUNCTION "<=" (Gauche, Droite: IN Ensemble) RETURN Boolean IS
-- Retourne True, si Droite contient tous les éléments 
-- qui sont dans Gauche, False autrement.
Résultat: Boolean;
Index: Type_Clef;
BEGIN
  Index := Type_Clef'First;
  LOOP
    Résultat := NOT Gauche.Éléments(Index).Présent 
                  OR Droite.Éléments(Index).Présent;
    EXIT WHEN NOT Résultat OR Index = Type_Clef'Last;
    Index := Type_Clef'Succ(Index);
  END LOOP;
  RETURN Résultat;
END "<=";

FUNCTION Vide(Ens: IN Ensemble) RETURN Boolean IS
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
BEGIN
  RETURN Ens.Compte = 0;
END Vide;

FUNCTION Cardinalité(Ens: IN Ensemble) RETURN Natural IS
-- Retourne le nombre d'éléments dans Ens.
BEGIN
  RETURN Ens.Compte;
END Cardinalité;

END Ensembles.Statiques;
