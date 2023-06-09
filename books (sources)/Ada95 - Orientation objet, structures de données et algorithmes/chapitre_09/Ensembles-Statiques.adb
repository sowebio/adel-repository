--          Copyright � 1998 Philippe J. Gabrini
PACKAGE BODY Ensembles.Statiques IS
PRAGMA PURE(Ensembles.Statiques);

PROCEDURE Vider(Ens: IN OUT Ensemble) IS
-- Affecte l'ensemble vide � Ens.
BEGIN
  Ens.Compte := 0;
END Vider;

PROCEDURE Affecter(Destination: OUT Ensemble;
                   Source: IN Ensemble) IS
-- Donne � Destination une copie de la valeur de Source.
BEGIN
  Destination.�l�ments := Source.�l�ments;
  Destination.Compte := Source.Compte;
END Affecter;

PROCEDURE Inclure(Ens: IN OUT Ensemble; �l�ment: IN Type_�l�ment) IS
-- Inclure �l�ment dans Ens.
BEGIN
  IF NOT Ens.�l�ments(Clef(�l�ment)).Pr�sent THEN
    Ens.�l�ments(Clef(�l�ment)).Pr�sent := True;
    Ens.�l�ments(Clef(�l�ment)).Valeur := �l�ment;
    Ens.Compte := Ens.Compte + 1;
  END IF;
END Inclure;

PROCEDURE Exclure(Ens: IN OUT Ensemble; �l�ment: IN OUT Type_�l�ment) IS
-- Exclure �l�ment de Ens et en copier la valeur.
BEGIN
  IF Ens.�l�ments(Clef(�l�ment)).Pr�sent THEN
    Ens.Compte := Ens.Compte - 1;
    Ens.�l�ments(Clef(�l�ment)).Pr�sent := False;
    �l�ment := Ens.�l�ments(Clef(�l�ment)).Valeur;
  END IF;
END Exclure;

FUNCTION Membre(�l�ment: IN Type_�l�ment; Ens: IN Ensemble) RETURN Boolean IS
-- Retourne True, si  �l�ment est dans Ens, False autrement.
BEGIN
  RETURN Ens.�l�ments(Clef(�l�ment)).Pr�sent;
END Membre;

PROCEDURE FaireUnion(Gauche, Droite: IN Ensemble;
                     R�sultat: OUT Ensemble) IS
-- Retourner l'ensemble contenant tous les �l�ments de Gauche et Droite.
BEGIN
  FOR Index IN Type_Clef LOOP
    IF Gauche.�l�ments(Index).Pr�sent THEN
      R�sultat.�l�ments(Index).Valeur := Gauche.�l�ments(Index).Valeur;
      R�sultat.Compte := R�sultat.Compte + 1;
    ELSIF Droite.�l�ments(Index).Pr�sent THEN
      R�sultat.�l�ments(Index).Valeur := Droite.�l�ments(Index).Valeur;
      R�sultat.Compte := R�sultat.Compte + 1;
    END IF;
    R�sultat.�l�ments(Index).Pr�sent := Gauche.�l�ments(Index).Pr�sent
                                     OR Droite.�l�ments(Index).Pr�sent;
  END LOOP;
END FaireUnion;

PROCEDURE FaireIntersection(Gauche, Droite: IN Ensemble;
                            R�sultat: OUT Ensemble) IS
-- Retourner l'ensemble contenant tous les �l�ments � la fois dans Gauche
-- et Droite.
BEGIN
  FOR Index IN Type_Clef LOOP
    IF Gauche.�l�ments(Index).Pr�sent AND Droite.�l�ments(Index).Pr�sent THEN
      R�sultat.�l�ments(Index).Valeur := Droite.�l�ments(Index).Valeur;
      R�sultat.Compte := R�sultat.Compte + 1;
    END IF;
    R�sultat.�l�ments(Index).Pr�sent := Gauche.�l�ments(Index).Pr�sent
                                    AND Droite.�l�ments(Index).Pr�sent;
  END LOOP;
END FaireIntersection;

PROCEDURE FaireDiff�rence(Gauche, Droite: IN Ensemble;
                          R�sultat: OUT Ensemble) IS
-- Retourner l'ensemble contenant tous les �l�ments de Gauche
-- qui ne sont pas dans Droite.
BEGIN
  FOR Index IN Type_Clef LOOP
    IF Gauche.�l�ments(Index).Pr�sent
                AND NOT Droite.�l�ments(Index).Pr�sent THEN
      R�sultat.�l�ments(Index).Valeur := Gauche.�l�ments(Index).Valeur;
      R�sultat.Compte := R�sultat.Compte + 1;
    END IF;
    R�sultat.�l�ments(Index).Pr�sent := Gauche.�l�ments(Index).Pr�sent 
                                AND NOT Droite.�l�ments(Index).Pr�sent;
  END LOOP;
END FaireDiff�rence;

FUNCTION "="(Gauche, Droite: IN Ensemble) RETURN Boolean IS
-- Retourne True, si les deux ensembles contiennent les m�mes �l�ments,
-- False autrement.
BEGIN
  FOR Index IN Type_Clef LOOP
    IF Gauche.�l�ments(Index).Pr�sent /= Droite.�l�ments(Index).Pr�sent THEN
      RETURN False;
    END IF;
  END LOOP;
  RETURN True;
END "=";

FUNCTION "<=" (Gauche, Droite: IN Ensemble) RETURN Boolean IS
-- Retourne True, si Droite contient tous les �l�ments 
-- qui sont dans Gauche, False autrement.
R�sultat: Boolean;
Index: Type_Clef;
BEGIN
  Index := Type_Clef'First;
  LOOP
    R�sultat := NOT Gauche.�l�ments(Index).Pr�sent 
                  OR Droite.�l�ments(Index).Pr�sent;
    EXIT WHEN NOT R�sultat OR Index = Type_Clef'Last;
    Index := Type_Clef'Succ(Index);
  END LOOP;
  RETURN R�sultat;
END "<=";

FUNCTION Vide(Ens: IN Ensemble) RETURN Boolean IS
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
BEGIN
  RETURN Ens.Compte = 0;
END Vide;

FUNCTION Cardinalit�(Ens: IN Ensemble) RETURN Natural IS
-- Retourne le nombre d'�l�ments dans Ens.
BEGIN
  RETURN Ens.Compte;
END Cardinalit�;

END Ensembles.Statiques;
