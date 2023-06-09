--          Copyright � 1998 Philippe J. Gabrini
PACKAGE BODY EnsemblesDiscrets IS

PROCEDURE Vider(Ens: IN OUT TypEnsemble) IS
-- Affecte l'ensemble vide � Ens.
BEGIN
  Ens := (�l�ments => (Type_�l�ment => False), Compte => 0);
END Vider;

PROCEDURE Inclure(Ens: IN OUT TypEnsemble; �l�ment: IN Type_�l�ment) IS
-- Inclure �l�ment dans Ens.
BEGIN
  IF NOT Ens.�l�ments(�l�ment) THEN
    Ens.�l�ments(�l�ment) := True;
    Ens.Compte := Ens.Compte + 1;
  END IF;
END Inclure;

PROCEDURE Exclure(Ens: IN OUT TypEnsemble; �l�ment: IN Type_�l�ment) IS
-- Exclure �l�ment de Ens.
BEGIN
  IF Ens.�l�ments(�l�ment) THEN
    Ens.Compte := Ens.Compte - 1;
    Ens.�l�ments(�l�ment) := False;
  END IF;
END Exclure;

FUNCTION Membre(�l�ment: Type_�l�ment; Ens: TypEnsemble)
               RETURN Boolean IS
-- Retourne True, si  �l�ment est dans Ens, False autrement.
BEGIN
  RETURN Boolean(Ens.�l�ments(�l�ment));
END Membre;

FUNCTION "+"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble IS
-- Retourner l'ensemble contenant tous les �l�ments de Gauche et Droite.
R�sultat: TypEnsemble;
BEGIN
  R�sultat.�l�ments := Gauche.�l�ments OR Droite.�l�ments;
  RETURN R�sultat;
END "+";

FUNCTION "*"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble IS
-- Retourner l'ensemble contenant tous les �l�ments � la fois
-- dans Gauche et Droite.
R�sultat: TypEnsemble;
BEGIN
  R�sultat.�l�ments := Gauche.�l�ments AND Droite.�l�ments;
  RETURN R�sultat;
END "*";

FUNCTION "-"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble IS
-- Retourner l'ensemble contenant tous les �l�ments de Gauche
-- qui ne sont pas dans Droite.
R�sultat: TypEnsemble;
BEGIN
  R�sultat.�l�ments := Gauche.�l�ments AND NOT Droite.�l�ments;
  RETURN R�sultat;
END "-";

FUNCTION "<=" (Gauche, Droite: TypEnsemble) RETURN Boolean IS
-- Retourne True, si Gauche contient tous les �l�ments 
-- qui sont dans Ens, False autrement.
BEGIN
  RETURN (Gauche.�l�ments AND Droite.�l�ments) = Gauche.�l�ments;
END "<=";

FUNCTION Vide(Ens: TypEnsemble) RETURN Boolean IS
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
EnsembleVide: Tableau := (Type_�l�ment => False);
BEGIN
  RETURN EnsembleVide = Ens.�l�ments;
END Vide;

FUNCTION Cardinalit�(Ens: TypEnsemble) RETURN Natural IS
-- Retourne le nombre d'�l�ments dans Ens.
BEGIN
  RETURN Ens.Compte;
END Cardinalit�;

FUNCTION Min(Ens: TypEnsemble) RETURN Type_�l�ment IS
-- Retourne le plus petit �l�ment de Ens ou la valeur maximum si vide.
BEGIN
  FOR Index IN Type_�l�ment LOOP
    IF Ens.�l�ments(Index) THEN
      RETURN Index;
    END IF;
  END LOOP;
  RETURN Type_�l�ment'Last;
END Min;

FUNCTION Max(Ens: TypEnsemble) RETURN Type_�l�ment IS
-- Retourne le plus grand �l�ment de Ens ou la valeur minimum si vide.
BEGIN
  FOR Index IN REVERSE Type_�l�ment LOOP
    IF Ens.�l�ments(Index) THEN
      RETURN Index;
    END IF;
  END LOOP;
  RETURN Type_�l�ment'First;
END Max;

END EnsemblesDiscrets;
