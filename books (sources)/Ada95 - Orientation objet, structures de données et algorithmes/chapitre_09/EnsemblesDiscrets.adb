--          Copyright © 1998 Philippe J. Gabrini
PACKAGE BODY EnsemblesDiscrets IS

PROCEDURE Vider(Ens: IN OUT TypEnsemble) IS
-- Affecte l'ensemble vide à Ens.
BEGIN
  Ens := (Éléments => (Type_Élément => False), Compte => 0);
END Vider;

PROCEDURE Inclure(Ens: IN OUT TypEnsemble; Élément: IN Type_Élément) IS
-- Inclure Élément dans Ens.
BEGIN
  IF NOT Ens.Éléments(Élément) THEN
    Ens.Éléments(Élément) := True;
    Ens.Compte := Ens.Compte + 1;
  END IF;
END Inclure;

PROCEDURE Exclure(Ens: IN OUT TypEnsemble; Élément: IN Type_Élément) IS
-- Exclure Élément de Ens.
BEGIN
  IF Ens.Éléments(Élément) THEN
    Ens.Compte := Ens.Compte - 1;
    Ens.Éléments(Élément) := False;
  END IF;
END Exclure;

FUNCTION Membre(Élément: Type_Élément; Ens: TypEnsemble)
               RETURN Boolean IS
-- Retourne True, si  Élément est dans Ens, False autrement.
BEGIN
  RETURN Boolean(Ens.Éléments(Élément));
END Membre;

FUNCTION "+"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble IS
-- Retourner l'ensemble contenant tous les éléments de Gauche et Droite.
Résultat: TypEnsemble;
BEGIN
  Résultat.Éléments := Gauche.Éléments OR Droite.Éléments;
  RETURN Résultat;
END "+";

FUNCTION "*"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble IS
-- Retourner l'ensemble contenant tous les éléments à la fois
-- dans Gauche et Droite.
Résultat: TypEnsemble;
BEGIN
  Résultat.Éléments := Gauche.Éléments AND Droite.Éléments;
  RETURN Résultat;
END "*";

FUNCTION "-"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble IS
-- Retourner l'ensemble contenant tous les éléments de Gauche
-- qui ne sont pas dans Droite.
Résultat: TypEnsemble;
BEGIN
  Résultat.Éléments := Gauche.Éléments AND NOT Droite.Éléments;
  RETURN Résultat;
END "-";

FUNCTION "<=" (Gauche, Droite: TypEnsemble) RETURN Boolean IS
-- Retourne True, si Gauche contient tous les éléments 
-- qui sont dans Ens, False autrement.
BEGIN
  RETURN (Gauche.Éléments AND Droite.Éléments) = Gauche.Éléments;
END "<=";

FUNCTION Vide(Ens: TypEnsemble) RETURN Boolean IS
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
EnsembleVide: Tableau := (Type_Élément => False);
BEGIN
  RETURN EnsembleVide = Ens.Éléments;
END Vide;

FUNCTION Cardinalité(Ens: TypEnsemble) RETURN Natural IS
-- Retourne le nombre d'éléments dans Ens.
BEGIN
  RETURN Ens.Compte;
END Cardinalité;

FUNCTION Min(Ens: TypEnsemble) RETURN Type_Élément IS
-- Retourne le plus petit Élément de Ens ou la valeur maximum si vide.
BEGIN
  FOR Index IN Type_Élément LOOP
    IF Ens.Éléments(Index) THEN
      RETURN Index;
    END IF;
  END LOOP;
  RETURN Type_Élément'Last;
END Min;

FUNCTION Max(Ens: TypEnsemble) RETURN Type_Élément IS
-- Retourne le plus grand Élément de Ens ou la valeur minimum si vide.
BEGIN
  FOR Index IN REVERSE Type_Élément LOOP
    IF Ens.Éléments(Index) THEN
      RETURN Index;
    END IF;
  END LOOP;
  RETURN Type_Élément'First;
END Max;

END EnsemblesDiscrets;
