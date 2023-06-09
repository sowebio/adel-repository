--          Copyright © 1998 Philippe J. Gabrini
PACKAGE BODY TDAFile IS
   
PROCEDURE Vider(Q: IN OUT File) IS
-- Vider la file Q
BEGIN
  Q.Tête := Max;
  Q.Queue := 0;
  Q.Nombre := 0;
END Vider;

PROCEDURE Enfiler(Q: IN OUT File; Item: IN TypeÉlément) IS
-- Insère élément Item à la fin de la file Q
BEGIN 
  IF NOT (Q.Nombre = Max) THEN
    Q.Queue := (Q.Queue MOD Max) + 1;
    Q.Données(Q.Queue) := Item;
    Q.Nombre := Q.Nombre + 1;
  ELSE
    RAISE FilePleine;      -- plus de place
  END IF;
END Enfiler;

PROCEDURE Défiler(Q: IN OUT File; Item: OUT TypeÉlément) IS
-- Supprime le premier élément de la file Q et le retourne dans Item
BEGIN 
  IF Q.Nombre /= 0 THEN
    Q.Tête := (Q.Tête MOD Max) + 1;
    Item := Q.Données(Q.Tête);
    Q.Nombre := Q.Nombre - 1;
    IF Q.Nombre = 0 THEN   -- file maintenant vide
      Q.Queue := 0;
      Q.Tête := Max;
    END IF;
  ELSE
    RAISE FileVide;      -- pas d'élément à récupérer
  END IF;
END Défiler;

FUNCTION Longueur(Q: IN File) RETURN Natural IS
-- Retourne le nombre d'éléments dans la file Q
BEGIN
  RETURN Q.Nombre;
END Longueur;

FUNCTION TêteDeFile(Q: IN File) RETURN TypeÉlément IS
-- Retourne la valeur du premier élément de la file Q
Item: TypeÉlément;
BEGIN
  IF Q.Nombre /= 0 THEN
    Item := Q.Données(Q.Tête MOD Max + 1);
    RETURN Item;
  ELSE
    RAISE FileVide;      -- pas de file
  END IF;
END TêteDeFile;

END TDAFile;
