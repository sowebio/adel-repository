--          Copyright � 1998 Philippe J. Gabrini
PACKAGE BODY TDAFile IS
   
PROCEDURE Vider(Q: IN OUT File) IS
-- Vider la file Q
BEGIN
  Q.T�te := Max;
  Q.Queue := 0;
  Q.Nombre := 0;
END Vider;

PROCEDURE Enfiler(Q: IN OUT File; Item: IN Type�l�ment) IS
-- Ins�re �l�ment Item � la fin de la file Q
BEGIN 
  IF NOT (Q.Nombre = Max) THEN
    Q.Queue := (Q.Queue MOD Max) + 1;
    Q.Donn�es(Q.Queue) := Item;
    Q.Nombre := Q.Nombre + 1;
  ELSE
    RAISE FilePleine;      -- plus de place
  END IF;
END Enfiler;

PROCEDURE D�filer(Q: IN OUT File; Item: OUT Type�l�ment) IS
-- Supprime le premier �l�ment de la file Q et le retourne dans Item
BEGIN 
  IF Q.Nombre /= 0 THEN
    Q.T�te := (Q.T�te MOD Max) + 1;
    Item := Q.Donn�es(Q.T�te);
    Q.Nombre := Q.Nombre - 1;
    IF Q.Nombre = 0 THEN   -- file maintenant vide
      Q.Queue := 0;
      Q.T�te := Max;
    END IF;
  ELSE
    RAISE FileVide;      -- pas d'�l�ment � r�cup�rer
  END IF;
END D�filer;

FUNCTION Longueur(Q: IN File) RETURN Natural IS
-- Retourne le nombre d'�l�ments dans la file Q
BEGIN
  RETURN Q.Nombre;
END Longueur;

FUNCTION T�teDeFile(Q: IN File) RETURN Type�l�ment IS
-- Retourne la valeur du premier �l�ment de la file Q
Item: Type�l�ment;
BEGIN
  IF Q.Nombre /= 0 THEN
    Item := Q.Donn�es(Q.T�te MOD Max + 1);
    RETURN Item;
  ELSE
    RAISE FileVide;      -- pas de file
  END IF;
END T�teDeFile;

END TDAFile;
