--          Copyright � 1998 Philippe J. Gabrini
PACKAGE BODY Chiffreur IS

PROCEDURE Chiffrer(Cha�ne: IN OUT Cha�nes.TypCha�ne) IS
-- Chiffrer Cha�ne
Car: Character;
    
  PROCEDURE RenverserCha�ne(Cha�ne: IN OUT Cha�nes.TypCha�ne) IS
  -- Renversement d'une Cha�ne
  Longueur: Natural;
  T�te, Queue: Cha�nes.TypCha�ne;
  BEGIN
    Longueur := Cha�nes.Longueur(Cha�ne);
    IF Longueur > 0 THEN
      T�te := Cha�nes.T�te(Cha�ne, Longueur-1);
      Queue := Cha�nes.Queue(Cha�ne, 1);
      RenverserCha�ne(T�te);
      Cha�ne := Cha�nes."&"(Queue, T�te);
    END IF;
  END RenverserCha�ne;
  
BEGIN
  RenverserCha�ne(Cha�ne);
  FOR Index IN 1..Cha�nes.Longueur(Cha�ne) LOOP
    Car := Cha�nes.�l�ment(Cha�ne, Index);
    Car := Character'Val((Character'Pos(Car)+Index) MOD 256);
    Cha�nes.Remplacer�l�ment(Cha�ne, Index, Car);
  END LOOP;
END Chiffrer;

END Chiffreur;
