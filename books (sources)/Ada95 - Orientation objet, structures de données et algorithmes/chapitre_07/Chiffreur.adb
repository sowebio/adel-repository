--          Copyright © 1998 Philippe J. Gabrini
PACKAGE BODY Chiffreur IS

PROCEDURE Chiffrer(Chaîne: IN OUT Chaînes.TypChaîne) IS
-- Chiffrer Chaîne
Car: Character;
    
  PROCEDURE RenverserChaîne(Chaîne: IN OUT Chaînes.TypChaîne) IS
  -- Renversement d'une Chaîne
  Longueur: Natural;
  Tête, Queue: Chaînes.TypChaîne;
  BEGIN
    Longueur := Chaînes.Longueur(Chaîne);
    IF Longueur > 0 THEN
      Tête := Chaînes.Tête(Chaîne, Longueur-1);
      Queue := Chaînes.Queue(Chaîne, 1);
      RenverserChaîne(Tête);
      Chaîne := Chaînes."&"(Queue, Tête);
    END IF;
  END RenverserChaîne;
  
BEGIN
  RenverserChaîne(Chaîne);
  FOR Index IN 1..Chaînes.Longueur(Chaîne) LOOP
    Car := Chaînes.Élément(Chaîne, Index);
    Car := Character'Val((Character'Pos(Car)+Index) MOD 256);
    Chaînes.RemplacerÉlément(Chaîne, Index, Car);
  END LOOP;
END Chiffrer;

END Chiffreur;
