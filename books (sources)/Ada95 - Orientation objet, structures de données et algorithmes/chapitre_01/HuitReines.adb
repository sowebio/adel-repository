--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Reines;
PROCEDURE HuitReines IS
Voisine, DernièreReine: Reines.Reine;
BEGIN
  Ada.Text_IO.Put_Line ("Problème des huit reines!");
  Ada.Text_IO.New_Line;
  FOR i IN 1..8 LOOP  -- allouer espace aux huit reines, leur donner leur rangée
    -- et leur voisine
    DernièreReine := Reines.ReineCréée(i, Voisine);
    Voisine := DernièreReine;
  END LOOP;
  IF Reines.PremièreSolution(DernièreReine) THEN
    -- première solution de la dernière reine créée
    Reines.Afficher(DernièreReine);
  END IF;
END HuitReines; 
