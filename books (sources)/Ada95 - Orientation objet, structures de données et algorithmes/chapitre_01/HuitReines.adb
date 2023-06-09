--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Reines;
PROCEDURE HuitReines IS
Voisine, Derni�reReine: Reines.Reine;
BEGIN
  Ada.Text_IO.Put_Line ("Probl�me des huit reines!");
  Ada.Text_IO.New_Line;
  FOR i IN 1..8 LOOP  -- allouer espace aux huit reines, leur donner leur rang�e
    -- et leur voisine
    Derni�reReine := Reines.ReineCr��e(i, Voisine);
    Voisine := Derni�reReine;
  END LOOP;
  IF Reines.Premi�reSolution(Derni�reReine) THEN
    -- premi�re solution de la derni�re reine cr��e
    Reines.Afficher(Derni�reReine);
  END IF;
END HuitReines; 
