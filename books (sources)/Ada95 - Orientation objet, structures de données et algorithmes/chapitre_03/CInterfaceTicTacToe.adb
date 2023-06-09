PACKAGE BODY CInterfaceTicTacToe IS

PROCEDURE Afficher(Élément: IN Situation) IS
-- Nécessaire à l'instanciation du module Arbres_Binaires, mais non utilisée.
BEGIN
  FOR Rangée IN Indice_Carré LOOP
    FOR Colonne IN Indice_Carré LOOP
      IF Élément.Carré(Rangée, Colonne) = X THEN
        Ada.Text_IO.Put("| X ");
      ELSIF Élément.Carré(Rangée, Colonne) = O THEN
        Ada.Text_IO.Put("| O ");
      ELSE
        Ada.Text_IO.Put("|   ");
      END IF;
    END LOOP;
  END LOOP;
  Ada.Text_IO.Put("|");
  IF Élément.Tour = Plus THEN
    Ada.Text_IO.Put(" +");
  ELSE
    Ada.Text_IO.Put(" -");
  END IF;
END Afficher;

END CInterfaceTicTacToe;

