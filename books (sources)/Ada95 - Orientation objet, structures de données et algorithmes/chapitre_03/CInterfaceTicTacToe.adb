PACKAGE BODY CInterfaceTicTacToe IS

PROCEDURE Afficher(�l�ment: IN Situation) IS
-- N�cessaire � l'instanciation du module Arbres_Binaires, mais non utilis�e.
BEGIN
  FOR Rang�e IN Indice_Carr� LOOP
    FOR Colonne IN Indice_Carr� LOOP
      IF �l�ment.Carr�(Rang�e, Colonne) = X THEN
        Ada.Text_IO.Put("| X ");
      ELSIF �l�ment.Carr�(Rang�e, Colonne) = O THEN
        Ada.Text_IO.Put("| O ");
      ELSE
        Ada.Text_IO.Put("|   ");
      END IF;
    END LOOP;
  END LOOP;
  Ada.Text_IO.Put("|");
  IF �l�ment.Tour = Plus THEN
    Ada.Text_IO.Put(" +");
  ELSE
    Ada.Text_IO.Put(" -");
  END IF;
END Afficher;

END CInterfaceTicTacToe;

