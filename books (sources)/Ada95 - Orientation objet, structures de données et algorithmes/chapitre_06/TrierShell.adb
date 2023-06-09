PROCEDURE TrierShell(Table: IN OUT Type_Table; OrdreDeTri: IN Ordre) IS
-- Trier Table en ordre ascendant ou descendant.
Copie: Quelconque;
Bas, �cart, Dernier: Integer;
BEGIN
  Dernier := Table'Last;
  �cart := Dernier;
  WHILE �cart > 1 LOOP
    �cart := �cart / 2;
    FOR Haut IN �cart+1..Dernier LOOP
      Bas := Haut - �cart;
      Copie := Table(Haut);                 -- nouvelle valeur � v�rifier
      LOOP
        EXIT WHEN Bas <= 0;
        IF Ordonn�(Copie, Table(Bas), OrdreDeTri) THEN
          Table(Bas + �cart) := Table(Bas); --avancer �l�ment
          Bas := Bas - �cart;               -- reculer
        ELSE
          EXIT;
        END IF;
      END LOOP;
      Table(Bas + �cart) := Copie;          -- position finale
    END LOOP;
    Ada.Text_IO.Put(Item => "�cart "); Sortir(Valeur => �cart, Largeur => 5, Base => 10);
    Ada.Text_IO.Put(Item => ':'); Afficher(Table);
  END LOOP;
END TrierShell;

