PROCEDURE TrierShell(Table: IN OUT Type_Table; OrdreDeTri: IN Ordre) IS
-- Trier Table en ordre ascendant ou descendant.
Copie: Quelconque;
Bas, Écart, Dernier: Integer;
BEGIN
  Dernier := Table'Last;
  Écart := Dernier;
  WHILE Écart > 1 LOOP
    Écart := Écart / 2;
    FOR Haut IN Écart+1..Dernier LOOP
      Bas := Haut - Écart;
      Copie := Table(Haut);                 -- nouvelle valeur à vérifier
      LOOP
        EXIT WHEN Bas <= 0;
        IF Ordonné(Copie, Table(Bas), OrdreDeTri) THEN
          Table(Bas + Écart) := Table(Bas); --avancer élément
          Bas := Bas - Écart;               -- reculer
        ELSE
          EXIT;
        END IF;
      END LOOP;
      Table(Bas + Écart) := Copie;          -- position finale
    END LOOP;
    Ada.Text_IO.Put(Item => "Écart "); Sortir(Valeur => Écart, Largeur => 5, Base => 10);
    Ada.Text_IO.Put(Item => ':'); Afficher(Table);
  END LOOP;
END TrierShell;

