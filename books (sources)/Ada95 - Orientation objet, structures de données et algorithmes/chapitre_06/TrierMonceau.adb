PROCEDURE TrierMonceau(Table: IN OUT Type_Table; OrdreTri: IN Ordre) IS
-- Trier Table en ordre ascendant ou descendant.
  
  PROCEDURE Tamiser(Tab: IN OUT Type_Table; Premier, Dernier: IN Natural) IS
  -- Tab(Premier)..Tab(Dernier) ont la propriété "monceau" sauf, peut-être,
  -- Tab(Premier). Cette procédure tamise Tab(Premier) jusqu'à ce que la
  -- propriété soit restaurée. 
  Pos: Natural;  -- position de Tab(Premier)
  BEGIN
    Pos := Premier;
    LOOP
      EXIT WHEN 2 * Pos > Dernier; -- pas de descendant
      IF 2 * Pos = Dernier THEN	   -- Pos a un descendant à 2*Pos
        IF Ordonné(Tab(2 * Pos), Tab(Pos), OrdreTri) THEN
          ÉchangerQuelc(Tab(Pos), Tab(2 * Pos));
        END IF;
        EXIT;
      ELSIF   -- Pos a 2 descendants à 2*Pos et 2*Pos+1
           Ordonné(Tab(2 * Pos), Tab(Pos), OrdreTri) AND
           Ordonné(Tab(2 * Pos), Tab(2 * Pos + 1), OrdreTri) THEN
        -- échanger Pos avec premier descendant, qui est le plus petit (grand)
        ÉchangerQuelc(Tab(Pos), Tab(2 * Pos));
        Pos := 2 * Pos;
      ELSIF Ordonné(Tab(2 * Pos + 1), Tab(Pos), OrdreTri) AND
            Ordonné(Tab(2 * Pos + 1), Tab(2 * Pos), OrdreTri) THEN
        -- échanger Pos avec second descendant, qui est le plus petit (grand)
        ÉchangerQuelc(Tab(Pos), Tab(2 * Pos + 1));
        Pos :=  2 * Pos + 1;
      ELSE -- la propriété est respectée
        EXIT;
      END IF;
    END LOOP;
  END Tamiser;

BEGIN
  FOR Index IN REVERSE 1..Table'Last / 2 LOOP  -- créer monceau
    Tamiser(Table, Index, Table'Last);
  END LOOP;
  Ada.Text_IO.Put(Item => "Monceau   :"); Afficher(Table);
  FOR Index IN REVERSE 2..Table'Last LOOP
    -- enlever le minimum (maximum) du début du monceau
    ÉchangerQuelc(Table(1), Table(Index));
    -- restaurer la propriété monceau
    Tamiser(Table, 1, Index-1);
    Ada.Text_IO.Put(Item => "Monceau   :"); Afficher(Table);
  END LOOP;
END TrierMonceau;

