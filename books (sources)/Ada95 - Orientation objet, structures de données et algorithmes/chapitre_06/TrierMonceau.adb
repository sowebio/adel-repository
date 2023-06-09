PROCEDURE TrierMonceau(Table: IN OUT Type_Table; OrdreTri: IN Ordre) IS
-- Trier Table en ordre ascendant ou descendant.
  
  PROCEDURE Tamiser(Tab: IN OUT Type_Table; Premier, Dernier: IN Natural) IS
  -- Tab(Premier)..Tab(Dernier) ont la propri�t� "monceau" sauf, peut-�tre,
  -- Tab(Premier). Cette proc�dure tamise Tab(Premier) jusqu'� ce que la
  -- propri�t� soit restaur�e. 
  Pos: Natural;  -- position de Tab(Premier)
  BEGIN
    Pos := Premier;
    LOOP
      EXIT WHEN 2 * Pos > Dernier; -- pas de descendant
      IF 2 * Pos = Dernier THEN	   -- Pos a un descendant � 2*Pos
        IF Ordonn�(Tab(2 * Pos), Tab(Pos), OrdreTri) THEN
          �changerQuelc(Tab(Pos), Tab(2 * Pos));
        END IF;
        EXIT;
      ELSIF   -- Pos a 2 descendants � 2*Pos et 2*Pos+1
           Ordonn�(Tab(2 * Pos), Tab(Pos), OrdreTri) AND
           Ordonn�(Tab(2 * Pos), Tab(2 * Pos + 1), OrdreTri) THEN
        -- �changer Pos avec premier descendant, qui est le plus petit (grand)
        �changerQuelc(Tab(Pos), Tab(2 * Pos));
        Pos := 2 * Pos;
      ELSIF Ordonn�(Tab(2 * Pos + 1), Tab(Pos), OrdreTri) AND
            Ordonn�(Tab(2 * Pos + 1), Tab(2 * Pos), OrdreTri) THEN
        -- �changer Pos avec second descendant, qui est le plus petit (grand)
        �changerQuelc(Tab(Pos), Tab(2 * Pos + 1));
        Pos :=  2 * Pos + 1;
      ELSE -- la propri�t� est respect�e
        EXIT;
      END IF;
    END LOOP;
  END Tamiser;

BEGIN
  FOR Index IN REVERSE 1..Table'Last / 2 LOOP  -- cr�er monceau
    Tamiser(Table, Index, Table'Last);
  END LOOP;
  Ada.Text_IO.Put(Item => "Monceau   :"); Afficher(Table);
  FOR Index IN REVERSE 2..Table'Last LOOP
    -- enlever le minimum (maximum) du d�but du monceau
    �changerQuelc(Table(1), Table(Index));
    -- restaurer la propri�t� monceau
    Tamiser(Table, 1, Index-1);
    Ada.Text_IO.Put(Item => "Monceau   :"); Afficher(Table);
  END LOOP;
END TrierMonceau;

