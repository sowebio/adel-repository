--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO;
GENERIC
  TYPE Ordre IS PRIVATE;
  TYPE Quelconque IS PRIVATE;
  TYPE Type_Table IS ARRAY (Integer RANGE <>) OF Quelconque;
  WITH FUNCTION Ordonné(E1, E2: Quelconque; Ord: Ordre) RETURN Boolean;
  WITH PROCEDURE Afficher(T: IN Type_Table);
  WITH PROCEDURE Sortir(Valeur: Integer; Largeur: Natural;
                        Base: Ada.Text_IO.Number_Base := 10);
PROCEDURE TrierShell(Table: IN OUT Type_Table; OrdreDeTri: IN Ordre);

