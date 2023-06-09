--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO;
GENERIC
  TYPE Ordre IS PRIVATE;
  TYPE Quelconque IS PRIVATE;
  TYPE Type_Table IS ARRAY (Integer RANGE <>) OF Quelconque;
  WITH FUNCTION Ordonn�(E1, E2: Quelconque; Ord: Ordre) RETURN Boolean;
  WITH PROCEDURE Afficher(T: IN Type_Table);
  WITH PROCEDURE �changerQuelc(E1, E2: IN OUT Quelconque);
PROCEDURE TrierMonceau(Table: IN OUT Type_Table; OrdreTri: IN Ordre);

