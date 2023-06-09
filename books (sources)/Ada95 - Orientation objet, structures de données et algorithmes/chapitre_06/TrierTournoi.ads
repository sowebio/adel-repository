--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
GENERIC
  Maximum: IN Natural := 256; -- taille du vecteur o� ranger les �l�ments
  TYPE Type�l�ment IS PRIVATE;
  Marqueur: IN Type�l�ment;
  TYPE TypeNonTri� IS ARRAY(Integer RANGE <>) OF Type�l�ment;
  WITH FUNCTION ">"(A, B: Type�l�ment) RETURN Boolean;
  WITH PROCEDURE Sortir(Item: IN Type�l�ment; Largeur: IN Natural;
                        Base: IN Ada.Text_IO.Number_Base := 10);
PROCEDURE TrierTournoi(Table: IN OUT TypeNonTri�;
                       Nombre�l�ments: IN Natural);

