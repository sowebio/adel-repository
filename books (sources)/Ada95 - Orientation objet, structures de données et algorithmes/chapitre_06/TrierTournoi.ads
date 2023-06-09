--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
GENERIC
  Maximum: IN Natural := 256; -- taille du vecteur où ranger les éléments
  TYPE TypeÉlément IS PRIVATE;
  Marqueur: IN TypeÉlément;
  TYPE TypeNonTrié IS ARRAY(Integer RANGE <>) OF TypeÉlément;
  WITH FUNCTION ">"(A, B: TypeÉlément) RETURN Boolean;
  WITH PROCEDURE Sortir(Item: IN TypeÉlément; Largeur: IN Natural;
                        Base: IN Ada.Text_IO.Number_Base := 10);
PROCEDURE TrierTournoi(Table: IN OUT TypeNonTrié;
                       NombreÉléments: IN Natural);

