--          Copyright © 1997 Philippe J. Gabrini
GENERIC
TYPE TypeÉlément IS PRIVATE;
Max: IN Integer := 100;  -- maximum prévu pour le nombre d'éléments dans la file

PACKAGE TDAFile IS

TYPE File IS PRIVATE;

PROCEDURE Vider(Q: IN OUT File);
-- Vider la file Q
-- Antécédent: aucun
-- Conséquent: Q' est vide

PROCEDURE Enfiler(Q: IN OUT File; Item: IN TypeÉlément);
-- Insère élément Item à la fin de la file Q
-- Antécédent: aucun
-- Conséquent: Item est le dernier élément de Q' 

PROCEDURE Défiler(Q: IN OUT File; Item: OUT TypeÉlément);
-- Supprime le premier élément de la file Q et le retourne dans Item
-- Antécédent: Q n'est pas vide
-- Conséquent: Item est une copie du premier élément de Q et 
--                Q' ne contient plus Item 

FUNCTION Longueur(Q: IN File) RETURN Natural;
-- Retourne le nombre d'éléments dans la file Q
-- Antécédent: aucun
-- Conséquent: on retourne le nombre d'éléments dans la file

FUNCTION TêteDeFile(Q: IN File) RETURN TypeÉlément;
-- Retourne la valeur du premier élément de la file Q
-- Antécédent: aucun
-- Conséquent: Q' = Q et retourne le premier élément de la file 

FileVide: EXCEPTION;
FilePleine:  EXCEPTION;

PRIVATE
SUBTYPE Index IS Natural RANGE 0..Max;
TYPE Table IS ARRAY (1..Max) OF TypeÉlément;
TYPE File IS RECORD
               Tête, Queue, Nombre: Index;
               Données: Table;
             END RECORD;
-- File vide: Queue = 0, Tête = Max, Nombre = 0
-- File pleine: Queue = Tête, Nombre = Max
-- File normale: Queue indique le dernier élément
--               Tete indique l'ancien premier élément

END TDAFile;
