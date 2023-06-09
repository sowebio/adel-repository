--          Copyright � 1997 Philippe J. Gabrini
GENERIC
TYPE Type�l�ment IS PRIVATE;
Max: IN Integer := 100;  -- maximum pr�vu pour le nombre d'�l�ments dans la file

PACKAGE TDAFile IS

TYPE File IS PRIVATE;

PROCEDURE Vider(Q: IN OUT File);
-- Vider la file Q
-- Ant�c�dent: aucun
-- Cons�quent: Q' est vide

PROCEDURE Enfiler(Q: IN OUT File; Item: IN Type�l�ment);
-- Ins�re �l�ment Item � la fin de la file Q
-- Ant�c�dent: aucun
-- Cons�quent: Item est le dernier �l�ment de Q' 

PROCEDURE D�filer(Q: IN OUT File; Item: OUT Type�l�ment);
-- Supprime le premier �l�ment de la file Q et le retourne dans Item
-- Ant�c�dent: Q n'est pas vide
-- Cons�quent: Item est une copie du premier �l�ment de Q et 
--                Q' ne contient plus Item 

FUNCTION Longueur(Q: IN File) RETURN Natural;
-- Retourne le nombre d'�l�ments dans la file Q
-- Ant�c�dent: aucun
-- Cons�quent: on retourne le nombre d'�l�ments dans la file

FUNCTION T�teDeFile(Q: IN File) RETURN Type�l�ment;
-- Retourne la valeur du premier �l�ment de la file Q
-- Ant�c�dent: aucun
-- Cons�quent: Q' = Q et retourne le premier �l�ment de la file 

FileVide: EXCEPTION;
FilePleine:  EXCEPTION;

PRIVATE
SUBTYPE Index IS Natural RANGE 0..Max;
TYPE Table IS ARRAY (1..Max) OF Type�l�ment;
TYPE File IS RECORD
               T�te, Queue, Nombre: Index;
               Donn�es: Table;
             END RECORD;
-- File vide: Queue = 0, T�te = Max, Nombre = 0
-- File pleine: Queue = T�te, Nombre = Max
-- File normale: Queue indique le dernier �l�ment
--               Tete indique l'ancien premier �l�ment

END TDAFile;
