--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Finalization;
PACKAGE Cha�nesD IS

MaxCha�ne: CONSTANT Natural := 10000;
FinDeCha�ne: CONSTANT Character := Character'Val(0);

TYPE TypCha�ne IS NEW Ada.Finalization.Controlled WITH PRIVATE;
-- une cha�ne est une suite de caract�res
Erreur_Indice: EXCEPTION;

FUNCTION Longueur(Cha�ne: TypCha�ne) RETURN Natural;
-- Retourne la longueur d'une cha�ne. 
-- Ant�c�dent: aucun.
-- Cons�quent: Longueur = nombre de caract�res dans Cha�ne.

FUNCTION Cha�neVide RETURN TypCha�ne;
-- Retourne la cha�ne vide. 
-- Ant�c�dent: aucun.
-- Cons�quent: la cha�ne vide est retourn�e.

FUNCTION Tranche(Source: TypCha�ne; Bas: Positive;
                 Haut: Natural) RETURN TypCha�ne;
-- Retourne la partie de Source allant de Bas � Haut.
-- Ant�c�dent: Bas <= Longueur(Source)+1.
-- Cons�quent: Retourne Source(Bas..Haut).
-- Exception:  Erreur_Indice.
                 
PROCEDURE Supprimer(Source: IN OUT TypCha�ne; De, �: IN Natural);
-- Supprimer une partie d'une cha�ne Source.
-- Ant�c�dent: De <= � <= Longueur(Source).
-- Cons�quent: Source' = Source (1..De-1) &
-- 			          Source (�+1..Longueur(Source)).

PROCEDURE Ins�rer(Source: IN OUT TypCha�ne; Avant: IN Positive;
                  Nouvelle: IN TypCha�ne);
-- Ins�rer Nouvelle dans Source avant Avant.
-- Ant�c�dent: Avant <= Longueur(Source)+1.
-- 			   Longueur(Source) + Longueur(Nouvelle) <= MaxCha�ne
-- Cons�quent: Source' = Source(1..Avant-1) & Nouvelle & 
-- 					   Source(Avant .. Longueur(Source))
-- 			       Si Avant = Longueur(Source)+1, Nouvelle est
--             concat�n�e � Source.
            
FUNCTION "&" (Gauche: TypCha�ne; Droite: TypCha�ne) RETURN TypCha�ne;
-- Concat�ner cha�nes Gauche et Droite. 
FUNCTION "&" (Gauche: TypCha�ne; Droite: Character) RETURN TypCha�ne;
-- Concat�ner cha�ne Gauche et caract�re Droite.
FUNCTION "&" (Gauche: Character; Droite: TypCha�ne) RETURN TypCha�ne;
-- Concat�ner caract�re Gauche et cha�ne Droite.
FUNCTION "&" (Gauche: TypCha�ne; Droite: String) RETURN TypCha�ne;
-- Concat�ner cha�ne Gauche et cah�ne statique Droite.
FUNCTION "&" (Gauche: String; Droite: TypCha�ne) RETURN TypCha�ne;
-- Concat�ner cha�ne statique Gauche et cha�ne Droite.
-- Ant�c�dent: Longueur(Gauche) + Longueur(Droite) <= MaxCha�ne.
-- Cons�quent: retourne Gauche & Droite.

FUNCTION ">"(Gauche, Droite: TypCha�ne) RETURN Boolean;
-- Comparaison lexicale selon l'ensemble de caract�res.
-- Ant�c�dent: aucun.
-- Cons�quent: retourne vrai si Gauche > Droite, sinon faux.

FUNCTION "="(Gauche, Droite: TypCha�ne) RETURN Boolean;
-- Comparaison de l'�galit� de deux cha�nes.
-- Ant�c�dent: aucun.
-- Cons�quent: retourne vrai si Gauche = Droite, sinon faux.

FUNCTION �l�ment(Source: TypCha�ne; Index: Natural) RETURN Character;
-- Extrait un caract�re d'une cha�ne.
-- Ant�c�dent: Index <= Longueur(Source).
-- Cons�quent: Retourne Source(Index).
-- Exception:  Erreur_Indice.

PROCEDURE Remplacer�l�ment(Car: IN Character;
                           Source: IN OUT TypCha�ne; Index: IN Natural);
-- Range un caract�re dans une cha�ne.
-- Ant�c�dent: Index <= Longueur(Source).
-- Cons�quent: Source(Index)' = Car.

FUNCTION Position(Source, Patron: TypCha�ne) RETURN Natural;
-- Recherche sous-cha�ne Patron dans cha�ne Source.
-- Ant�c�dent: aucun.
-- Cons�quent: Retourne 0 si Patron ne se trouve pas dans Source,
-- 			       autrement retourne Pos tel que 0 < Pos <= Longueur(Source)
-- 			       et Source(Pos..Pos+Longueur(Patron)-1) = Patron 
-- 			       et Patron n'appara�t pas avant Pos.

FUNCTION �_Statique(Cha�ne: TypCha�ne) RETURN String;
-- Conversion d'une cha�ne dynamique � une cha�ne statique de type String.
-- Ant�c�dent: aucun.
-- Cons�quent: les caract�res de Cha�ne sont retourn�s sous forme de String.
  
FUNCTION �_Dynamique(Source: String) RETURN TypCha�ne;
-- Conversion du type statique String au type cha�ne dynamique. 
-- Ant�c�dent: aucun.
-- Cons�quent: les caract�res de Source sont retourn�s sous forme
--             de cha�ne dynamique.
  
PRIVATE
LongueurBloc: CONSTANT Natural := 100;
TYPE Bloc;
TYPE PointeurBloc IS ACCESS Bloc;
TYPE Bloc IS RECORD
			   SuiteCar: String(1..LongueurBloc) := (OTHERS => FinDeCha�ne);
			   Suivant: PointeurBloc := NULL;
			 END RECORD;

TYPE TypCha�ne IS NEW Ada.Finalization.Controlled WITH
    RECORD
      D�but: PointeurBloc := NULL;
      Longueur: Natural := 0;
    END RECORD;
-- Ce type de cha�ne est fourni pour les applications ayant besoin 
-- de cha�nes de longueur illimit�e.  Si la longueur de la cha�ne n'est 
-- pas un multiple de LongueurBloc caract�res elle est termin�e par le
-- caract�re FinDeCha�ne (NUL).

PROCEDURE Initialize(Cha�ne: IN OUT TypCha�ne);
PROCEDURE Finalize(Cha�ne: IN OUT TypCha�ne);
PROCEDURE Adjust(Cha�ne: IN OUT TypCha�ne);

END Cha�nesD;



