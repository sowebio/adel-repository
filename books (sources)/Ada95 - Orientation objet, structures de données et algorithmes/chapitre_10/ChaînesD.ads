--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Finalization;
PACKAGE ChaînesD IS

MaxChaîne: CONSTANT Natural := 10000;
FinDeChaîne: CONSTANT Character := Character'Val(0);

TYPE TypChaîne IS NEW Ada.Finalization.Controlled WITH PRIVATE;
-- une chaîne est une suite de caractères
Erreur_Indice: EXCEPTION;

FUNCTION Longueur(Chaîne: TypChaîne) RETURN Natural;
-- Retourne la longueur d'une chaîne. 
-- Antécédent: aucun.
-- Conséquent: Longueur = nombre de caractères dans Chaîne.

FUNCTION ChaîneVide RETURN TypChaîne;
-- Retourne la chaîne vide. 
-- Antécédent: aucun.
-- Conséquent: la chaîne vide est retournée.

FUNCTION Tranche(Source: TypChaîne; Bas: Positive;
                 Haut: Natural) RETURN TypChaîne;
-- Retourne la partie de Source allant de Bas à Haut.
-- Antécédent: Bas <= Longueur(Source)+1.
-- Conséquent: Retourne Source(Bas..Haut).
-- Exception:  Erreur_Indice.
                 
PROCEDURE Supprimer(Source: IN OUT TypChaîne; De, À: IN Natural);
-- Supprimer une partie d'une chaîne Source.
-- Antécédent: De <= À <= Longueur(Source).
-- Conséquent: Source' = Source (1..De-1) &
-- 			          Source (À+1..Longueur(Source)).

PROCEDURE Insérer(Source: IN OUT TypChaîne; Avant: IN Positive;
                  Nouvelle: IN TypChaîne);
-- Insérer Nouvelle dans Source avant Avant.
-- Antécédent: Avant <= Longueur(Source)+1.
-- 			   Longueur(Source) + Longueur(Nouvelle) <= MaxChaîne
-- Conséquent: Source' = Source(1..Avant-1) & Nouvelle & 
-- 					   Source(Avant .. Longueur(Source))
-- 			       Si Avant = Longueur(Source)+1, Nouvelle est
--             concaténée à Source.
            
FUNCTION "&" (Gauche: TypChaîne; Droite: TypChaîne) RETURN TypChaîne;
-- Concaténer chaînes Gauche et Droite. 
FUNCTION "&" (Gauche: TypChaîne; Droite: Character) RETURN TypChaîne;
-- Concaténer chaîne Gauche et caractère Droite.
FUNCTION "&" (Gauche: Character; Droite: TypChaîne) RETURN TypChaîne;
-- Concaténer caractère Gauche et chaîne Droite.
FUNCTION "&" (Gauche: TypChaîne; Droite: String) RETURN TypChaîne;
-- Concaténer chaîne Gauche et cahîne statique Droite.
FUNCTION "&" (Gauche: String; Droite: TypChaîne) RETURN TypChaîne;
-- Concaténer chaîne statique Gauche et chaîne Droite.
-- Antécédent: Longueur(Gauche) + Longueur(Droite) <= MaxChaîne.
-- Conséquent: retourne Gauche & Droite.

FUNCTION ">"(Gauche, Droite: TypChaîne) RETURN Boolean;
-- Comparaison lexicale selon l'ensemble de caractères.
-- Antécédent: aucun.
-- Conséquent: retourne vrai si Gauche > Droite, sinon faux.

FUNCTION "="(Gauche, Droite: TypChaîne) RETURN Boolean;
-- Comparaison de l'égalité de deux chaînes.
-- Antécédent: aucun.
-- Conséquent: retourne vrai si Gauche = Droite, sinon faux.

FUNCTION Élément(Source: TypChaîne; Index: Natural) RETURN Character;
-- Extrait un caractère d'une chaîne.
-- Antécédent: Index <= Longueur(Source).
-- Conséquent: Retourne Source(Index).
-- Exception:  Erreur_Indice.

PROCEDURE RemplacerÉlément(Car: IN Character;
                           Source: IN OUT TypChaîne; Index: IN Natural);
-- Range un caractère dans une chaîne.
-- Antécédent: Index <= Longueur(Source).
-- Conséquent: Source(Index)' = Car.

FUNCTION Position(Source, Patron: TypChaîne) RETURN Natural;
-- Recherche sous-chaîne Patron dans chaîne Source.
-- Antécédent: aucun.
-- Conséquent: Retourne 0 si Patron ne se trouve pas dans Source,
-- 			       autrement retourne Pos tel que 0 < Pos <= Longueur(Source)
-- 			       et Source(Pos..Pos+Longueur(Patron)-1) = Patron 
-- 			       et Patron n'apparaît pas avant Pos.

FUNCTION À_Statique(Chaîne: TypChaîne) RETURN String;
-- Conversion d'une chaîne dynamique à une chaîne statique de type String.
-- Antécédent: aucun.
-- Conséquent: les caractères de Chaîne sont retournés sous forme de String.
  
FUNCTION À_Dynamique(Source: String) RETURN TypChaîne;
-- Conversion du type statique String au type chaîne dynamique. 
-- Antécédent: aucun.
-- Conséquent: les caractères de Source sont retournés sous forme
--             de chaîne dynamique.
  
PRIVATE
LongueurBloc: CONSTANT Natural := 100;
TYPE Bloc;
TYPE PointeurBloc IS ACCESS Bloc;
TYPE Bloc IS RECORD
			   SuiteCar: String(1..LongueurBloc) := (OTHERS => FinDeChaîne);
			   Suivant: PointeurBloc := NULL;
			 END RECORD;

TYPE TypChaîne IS NEW Ada.Finalization.Controlled WITH
    RECORD
      Début: PointeurBloc := NULL;
      Longueur: Natural := 0;
    END RECORD;
-- Ce type de chaîne est fourni pour les applications ayant besoin 
-- de chaînes de longueur illimitée.  Si la longueur de la chaîne n'est 
-- pas un multiple de LongueurBloc caractères elle est terminée par le
-- caractère FinDeChaîne (NUL).

PROCEDURE Initialize(Chaîne: IN OUT TypChaîne);
PROCEDURE Finalize(Chaîne: IN OUT TypChaîne);
PROCEDURE Adjust(Chaîne: IN OUT TypChaîne);

END ChaînesD;



