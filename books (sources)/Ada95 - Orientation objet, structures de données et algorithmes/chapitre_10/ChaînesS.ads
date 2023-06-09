--          Copyright © 1998 Philippe J. Gabrini
PACKAGE ChaînesS IS

MaxChaîne: CONSTANT Natural := 1024;
FinDeChaîne: CONSTANT Character := Character'Val(0);

TYPE TypChaîne IS PRIVATE;
-- une chaîne est une suite de caractères
Erreur_Indice: EXCEPTION;

FUNCTION Longueur(Chaîne: TypChaîne) RETURN Natural;
-- Retourne la longueur d'une chaîne. 
-- Antécédent: aucun.
-- Conséquent: Longueur = nombre de caractères dans Chaîne.

FUNCTION Tranche(Source: TypChaîne; Bas: Positive;
                 Haut: Natural) RETURN TypChaîne;
-- Retourne la partie de Source allant de Bas à Haut.
-- Antécédent: Bas <= Longueur(Source)+1.
-- Conséquent: Retourne Source(Bas..Haut).

PROCEDURE Supprimer(Source: IN OUT TypChaîne; De, À: IN Natural);
-- Supprimer une partie d'une chaîne Source.
-- Antécédent: De <= À <= Longueur(Source).
-- Conséquent: Source' = Source (1..De-1) &
-- 			       Source (À..Longueur(Source)).

PROCEDURE Insérer(Source: IN OUT TypChaîne; Avant: IN Positive;
                  Nouveau: IN TypChaîne);
-- Insérer Nouveau dans Source avant Avant.
-- Antécédent: Avant <= Longueur(Source)+1 et
-- 			       Longueur(Source) + Longueur(Nouveau) <= MaxChaîne.
-- Conséquent: Source' = Source(1..Avant-1) & Nouveau & 
-- 					   Source(Avant..Longueur(Source))
-- 			       Si Avant = Longueur(Source)+1, Nouveau
--             est concaténée à Source.
            
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
-- Conséquent: Retourne Gauche & Droite.

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
-- Conséquent: les caractères de Source sont retournés sous forme de
--             chaîne dynamique.
  
PRIVATE
SUBTYPE TypeIndice IS Integer RANGE 1..MaxChaîne;
SUBTYPE TypeLongueur  IS Integer RANGE 0..MaxChaîne;
SUBTYPE VecteurChaîne IS String(TypeIndice);  
-- un vecteur de caractères contraint

TYPE TypChaîne IS
  RECORD
    Longueur: TypeLongueur := 0; -- nombre de caractères dans la chaîne
    Caractères: VecteurChaîne;   -- les caractères de la chaîne
  END RECORD;
END ChaînesS;

