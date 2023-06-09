--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Strings.Bounded;
PACKAGE Chaînes IS

TailleChaîne: CONSTANT Natural := 50;

PACKAGE ChaînesDynamiques IS NEW Ada.Strings.Bounded.Generic_Bounded_Length(Max => TailleChaîne);
                    
SUBTYPE TypChaîne IS ChaînesDynamiques.Bounded_String;
SUBTYPE Intervalle IS ChaînesDynamiques.Length_Range;

FUNCTION Longueur(Source: IN TypChaîne) RETURN Intervalle
                 RENAMES ChaînesDynamiques.Length;
FUNCTION À_Dynamique(Source: IN String;
                     Drop: IN Ada.Strings.Truncation := Ada.Strings.Error)
                    RETURN TypChaîne
                 RENAMES ChaînesDynamiques.To_Bounded_String;
FUNCTION À_Statique(Source: IN TypChaîne) RETURN String
                 RENAMES ChaînesDynamiques.To_String;
FUNCTION "&"(Gauche, Droite: IN TypChaîne) RETURN TypChaîne
                 RENAMES ChaînesDynamiques."&";
FUNCTION "="(Gauche, Droite: IN TypChaîne) RETURN Boolean
                 RENAMES ChaînesDynamiques."=";
FUNCTION ">"(Gauche, Droite: IN TypChaîne) RETURN Boolean
                 RENAMES ChaînesDynamiques.">";
FUNCTION Couper(Source: IN TypChaîne; Côté: IN Ada.Strings.Trim_End) RETURN TypChaîne
                 RENAMES ChaînesDynamiques.Trim;
FUNCTION Tête(Source: IN TypChaîne; Compte: IN Natural;
              Remplis: Character := Ada.Strings.Space;
              Drop: IN Ada.Strings.Truncation := Ada.Strings.Error) RETURN TypChaîne
                 RENAMES ChaînesDynamiques.Head;
FUNCTION Queue(Source: IN TypChaîne; Compte: IN Natural;
              Remplis: Character := Ada.Strings.Space;
              Drop: IN Ada.Strings.Truncation := Ada.Strings.Error) RETURN TypChaîne
                 RENAMES ChaînesDynamiques.Tail;
FUNCTION Élément(Source: IN TypChaîne; Index: IN Positive) RETURN Character
                 RENAMES ChaînesDynamiques.Element;
PROCEDURE RemplacerÉlément(Source: IN OUT TypChaîne; Index: IN Positive;
                           Par: IN Character)
                 RENAMES ChaînesDynamiques.Replace_Element;

END Chaînes;

