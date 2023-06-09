--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Strings.Bounded;
PACKAGE Cha�nes IS

TailleCha�ne: CONSTANT Natural := 50;

PACKAGE Cha�nesDynamiques IS NEW Ada.Strings.Bounded.Generic_Bounded_Length(Max => TailleCha�ne);
                    
SUBTYPE TypCha�ne IS Cha�nesDynamiques.Bounded_String;
SUBTYPE Intervalle IS Cha�nesDynamiques.Length_Range;

FUNCTION Longueur(Source: IN TypCha�ne) RETURN Intervalle
                 RENAMES Cha�nesDynamiques.Length;
FUNCTION �_Dynamique(Source: IN String;
                     Drop: IN Ada.Strings.Truncation := Ada.Strings.Error)
                    RETURN TypCha�ne
                 RENAMES Cha�nesDynamiques.To_Bounded_String;
FUNCTION �_Statique(Source: IN TypCha�ne) RETURN String
                 RENAMES Cha�nesDynamiques.To_String;
FUNCTION "&"(Gauche, Droite: IN TypCha�ne) RETURN TypCha�ne
                 RENAMES Cha�nesDynamiques."&";
FUNCTION "="(Gauche, Droite: IN TypCha�ne) RETURN Boolean
                 RENAMES Cha�nesDynamiques."=";
FUNCTION ">"(Gauche, Droite: IN TypCha�ne) RETURN Boolean
                 RENAMES Cha�nesDynamiques.">";
FUNCTION Couper(Source: IN TypCha�ne; C�t�: IN Ada.Strings.Trim_End) RETURN TypCha�ne
                 RENAMES Cha�nesDynamiques.Trim;
FUNCTION T�te(Source: IN TypCha�ne; Compte: IN Natural;
              Remplis: Character := Ada.Strings.Space;
              Drop: IN Ada.Strings.Truncation := Ada.Strings.Error) RETURN TypCha�ne
                 RENAMES Cha�nesDynamiques.Head;
FUNCTION Queue(Source: IN TypCha�ne; Compte: IN Natural;
              Remplis: Character := Ada.Strings.Space;
              Drop: IN Ada.Strings.Truncation := Ada.Strings.Error) RETURN TypCha�ne
                 RENAMES Cha�nesDynamiques.Tail;
FUNCTION �l�ment(Source: IN TypCha�ne; Index: IN Positive) RETURN Character
                 RENAMES Cha�nesDynamiques.Element;
PROCEDURE Remplacer�l�ment(Source: IN OUT TypCha�ne; Index: IN Positive;
                           Par: IN Character)
                 RENAMES Cha�nesDynamiques.Replace_Element;

END Cha�nes;

