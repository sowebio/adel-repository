--          Copyright � 1998 Philippe J. Gabrini
GENERIC
  TYPE Type_�l�ment IS (<>);
PACKAGE EnsemblesDiscrets IS
-- ensembles d'entiers

TYPE TypEnsemble IS PRIVATE;

PROCEDURE Vider(Ens: IN OUT TypEnsemble);
-- Donne � Ens la valeur de l'ensemble vide.
-- Ant�c�dent: aucun.
-- Cons�quent: Ens' est l'ensemble vide.

PROCEDURE Inclure(Ens: IN OUT TypEnsemble; �l�ment: IN Type_�l�ment);
-- Inclure �l�ment dans Ens.
-- Ant�c�dent: Ens existe.
-- Cons�quent: Ens' inclut �l�ment.

PROCEDURE Exclure(Ens: IN OUT TypEnsemble; �l�ment: IN Type_�l�ment);
-- Exclure �l�ment de Ens.
-- Ant�c�dent: Ens existe.
-- Cons�quent: Ens' n'inclut pas �l�ment.

FUNCTION Membre(�l�ment: Type_�l�ment; Ens: TypEnsemble) RETURN Boolean;
-- Retourne True, si  �l�ment est dans Ens, False autrement.
-- Ant�c�dent: Ens existe.
-- Cons�quent: retourne True si Ens inclut �l�ment, False autrement.

FUNCTION "+"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble;
-- Retourne l'ensemble contenant tous les �l�ments de Gauche et Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments de Gauche et Droite.

FUNCTION "*"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble;
-- Retourne l'ensemble contenant tous les �l�ments � la fois 
-- dans Gauche et Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments communs � Gauche et Droite.

FUNCTION "-"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble;
-- retourne un ensemble comprenant tous les 
-- �l�ments de Gauche qui ne sont pas membres de Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments de Gauche qui ne sont pas membres de Droite.

FUNCTION "<="(Gauche, Droite: TypEnsemble) RETURN Boolean;
-- Retourne True, si Droite contient tous les  �l�ments de Gauche,
-- False autrement.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne True si Gauche est un sous-ensemble de
--             Droite, False autrement.

FUNCTION Vide(Ens: TypEnsemble) RETURN Boolean;
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
-- Ant�c�dent: Ens existe.
-- Cons�quent: retourne True si Ens est l'ensemble vide,
--             False autrement.

FUNCTION Cardinalit�(Ens: TypEnsemble) RETURN Natural;
-- Retourne le nombre d'�l�ments de Ens.
-- Ant�c�dent: Ens existe.
-- Cons�quent: nombre d'�l�ments de Ens retourn�.

FUNCTION Min(Ens: TypEnsemble) RETURN Type_�l�ment;
-- Retourne le plus petit �l�ment de Ens ou la valeur maximum si vide.
-- Ant�c�dent: Ens existe.
-- Cons�quent: �l�ment' est le plus petit �l�ment de l'ensemble Ens.

FUNCTION Max(Ens: TypEnsemble) RETURN Type_�l�ment;
-- Retourne le plus grand �l�ment de Ens ou la valeur minimum si vide.
-- Ant�c�dent: Ens existe.
-- Cons�quent: �l�ment' est l'�l�ment maximum de l'ensemble Ens.

PRIVATE
TYPE Bit IS NEW Boolean;  -- h�rite de tous les op�rateurs
FOR Bit'Size USE 1;  -- redondant car Boolean ne prend qu'un bit (LRM 13.3)
TYPE Tableau IS ARRAY (Type_�l�ment) OF Bit;
--PRAGMA PACK(Tableau);     -- sinon un �l�ment par octet
TYPE TypEnsemble IS RECORD
     			      �l�ments: Tableau := (Type_�l�ment => False);
     			      Compte: Natural := 0;
                    END RECORD;
--PRAGMA PACK(TypEnsemble);  -- que gagne-t-on?
END EnsemblesDiscrets;
