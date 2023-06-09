--          Copyright © 1998 Philippe J. Gabrini
GENERIC
  TYPE Type_Élément IS (<>);
PACKAGE EnsemblesDiscrets IS
-- ensembles d'entiers

TYPE TypEnsemble IS PRIVATE;

PROCEDURE Vider(Ens: IN OUT TypEnsemble);
-- Donne à Ens la valeur de l'ensemble vide.
-- Antécédent: aucun.
-- Conséquent: Ens' est l'ensemble vide.

PROCEDURE Inclure(Ens: IN OUT TypEnsemble; Élément: IN Type_Élément);
-- Inclure Élément dans Ens.
-- Antécédent: Ens existe.
-- Conséquent: Ens' inclut Élément.

PROCEDURE Exclure(Ens: IN OUT TypEnsemble; Élément: IN Type_Élément);
-- Exclure Élément de Ens.
-- Antécédent: Ens existe.
-- Conséquent: Ens' n'inclut pas Élément.

FUNCTION Membre(Élément: Type_Élément; Ens: TypEnsemble) RETURN Boolean;
-- Retourne True, si  Élément est dans Ens, False autrement.
-- Antécédent: Ens existe.
-- Conséquent: retourne True si Ens inclut Élément, False autrement.

FUNCTION "+"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble;
-- Retourne l'ensemble contenant tous les éléments de Gauche et Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments de Gauche et Droite.

FUNCTION "*"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble;
-- Retourne l'ensemble contenant tous les éléments à la fois 
-- dans Gauche et Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments communs à Gauche et Droite.

FUNCTION "-"(Gauche, Droite: TypEnsemble) RETURN TypEnsemble;
-- retourne un ensemble comprenant tous les 
-- éléments de Gauche qui ne sont pas membres de Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments de Gauche qui ne sont pas membres de Droite.

FUNCTION "<="(Gauche, Droite: TypEnsemble) RETURN Boolean;
-- Retourne True, si Droite contient tous les  éléments de Gauche,
-- False autrement.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne True si Gauche est un sous-ensemble de
--             Droite, False autrement.

FUNCTION Vide(Ens: TypEnsemble) RETURN Boolean;
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
-- Antécédent: Ens existe.
-- Conséquent: retourne True si Ens est l'ensemble vide,
--             False autrement.

FUNCTION Cardinalité(Ens: TypEnsemble) RETURN Natural;
-- Retourne le nombre d'éléments de Ens.
-- Antécédent: Ens existe.
-- Conséquent: nombre d'éléments de Ens retourné.

FUNCTION Min(Ens: TypEnsemble) RETURN Type_Élément;
-- Retourne le plus petit Élément de Ens ou la valeur maximum si vide.
-- Antécédent: Ens existe.
-- Conséquent: Élément' est le plus petit élément de l'ensemble Ens.

FUNCTION Max(Ens: TypEnsemble) RETURN Type_Élément;
-- Retourne le plus grand Élément de Ens ou la valeur minimum si vide.
-- Antécédent: Ens existe.
-- Conséquent: Élément' est l'élément maximum de l'ensemble Ens.

PRIVATE
TYPE Bit IS NEW Boolean;  -- hérite de tous les opérateurs
FOR Bit'Size USE 1;  -- redondant car Boolean ne prend qu'un bit (LRM 13.3)
TYPE Tableau IS ARRAY (Type_Élément) OF Bit;
--PRAGMA PACK(Tableau);     -- sinon un élément par octet
TYPE TypEnsemble IS RECORD
     			      Éléments: Tableau := (Type_Élément => False);
     			      Compte: Natural := 0;
                    END RECORD;
--PRAGMA PACK(TypEnsemble);  -- que gagne-t-on?
END EnsemblesDiscrets;
