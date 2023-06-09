--          Copyright � 1998 Philippe J. Gabrini
GENERIC
  TYPE Type_Clef IS (<>);
  WITH FUNCTION Clef(�lt: Type_�l�ment) RETURN Type_Clef;
PACKAGE Ensembles.Statiques IS
PRAGMA PURE(Ensembles.Statiques);

TYPE Ensemble IS NEW Ensemble_Abstrait WITH PRIVATE;

PROCEDURE Vider(Ens: IN OUT Ensemble);
-- Donne � Ens la valeur de l'ensemble vide.
-- Ant�c�dent: aucun.
-- Cons�quent: Ens' est l'ensemble vide.

PROCEDURE Affecter(Destination: OUT Ensemble;
                   Source: IN Ensemble);
-- Donne � Destination une copie de la valeur de Source.
-- Ant�c�dent: aucun.
-- Cons�quent: Destination' a une valeur identique � Source.

PROCEDURE Inclure(Ens: IN OUT Ensemble; �l�ment: IN Type_�l�ment);
-- Inclure �l�ment dans Ens.
-- Ant�c�dent: Ens existe.
-- Cons�quent: Ens' inclut �l�ment.

PROCEDURE Exclure(Ens: IN OUT Ensemble; �l�ment: IN OUT Type_�l�ment);
-- Exclure �l�ment de Ens et en retourner la valeur.
-- Ant�c�dent: Ens existe.
-- Cons�quent: Ens' n'inclut pas �l�ment.

FUNCTION Membre(�l�ment: IN Type_�l�ment; Ens: IN Ensemble) RETURN Boolean;
-- Retourne True, si  �l�ment est dans Ens, False autrement.
-- Ant�c�dent: Ens existe.
-- Cons�quent: retourne True si Ens inclut �l�ment,
--             False autrement.

PROCEDURE FaireUnion(Gauche, Droite: IN Ensemble; R�sultat: OUT Ensemble);
-- Place dans R�sultat l'ensemble contenant tous les �l�ments de Gauche
-- et Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments de Gauche et Droite.

PROCEDURE FaireIntersection(Gauche, Droite: IN Ensemble;
                            R�sultat: OUT Ensemble);
-- Place dans R�sultat l'ensemble contenant tous les �l�ments � la fois 
-- dans Gauche et Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments communs � Gauche et Droite.

PROCEDURE FaireDiff�rence(Gauche, Droite: IN Ensemble;
                          R�sultat: OUT Ensemble);
-- Place dans R�sultat un ensemble comprenant tous les �l�ments de 
-- Gauche qui ne sont pas membres de Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments de Gauche qui ne sont pas membres de Droite.

FUNCTION "="(Gauche, Droite: Ensemble) RETURN Boolean;
-- Retourne True, si les deux ensembles contiennent les m�mes �l�ments,
-- False autrement.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne True si les ensembles Gauche et Droite sont �gaux,
--             False autrement.

FUNCTION "<="(Gauche, Droite: Ensemble) RETURN Boolean;
-- Retourne True, si Droite contient tous les  �l�ments de Gauche,
-- False autrement.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne True si Gauche est un sous-ensemble de
--             Droite, False autrement.

FUNCTION Vide(Ens: Ensemble) RETURN Boolean;
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
-- Ant�c�dent: Ens existe.
-- Cons�quent: retourne True si Ens est l'ensemble vide,
--             False autrement.

FUNCTION Cardinalit�(Ens: Ensemble) RETURN Natural;
-- Retourne le nombre d'�l�ments de Ens.
-- Ant�c�dent: Ens existe.
-- Cons�quent: nombre d'�l�ments de Ens retourn�.

PRIVATE
TYPE Article IS RECORD
                  Pr�sent: Boolean := False;
                  Valeur: Type_�l�ment;
                END RECORD;
TYPE Tableau IS ARRAY (Type_Clef) OF Article;
TYPE Ensemble IS NEW Ensemble_Abstrait WITH RECORD
     			                                    �l�ments: Tableau;
     			                                    Compte: Natural := 0;
     		                                    END RECORD;
END Ensembles.Statiques;
