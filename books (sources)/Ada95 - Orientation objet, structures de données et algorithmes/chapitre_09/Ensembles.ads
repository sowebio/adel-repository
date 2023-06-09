--          Copyright � 1998 Philippe J. Gabrini
GENERIC
  Taille_Ensemble_Abstrait: Positive := 50;
  TYPE Type_�l�ment IS PRIVATE;  
  WITH FUNCTION Comparaison(E1, E2: Type_�l�ment) RETURN Integer;
  WITH PROCEDURE Afficher�l�ment(E: IN Type_�l�ment);
  WITH PROCEDURE Afficherclef(E: IN Type_�l�ment);
PACKAGE Ensembles IS
-- Ce module d�finit le type abstrait Ensemble_Abstrait.  Ce type servira � d�river
-- d'autres types d'ensembles pour diff�rentes m�thodes de r�alisation.
PRAGMA Pure(Ensembles);

TYPE Ensemble_Abstrait IS ABSTRACT TAGGED LIMITED NULL RECORD;
-- Le type d'ensemble, d�rivable et extensible pour cr�er d'autres
-- types d'ensembles de la m�me classe.

PROCEDURE Vider(Ens: IN OUT Ensemble_Abstrait) IS ABSTRACT;
-- Donne � Ens la valeur de l'ensemble vide.
-- Ant�c�dent: aucun.
-- Cons�quent: Ens' est l'ensemble vide.

PROCEDURE Affecter(Destination: OUT Ensemble_Abstrait;
                   Source: IN Ensemble_Abstrait) IS ABSTRACT;
-- Donne comme valeur � Destination une copie de la valeur de Source
-- (l'affectation n'est pas disponible aux types limit�s).
-- Ant�c�dent: aucun.
-- Cons�quent: Destination' a une valeur identique � Source.

PROCEDURE Inclure(Ens: IN OUT Ensemble_Abstrait;
                  �l�ment: IN Type_�l�ment) IS ABSTRACT;
-- Inclure �l�ment dans Ens.
-- Ant�c�dent: Ens existe.
-- Cons�quent: Ens' inclut �l�ment.

PROCEDURE Exclure(Ens: IN OUT Ensemble_Abstrait;
                  �l�ment: IN OUT Type_�l�ment) IS ABSTRACT;
-- Exclure �l�ment de Ens et en retourner la valeur.
-- Ant�c�dent: Ens existe
-- Cons�quent: Ens' n'inclut pas �l�ment

FUNCTION Membre(�l�ment: Type_�l�ment;
                Ens: Ensemble_Abstrait) RETURN Boolean IS ABSTRACT;
-- Retourne True, si  �l�ment est dans Ens, False autrement.
-- Ant�c�dent: Ens existe.
-- Cons�quent: retourne True si Ens inclut �l�ment,
--             False autrement

PROCEDURE FaireUnion(Gauche, Droite: IN Ensemble_Abstrait;
                     R�sultat: OUT Ensemble_Abstrait) IS ABSTRACT;
-- Place dans R�sultat l'ensemble contenant tous les �l�ments de Gauche
-- et Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments de Gauche et Droite.

PROCEDURE FaireIntersection(Gauche, Droite: IN Ensemble_Abstrait;
                            R�sultat: OUT Ensemble_Abstrait) IS ABSTRACT;
-- Place dans R�sultat l'ensemble contenant tous les �l�ments � la fois 
-- dans Gauche et Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments communs � Gauche et Droite.

PROCEDURE FaireDiff�rence(Gauche, Droite: IN Ensemble_Abstrait;
                          R�sultat: OUT Ensemble_Abstrait) IS ABSTRACT;
-- Place dans R�sultat un ensemble comprenant tous les �l�ments de 
-- Gauche qui ne sont pas membres de Droite.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne un ensemble comprenant tous les 
--             �l�ments de Gauche qui ne sont pas membres de Droite.

FUNCTION "="(Gauche, Droite: Ensemble_Abstrait)
            RETURN Boolean IS ABSTRACT;
-- Retourne True, si les deux ensembles contiennent les m�mes �l�ments,
-- False autrement.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne True si les ensembles Gauche et Droite sont �gaux,
--             False autrement.

FUNCTION "<="(Gauche, Droite: Ensemble_Abstrait) RETURN Boolean IS ABSTRACT;
-- Retourne True, si Droite contient tous les  �l�ments de Gauche,
-- False autrement.
-- Ant�c�dent: Gauche et Droite existent.
-- Cons�quent: retourne True si Gauche est un sous-ensemble de
--             Droite, False autrement.

FUNCTION Vide(Ens: Ensemble_Abstrait) RETURN Boolean IS ABSTRACT;
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
-- Ant�c�dent: Ens existe.
-- Cons�quent: retourne True si Ens est l'ensemble vide,
--             False autrement.

FUNCTION Cardinalit�(Ens: Ensemble_Abstrait) RETURN Natural IS ABSTRACT;
-- Retourne le nombre d'�l�ments de Ens.
-- Ant�c�dent: Ens existe.
-- Cons�quent: nombre d'�l�ments de Ens retourn�.

END Ensembles;
