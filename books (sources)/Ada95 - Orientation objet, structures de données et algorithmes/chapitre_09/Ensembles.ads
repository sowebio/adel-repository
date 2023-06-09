--          Copyright © 1998 Philippe J. Gabrini
GENERIC
  Taille_Ensemble_Abstrait: Positive := 50;
  TYPE Type_Élément IS PRIVATE;  
  WITH FUNCTION Comparaison(E1, E2: Type_Élément) RETURN Integer;
  WITH PROCEDURE Afficherélément(E: IN Type_Élément);
  WITH PROCEDURE Afficherclef(E: IN Type_Élément);
PACKAGE Ensembles IS
-- Ce module définit le type abstrait Ensemble_Abstrait.  Ce type servira à dériver
-- d'autres types d'ensembles pour différentes méthodes de réalisation.
PRAGMA Pure(Ensembles);

TYPE Ensemble_Abstrait IS ABSTRACT TAGGED LIMITED NULL RECORD;
-- Le type d'ensemble, dérivable et extensible pour créer d'autres
-- types d'ensembles de la même classe.

PROCEDURE Vider(Ens: IN OUT Ensemble_Abstrait) IS ABSTRACT;
-- Donne à Ens la valeur de l'ensemble vide.
-- Antécédent: aucun.
-- Conséquent: Ens' est l'ensemble vide.

PROCEDURE Affecter(Destination: OUT Ensemble_Abstrait;
                   Source: IN Ensemble_Abstrait) IS ABSTRACT;
-- Donne comme valeur à Destination une copie de la valeur de Source
-- (l'affectation n'est pas disponible aux types limités).
-- Antécédent: aucun.
-- Conséquent: Destination' a une valeur identique à Source.

PROCEDURE Inclure(Ens: IN OUT Ensemble_Abstrait;
                  Élément: IN Type_Élément) IS ABSTRACT;
-- Inclure Élément dans Ens.
-- Antécédent: Ens existe.
-- Conséquent: Ens' inclut Élément.

PROCEDURE Exclure(Ens: IN OUT Ensemble_Abstrait;
                  Élément: IN OUT Type_Élément) IS ABSTRACT;
-- Exclure Élément de Ens et en retourner la valeur.
-- Antécédent: Ens existe
-- Conséquent: Ens' n'inclut pas Élément

FUNCTION Membre(Élément: Type_Élément;
                Ens: Ensemble_Abstrait) RETURN Boolean IS ABSTRACT;
-- Retourne True, si  Élément est dans Ens, False autrement.
-- Antécédent: Ens existe.
-- Conséquent: retourne True si Ens inclut Élément,
--             False autrement

PROCEDURE FaireUnion(Gauche, Droite: IN Ensemble_Abstrait;
                     Résultat: OUT Ensemble_Abstrait) IS ABSTRACT;
-- Place dans Résultat l'ensemble contenant tous les éléments de Gauche
-- et Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments de Gauche et Droite.

PROCEDURE FaireIntersection(Gauche, Droite: IN Ensemble_Abstrait;
                            Résultat: OUT Ensemble_Abstrait) IS ABSTRACT;
-- Place dans Résultat l'ensemble contenant tous les éléments à la fois 
-- dans Gauche et Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments communs à Gauche et Droite.

PROCEDURE FaireDifférence(Gauche, Droite: IN Ensemble_Abstrait;
                          Résultat: OUT Ensemble_Abstrait) IS ABSTRACT;
-- Place dans Résultat un ensemble comprenant tous les éléments de 
-- Gauche qui ne sont pas membres de Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments de Gauche qui ne sont pas membres de Droite.

FUNCTION "="(Gauche, Droite: Ensemble_Abstrait)
            RETURN Boolean IS ABSTRACT;
-- Retourne True, si les deux ensembles contiennent les mêmes éléments,
-- False autrement.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne True si les ensembles Gauche et Droite sont égaux,
--             False autrement.

FUNCTION "<="(Gauche, Droite: Ensemble_Abstrait) RETURN Boolean IS ABSTRACT;
-- Retourne True, si Droite contient tous les  éléments de Gauche,
-- False autrement.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne True si Gauche est un sous-ensemble de
--             Droite, False autrement.

FUNCTION Vide(Ens: Ensemble_Abstrait) RETURN Boolean IS ABSTRACT;
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
-- Antécédent: Ens existe.
-- Conséquent: retourne True si Ens est l'ensemble vide,
--             False autrement.

FUNCTION Cardinalité(Ens: Ensemble_Abstrait) RETURN Natural IS ABSTRACT;
-- Retourne le nombre d'éléments de Ens.
-- Antécédent: Ens existe.
-- Conséquent: nombre d'éléments de Ens retourné.

END Ensembles;
