--          Copyright © 1998 Philippe J. Gabrini
GENERIC
  TYPE Type_Clef IS (<>);
  WITH FUNCTION Clef(Élt: Type_Élément) RETURN Type_Clef;
PACKAGE Ensembles.Dynamiques IS

TYPE Ensemble IS NEW Ensemble_Abstrait WITH PRIVATE;

PROCEDURE Vider(Ens: IN OUT Ensemble);
-- Donne à Ens la valeur de l'ensemble vide.
-- Antécédent: aucun.
-- Conséquent: Ens' est l'ensemble vide.

PROCEDURE Affecter(Destination: OUT Ensemble;
                   Source: IN Ensemble);
-- Donne à Destination une copie de la valeur de Source.
-- Antécédent: aucun.
-- Conséquent: Destination' a une valeur identique à Source.

PROCEDURE Inclure(Ens: IN OUT Ensemble; Élt: IN Type_Élément);
-- Inclure Élt dans Ens.
-- Antécédent: Ens existe.
-- Conséquent: Ens' inclut Élt.

PROCEDURE Exclure(Ens: IN OUT Ensemble; Élt: IN OUT Type_Élément);
-- Exclure Élt de Ens et en retourne la valeur.
-- Antécédent: Ens existe.
-- Conséquent: Ens' n'inclut pas Élt.

FUNCTION Membre(Élt: Type_Élément; Ens: Ensemble) RETURN Boolean;
-- Retourne True, si  Élt est dans Ens, False autrement.
-- Antécédent: Ens existe.
-- Conséquent: retourne True si Ens inclut Élt,
--             False autrement.

PROCEDURE FaireUnion(Gauche, Droite: IN Ensemble;
                     Résultat: OUT Ensemble);
-- Place dans Résultat l'ensemble contenant tous les éléments de
-- Gauche et Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments de Gauche et Droite.

PROCEDURE FaireIntersection(Gauche, Droite: IN Ensemble;
                            Résultat: OUT Ensemble);
-- Place dans Résultat l'ensemble contenant tous les éléments 
-- à la fois dans Gauche et Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments communs à Gauche et Droite.

PROCEDURE FaireDifférence(Gauche, Droite: IN Ensemble;
                          Résultat: OUT Ensemble);
-- Place dans Résultat un ensemble comprenant tous les éléments de
-- Gauche qui ne sont pas membres de Droite.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne un ensemble comprenant tous les 
--             éléments de Gauche qui ne sont pas membres de Droite.

FUNCTION "="(Gauche, Droite: Ensemble) RETURN Boolean;
-- Retourne True, si les deux ensembles contiennent les mêmes éléments,
-- False autrement.
-- Antécédent: Gauche et Droite existent
-- Conséquent: retourne True si les ensembles Gauche et Droite sont égaux,
--             False autrement.

FUNCTION "<="(Gauche, Droite: Ensemble) RETURN Boolean;
-- Retourne True, si Droite contient tous les  éléments de Gauche,
-- False autrement.
-- Antécédent: Gauche et Droite existent.
-- Conséquent: retourne True si Gauche est un sous-ensemble de
--             Droite, False autrement.

FUNCTION Vide(Ens: Ensemble) RETURN Boolean;
-- Vide retourne True si la valeur de Ens est l'ensemble vide.
-- Antécédent: Ens existe.
-- Conséquent: retourne True si Ens est l'ensemble vide,
--             False autrement.

FUNCTION Cardinalité(Ens: Ensemble) RETURN Natural;
-- Retourne le nombre d'éléments de Ens.
-- Antécédent: Ens existe.
-- Conséquent: nombre d'éléments de Ens retourné.

PRIVATE
TYPE Élément;
TYPE PtrÉlément IS ACCESS Élément;
TYPE Élément IS  RECORD
     			         ValeurÉlément: Type_Élément;
     			         Suivant: PtrÉlément := NULL;
     		         END RECORD;
TYPE Ensemble IS NEW Ensemble_Abstrait WITH RECORD
     			                     Éléments: PtrÉlément := NULL;
     			                     Compte: Natural := 0;
     		                     END RECORD;
END Ensembles.Dynamiques;
