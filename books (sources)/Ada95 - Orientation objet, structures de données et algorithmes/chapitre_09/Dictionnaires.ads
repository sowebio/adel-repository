--          Copyright © 1998 Philippe J. Gabrini
WITH Ensembles.Statiques;
GENERIC
  TYPE Type_Chose IS PRIVATE;
  TYPE Type_Mot IS (<>);
  WITH FUNCTION Comparés(E1, E2: Type_Chose) RETURN Integer;
  WITH PROCEDURE Afficher(E: IN Type_Chose);
  WITH PROCEDURE MontrerClef(E: IN Type_Chose);
  WITH FUNCTION Convertie(Élt: Type_Chose) RETURN Type_Mot;
PACKAGE Dictionnaires IS

PACKAGE EnsemblesBase IS NEW Ensembles(Type_Élément => Type_Chose,
                                       Comparaison => Comparés,
                                       AfficherÉlément => Afficher,
                                       AfficherClef => MontrerClef);
PACKAGE EnsemblesD IS NEW EnsemblesBase.Statiques(Type_Clef => Type_Mot,
                                                  Clef => Convertie);
SUBTYPE Dictionnaire IS EnsemblesD.Ensemble;

PROCEDURE Insérer(Dict: IN OUT Dictionnaire; Élément: IN Type_Chose);
-- Insérer Élément dans dictionnaire.

FUNCTION Présent(Dict: Dictionnaire; Élément: Type_Chose) RETURN Boolean;
-- Retourne True si Élément est dans dictionnaire.

PROCEDURE Supprimer(Dict: IN OUT Dictionnaire; Élément: IN OUT Type_Chose);
-- Supprimer Élément du dictionnaire en ramenant sa valeur.

END Dictionnaires;
