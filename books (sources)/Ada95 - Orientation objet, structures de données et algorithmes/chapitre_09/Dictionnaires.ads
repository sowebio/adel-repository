--          Copyright � 1998 Philippe J. Gabrini
WITH Ensembles.Statiques;
GENERIC
  TYPE Type_Chose IS PRIVATE;
  TYPE Type_Mot IS (<>);
  WITH FUNCTION Compar�s(E1, E2: Type_Chose) RETURN Integer;
  WITH PROCEDURE Afficher(E: IN Type_Chose);
  WITH PROCEDURE MontrerClef(E: IN Type_Chose);
  WITH FUNCTION Convertie(�lt: Type_Chose) RETURN Type_Mot;
PACKAGE Dictionnaires IS

PACKAGE EnsemblesBase IS NEW Ensembles(Type_�l�ment => Type_Chose,
                                       Comparaison => Compar�s,
                                       Afficher�l�ment => Afficher,
                                       AfficherClef => MontrerClef);
PACKAGE EnsemblesD IS NEW EnsemblesBase.Statiques(Type_Clef => Type_Mot,
                                                  Clef => Convertie);
SUBTYPE Dictionnaire IS EnsemblesD.Ensemble;

PROCEDURE Ins�rer(Dict: IN OUT Dictionnaire; �l�ment: IN Type_Chose);
-- Ins�rer �l�ment dans dictionnaire.

FUNCTION Pr�sent(Dict: Dictionnaire; �l�ment: Type_Chose) RETURN Boolean;
-- Retourne True si �l�ment est dans dictionnaire.

PROCEDURE Supprimer(Dict: IN OUT Dictionnaire; �l�ment: IN OUT Type_Chose);
-- Supprimer �l�ment du dictionnaire en ramenant sa valeur.

END Dictionnaires;
