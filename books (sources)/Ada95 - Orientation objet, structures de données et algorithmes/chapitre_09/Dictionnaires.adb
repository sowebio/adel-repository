--          Copyright © 1998 Philippe J. Gabrini
PACKAGE BODY Dictionnaires IS

PROCEDURE Insérer(Dict: IN OUT Dictionnaire; Élément: IN Type_Chose) IS
-- Insérer Élément dans dictionnaire.
BEGIN
  EnsemblesD.Inclure(Dict, Élément);
END Insérer;

FUNCTION Présent(Dict: Dictionnaire; Élément: Type_Chose) RETURN Boolean IS
-- Retourner True si Élément est dans dictionnaire.
BEGIN
  RETURN EnsemblesD.Membre(Élément, Dict);
END Présent;

PROCEDURE Supprimer(Dict: IN OUT Dictionnaire; Élément: IN OUT Type_Chose) IS
-- Supprimer Élément du dictionnaire en ramenant sa valeur.
BEGIN
  EnsemblesD.Exclure(Dict, Élément);
END Supprimer;

END Dictionnaires;
