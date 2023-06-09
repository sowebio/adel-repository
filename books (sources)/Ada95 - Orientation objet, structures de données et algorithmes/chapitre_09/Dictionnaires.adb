--          Copyright � 1998 Philippe J. Gabrini
PACKAGE BODY Dictionnaires IS

PROCEDURE Ins�rer(Dict: IN OUT Dictionnaire; �l�ment: IN Type_Chose) IS
-- Ins�rer �l�ment dans dictionnaire.
BEGIN
  EnsemblesD.Inclure(Dict, �l�ment);
END Ins�rer;

FUNCTION Pr�sent(Dict: Dictionnaire; �l�ment: Type_Chose) RETURN Boolean IS
-- Retourner True si �l�ment est dans dictionnaire.
BEGIN
  RETURN EnsemblesD.Membre(�l�ment, Dict);
END Pr�sent;

PROCEDURE Supprimer(Dict: IN OUT Dictionnaire; �l�ment: IN OUT Type_Chose) IS
-- Supprimer �l�ment du dictionnaire en ramenant sa valeur.
BEGIN
  EnsemblesD.Exclure(Dict, �l�ment);
END Supprimer;

END Dictionnaires;
