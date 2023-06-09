----------------------------------------------------------------------------
--   Objectif: Sp�cification des listes cha�n�es
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
GENERIC
  TYPE Type_Valeur IS PRIVATE;
PACKAGE PJG.Listes IS

TYPE Type_Liste IS LIMITED PRIVATE;
TYPE �l�ment_Liste is PRIVATE;

Erreur_Liste: EXCEPTION;

FUNCTION Taille(Liste: Type_Liste) RETURN Natural;
FUNCTION Premier(Liste: Type_Liste) RETURN �l�ment_Liste;
FUNCTION Dernier(Liste: Type_Liste) RETURN �l�ment_Liste;
PROCEDURE Vider_Liste(Liste: IN OUT Type_Liste);
FUNCTION Successeur(�l�ment : �l�ment_Liste) RETURN �l�ment_Liste;
FUNCTION Pr�d�cesseur(�l�ment : �l�ment_Liste) RETURN �l�ment_Liste;
FUNCTION Valeur(�l�ment : �l�ment_Liste) RETURN Type_Valeur;
PROCEDURE Ins�rer(�l�ment: IN �l�ment_Liste;
                  Item: IN Type_Valeur);
PROCEDURE Supprimer(�l�ment: IN �l�ment_Liste);

PRIVATE
  TYPE Composant;
  TYPE Pointeur_Composant IS ACCESS Composant;

  TYPE Composant is
    RECORD
      Item: Type_Valeur;
      Suivant: Pointeur_Composant;
      Pr�c�dent: Pointeur_Composant;
    END RECORD;

  TYPE T�te_de_Liste is
    RECORD
      Premier: Pointeur_Composant;
      Dernier: Pointeur_Composant;
      Compteur: Natural := 0;
    END RECORD;
  TYPE Pointeur_Liste IS ACCESS T�te_de_Liste;

  TYPE Type_Liste is
    RECORD
      Liste: Pointeur_Liste := NEW T�te_de_Liste;
    END RECORD;

  TYPE �l�ment_Liste is
    RECORD
      Liste: Pointeur_Liste;
      Courant: Pointeur_Composant;
    END RECORD;

END PJG.Listes;
