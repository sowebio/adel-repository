----------------------------------------------------------------------------
--   Fichier:  PJG-Listes.ads
--   Objectif: Spécification des listes chaînées.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
GENERIC
  TYPE Type_Valeur IS PRIVATE;
PACKAGE PJG.Listes IS

TYPE Type_Liste IS TAGGED LIMITED PRIVATE;
TYPE Élément_Liste IS PRIVATE;

FUNCTION Taille(Liste: Type_Liste) RETURN Natural;
FUNCTION Premier(Liste: Type_Liste) RETURN Élément_Liste;
FUNCTION Dernier(Liste: Type_Liste) RETURN Élément_Liste;
PROCEDURE Vider_Liste(Liste: IN OUT Type_Liste);
FUNCTION Successeur(Élément : Élément_Liste) RETURN Élément_Liste;
FUNCTION Prédécesseur(Élément : Élément_Liste) RETURN Élément_Liste;
FUNCTION Valeur(Élément : Élément_Liste) RETURN Type_Valeur;
PROCEDURE Insérer(Élément: IN Élément_Liste;
                  Item: IN Type_Valeur);
PROCEDURE Supprimer(Élément: IN Élément_Liste);

Erreur_Liste: EXCEPTION;

PRIVATE
  TYPE Composant;
  TYPE Pointeur_Composant IS ACCESS Composant;

  TYPE Composant IS
    RECORD
      Item: Type_Valeur;
      Suivant: Pointeur_Composant;
      Précédent: Pointeur_Composant;
    END RECORD;

  TYPE Tête_de_Liste IS
    RECORD
      Premier: Pointeur_Composant;
      Dernier: Pointeur_Composant;
      Compteur: Natural := 0;
    END RECORD;
  TYPE Pointeur_Liste IS ACCESS Tête_de_Liste;

  TYPE Type_Liste IS TAGGED LIMITED
    RECORD
      Liste: Pointeur_Liste := NEW Tête_de_Liste;
    END RECORD;

  TYPE Élément_Liste is
    RECORD
      Liste: Pointeur_Liste;
      Courant: Pointeur_Composant;
    END RECORD;

END PJG.Listes;
