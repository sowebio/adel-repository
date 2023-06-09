----------------------------------------------------------------------------
--   Fichier:  Listes.ads
--   Objectif: Spécification des listes linéaires chaînées génériques.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
GENERIC
  TYPE Type_Valeur IS PRIVATE;      -- éléments de la liste
PACKAGE Listes IS

TYPE Type_Liste IS LIMITED PRIVATE; -- liste linéaire
TYPE Élément_Liste is PRIVATE;      -- repère d'un des éléments d'une liste

Erreur_Liste: EXCEPTION;

FUNCTION Taille(Liste: Type_Liste) RETURN Natural;
-- Antécédent: aucun.
-- Conséquent: retourne le nombre d'éléments dans la Liste.

FUNCTION Premier(Liste: Type_Liste) RETURN Élément_Liste;
-- Antécédent: aucun.
-- Conséquent: retourne un repère au premier élément de la Liste.

FUNCTION Dernier(Liste: Type_Liste) RETURN Élément_Liste;
-- Antécédent: aucun.
-- Conséquent: retourne un repère au dernier élément de la Liste.

PROCEDURE Vider_Liste(Liste: IN OUT Type_Liste);
-- Antécédent: aucun.
-- Conséquent: la Liste est vide.

FUNCTION Successeur(Composant: Élément_Liste) RETURN Élément_Liste;
-- Antécédent: la liste existe et Composant repère un de ses éléments.
-- Conséquent: retourne un repère au successeur de Composant dans la liste;
--             le successeur du dernier élément est un élément inexistant.
-- Exception:  Erreur_Liste.

FUNCTION Prédécesseur(Composant: Élément_Liste) RETURN Élément_Liste;
-- Antécédent: la liste existe et Composant ne repère pas le premier élément.
-- Conséquent: retourne un repère au prédécesseur de Composant dans la liste;
--             le prédécesseur d'un élément inexistant est le dernier élément.
-- Exception:  Erreur_Liste.

FUNCTION Valeur(Composant: Élément_Liste) RETURN Type_Valeur;
-- Antécédent: la liste existe ainsi que l'élément repéré par Composant.
-- Conséquent: retourne la valeur repérée par Composant dans la liste.
-- Exception:  Erreur_Liste.

PROCEDURE Insérer(Composant: IN Élément_Liste;
                  Item: IN Type_Valeur);
-- Antécédent: la liste dont fait partie l'élément repéré par Composant existe.
-- Conséquent: l'élément Item est inséré dans la liste avant celui repéré par
--             Composant; si Composant repère un élément inexistant on insère
--             en fin de liste.
-- Exception:  Erreur_Liste.

PROCEDURE Supprimer(Composant: IN Élément_Liste);
-- Antécédent: la liste dont fait partie l'élément repéré par Composant
--             ainsi que l'élément existent.
-- Conséquent: l'élément repéré par Composant est supprimé de la liste.
-- Exception:  Erreur_Liste.

PRIVATE
  TYPE Un_Composant;         
  TYPE Pointeur_Composant IS ACCESS Un_Composant;
  TYPE Un_Composant IS RECORD        -- pour les éléments de la liste
                         Item: Type_Valeur;
                         Suivant: Pointeur_Composant;   -- pointeur avant
                         Précédent: Pointeur_Composant; -- pointeur arrière
                       END RECORD;

  TYPE Tête_de_Liste IS RECORD
                          Premier: Pointeur_Composant;
                          Dernier: Pointeur_Composant;
                          Compteur: Natural := 0;  -- nombre d'éléments
                        END RECORD;
  TYPE Pointeur_Liste IS ACCESS Tête_de_Liste;

  TYPE Type_Liste IS RECORD                -- liste linéaire symétrique
                       Tête: Pointeur_Liste := NEW Tête_de_Liste;
                     END RECORD;

  TYPE Élément_Liste IS RECORD -- repère d'un des composants d'une liste
                          Liste: Pointeur_Liste;       -- la liste
                          Élément: Pointeur_Composant; -- l'élément
                        END RECORD;

END Listes;
