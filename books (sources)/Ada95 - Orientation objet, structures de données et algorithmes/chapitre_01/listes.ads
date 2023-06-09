----------------------------------------------------------------------------
--   Fichier:  Listes.ads
--   Objectif: Sp�cification des listes lin�aires cha�n�es g�n�riques.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
GENERIC
  TYPE Type_Valeur IS PRIVATE;      -- �l�ments de la liste
PACKAGE Listes IS

TYPE Type_Liste IS LIMITED PRIVATE; -- liste lin�aire
TYPE �l�ment_Liste is PRIVATE;      -- rep�re d'un des �l�ments d'une liste

Erreur_Liste: EXCEPTION;

FUNCTION Taille(Liste: Type_Liste) RETURN Natural;
-- Ant�c�dent: aucun.
-- Cons�quent: retourne le nombre d'�l�ments dans la Liste.

FUNCTION Premier(Liste: Type_Liste) RETURN �l�ment_Liste;
-- Ant�c�dent: aucun.
-- Cons�quent: retourne un rep�re au premier �l�ment de la Liste.

FUNCTION Dernier(Liste: Type_Liste) RETURN �l�ment_Liste;
-- Ant�c�dent: aucun.
-- Cons�quent: retourne un rep�re au dernier �l�ment de la Liste.

PROCEDURE Vider_Liste(Liste: IN OUT Type_Liste);
-- Ant�c�dent: aucun.
-- Cons�quent: la Liste est vide.

FUNCTION Successeur(Composant: �l�ment_Liste) RETURN �l�ment_Liste;
-- Ant�c�dent: la liste existe et Composant rep�re un de ses �l�ments.
-- Cons�quent: retourne un rep�re au successeur de Composant dans la liste;
--             le successeur du dernier �l�ment est un �l�ment inexistant.
-- Exception:  Erreur_Liste.

FUNCTION Pr�d�cesseur(Composant: �l�ment_Liste) RETURN �l�ment_Liste;
-- Ant�c�dent: la liste existe et Composant ne rep�re pas le premier �l�ment.
-- Cons�quent: retourne un rep�re au pr�d�cesseur de Composant dans la liste;
--             le pr�d�cesseur d'un �l�ment inexistant est le dernier �l�ment.
-- Exception:  Erreur_Liste.

FUNCTION Valeur(Composant: �l�ment_Liste) RETURN Type_Valeur;
-- Ant�c�dent: la liste existe ainsi que l'�l�ment rep�r� par Composant.
-- Cons�quent: retourne la valeur rep�r�e par Composant dans la liste.
-- Exception:  Erreur_Liste.

PROCEDURE Ins�rer(Composant: IN �l�ment_Liste;
                  Item: IN Type_Valeur);
-- Ant�c�dent: la liste dont fait partie l'�l�ment rep�r� par Composant existe.
-- Cons�quent: l'�l�ment Item est ins�r� dans la liste avant celui rep�r� par
--             Composant; si Composant rep�re un �l�ment inexistant on ins�re
--             en fin de liste.
-- Exception:  Erreur_Liste.

PROCEDURE Supprimer(Composant: IN �l�ment_Liste);
-- Ant�c�dent: la liste dont fait partie l'�l�ment rep�r� par Composant
--             ainsi que l'�l�ment existent.
-- Cons�quent: l'�l�ment rep�r� par Composant est supprim� de la liste.
-- Exception:  Erreur_Liste.

PRIVATE
  TYPE Un_Composant;         
  TYPE Pointeur_Composant IS ACCESS Un_Composant;
  TYPE Un_Composant IS RECORD        -- pour les �l�ments de la liste
                         Item: Type_Valeur;
                         Suivant: Pointeur_Composant;   -- pointeur avant
                         Pr�c�dent: Pointeur_Composant; -- pointeur arri�re
                       END RECORD;

  TYPE T�te_de_Liste IS RECORD
                          Premier: Pointeur_Composant;
                          Dernier: Pointeur_Composant;
                          Compteur: Natural := 0;  -- nombre d'�l�ments
                        END RECORD;
  TYPE Pointeur_Liste IS ACCESS T�te_de_Liste;

  TYPE Type_Liste IS RECORD                -- liste lin�aire sym�trique
                       T�te: Pointeur_Liste := NEW T�te_de_Liste;
                     END RECORD;

  TYPE �l�ment_Liste IS RECORD -- rep�re d'un des composants d'une liste
                          Liste: Pointeur_Liste;       -- la liste
                          �l�ment: Pointeur_Composant; -- l'�l�ment
                        END RECORD;

END Listes;
