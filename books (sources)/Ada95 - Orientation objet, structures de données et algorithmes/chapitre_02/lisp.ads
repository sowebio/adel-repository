-- Type de donn�es abstrait Listes G�n�ralis�es.
-- Une liste d'objets r�cursive est:
--      -soit vide,
--      -soit une paire d'�l�ments (t�te et queue).
-- La t�te d'une liste est un objet de type atomique ou une sous-liste
-- et la queue d'une liste est une sous-liste (qui peut aussi �tre vide).
-- Une telle d�finition permet la d�finition de processus de traitement
-- r�cursifs pour ces listes.
-- La sp�cification de ces listes sera bas�e sur une r�alisation dynamique
-- (listes cha�n�es) et ces structures seront contr�l�es pour permettre
-- une r�cup�ration automatique de l'espace m�moire utilis�.
--
--          Copyright � 1998 Philippe J. Gabrini
--
----------------------------------------------------------------------------
WITH Ada.Finalization;
PACKAGE LISP IS

TYPE Type_�l�ment IS ABSTRACT TAGGED PRIVATE;
-- �l�ments composant la liste � compl�ter par l'utilisateur.

TYPE Ptr_�l�ment IS ACCESS Type_�l�ment'CLASS;
-- pointeur aux divers �l�ments de la liste.

TYPE Type_Liste IS NEW Ada.Finalization.Controlled WITH PRIVATE;
-- liste r�cursive.

Erreur_Liste: EXCEPTION;

FUNCTION Vide(Liste: Type_Liste) RETURN Boolean;
-- V�rifier si la Liste est vide.
-- Ant�c�dent: aucun.
-- Cons�quent: retourne vrai si la Liste est vide.

FUNCTION Atome(�l�ment: Ptr_�l�ment) RETURN Boolean;
-- V�rifier si l'�l�ment auquel on pointe est un atome.
-- Ant�c�dent: aucun.
-- Cons�quent: retourne vrai si l'�l�ment est un atome.

FUNCTION Liste_Vide RETURN Type_Liste;
-- Retourne la liste vide.
-- Ant�c�dent: aucun.
-- Cons�quent: retourne la liste vide.

FUNCTION Nouvel_�l�ment(�l�ment: Type_�l�ment'CLASS) RETURN Ptr_�l�ment;
-- Construire un objet de valeur �l�ment.
-- Ant�c�dent: aucun.
-- Cons�quent: retourne un pointeur au nouvel �l�ment.

FUNCTION Pointeur_T�te(Liste: Type_Liste) RETURN Ptr_�l�ment;
-- Retourne un pointeur � une copie du premier �l�ment de la liste.
-- Ant�c�dent: Liste n'est pas vide.
-- Cons�quent: retourne un pointeur au premier �l�ment de la Liste.
-- Exception:  Erreur_Liste.

FUNCTION Sous_Liste(�l�ment: Ptr_�l�ment) RETURN Type_Liste;
-- Retourne une copie de la sous-liste de l'�l�ment.
-- Ant�c�dent: �l�ment existe et n'est pas atomique.
-- Cons�quent: retourne une liste commen�ant au premier �l�ment
--             de la sous-liste.
-- Exception:  Erreur_Liste.

FUNCTION Queue(Liste: IN Type_Liste) RETURN Type_Liste;
-- Retourne une copie de la queue de la Liste.
-- Ant�c�dent: Liste n'est pas vide.
-- Cons�quent: retourne la liste obtenue en enlevant la t�te
--             ou la liste vide si la liste est vide.

PROCEDURE Copier_T�te(Liste: IN Type_Liste;
                      �l�ment : OUT Type_�l�ment'CLASS);
-- Copie dans �l�ment la valeur de la t�te de Liste.
-- Ant�c�dent: la liste existe et a au moins un �l�ment.
-- Cons�quent: retourne une copie de l'�l�ment en t�te de la Liste.
-- Exception:  Erreur_Liste.
  
FUNCTION Composition(�l�ment: IN Ptr_�l�ment;
                     Liste: IN Type_Liste) RETURN Type_Liste;
-- Construire une nouvelle liste � partir d'�l�ment et de Liste.
-- Ant�c�dent: aucun.
-- Cons�quent: compose une liste � partir de l'�l�ment (qui devient
--             la t�te de liste) et de Liste.
                      
FUNCTION Composition(Premi�re: IN Type_Liste;
                     Seconde: IN Type_Liste) RETURN Type_Liste;
-- Construire une nouvelle liste � partir de Premi�re et de Seconde
-- ou Premi�re est la t�te de la nouvelle liste.
-- Ant�c�dent: aucun.
-- Cons�quent: compose une liste � partir de la Premi�re liste
--             et de la Seconde.
                      
PROCEDURE �changer(Source : IN OUT Type_Liste;
                   Cible: IN OUT Type_Liste);
-- �changer Source et Cible.
-- Ant�c�dent: aucun.
-- Cons�quent: les deux listes sont �chang�es.

PROCEDURE Relier(Courant : IN OUT Ptr_�l�ment;
                 Suite: IN Ptr_�l�ment);
-- Relier liste Courant (dernier �l�ment) � �l�ment Suite.
-- Ant�c�dent: Courant et Suite existent.
-- Cons�quent: si non nuls les deux �l�ments sont reli�s.

PROCEDURE Cr�er_Sous_Liste(Courant : IN OUT Ptr_�l�ment;
                           Sous_Liste: IN Ptr_�l�ment);
-- Relier Sous_Liste comme une sous-liste � la fin de Courant.
-- Ant�c�dent: Courant et Sous-Liste existent.
-- Cons�quent: Sous_Liste est une sous-liste de Courant.

GENERIC
  TYPE Type_�tendu IS NEW Type_�l�ment WITH PRIVATE;
FUNCTION Valeur_T�te(Liste: Type_Liste) RETURN Type_�tendu;
-- Retourner la valeur de la t�te de Liste en type �tendu utilisateur.
-- Ant�c�dent: Liste n'est pas vide.
-- Cons�quent: retourne la valeur du premier �l�ment de la Liste.
-- Exception:  Erreur_Liste

PRIVATE
  TYPE Type_�l�ment IS ABSTRACT TAGGED -- devra �tre compl�t� par utilisateur
    RECORD
      Suivant: Ptr_�l�ment;            -- liste lin�aire
      Atomique: Boolean := False;      -- atome ou noeud
    END RECORD;
  
  TYPE En_T�te IS NEW Type_�l�ment WITH
    RECORD
      Sous_Niveau: Ptr_�l�ment;        -- noeud de sous-liste
    END RECORD;

  TYPE Type_Liste IS NEW Ada.Finalization.Controlled WITH
    RECORD
      V�ritable: Ptr_�l�ment;          -- premier �l�ment de la liste
    END RECORD;

  PROCEDURE Initialize(L: IN OUT Type_Liste);
  PROCEDURE Finalize(L: IN OUT Type_Liste);
  PROCEDURE Adjust(L: IN OUT Type_Liste);

END LISP;
