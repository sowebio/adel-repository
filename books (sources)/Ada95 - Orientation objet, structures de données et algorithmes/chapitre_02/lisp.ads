-- Type de données abstrait Listes Généralisées.
-- Une liste d'objets récursive est:
--      -soit vide,
--      -soit une paire d'éléments (tête et queue).
-- La tête d'une liste est un objet de type atomique ou une sous-liste
-- et la queue d'une liste est une sous-liste (qui peut aussi être vide).
-- Une telle définition permet la définition de processus de traitement
-- récursifs pour ces listes.
-- La spécification de ces listes sera basée sur une réalisation dynamique
-- (listes chaînées) et ces structures seront contrôlées pour permettre
-- une récupération automatique de l'espace mémoire utilisé.
--
--          Copyright © 1998 Philippe J. Gabrini
--
----------------------------------------------------------------------------
WITH Ada.Finalization;
PACKAGE LISP IS

TYPE Type_Élément IS ABSTRACT TAGGED PRIVATE;
-- éléments composant la liste à compléter par l'utilisateur.

TYPE Ptr_Élément IS ACCESS Type_Élément'CLASS;
-- pointeur aux divers éléments de la liste.

TYPE Type_Liste IS NEW Ada.Finalization.Controlled WITH PRIVATE;
-- liste récursive.

Erreur_Liste: EXCEPTION;

FUNCTION Vide(Liste: Type_Liste) RETURN Boolean;
-- Vérifier si la Liste est vide.
-- Antécédent: aucun.
-- Conséquent: retourne vrai si la Liste est vide.

FUNCTION Atome(Élément: Ptr_Élément) RETURN Boolean;
-- Vérifier si l'Élément auquel on pointe est un atome.
-- Antécédent: aucun.
-- Conséquent: retourne vrai si l'Élément est un atome.

FUNCTION Liste_Vide RETURN Type_Liste;
-- Retourne la liste vide.
-- Antécédent: aucun.
-- Conséquent: retourne la liste vide.

FUNCTION Nouvel_Élément(Élément: Type_Élément'CLASS) RETURN Ptr_Élément;
-- Construire un objet de valeur Élément.
-- Antécédent: aucun.
-- Conséquent: retourne un pointeur au nouvel Élément.

FUNCTION Pointeur_Tête(Liste: Type_Liste) RETURN Ptr_Élément;
-- Retourne un pointeur à une copie du premier élément de la liste.
-- Antécédent: Liste n'est pas vide.
-- Conséquent: retourne un pointeur au premier élément de la Liste.
-- Exception:  Erreur_Liste.

FUNCTION Sous_Liste(Élément: Ptr_Élément) RETURN Type_Liste;
-- Retourne une copie de la sous-liste de l'Élément.
-- Antécédent: Élément existe et n'est pas atomique.
-- Conséquent: retourne une liste commençant au premier élément
--             de la sous-liste.
-- Exception:  Erreur_Liste.

FUNCTION Queue(Liste: IN Type_Liste) RETURN Type_Liste;
-- Retourne une copie de la queue de la Liste.
-- Antécédent: Liste n'est pas vide.
-- Conséquent: retourne la liste obtenue en enlevant la tête
--             ou la liste vide si la liste est vide.

PROCEDURE Copier_Tête(Liste: IN Type_Liste;
                      Élément : OUT Type_Élément'CLASS);
-- Copie dans Élément la valeur de la tête de Liste.
-- Antécédent: la liste existe et a au moins un élément.
-- Conséquent: retourne une copie de l'élément en tête de la Liste.
-- Exception:  Erreur_Liste.
  
FUNCTION Composition(Élément: IN Ptr_Élément;
                     Liste: IN Type_Liste) RETURN Type_Liste;
-- Construire une nouvelle liste à partir d'Élément et de Liste.
-- Antécédent: aucun.
-- Conséquent: compose une liste à partir de l'Élément (qui devient
--             la tête de liste) et de Liste.
                      
FUNCTION Composition(Première: IN Type_Liste;
                     Seconde: IN Type_Liste) RETURN Type_Liste;
-- Construire une nouvelle liste à partir de Première et de Seconde
-- ou Première est la tête de la nouvelle liste.
-- Antécédent: aucun.
-- Conséquent: compose une liste à partir de la Première liste
--             et de la Seconde.
                      
PROCEDURE Échanger(Source : IN OUT Type_Liste;
                   Cible: IN OUT Type_Liste);
-- Échanger Source et Cible.
-- Antécédent: aucun.
-- Conséquent: les deux listes sont échangées.

PROCEDURE Relier(Courant : IN OUT Ptr_Élément;
                 Suite: IN Ptr_Élément);
-- Relier liste Courant (dernier élément) à élément Suite.
-- Antécédent: Courant et Suite existent.
-- Conséquent: si non nuls les deux éléments sont reliés.

PROCEDURE Créer_Sous_Liste(Courant : IN OUT Ptr_Élément;
                           Sous_Liste: IN Ptr_Élément);
-- Relier Sous_Liste comme une sous-liste à la fin de Courant.
-- Antécédent: Courant et Sous-Liste existent.
-- Conséquent: Sous_Liste est une sous-liste de Courant.

GENERIC
  TYPE Type_Étendu IS NEW Type_Élément WITH PRIVATE;
FUNCTION Valeur_Tête(Liste: Type_Liste) RETURN Type_Étendu;
-- Retourner la valeur de la tête de Liste en type étendu utilisateur.
-- Antécédent: Liste n'est pas vide.
-- Conséquent: retourne la valeur du premier élément de la Liste.
-- Exception:  Erreur_Liste

PRIVATE
  TYPE Type_Élément IS ABSTRACT TAGGED -- devra être complété par utilisateur
    RECORD
      Suivant: Ptr_Élément;            -- liste linéaire
      Atomique: Boolean := False;      -- atome ou noeud
    END RECORD;
  
  TYPE En_Tête IS NEW Type_Élément WITH
    RECORD
      Sous_Niveau: Ptr_Élément;        -- noeud de sous-liste
    END RECORD;

  TYPE Type_Liste IS NEW Ada.Finalization.Controlled WITH
    RECORD
      Véritable: Ptr_Élément;          -- premier élément de la liste
    END RECORD;

  PROCEDURE Initialize(L: IN OUT Type_Liste);
  PROCEDURE Finalize(L: IN OUT Type_Liste);
  PROCEDURE Adjust(L: IN OUT Type_Liste);

END LISP;
