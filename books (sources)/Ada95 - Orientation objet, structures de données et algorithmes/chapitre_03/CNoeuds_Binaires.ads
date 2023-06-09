--          Copyright © 1998 Philippe J. Gabrini
-- Programmeurs:     Denis Mouraux et Philippe Gabrini
-- Date:             Juin 1998
--
-- Ce module définit le type Noeud_Binaire, qui pourra, en particulier, 
-- être utilisé pour la réalisation d'arbres binaires.
GENERIC
  TYPE Type_Élément IS PRIVATE; -- le type d'élément contenu dans un noeud
PACKAGE CNoeuds_Binaires IS

TYPE Noeud_Binaire IS PRIVATE; 
-- Noeud binaire (avec descendant gauche et descendant droit).

Erreur_Noeud: EXCEPTION;
-- Levée si un antécédent n'est pas respecté, ou si une erreur reliée 
-- à la relation hiérarchique des noeuds survient lors d'une opération
-- quelconque.

TYPE Endroit IS (Ici, Parent, Gauche, Droite);
-- Pour indiquer un endroit par rapport à un noeud.

FUNCTION Nouveau_Noeud(Élément: Type_Élément) RETURN Noeud_Binaire;
-- Crée un nouveau noeud orphelin sans enfants
-- Antécédent:  Aucun.
-- Conséquent:  retourne un noeud qui contient Élément.
-- Exception:   Erreur_Noeud.

PROCEDURE Lier_Noeuds(Géniteur, Rejeton: IN Noeud_Binaire;
                      Où: IN Endroit);
-- Relie Géniteur (le parent) et Rejeton (le descendant).
-- Antécédents: Géniteur et Rejeton existent,
--              Où n'est ni Ici, ni Parent,
--              Géniteur n'a pas de descendant à l'endroit désigné par Où,
--              Rejeton n'a pas de parent.
-- Conséquent:  Où = Gauche ==> Rejeton est le descendant gauche de Géniteur.
--              Où = Droite ==> Rejeton est le descendant droit de Géniteur.
-- Remarque:    Où = Parent n'est pas permis car on ne saurait pas à quel.
--              branche (Gauche ou Droite) de Rejeton relier Géniteur.
-- Exception:   Erreur_Noeud.

PROCEDURE Détacher_Noeud(Noeud: IN Noeud_Binaire; Où: IN Endroit);
-- Détache le noeud du ou des noeuds voisins (selon Où).
-- Antécédent:  Noeud existe.
-- Conséquent:  Où = Parent ==> Noeud est détaché de son parent.
--              Où = Gauche ==> Noeud est détaché de son descendant gauche.
--              Où = Droite ==> Noeud est détaché de son descendant droit.
--              Où = Ici    ==> Noeud est détaché de tous ses voisins.
-- Exception:   Erreur_Noeud.

PROCEDURE Modifier_Noeud(Noeud: IN Noeud_Binaire;
                         Élément: IN Type_Élément);
-- Remplace la valeur de l'élément de Noeud par Élément.
-- Antécédent:  Noeud existe.
-- Conséquent:  l'élément de Noeud a pris la valeur Élément.
-- Exception:   Erreur_Noeud.

PROCEDURE Détruire_Noeud(Noeud: IN OUT Noeud_Binaire);
-- Détruit Noeud.
-- Antécédent:  Noeud existe et n'est attaché à aucun autre noeud.
-- Conséquent:  Noeud n'existe plus.
-- Exception:   Erreur_Noeud.

PROCEDURE Détruire_Noeud_Et_Descendants(Noeud: IN OUT Noeud_Binaire);
-- Détruit le noeud désigné par Noeud ainsi que tous ses descendants.
-- Antécédent:  Aucun.
-- Conséquent:  Noeud est détaché de son parent, et est détruit, de
--              même que tous ses descendants.

FUNCTION Noeud_Existe(Noeud: Noeud_Binaire) RETURN Boolean;
-- Vérifie si Noeud existe.
-- Antécédent:  Aucun.
-- Conséquent:  Retourne True si Noeud existe et False sinon.

FUNCTION Noeud_Existe(Noeud: Noeud_Binaire;
                      Où: Endroit) RETURN Boolean;
-- Vérifie si le noeud indiqué par Noeud et Où existe.
-- Antécédent:  Noeud existe.
-- Conséquent:  Retourne True si un noeud existe à l'endroit indiqué
--              par Où (par rapport à Noeud), et False sinon.
  
FUNCTION Valeur_Noeud(Noeud: Noeud_Binaire) RETURN Type_Élément;
-- Retourne l'élément situé dans le Noeud.
-- Antécédent:  Noeud existe.
-- Conséquent:  Retourne la valeur de l'élément contenu dans Noeud.
-- Exception:   Erreur_Noeud.

FUNCTION Noeud_Voisin(Noeud: Noeud_Binaire;
                      Où: Endroit) RETURN Noeud_Binaire;
-- Retourne le voisin de Noeud spécifié par Où.
-- Antécédent:  Noeud existe.
-- Conséquent:  Où = Parent ==> Retourne le parent de Noeud.
--              Où = Gauche ==> Retourne le fils de gauche de Noeud.
--              Où = Droite ==> Retourne le fils de droite de Noeud.
--              Où = Ici    ==> Retourne Noeud.
-- Remarques:   Le noeud retourné peut ne pas exister (Sauf si Où = Ici).
-- Exception:   Erreur_Noeud.

FUNCTION Parenté(Noeud: Noeud_Binaire;
                 Voisin : Noeud_Binaire) RETURN Endroit;
-- Retourne le lien de parenté de Voisin par rapport à Noeud.
-- Antécédent:  Noeud et Voisin existent et ont un lien de parenté direct.
-- Conséquent:  Voisin = Noeud                        ==> On retourne Ici.
--              Voisin est le parent de Noeud         ==> On retourne Parent.
--              Voisin est le fils de gauche de Noeud ==> On retourne Gauche.
--              Voisin est le fils de droite de Noeud ==> On retourne Droite.
-- Exception:   Erreur_Noeud.

PRIVATE
  TYPE Contenu_Noeud IS
    RECORD
      Élément: Type_Élément; -- L'élément proprement dit
      Parent: Noeud_Binaire; -- Pointeur vers le parent du noeud
      Gauche: Noeud_Binaire; -- Pointeur vers le descendant gauche
      Droite: Noeud_Binaire; -- Pointeur vers le descendant droite
    END RECORD;
  TYPE Noeud_Binaire IS ACCESS Contenu_Noeud;
       
END CNoeuds_Binaires;
