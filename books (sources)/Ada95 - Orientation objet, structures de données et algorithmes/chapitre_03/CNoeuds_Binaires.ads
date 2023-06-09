--          Copyright � 1998 Philippe J. Gabrini
-- Programmeurs:     Denis Mouraux et Philippe Gabrini
-- Date:             Juin 1998
--
-- Ce module d�finit le type Noeud_Binaire, qui pourra, en particulier, 
-- �tre utilis� pour la r�alisation d'arbres binaires.
GENERIC
  TYPE Type_�l�ment IS PRIVATE; -- le type d'�l�ment contenu dans un noeud
PACKAGE CNoeuds_Binaires IS

TYPE Noeud_Binaire IS PRIVATE; 
-- Noeud binaire (avec descendant gauche et descendant droit).

Erreur_Noeud: EXCEPTION;
-- Lev�e si un ant�c�dent n'est pas respect�, ou si une erreur reli�e 
-- � la relation hi�rarchique des noeuds survient lors d'une op�ration
-- quelconque.

TYPE Endroit IS (Ici, Parent, Gauche, Droite);
-- Pour indiquer un endroit par rapport � un noeud.

FUNCTION Nouveau_Noeud(�l�ment: Type_�l�ment) RETURN Noeud_Binaire;
-- Cr�e un nouveau noeud orphelin sans enfants
-- Ant�c�dent:  Aucun.
-- Cons�quent:  retourne un noeud qui contient �l�ment.
-- Exception:   Erreur_Noeud.

PROCEDURE Lier_Noeuds(G�niteur, Rejeton: IN Noeud_Binaire;
                      O�: IN Endroit);
-- Relie G�niteur (le parent) et Rejeton (le descendant).
-- Ant�c�dents: G�niteur et Rejeton existent,
--              O� n'est ni Ici, ni Parent,
--              G�niteur n'a pas de descendant � l'endroit d�sign� par O�,
--              Rejeton n'a pas de parent.
-- Cons�quent:  O� = Gauche ==> Rejeton est le descendant gauche de G�niteur.
--              O� = Droite ==> Rejeton est le descendant droit de G�niteur.
-- Remarque:    O� = Parent n'est pas permis car on ne saurait pas � quel.
--              branche (Gauche ou Droite) de Rejeton relier G�niteur.
-- Exception:   Erreur_Noeud.

PROCEDURE D�tacher_Noeud(Noeud: IN Noeud_Binaire; O�: IN Endroit);
-- D�tache le noeud du ou des noeuds voisins (selon O�).
-- Ant�c�dent:  Noeud existe.
-- Cons�quent:  O� = Parent ==> Noeud est d�tach� de son parent.
--              O� = Gauche ==> Noeud est d�tach� de son descendant gauche.
--              O� = Droite ==> Noeud est d�tach� de son descendant droit.
--              O� = Ici    ==> Noeud est d�tach� de tous ses voisins.
-- Exception:   Erreur_Noeud.

PROCEDURE Modifier_Noeud(Noeud: IN Noeud_Binaire;
                         �l�ment: IN Type_�l�ment);
-- Remplace la valeur de l'�l�ment de Noeud par �l�ment.
-- Ant�c�dent:  Noeud existe.
-- Cons�quent:  l'�l�ment de Noeud a pris la valeur �l�ment.
-- Exception:   Erreur_Noeud.

PROCEDURE D�truire_Noeud(Noeud: IN OUT Noeud_Binaire);
-- D�truit Noeud.
-- Ant�c�dent:  Noeud existe et n'est attach� � aucun autre noeud.
-- Cons�quent:  Noeud n'existe plus.
-- Exception:   Erreur_Noeud.

PROCEDURE D�truire_Noeud_Et_Descendants(Noeud: IN OUT Noeud_Binaire);
-- D�truit le noeud d�sign� par Noeud ainsi que tous ses descendants.
-- Ant�c�dent:  Aucun.
-- Cons�quent:  Noeud est d�tach� de son parent, et est d�truit, de
--              m�me que tous ses descendants.

FUNCTION Noeud_Existe(Noeud: Noeud_Binaire) RETURN Boolean;
-- V�rifie si Noeud existe.
-- Ant�c�dent:  Aucun.
-- Cons�quent:  Retourne True si Noeud existe et False sinon.

FUNCTION Noeud_Existe(Noeud: Noeud_Binaire;
                      O�: Endroit) RETURN Boolean;
-- V�rifie si le noeud indiqu� par Noeud et O� existe.
-- Ant�c�dent:  Noeud existe.
-- Cons�quent:  Retourne True si un noeud existe � l'endroit indiqu�
--              par O� (par rapport � Noeud), et False sinon.
  
FUNCTION Valeur_Noeud(Noeud: Noeud_Binaire) RETURN Type_�l�ment;
-- Retourne l'�l�ment situ� dans le Noeud.
-- Ant�c�dent:  Noeud existe.
-- Cons�quent:  Retourne la valeur de l'�l�ment contenu dans Noeud.
-- Exception:   Erreur_Noeud.

FUNCTION Noeud_Voisin(Noeud: Noeud_Binaire;
                      O�: Endroit) RETURN Noeud_Binaire;
-- Retourne le voisin de Noeud sp�cifi� par O�.
-- Ant�c�dent:  Noeud existe.
-- Cons�quent:  O� = Parent ==> Retourne le parent de Noeud.
--              O� = Gauche ==> Retourne le fils de gauche de Noeud.
--              O� = Droite ==> Retourne le fils de droite de Noeud.
--              O� = Ici    ==> Retourne Noeud.
-- Remarques:   Le noeud retourn� peut ne pas exister (Sauf si O� = Ici).
-- Exception:   Erreur_Noeud.

FUNCTION Parent�(Noeud: Noeud_Binaire;
                 Voisin : Noeud_Binaire) RETURN Endroit;
-- Retourne le lien de parent� de Voisin par rapport � Noeud.
-- Ant�c�dent:  Noeud et Voisin existent et ont un lien de parent� direct.
-- Cons�quent:  Voisin = Noeud                        ==> On retourne Ici.
--              Voisin est le parent de Noeud         ==> On retourne Parent.
--              Voisin est le fils de gauche de Noeud ==> On retourne Gauche.
--              Voisin est le fils de droite de Noeud ==> On retourne Droite.
-- Exception:   Erreur_Noeud.

PRIVATE
  TYPE Contenu_Noeud IS
    RECORD
      �l�ment: Type_�l�ment; -- L'�l�ment proprement dit
      Parent: Noeud_Binaire; -- Pointeur vers le parent du noeud
      Gauche: Noeud_Binaire; -- Pointeur vers le descendant gauche
      Droite: Noeud_Binaire; -- Pointeur vers le descendant droite
    END RECORD;
  TYPE Noeud_Binaire IS ACCESS Contenu_Noeud;
       
END CNoeuds_Binaires;
