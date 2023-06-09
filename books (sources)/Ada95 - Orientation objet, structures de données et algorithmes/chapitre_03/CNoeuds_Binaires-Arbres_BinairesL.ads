--          Copyright � 1998 Philippe J. Gabrini
-- Programmeur:     Denis Mouraux et Philippe Gabrini
-- Date:            Juin 1998
--
-- Ce module d�finit le type Arbre_Binaire, qui permet de d�clarer des
-- arbres binaires.  L'utilisateur devra instancier ArbresBinaires de 
-- la fa�on suivante:
--  PACKAGE Noeuds IS NEW CNoeuds_Binaires(Type_�l�ment => MonType);
--  PACKAGE Arbres IS NEW Noeuds.Arbres_Binaires(�l�ments_�gaux   => Mon�gal,
--                                               Afficher_�l�ment => MonAfficher);
WITH Ada.Finalization;
GENERIC
  WITH FUNCTION �l�ments_�gaux(E1, E2: IN Type_�l�ment) RETURN Boolean;
  -- v�rifie l'�galit� de deux �l�ments
  WITH PROCEDURE Afficher_�l�ment(�l�ment: IN Type_�l�ment);
  -- affichage de la valeur d'un �l�ment
PACKAGE CNoeuds_Binaires.Arbres_BinairesL IS

TYPE Arbre_Binaire IS NEW Ada.Finalization.Limited_Controlled WITH PRIVATE;
-- Permet de d�clarer des objets repr�sentant des arbres binaires
TYPE Traitement IS ACCESS PROCEDURE(�l�ment: IN Type_�l�ment);
-- Proc�dure de traitement utilis�e dans la travers�e de l'arbre
TYPE Position IS (Racine, Courant, Parent, Gauche, Droite);
-- Permet de sp�cifier une position par rapport � un noeud
TYPE Type_Mode IS (Pr�fixe, Infixe, Suffixe);
-- Mode de trajet dans un arbre

Erreur_Arbre: EXCEPTION;
-- Lev�e si un ant�c�dent n'est pas respect� ou si une erreur li�e �
-- la structure des arbres survient lors d'une op�ration quelconque.

PROCEDURE Vider_Arbre(Arbre: IN OUT Arbre_Binaire);
-- D�truit la racine l'Arbre et tous ses descendants.
-- Ant�c�dent:  Aucun.
-- Cons�quent:  L'Arbre est vide.

PROCEDURE Ins�rer_Noeud(Arbre: IN OUT Arbre_Binaire;
                        �l�ment: IN Type_�l�ment;
                        O�: IN Position);
-- Ins�re �l�ment dans l'Arbre � l'endroit indiqu� par rapport
-- � l'�l�ment courant.
-- Ant�c�dent:  O� = Racine et l'arbre est vide OU
--              O� = Gauche ou Droite
--                   ET le noeud courant existe
--                   ET le noeud vis� n'existe pas.
-- Cons�quent:  �l�ment a �t� ins�r� � l'endroit voulu, et le noeud
--              courant de l'arbre est maintenant ce nouveau noeud.
-- Exception:   Erreur_Arbre.

PROCEDURE D�truire_Courant(Arbre: IN OUT Arbre_Binaire);
-- D�truit le noeud courant de l'Arbre.
-- Ant�c�dent:  Le noeud courant de l'Arbre existe.
-- Cons�quent:  Le noeud courant a �t� d�truit et ses voisins on �t�
--              raccord�s (Son fils de gauche a pris sa place, son
--              fils de droite se retrouve � l'extr�me droite de
--              son rempla�ant).
-- Exception:   Erreur_Arbre.

PROCEDURE Traverser_Arbre(Arbre: IN Arbre_Binaire;
                          Mode: IN Type_Mode := Infixe;
                          Proc_Traiter: IN Traitement);
-- Parcourt l'Arbre, et applique la proc�dure Proc_Traiter � chacun
-- des �l�ments. Le type de parcours est d�termin� par le param�tre
-- Mode (Pr�fixe, Infixe ou Suffixe).
-- Ant�c�dent:  Aucun.
-- Cons�quent:  Tous les noeuds ont �t� visit�s et la proc�dure
--              Proc_Traiter a �t� appliqu�e � chacun d'eux.

PROCEDURE Chercher(Arbre: IN OUT Arbre_Binaire;
                   �l�ment: IN Type_�l�ment);
-- Cherche �l�ment dans l'Arbre.
-- Ant�c�dent:  Aucun.
-- Cons�quent:  Si �l�ment existe dans l'Arbre alors le noeud courant est
--              maintenant le premier noeud o� il se trouve
--              Sinon le noeud courant de l'Arbre n'existe pas.

FUNCTION Noeud_Dans_Arbre(Arbre: Arbre_Binaire;
                          Noeud : Noeud_Binaire) RETURN Boolean;
-- V�rifie si un Noeud appartient bien � un Arbre donn�.
-- Ant�c�dent:  L'Arbre n'est pas vide et Noeud existe.
-- Cons�quent:  Retourne True si Noeud appartient � Arbre, et False autrement.
-- Exception:   Erreur_Arbre.

PROCEDURE D�placer_Courant(Arbre: IN OUT Arbre_Binaire;
                           O�: IN Position);
-- D�place le noeud courant de l'Arbre vers l'endroit indiqu� par O�.
-- Ant�c�dent:  O� = Racine, 
--              ou bien le noeud courant existe et la destination est libre.
-- Cons�quent:  O� = Racine  ==> le noeud courant est maintenant la racine.
--              O� = Parent  ==> le noeud courant est maintenant le parent
--                               de l'ancien noeud courant.
--              O� = Gauche  ==> le noeud courant est maintenant le 
--                               descendant gauche de l'ancien noeud courant.
--              O� = Droite  ==> le noeud courant est maintenant le 
--                               descendant droit de l'ancien noeud courant.
--              O� = Courant ==> le noeud courant reste inchang�.
-- Exception:   Erreur_Arbre.

PROCEDURE Modifier_Courant(Arbre: IN OUT Arbre_Binaire;
                           �l�ment: IN Type_�l�ment);
-- Remplace la valeur de l'�l�ment du noeud courant par �l�ment
-- Ant�c�dent:  Le noeud courant existe
-- Cons�quent:  La valeur de l'�l�ment du noeud courant est �l�ment
-- Exception:   Erreur_Arbre.

FUNCTION Valeur_Courante(Arbre: Arbre_Binaire) RETURN Type_�l�ment;
-- Retourne la valeur du noeud courant de l'Arbre.
-- Ant�c�dent:  Le noeud courant existe.
-- Cons�quent:  La valeur du noeud courant est retourn�e.
-- Exception:   Erreur_Arbre.

FUNCTION Nombre_Noeuds(Arbre: Arbre_Binaire) RETURN Natural;
-- Donne le nombre de noeuds dans l'Arbre.
-- Ant�c�dent:  Aucun.
-- Cons�quent:  Le nombre de noeuds dans l'Arbre est retourn�.

PROCEDURE Afficher_Arbre(Arbre: IN Arbre_Binaire);
-- Affiche le sous-arbre Courant avec d�calages pour montrer sa structure
-- hi�rarchique en utilisant la proc�dure g�n�rique Afficher_�l�ment. L'Arbre
-- est affich� couch� sur le c�t�, sur l'organe d'affichage courant.
-- Ant�c�dent:  Aucun.
-- Cons�quent:  L'Arbre est affich� horizontalement sur l'affichage courant,
--              la racine �tant � gauche et les descendants vers la droite.

FUNCTION Noeud_Courant(Arbre: Arbre_Binaire) RETURN Noeud_Binaire;
-- Retourne un pointeur au noeud courant.
-- Ant�c�dent:  Aucun.
-- Cons�quent:  Le noeud courant de l'Arbre est retourn�.

PROCEDURE Positionner_Noeud_Courant(Arbre: IN OUT Arbre_Binaire;
                                    Noeud: IN Noeud_Binaire);
-- Met le noeud courant � Noeud.
-- Ant�c�dent:  Noeud existe et fait partie de l'Arbre.
-- Cons�quent:  Le noeud courant est maintenant Noeud.
-- Exception:   Erreur_Arbre.

FUNCTION Noeud_Existe(Arbre: Arbre_Binaire;
                      O�: Position) RETURN Boolean;
-- V�rifie si un noeud existe dans l'Arbre � l'endroit indiqu� par
-- rapport au noeud courant.
-- Ant�c�dent:  Si O� n'est ni Racine, ni Courant, le noeud courant de
--              l'Arbre doit exister.
-- Cons�quent:  Si un noeud existe � l'endroit indiqu� alors
--              True est retourn�, autrement False est retourn�.
-- Exception:   Erreur_Arbre.

PRIVATE
  TYPE Arbre_Binaire IS NEW Ada.Finalization.Limited_Controlled WITH RECORD
                          Racine: Noeud_Binaire;
                          Courant: Noeud_Binaire;
                          Nombre_�l�ments: Natural := 0;
                        END RECORD;
  
  PROCEDURE Initialize(A: IN OUT Arbre_Binaire);
  PROCEDURE Finalize(A: IN OUT Arbre_Binaire);
END CNoeuds_Binaires.Arbres_BinairesL;
