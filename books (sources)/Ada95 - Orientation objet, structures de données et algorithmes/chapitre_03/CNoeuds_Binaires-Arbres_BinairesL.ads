--          Copyright © 1998 Philippe J. Gabrini
-- Programmeur:     Denis Mouraux et Philippe Gabrini
-- Date:            Juin 1998
--
-- Ce module définit le type Arbre_Binaire, qui permet de déclarer des
-- arbres binaires.  L'utilisateur devra instancier ArbresBinaires de 
-- la façon suivante:
--  PACKAGE Noeuds IS NEW CNoeuds_Binaires(Type_Élément => MonType);
--  PACKAGE Arbres IS NEW Noeuds.Arbres_Binaires(Éléments_Égaux   => MonÉgal,
--                                               Afficher_Élément => MonAfficher);
WITH Ada.Finalization;
GENERIC
  WITH FUNCTION Éléments_Égaux(E1, E2: IN Type_Élément) RETURN Boolean;
  -- vérifie l'égalité de deux éléments
  WITH PROCEDURE Afficher_Élément(Élément: IN Type_Élément);
  -- affichage de la valeur d'un élément
PACKAGE CNoeuds_Binaires.Arbres_BinairesL IS

TYPE Arbre_Binaire IS NEW Ada.Finalization.Limited_Controlled WITH PRIVATE;
-- Permet de déclarer des objets représentant des arbres binaires
TYPE Traitement IS ACCESS PROCEDURE(Élément: IN Type_Élément);
-- Procédure de traitement utilisée dans la traversée de l'arbre
TYPE Position IS (Racine, Courant, Parent, Gauche, Droite);
-- Permet de spécifier une position par rapport à un noeud
TYPE Type_Mode IS (Préfixe, Infixe, Suffixe);
-- Mode de trajet dans un arbre

Erreur_Arbre: EXCEPTION;
-- Levée si un antécédent n'est pas respecté ou si une erreur liée à
-- la structure des arbres survient lors d'une opération quelconque.

PROCEDURE Vider_Arbre(Arbre: IN OUT Arbre_Binaire);
-- Détruit la racine l'Arbre et tous ses descendants.
-- Antécédent:  Aucun.
-- Conséquent:  L'Arbre est vide.

PROCEDURE Insérer_Noeud(Arbre: IN OUT Arbre_Binaire;
                        Élément: IN Type_Élément;
                        Où: IN Position);
-- Insère Élément dans l'Arbre à l'endroit indiqué par rapport
-- à l'élément courant.
-- Antécédent:  Où = Racine et l'arbre est vide OU
--              Où = Gauche ou Droite
--                   ET le noeud courant existe
--                   ET le noeud visé n'existe pas.
-- Conséquent:  Élément a été inséré à l'endroit voulu, et le noeud
--              courant de l'arbre est maintenant ce nouveau noeud.
-- Exception:   Erreur_Arbre.

PROCEDURE Détruire_Courant(Arbre: IN OUT Arbre_Binaire);
-- Détruit le noeud courant de l'Arbre.
-- Antécédent:  Le noeud courant de l'Arbre existe.
-- Conséquent:  Le noeud courant a été détruit et ses voisins on été
--              raccordés (Son fils de gauche a pris sa place, son
--              fils de droite se retrouve à l'extrême droite de
--              son remplaçant).
-- Exception:   Erreur_Arbre.

PROCEDURE Traverser_Arbre(Arbre: IN Arbre_Binaire;
                          Mode: IN Type_Mode := Infixe;
                          Proc_Traiter: IN Traitement);
-- Parcourt l'Arbre, et applique la procédure Proc_Traiter à chacun
-- des Éléments. Le type de parcours est déterminé par le paramètre
-- Mode (Préfixe, Infixe ou Suffixe).
-- Antécédent:  Aucun.
-- Conséquent:  Tous les noeuds ont été visités et la procédure
--              Proc_Traiter a été appliquée à chacun d'eux.

PROCEDURE Chercher(Arbre: IN OUT Arbre_Binaire;
                   Élément: IN Type_Élément);
-- Cherche Élément dans l'Arbre.
-- Antécédent:  Aucun.
-- Conséquent:  Si Élément existe dans l'Arbre alors le noeud courant est
--              maintenant le premier noeud où il se trouve
--              Sinon le noeud courant de l'Arbre n'existe pas.

FUNCTION Noeud_Dans_Arbre(Arbre: Arbre_Binaire;
                          Noeud : Noeud_Binaire) RETURN Boolean;
-- Vérifie si un Noeud appartient bien à un Arbre donné.
-- Antécédent:  L'Arbre n'est pas vide et Noeud existe.
-- Conséquent:  Retourne True si Noeud appartient à Arbre, et False autrement.
-- Exception:   Erreur_Arbre.

PROCEDURE Déplacer_Courant(Arbre: IN OUT Arbre_Binaire;
                           Où: IN Position);
-- Déplace le noeud courant de l'Arbre vers l'endroit indiqué par Où.
-- Antécédent:  Où = Racine, 
--              ou bien le noeud courant existe et la destination est libre.
-- Conséquent:  Où = Racine  ==> le noeud courant est maintenant la racine.
--              Où = Parent  ==> le noeud courant est maintenant le parent
--                               de l'ancien noeud courant.
--              Où = Gauche  ==> le noeud courant est maintenant le 
--                               descendant gauche de l'ancien noeud courant.
--              Où = Droite  ==> le noeud courant est maintenant le 
--                               descendant droit de l'ancien noeud courant.
--              Où = Courant ==> le noeud courant reste inchangé.
-- Exception:   Erreur_Arbre.

PROCEDURE Modifier_Courant(Arbre: IN OUT Arbre_Binaire;
                           Élément: IN Type_Élément);
-- Remplace la valeur de l'élément du noeud courant par Élément
-- Antécédent:  Le noeud courant existe
-- Conséquent:  La valeur de l'élément du noeud courant est Élément
-- Exception:   Erreur_Arbre.

FUNCTION Valeur_Courante(Arbre: Arbre_Binaire) RETURN Type_Élément;
-- Retourne la valeur du noeud courant de l'Arbre.
-- Antécédent:  Le noeud courant existe.
-- Conséquent:  La valeur du noeud courant est retournée.
-- Exception:   Erreur_Arbre.

FUNCTION Nombre_Noeuds(Arbre: Arbre_Binaire) RETURN Natural;
-- Donne le nombre de noeuds dans l'Arbre.
-- Antécédent:  Aucun.
-- Conséquent:  Le nombre de noeuds dans l'Arbre est retourné.

PROCEDURE Afficher_Arbre(Arbre: IN Arbre_Binaire);
-- Affiche le sous-arbre Courant avec décalages pour montrer sa structure
-- hiérarchique en utilisant la procédure générique Afficher_Élément. L'Arbre
-- est affiché couché sur le côté, sur l'organe d'affichage courant.
-- Antécédent:  Aucun.
-- Conséquent:  L'Arbre est affiché horizontalement sur l'affichage courant,
--              la racine étant à gauche et les descendants vers la droite.

FUNCTION Noeud_Courant(Arbre: Arbre_Binaire) RETURN Noeud_Binaire;
-- Retourne un pointeur au noeud courant.
-- Antécédent:  Aucun.
-- Conséquent:  Le noeud courant de l'Arbre est retourné.

PROCEDURE Positionner_Noeud_Courant(Arbre: IN OUT Arbre_Binaire;
                                    Noeud: IN Noeud_Binaire);
-- Met le noeud courant à Noeud.
-- Antécédent:  Noeud existe et fait partie de l'Arbre.
-- Conséquent:  Le noeud courant est maintenant Noeud.
-- Exception:   Erreur_Arbre.

FUNCTION Noeud_Existe(Arbre: Arbre_Binaire;
                      Où: Position) RETURN Boolean;
-- Vérifie si un noeud existe dans l'Arbre à l'endroit indiqué par
-- rapport au noeud courant.
-- Antécédent:  Si Où n'est ni Racine, ni Courant, le noeud courant de
--              l'Arbre doit exister.
-- Conséquent:  Si un noeud existe à l'endroit indiqué alors
--              True est retourné, autrement False est retourné.
-- Exception:   Erreur_Arbre.

PRIVATE
  TYPE Arbre_Binaire IS NEW Ada.Finalization.Limited_Controlled WITH RECORD
                          Racine: Noeud_Binaire;
                          Courant: Noeud_Binaire;
                          Nombre_Éléments: Natural := 0;
                        END RECORD;
  
  PROCEDURE Initialize(A: IN OUT Arbre_Binaire);
  PROCEDURE Finalize(A: IN OUT Arbre_Binaire);
END CNoeuds_Binaires.Arbres_BinairesL;
