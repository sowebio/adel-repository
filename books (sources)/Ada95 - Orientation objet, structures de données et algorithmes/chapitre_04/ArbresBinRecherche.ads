--          Copyright © 1998 Philippe J. Gabrini
-- Définition des arbres binaires de recherche génériques, de façon à pouvoir
-- les réutiliser dans un paquetage enfant.
--     Philippe Gabrini      Avril 1998 
GENERIC
  TYPE TypeÉlément IS PRIVATE;
  WITH FUNCTION Comparaison(E1, E2: TypeÉlément) RETURN Integer;
  WITH PROCEDURE AfficherÉlément(E1: IN TypeÉlément);
PACKAGE ArbresBinRecherche IS

TYPE ArbreBinaireRecherche IS LIMITED PRIVATE;
TYPE ProcTraitement IS ACCESS PROCEDURE(Élément: IN TypeÉlément);
-- Procédure de traitement utilisée dans la traversée de l'arbre.

PROCEDURE SupprimerArbre(Arbre: IN OUT ArbreBinaireRecherche);
-- Toute l'information sur l'Arbre et les données qu'il contient est détruite.
-- Antécédent: Arbre existe.
-- Conséquent: Arbre' n'existe pas.

PROCEDURE InsérerNoeud(Arbre: IN OUT ArbreBinaireRecherche;
                       Élément: IN TypeÉlément);
-- Insérer Élément à la position voulue dans Arbre selon la clef de
-- Élément.  Utiliser Comparaison pour la comparison des clefs.  S'il
-- existait déjà un noeud avec la même clef, on lui donne la valeur Élément.
-- Antécédent: Arbre et Élément existent.
-- Conséquent: Arbre' contient un  noeud avec la valeur Élément.

PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreBinaireRecherche;
                         Élément: IN TypeÉlément);
-- Trouver le noeud avec la clef d'Élément et le supprimer de l'Arbre.
-- Utiliser Comparaison pour les comparaisons.  Si aucun noeud ne 
-- possède cette clef, Arbre demeure inchangé.
-- Antécédent: Arbre existe.
-- Conséquent: Arbre' ne contient plus de noeud avec cette clef.

PROCEDURE TraverserArbre(Arbre: IN ArbreBinaireRecherche;
                         Traiter: IN ProcTraitement);
-- Appliquer Traiter à chaque noeud de l'Arbre en ordre infixe.
-- Antécédent: Arbre existe.
-- Conséquent: on a rendu visite à tous les noeuds de l'arbre et on leur
--             a appliqué Traiter.

PROCEDURE Rechercher(Arbre: IN ArbreBinaireRecherche;
                     Élément: IN OUT TypeÉlément; Succès: OUT Boolean);
-- Rechercher un Élément dans Arbre identifié par la clef d'Élément.  Utiliser 
-- Comparaison pour les éléments et retourner Élément pris dans Arbre.
-- Antécédent: Arbre et Élément existent.
-- Conséquent: si clef apparaît dans Arbre Succès est True et Élément
--             prend la valeur du noeud autrement Succès est False.

PROCEDURE AfficherArbre(Arbre: IN ArbreBinaireRecherche;
                        Décalage: IN Natural);
-- Afficher les clefs de l'Arbre avec décalages en utilisant AfficherÉlément 
-- pour montrer la structure de l'arbre.
-- Antécédent: Arbre existe.
-- Conséquent: Arbre est affiché verticalement sur l'écran avec éléments décalés.

PRIVATE
TYPE NoeudArbre;
TYPE ArbreBinaireRecherche IS ACCESS NoeudArbre'CLASS;
-- Pointeur à tous les noeuds de type NoeudArbre et de types dérivés.
TYPE NoeudArbre IS TAGGED RECORD  -- pour extension possible
                            Élément: TypeÉlément;
                            Gauche, Droite: ArbreBinaireRecherche;
                          END RECORD;
END ArbresBinRecherche;
