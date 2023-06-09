--          Copyright © 1998 Philippe J. Gabrini
GENERIC
  TYPE TypeÉlément IS PRIVATE;
  WITH FUNCTION ÉlémentsÉgaux(E1, E2: IN TypeÉlément) RETURN Boolean;
  WITH FUNCTION Inférieur(E1, E2: IN TypeÉlément) RETURN Boolean;
  WITH PROCEDURE AfficherÉlément(E: IN TypeÉlément);
PACKAGE ArbresBinRechFilés IS
-- Ce module présente la réalisation d'arbres binaires de recherche qui 
-- utilise un arbre doublement "filé".  L'utilisation de "fils" permet
-- non seulement de traverser l'arbre de façon itérative dans toutes les
-- directions, mais aussi de faire des traversées incrémentales, un
-- noeud à la fois.

TYPE Arbre_Bin_Rech_Filé IS PRIVATE;

FUNCTION ArbreVide(Arbre: Arbre_Bin_Rech_Filé) RETURN Boolean;
-- Retourne Vrai si l'Arbre est vide, et Faux autrement.

PROCEDURE DétruireArbre(Arbre: IN OUT Arbre_Bin_Rech_Filé);
-- Détruit toute la structure d'Arbre.

FUNCTION Prédécesseur(Arbre: Arbre_Bin_Rech_Filé) RETURN Arbre_Bin_Rech_Filé;
-- Retourne le prédécesseur d'un noeud donné d'Arbre.

FUNCTION Successeur(Arbre: Arbre_Bin_Rech_Filé) RETURN Arbre_Bin_Rech_Filé;
-- Retourne le successeur d'un noeud donné d'Arbre.

FUNCTION Noeud(Arbre: Arbre_Bin_Rech_Filé; Clef: TypeÉlément)
              RETURN Arbre_Bin_Rech_Filé;
-- Retourne un pointeur au noeud qui possède la Clef donnée; si la Clef n'est
-- pas dans Arbre, on retourne NULL.

PROCEDURE InsérerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Filé;
                       Élément: IN TypeÉlément);
-- Insère Élément dans Arbre; si Élément existe déjà dans l'Arbre, ce dernier
-- n'est pas modifié. 

PROCEDURE SupprimerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Filé;
                         Élément: IN TypeÉlément);
-- Supprime le noeud de l'Arbre ayant la même clef qu'Élément; s'il n'y a pas
-- de noeud possédant la même clef, Arbre n'est pas modifié. 

PROCEDURE TraverserAvant(Arbre: IN Arbre_Bin_Rech_Filé);
-- Effectue une traversée infixe de l'Arbre et affiche les éléments en ordre  
-- ascendant des clefs; on utilise AfficherÉlément et on sépare les valeurs 
-- par un espace.

PROCEDURE TraverserArrière(Arbre: IN Arbre_Bin_Rech_Filé);
-- Effectue une traversée infixe de l'Arbre et affiche les éléments en ordre  
-- descendant des clefs; on utilise AfficherÉlément et on sépare les valeurs 
-- par un espace.

PROCEDURE AfficherNoeud(Arbre: IN Arbre_Bin_Rech_Filé);
-- Affiche la valeur de l'élément au noeud Arbre.

PROCEDURE AfficherArbre(Arbre: IN Arbre_Bin_Rech_Filé);
-- Affiche l'Arbre de gauche à droite avec la racine à gauche.

PRIVATE
TYPE NoeudArbre IS RECORD
         			 Élément: TypeÉlément;
         			 Gauche, Droite: Arbre_Bin_Rech_Filé;
         			 FilGauche, FilDroit: Boolean;
      		       END RECORD;
TYPE Arbre_Bin_Rech_Filé IS ACCESS NoeudArbre;
END ArbresBinRechFilés;

