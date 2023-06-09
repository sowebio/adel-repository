--          Copyright © 1998 Philippe J. Gabrini
GENERIC
  Nombre_Sommets_Maxi: IN Positive := 50;
  TYPE Type_Élément IS PRIVATE;  
  WITH PROCEDURE AfficherSommet(S: IN Type_Élément);
  WITH PROCEDURE AfficherClef(S: IN Type_Élément);
PACKAGE Graphes IS

TYPE Graphe_Abstrait IS ABSTRACT TAGGED LIMITED NULL RECORD;
-- Le type de graphe, dérivable et extensible pour créer d'autres
-- types de graphes de la même classe.

PROCEDURE Détruire(G: IN OUT Graphe_Abstrait) IS ABSTRACT;
-- Éliminer tous les sommets et les arêtes (arcs) du graphe G.
-- Antécédent: le graphe G existe.
-- Conséquent: le graphe G' est vide.

FUNCTION NombreSommets(G: Graphe_Abstrait) RETURN Natural IS ABSTRACT;
-- Compte du nombre de sommets du graphe G.
-- Antécédent: le graphe G existe.
-- Conséquent: retourne le nombre de sommets de G.

PROCEDURE InsérerSommet(G: IN OUT Graphe_Abstrait; 
                        Élt: IN Type_Élément) IS ABSTRACT;
-- Insérer (ou mettre à jour) un sommet dans le graphe G identifié par Élt.
-- Antécédent: aucun.
-- Conséquent: soit un nouveau sommet du graphe G' a la valeur Élt et n'est pas
--             adjacent à d'autres sommets, soit le sommet Élt a été mis à jour.

PROCEDURE InsérerArc(G: IN OUT Graphe_Abstrait;
                     Sommet1, Sommet2: IN Type_Élément; 
                     Poids: IN Integer) IS ABSTRACT;
-- Insérer une arête (arc) avec pondération Poids dans le graphe G.
-- entre les sommets Sommet1 et Sommet2.
-- Antécédent: le graphe G contient les sommets Sommet1 et Sommet2.
-- Conséquent: le graphe G' contient une arête (arc) entre les sommets
--             Sommet1 et Sommet2, avec la pondération Poids.

PROCEDURE SupprimerSommet(G: IN OUT Graphe_Abstrait;
                          Sommet: IN Type_Élément) IS ABSTRACT;
-- Supprimer le Sommet du graphe G.
-- Antécédent: il existe un sommet Sommet dans le graphe G.
-- Conséquent: le sommet Sommet ne fait plus partie du graphe G, pas plus
--             que les arêtes (arcs) antérieurement reliées à ce sommet.

PROCEDURE SupprimerArc(G: IN OUT Graphe_Abstrait;
                       Sommet1, Sommet2: IN Type_Élément) IS ABSTRACT;
-- Supprimer l'arête (arc) entre les sommets Sommet1 et Sommet2 du graphe G.
-- Antécédent: il y a une arête (arc) entre les sommets Sommet1 et Sommet2.
-- Conséquent: l'arête (arc) ne fait plus partie du graphe G'.

PROCEDURE TrouverSommet(G: IN Graphe_Abstrait; Sommet: IN OUT Type_Élément;
                        Trouvé: OUT Boolean) IS ABSTRACT;
-- Chercher le sommet Sommet dans le graphe G et retourner sa valeur.
-- Antécédent: le graphe G existe.
-- Conséquent: si le graphe G contient un sommet Sommet,
--             alors Sommet' a la valeur du sommet,
--             et Trouvé est vrai, sinon Trouvé est faux.

FUNCTION Poids(G: Graphe_Abstrait;
               Sommet1, Sommet2: IN Type_Élément) RETURN Integer IS ABSTRACT;
-- Retourne le poids associé à l'arête (arc) entre les deux sommets
-- Sommet1 et Sommet2 dans le graphe G.
-- Antécédent: il y a une arête (arc) dans le graphe G entre les sommets
--             Sommet1 et Sommet2.
-- Conséquent: retourne le poids associé à l'arête correspondante.

PROCEDURE TrouverAdjacent(G: IN Graphe_Abstrait;
                          Sommet: IN Type_Élément; N: IN Natural;
                          SommetAdjacent: OUT Type_Élément;
                          Succès: OUT Boolean) IS ABSTRACT;
-- Retourne le Nième sommet adjacent au Sommet dans le graphe G, avec indication
-- de succès ou d'échec.
-- Antécédent: le Sommet existe dans le graphe G.
-- Conséquent: retourne le Nième sommet adjacent à Sommet et Succès vrai
--             ou Succès faux si absent.

END Graphes;
