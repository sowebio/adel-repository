--          Copyright © 1998 Philippe J. Gabrini
GENERIC
  TYPE Nom_Sommet IS (<>);
  WITH FUNCTION Nom(Élt: Type_Élément) RETURN Nom_Sommet;
PACKAGE Graphes.NonOrientés IS

TYPE Graphe IS NEW Graphe_Abstrait WITH PRIVATE;

PROCEDURE Détruire(G: IN OUT Graphe);
-- Éliminer tous les sommets et les arêtes du graphe G.
-- Antécédent: le graphe G existe.
-- Conséquent: le graphe G' est vide.

FUNCTION NombreSommets(G: Graphe) RETURN Natural;
-- Compte du nombre de sommets du graphe G.
-- Antécédent: le graphe G existe.
-- Conséquent: retourne le nombre de sommets de G.

PROCEDURE InsérerSommet(G: IN OUT Graphe; Élt: IN Type_Élément);
-- Insérer (ou mettre à jour) un sommet dans le graphe G identifié par Élt.
-- Antécédent: le graphe G existe.
-- Conséquent: un sommet du graphe G' a la valeur Élt.

PROCEDURE InsérerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_Élément; 
                     Poids: IN Integer);
-- Insérer une nouvelle arête avec pondération Poids dans le graphe G
-- entre les sommets Sommet1 et Sommet2.
-- Antécédent: le graphe G contient les sommets Sommet1 et Sommet2
-- Conséquent: le graphe G' contient une arête entre les sommets
--                Sommet1 et Sommet2, avec la pondération Poids.

PROCEDURE SupprimerSommet(G: IN OUT Graphe; Sommet: IN Type_Élément);
-- Supprimer le sommet Sommet du graphe G.
-- Antécédent: il existe un sommet Sommet dans le graphe G.
-- Conséquent: le sommet Sommet ne fait plus partie du graphe G,
--                pas plus que les arêtes antérieurement reliées 
--                à ce sommet.

PROCEDURE SupprimerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_Élément);
-- Supprimer l'arête entre les sommets Sommet1 et Sommet2 dans le graphe G.
-- Antécédent: il y a une arête entre les sommets Sommet1 et Sommet2.
-- Conséquent: l'arête ne fait plus partie du graphe G'.

PROCEDURE TrouverSommet(G: IN Graphe; Sommet: IN OUT Type_Élément;
                        Trouvé: OUT Boolean);
-- Chercher le sommet Sommet dans le graphe G et retourner la valeur du sommet.
-- Antécédent: le graphe G existe.
-- Conséquent: si le graphe G contient un sommet Sommet,
--                alors Élt' a la valeur du sommet,
--                et Trouvé est vrai, sinon Trouvé est faux.

FUNCTION Poids(G: Graphe; Sommet1, Sommet2: Type_Élément) RETURN Integer;
-- Retourne le poids associé à l'arête entre les deux sommets
-- Sommet1 et Sommet2 dans le graphe G.
-- Antécédent: il y a une arête dans le graphe G entre les sommets
--             Sommet1 et Sommet2.
-- Conséquent: retourne le poids associé à l'arête correspondante.

PROCEDURE TrouverAdjacent(G: IN Graphe;
                          Sommet: IN Type_Élément; N: IN Natural;
                          SommetAdjacent: OUT Type_Élément;
                          Succès: OUT Boolean);
-- Retourne le Nième sommet adjacent au sommet Sommet dans le graphe G.
-- Antécédent: le Sommet existe dans le graphe G.
-- Conséquent: SommetAdjacent = Nième sommet adjacent à Sommet et Succès vrai
--             ou Succès faux si absent.
   
PRIVATE
TYPE Matrice IS ARRAY (Nom_Sommet, Nom_Sommet) OF Integer;
TYPE Type_Sommet IS RECORD
                      Valeur: Type_Élément;
                      Présent: Boolean := False;
                    END RECORD;
TYPE Vecteur IS ARRAY (Nom_Sommet) OF Type_Sommet;
TYPE Graphe IS NEW Graphe_Abstrait WITH RECORD
                                          N: Natural := 0;
                                          Arêtes: Matrice;
                                          Sommets: Vecteur;
                                        END RECORD;
END Graphes.NonOrientés;
