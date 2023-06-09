--          Copyright � 1998 Philippe J. Gabrini
GENERIC
  Nombre_Sommets_Maxi: IN Positive := 50;
  TYPE Type_�l�ment IS PRIVATE;  
  WITH PROCEDURE AfficherSommet(S: IN Type_�l�ment);
  WITH PROCEDURE AfficherClef(S: IN Type_�l�ment);
PACKAGE Graphes IS

TYPE Graphe_Abstrait IS ABSTRACT TAGGED LIMITED NULL RECORD;
-- Le type de graphe, d�rivable et extensible pour cr�er d'autres
-- types de graphes de la m�me classe.

PROCEDURE D�truire(G: IN OUT Graphe_Abstrait) IS ABSTRACT;
-- �liminer tous les sommets et les ar�tes (arcs) du graphe G.
-- Ant�c�dent: le graphe G existe.
-- Cons�quent: le graphe G' est vide.

FUNCTION NombreSommets(G: Graphe_Abstrait) RETURN Natural IS ABSTRACT;
-- Compte du nombre de sommets du graphe G.
-- Ant�c�dent: le graphe G existe.
-- Cons�quent: retourne le nombre de sommets de G.

PROCEDURE Ins�rerSommet(G: IN OUT Graphe_Abstrait; 
                        �lt: IN Type_�l�ment) IS ABSTRACT;
-- Ins�rer (ou mettre � jour) un sommet dans le graphe G identifi� par �lt.
-- Ant�c�dent: aucun.
-- Cons�quent: soit un nouveau sommet du graphe G' a la valeur �lt et n'est pas
--             adjacent � d'autres sommets, soit le sommet �lt a �t� mis � jour.

PROCEDURE Ins�rerArc(G: IN OUT Graphe_Abstrait;
                     Sommet1, Sommet2: IN Type_�l�ment; 
                     Poids: IN Integer) IS ABSTRACT;
-- Ins�rer une ar�te (arc) avec pond�ration Poids dans le graphe G.
-- entre les sommets Sommet1 et Sommet2.
-- Ant�c�dent: le graphe G contient les sommets Sommet1 et Sommet2.
-- Cons�quent: le graphe G' contient une ar�te (arc) entre les sommets
--             Sommet1 et Sommet2, avec la pond�ration Poids.

PROCEDURE SupprimerSommet(G: IN OUT Graphe_Abstrait;
                          Sommet: IN Type_�l�ment) IS ABSTRACT;
-- Supprimer le Sommet du graphe G.
-- Ant�c�dent: il existe un sommet Sommet dans le graphe G.
-- Cons�quent: le sommet Sommet ne fait plus partie du graphe G, pas plus
--             que les ar�tes (arcs) ant�rieurement reli�es � ce sommet.

PROCEDURE SupprimerArc(G: IN OUT Graphe_Abstrait;
                       Sommet1, Sommet2: IN Type_�l�ment) IS ABSTRACT;
-- Supprimer l'ar�te (arc) entre les sommets Sommet1 et Sommet2 du graphe G.
-- Ant�c�dent: il y a une ar�te (arc) entre les sommets Sommet1 et Sommet2.
-- Cons�quent: l'ar�te (arc) ne fait plus partie du graphe G'.

PROCEDURE TrouverSommet(G: IN Graphe_Abstrait; Sommet: IN OUT Type_�l�ment;
                        Trouv�: OUT Boolean) IS ABSTRACT;
-- Chercher le sommet Sommet dans le graphe G et retourner sa valeur.
-- Ant�c�dent: le graphe G existe.
-- Cons�quent: si le graphe G contient un sommet Sommet,
--             alors Sommet' a la valeur du sommet,
--             et Trouv� est vrai, sinon Trouv� est faux.

FUNCTION Poids(G: Graphe_Abstrait;
               Sommet1, Sommet2: IN Type_�l�ment) RETURN Integer IS ABSTRACT;
-- Retourne le poids associ� � l'ar�te (arc) entre les deux sommets
-- Sommet1 et Sommet2 dans le graphe G.
-- Ant�c�dent: il y a une ar�te (arc) dans le graphe G entre les sommets
--             Sommet1 et Sommet2.
-- Cons�quent: retourne le poids associ� � l'ar�te correspondante.

PROCEDURE TrouverAdjacent(G: IN Graphe_Abstrait;
                          Sommet: IN Type_�l�ment; N: IN Natural;
                          SommetAdjacent: OUT Type_�l�ment;
                          Succ�s: OUT Boolean) IS ABSTRACT;
-- Retourne le Ni�me sommet adjacent au Sommet dans le graphe G, avec indication
-- de succ�s ou d'�chec.
-- Ant�c�dent: le Sommet existe dans le graphe G.
-- Cons�quent: retourne le Ni�me sommet adjacent � Sommet et Succ�s vrai
--             ou Succ�s faux si absent.

END Graphes;
