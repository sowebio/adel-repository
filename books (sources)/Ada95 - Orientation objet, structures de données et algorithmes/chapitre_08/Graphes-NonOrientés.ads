--          Copyright � 1998 Philippe J. Gabrini
GENERIC
  TYPE Nom_Sommet IS (<>);
  WITH FUNCTION Nom(�lt: Type_�l�ment) RETURN Nom_Sommet;
PACKAGE Graphes.NonOrient�s IS

TYPE Graphe IS NEW Graphe_Abstrait WITH PRIVATE;

PROCEDURE D�truire(G: IN OUT Graphe);
-- �liminer tous les sommets et les ar�tes du graphe G.
-- Ant�c�dent: le graphe G existe.
-- Cons�quent: le graphe G' est vide.

FUNCTION NombreSommets(G: Graphe) RETURN Natural;
-- Compte du nombre de sommets du graphe G.
-- Ant�c�dent: le graphe G existe.
-- Cons�quent: retourne le nombre de sommets de G.

PROCEDURE Ins�rerSommet(G: IN OUT Graphe; �lt: IN Type_�l�ment);
-- Ins�rer (ou mettre � jour) un sommet dans le graphe G identifi� par �lt.
-- Ant�c�dent: le graphe G existe.
-- Cons�quent: un sommet du graphe G' a la valeur �lt.

PROCEDURE Ins�rerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_�l�ment; 
                     Poids: IN Integer);
-- Ins�rer une nouvelle ar�te avec pond�ration Poids dans le graphe G
-- entre les sommets Sommet1 et Sommet2.
-- Ant�c�dent: le graphe G contient les sommets Sommet1 et Sommet2
-- Cons�quent: le graphe G' contient une ar�te entre les sommets
--                Sommet1 et Sommet2, avec la pond�ration Poids.

PROCEDURE SupprimerSommet(G: IN OUT Graphe; Sommet: IN Type_�l�ment);
-- Supprimer le sommet Sommet du graphe G.
-- Ant�c�dent: il existe un sommet Sommet dans le graphe G.
-- Cons�quent: le sommet Sommet ne fait plus partie du graphe G,
--                pas plus que les ar�tes ant�rieurement reli�es 
--                � ce sommet.

PROCEDURE SupprimerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_�l�ment);
-- Supprimer l'ar�te entre les sommets Sommet1 et Sommet2 dans le graphe G.
-- Ant�c�dent: il y a une ar�te entre les sommets Sommet1 et Sommet2.
-- Cons�quent: l'ar�te ne fait plus partie du graphe G'.

PROCEDURE TrouverSommet(G: IN Graphe; Sommet: IN OUT Type_�l�ment;
                        Trouv�: OUT Boolean);
-- Chercher le sommet Sommet dans le graphe G et retourner la valeur du sommet.
-- Ant�c�dent: le graphe G existe.
-- Cons�quent: si le graphe G contient un sommet Sommet,
--                alors �lt' a la valeur du sommet,
--                et Trouv� est vrai, sinon Trouv� est faux.

FUNCTION Poids(G: Graphe; Sommet1, Sommet2: Type_�l�ment) RETURN Integer;
-- Retourne le poids associ� � l'ar�te entre les deux sommets
-- Sommet1 et Sommet2 dans le graphe G.
-- Ant�c�dent: il y a une ar�te dans le graphe G entre les sommets
--             Sommet1 et Sommet2.
-- Cons�quent: retourne le poids associ� � l'ar�te correspondante.

PROCEDURE TrouverAdjacent(G: IN Graphe;
                          Sommet: IN Type_�l�ment; N: IN Natural;
                          SommetAdjacent: OUT Type_�l�ment;
                          Succ�s: OUT Boolean);
-- Retourne le Ni�me sommet adjacent au sommet Sommet dans le graphe G.
-- Ant�c�dent: le Sommet existe dans le graphe G.
-- Cons�quent: SommetAdjacent = Ni�me sommet adjacent � Sommet et Succ�s vrai
--             ou Succ�s faux si absent.
   
PRIVATE
TYPE Matrice IS ARRAY (Nom_Sommet, Nom_Sommet) OF Integer;
TYPE Type_Sommet IS RECORD
                      Valeur: Type_�l�ment;
                      Pr�sent: Boolean := False;
                    END RECORD;
TYPE Vecteur IS ARRAY (Nom_Sommet) OF Type_Sommet;
TYPE Graphe IS NEW Graphe_Abstrait WITH RECORD
                                          N: Natural := 0;
                                          Ar�tes: Matrice;
                                          Sommets: Vecteur;
                                        END RECORD;
END Graphes.NonOrient�s;
