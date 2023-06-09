--          Copyright � 1998 Philippe J. Gabrini
GENERIC
  TYPE Type�l�ment IS PRIVATE;
  WITH FUNCTION �l�ments�gaux(E1, E2: IN Type�l�ment) RETURN Boolean;
  WITH FUNCTION Inf�rieur(E1, E2: IN Type�l�ment) RETURN Boolean;
  WITH PROCEDURE Afficher�l�ment(E: IN Type�l�ment);
PACKAGE ArbresBinRechFil�s IS
-- Ce module pr�sente la r�alisation d'arbres binaires de recherche qui 
-- utilise un arbre doublement "fil�".  L'utilisation de "fils" permet
-- non seulement de traverser l'arbre de fa�on it�rative dans toutes les
-- directions, mais aussi de faire des travers�es incr�mentales, un
-- noeud � la fois.

TYPE Arbre_Bin_Rech_Fil� IS PRIVATE;

FUNCTION ArbreVide(Arbre: Arbre_Bin_Rech_Fil�) RETURN Boolean;
-- Retourne Vrai si l'Arbre est vide, et Faux autrement.

PROCEDURE D�truireArbre(Arbre: IN OUT Arbre_Bin_Rech_Fil�);
-- D�truit toute la structure d'Arbre.

FUNCTION Pr�d�cesseur(Arbre: Arbre_Bin_Rech_Fil�) RETURN Arbre_Bin_Rech_Fil�;
-- Retourne le pr�d�cesseur d'un noeud donn� d'Arbre.

FUNCTION Successeur(Arbre: Arbre_Bin_Rech_Fil�) RETURN Arbre_Bin_Rech_Fil�;
-- Retourne le successeur d'un noeud donn� d'Arbre.

FUNCTION Noeud(Arbre: Arbre_Bin_Rech_Fil�; Clef: Type�l�ment)
              RETURN Arbre_Bin_Rech_Fil�;
-- Retourne un pointeur au noeud qui poss�de la Clef donn�e; si la Clef n'est
-- pas dans Arbre, on retourne NULL.

PROCEDURE Ins�rerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Fil�;
                       �l�ment: IN Type�l�ment);
-- Ins�re �l�ment dans Arbre; si �l�ment existe d�j� dans l'Arbre, ce dernier
-- n'est pas modifi�. 

PROCEDURE SupprimerNoeud(Arbre: IN OUT Arbre_Bin_Rech_Fil�;
                         �l�ment: IN Type�l�ment);
-- Supprime le noeud de l'Arbre ayant la m�me clef qu'�l�ment; s'il n'y a pas
-- de noeud poss�dant la m�me clef, Arbre n'est pas modifi�. 

PROCEDURE TraverserAvant(Arbre: IN Arbre_Bin_Rech_Fil�);
-- Effectue une travers�e infixe de l'Arbre et affiche les �l�ments en ordre  
-- ascendant des clefs; on utilise Afficher�l�ment et on s�pare les valeurs 
-- par un espace.

PROCEDURE TraverserArri�re(Arbre: IN Arbre_Bin_Rech_Fil�);
-- Effectue une travers�e infixe de l'Arbre et affiche les �l�ments en ordre  
-- descendant des clefs; on utilise Afficher�l�ment et on s�pare les valeurs 
-- par un espace.

PROCEDURE AfficherNoeud(Arbre: IN Arbre_Bin_Rech_Fil�);
-- Affiche la valeur de l'�l�ment au noeud Arbre.

PROCEDURE AfficherArbre(Arbre: IN Arbre_Bin_Rech_Fil�);
-- Affiche l'Arbre de gauche � droite avec la racine � gauche.

PRIVATE
TYPE NoeudArbre IS RECORD
         			 �l�ment: Type�l�ment;
         			 Gauche, Droite: Arbre_Bin_Rech_Fil�;
         			 FilGauche, FilDroit: Boolean;
      		       END RECORD;
TYPE Arbre_Bin_Rech_Fil� IS ACCESS NoeudArbre;
END ArbresBinRechFil�s;

