--          Copyright � 1998 Philippe J. Gabrini
-- D�finition des arbres binaires de recherche g�n�riques, de fa�on � pouvoir
-- les r�utiliser dans un paquetage enfant.
--     Philippe Gabrini      Avril 1998 
GENERIC
  TYPE Type�l�ment IS PRIVATE;
  WITH FUNCTION Comparaison(E1, E2: Type�l�ment) RETURN Integer;
  WITH PROCEDURE Afficher�l�ment(E1: IN Type�l�ment);
PACKAGE ArbresBinRecherche IS

TYPE ArbreBinaireRecherche IS LIMITED PRIVATE;
TYPE ProcTraitement IS ACCESS PROCEDURE(�l�ment: IN Type�l�ment);
-- Proc�dure de traitement utilis�e dans la travers�e de l'arbre.

PROCEDURE SupprimerArbre(Arbre: IN OUT ArbreBinaireRecherche);
-- Toute l'information sur l'Arbre et les donn�es qu'il contient est d�truite.
-- Ant�c�dent: Arbre existe.
-- Cons�quent: Arbre' n'existe pas.

PROCEDURE Ins�rerNoeud(Arbre: IN OUT ArbreBinaireRecherche;
                       �l�ment: IN Type�l�ment);
-- Ins�rer �l�ment � la position voulue dans Arbre selon la clef de
-- �l�ment.  Utiliser Comparaison pour la comparison des clefs.  S'il
-- existait d�j� un noeud avec la m�me clef, on lui donne la valeur �l�ment.
-- Ant�c�dent: Arbre et �l�ment existent.
-- Cons�quent: Arbre' contient un  noeud avec la valeur �l�ment.

PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreBinaireRecherche;
                         �l�ment: IN Type�l�ment);
-- Trouver le noeud avec la clef d'�l�ment et le supprimer de l'Arbre.
-- Utiliser Comparaison pour les comparaisons.  Si aucun noeud ne 
-- poss�de cette clef, Arbre demeure inchang�.
-- Ant�c�dent: Arbre existe.
-- Cons�quent: Arbre' ne contient plus de noeud avec cette clef.

PROCEDURE TraverserArbre(Arbre: IN ArbreBinaireRecherche;
                         Traiter: IN ProcTraitement);
-- Appliquer Traiter � chaque noeud de l'Arbre en ordre infixe.
-- Ant�c�dent: Arbre existe.
-- Cons�quent: on a rendu visite � tous les noeuds de l'arbre et on leur
--             a appliqu� Traiter.

PROCEDURE Rechercher(Arbre: IN ArbreBinaireRecherche;
                     �l�ment: IN OUT Type�l�ment; Succ�s: OUT Boolean);
-- Rechercher un �l�ment dans Arbre identifi� par la clef d'�l�ment.  Utiliser 
-- Comparaison pour les �l�ments et retourner �l�ment pris dans Arbre.
-- Ant�c�dent: Arbre et �l�ment existent.
-- Cons�quent: si clef appara�t dans Arbre Succ�s est True et �l�ment
--             prend la valeur du noeud autrement Succ�s est False.

PROCEDURE AfficherArbre(Arbre: IN ArbreBinaireRecherche;
                        D�calage: IN Natural);
-- Afficher les clefs de l'Arbre avec d�calages en utilisant Afficher�l�ment 
-- pour montrer la structure de l'arbre.
-- Ant�c�dent: Arbre existe.
-- Cons�quent: Arbre est affich� verticalement sur l'�cran avec �l�ments d�cal�s.

PRIVATE
TYPE NoeudArbre;
TYPE ArbreBinaireRecherche IS ACCESS NoeudArbre'CLASS;
-- Pointeur � tous les noeuds de type NoeudArbre et de types d�riv�s.
TYPE NoeudArbre IS TAGGED RECORD  -- pour extension possible
                            �l�ment: Type�l�ment;
                            Gauche, Droite: ArbreBinaireRecherche;
                          END RECORD;
END ArbresBinRecherche;
