--          Copyright � 1998 Philippe J. Gabrini
GENERIC
  Ordre: IN Natural := 2;
  TYPE Type�l�ment IS PRIVATE;
  TYPE TypeClef IS PRIVATE;
  WITH FUNCTION Comparaison(E1, E2: Type�l�ment) RETURN Integer;
  WITH PROCEDURE Afficher�l�ment(E1: IN Type�l�ment);
PACKAGE ArbresB IS

TYPE ArbreB IS LIMITED PRIVATE;

TYPE Traitement IS ACCESS PROCEDURE(�l�ment: IN Type�l�ment);
-- Proc�dure de traitement utilis�e dans la travers�e de l'arbre

PROCEDURE D�truireArbre(Arbre: IN OUT ArbreB);
-- Toute l'information de l'Arbre et les donn�es qu'il contient 
-- sont supprim�es.

PROCEDURE Ins�rerNoeud(Arbre: IN OUT ArbreB; �l�ment: IN Type�l�ment);
-- Ins�rer �l�ment dans Arbre; la clef se trouve dans �l�ment.

PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreB; �l�ment: IN Type�l�ment);
-- Trouver le noeud de l'arbre poss�dant la clef d'�l�ment et l'enlever.

PROCEDURE Traverser(Arbre: IN ArbreB; Proc_Traiter: IN Traitement);
-- Appliquer Proc_Traiter � chaque noeud de l'arbre dans l'ordre infixe. 

PROCEDURE Chercher(Arbre: IN ArbreB; �l�ment: IN OUT Type�l�ment;
                   Succ�s: OUT Boolean);
-- Recherche �l�ment dans l'Arbre. Si trouv�, Succ�s est Vrai et �l�ment
-- contient l'information du noeud, sinon Succ�s est Faux.

PROCEDURE AfficherArbre(Arbre: IN ArbreB; D�calage: IN Natural);
-- Afficher l'arbre avec d�calages pour montrer la structure.

PRIVATE
ClefsMin: CONSTANT Natural := Ordre; -- nombre de clefs minimum dans un noeud
ClefsMax: CONSTANT Natural := ClefsMin * 2; 
-- nombre de clefs maximum dans un noeud

TYPE TypeNoeud;
TYPE ArbreB IS ACCESS TypeNoeud;
TYPE Item IS RECORD
               �l�ment: Type�l�ment;
               Pointeur: ArbreB;
             END RECORD;
SUBTYPE IntervalleClefs IS Natural RANGE 0..ClefsMax;
TYPE Vecteur IS ARRAY (1..ClefsMax) OF Item;
TYPE TypeNoeud IS RECORD                         -- noeud multi-clefs de l'arbre
      	            PremierPointeur: ArbreB;     -- pointeur extr�me gauche
	                Items: Vecteur;
	                Dernier: IntervalleClefs := 0; -- derni�re position utilis�e
	              END RECORD;
END ArbresB;


