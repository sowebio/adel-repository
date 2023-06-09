--          Copyright © 1998 Philippe J. Gabrini
GENERIC
  Ordre: IN Natural := 2;
  TYPE TypeÉlément IS PRIVATE;
  TYPE TypeClef IS PRIVATE;
  WITH FUNCTION Comparaison(E1, E2: TypeÉlément) RETURN Integer;
  WITH PROCEDURE AfficherÉlément(E1: IN TypeÉlément);
PACKAGE ArbresB IS

TYPE ArbreB IS LIMITED PRIVATE;

TYPE Traitement IS ACCESS PROCEDURE(Élément: IN TypeÉlément);
-- Procédure de traitement utilisée dans la traversée de l'arbre

PROCEDURE DétruireArbre(Arbre: IN OUT ArbreB);
-- Toute l'information de l'Arbre et les données qu'il contient 
-- sont supprimées.

PROCEDURE InsérerNoeud(Arbre: IN OUT ArbreB; Élément: IN TypeÉlément);
-- Insérer Élément dans Arbre; la clef se trouve dans Élément.

PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreB; Élément: IN TypeÉlément);
-- Trouver le noeud de l'arbre possédant la clef d'Élément et l'enlever.

PROCEDURE Traverser(Arbre: IN ArbreB; Proc_Traiter: IN Traitement);
-- Appliquer Proc_Traiter à chaque noeud de l'arbre dans l'ordre infixe. 

PROCEDURE Chercher(Arbre: IN ArbreB; Élément: IN OUT TypeÉlément;
                   Succès: OUT Boolean);
-- Recherche Élément dans l'Arbre. Si trouvé, Succès est Vrai et Élément
-- contient l'information du noeud, sinon Succès est Faux.

PROCEDURE AfficherArbre(Arbre: IN ArbreB; Décalage: IN Natural);
-- Afficher l'arbre avec décalages pour montrer la structure.

PRIVATE
ClefsMin: CONSTANT Natural := Ordre; -- nombre de clefs minimum dans un noeud
ClefsMax: CONSTANT Natural := ClefsMin * 2; 
-- nombre de clefs maximum dans un noeud

TYPE TypeNoeud;
TYPE ArbreB IS ACCESS TypeNoeud;
TYPE Item IS RECORD
               Élément: TypeÉlément;
               Pointeur: ArbreB;
             END RECORD;
SUBTYPE IntervalleClefs IS Natural RANGE 0..ClefsMax;
TYPE Vecteur IS ARRAY (1..ClefsMax) OF Item;
TYPE TypeNoeud IS RECORD                         -- noeud multi-clefs de l'arbre
      	            PremierPointeur: ArbreB;     -- pointeur extrême gauche
	                Items: Vecteur;
	                Dernier: IntervalleClefs := 0; -- dernière position utilisée
	              END RECORD;
END ArbresB;


