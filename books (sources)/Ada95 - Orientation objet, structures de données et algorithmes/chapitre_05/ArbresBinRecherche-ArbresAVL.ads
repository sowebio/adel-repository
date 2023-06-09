--          Copyright © 1998 Philippe J. Gabrini
-- Arbres binaires de recherche équilibrés; les procédures d'insertion et de 
-- suppression d'un élément doivent rééquilibrer l'arbre si nécessaire.
--     Philippe Gabrini     Juin 1998 
GENERIC  -- un paquetage enfant d'un paquetage générique doit être générique.
PACKAGE ArbresBinRecherche.ArbresAVL IS

SUBTYPE ArbreAVL IS ArbreBinaireRecherche; -- pointeur de classe équivalent

PROCEDURE DétruireArbre(Arbre: IN OUT ArbreAVL);
-- Toute l'information sur l'Arbre et les données qu'il contient est détruite.

PROCEDURE InsérerNoeud(Arbre: IN OUT ArbreAVL;
                       Élément: IN TypeÉlément);
-- Insérer Élément dans Arbre. La clef se trouve dans Élément.

PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreAVL; Élément: IN TypeÉlément);
-- Trouver l'élément de l'Arbre qui possède la clef d'Élément et le supprimer
-- de l'Arbre.

PRIVATE
TYPE TypeNoeud IS NEW NoeudArbre WITH RECORD  -- ajout d'un champ
	                                    Équil: Integer;
	                                  END RECORD;
-- Comme ArbreAVL est un pointeur de classe, pour accéder au champ Équil
-- il sera nécessaire d'utiliser une conversion de l'objet pointé par une
-- variable de type ArbreAVL: MonArbre.ALL pour spécifier qu'il s'agit bien
-- d'un objet du type étendu: TypeNoeud(MonArbre.ALL).Équil.

END ArbresBinRecherche.ArbresAVL;


