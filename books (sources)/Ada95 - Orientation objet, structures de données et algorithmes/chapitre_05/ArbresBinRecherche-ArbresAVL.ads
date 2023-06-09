--          Copyright � 1998 Philippe J. Gabrini
-- Arbres binaires de recherche �quilibr�s; les proc�dures d'insertion et de 
-- suppression d'un �l�ment doivent r��quilibrer l'arbre si n�cessaire.
--     Philippe Gabrini     Juin 1998 
GENERIC  -- un paquetage enfant d'un paquetage g�n�rique doit �tre g�n�rique.
PACKAGE ArbresBinRecherche.ArbresAVL IS

SUBTYPE ArbreAVL IS ArbreBinaireRecherche; -- pointeur de classe �quivalent

PROCEDURE D�truireArbre(Arbre: IN OUT ArbreAVL);
-- Toute l'information sur l'Arbre et les donn�es qu'il contient est d�truite.

PROCEDURE Ins�rerNoeud(Arbre: IN OUT ArbreAVL;
                       �l�ment: IN Type�l�ment);
-- Ins�rer �l�ment dans Arbre. La clef se trouve dans �l�ment.

PROCEDURE SupprimerNoeud(Arbre: IN OUT ArbreAVL; �l�ment: IN Type�l�ment);
-- Trouver l'�l�ment de l'Arbre qui poss�de la clef d'�l�ment et le supprimer
-- de l'Arbre.

PRIVATE
TYPE TypeNoeud IS NEW NoeudArbre WITH RECORD  -- ajout d'un champ
	                                    �quil: Integer;
	                                  END RECORD;
-- Comme ArbreAVL est un pointeur de classe, pour acc�der au champ �quil
-- il sera n�cessaire d'utiliser une conversion de l'objet point� par une
-- variable de type ArbreAVL: MonArbre.ALL pour sp�cifier qu'il s'agit bien
-- d'un objet du type �tendu: TypeNoeud(MonArbre.ALL).�quil.

END ArbresBinRecherche.ArbresAVL;


