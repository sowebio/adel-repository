--          Copyright © 1997 Philippe J. Gabrini
PACKAGE RéÉquilibrage IS

TYPE ArbreBinaireRecherche IS LIMITED PRIVATE;

PROCEDURE Rééquilibrer(Arbre: IN OUT ArbreBinaireRecherche);
-- Étant donné un Arbre binaire de recherche, retourner un arbre binaire 
-- de recherche équilibré.

PROCEDURE ConstruireArbre(Clefs: IN String; OrdreNum: IN Boolean;
                          NumMax: IN Natural;  Arbre: IN OUT ArbreBinaireRecherche);
-- Construire un arbre binaire de recherche avec une clef en deux parties: 
-- un caractère pris séquentiellement dans Clefs et un numéro allant de 0 à NumMax.
-- Si OrdreNum est Vrai, on insère les clefs avec des numéros changeant le plus
-- rapidement; sinon les caractères changent le plus rapidement.
-- Par exemple: si Clefs vaut "ABC" et si NumMax = 2 alors, si OrdreNum est vrai
-- l'ordre d'insertion est A0, A1, A2, B0, B1, B2, C0, C1, C2. Sinon, l'ordre 
-- d'insertion est A0, B0, C0, A1, B1, C1, A2, B2, C2.
 
PROCEDURE TraverserInfixe(Arbre: IN ArbreBinaireRecherche);
-- Effectuer une traversée en ordre infixe de Arbre et afficher la clef de chaque noeud.

PROCEDURE AfficherArbre(Arbre: IN ArbreBinaireRecherche);
-- Afficher l'arbre au complet de gauche à droite, avec la racine à gauche.

PRIVATE
NoeudsMax: CONSTANT Natural := 100;   -- nombre maximum de noeuds dans l'arbre

TYPE NoeudArbre IS RECORD
                     Caractère: Character;
                     Numéro: Natural;
                     Gauche, Droite: ArbreBinaireRecherche;
                   END RECORD;
TYPE ArbreBinaireRecherche IS ACCESS NoeudArbre;
END RéÉquilibrage;

