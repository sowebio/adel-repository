--          Copyright � 1998 Philippe J. Gabrini
PACKAGE EnsemblesFT IS

N: CONSTANT Natural := 15;
TYPE EnsembleFT IS PRIVATE;

SUBTYPE TypeNom IS Natural RANGE 1..N;
SUBTYPE Type�l�ment IS Natural RANGE 1..N;

PROCEDURE Initialiser(Comp: IN TypeNom; �lt: IN Type�l�ment;
                      Composants: IN OUT EnsembleFT);
-- Initialiser Comp � un ensemble ne comprenant que �lt.
-- Ant�c�dent: l'ensemble Composants existe.
-- Cons�quent: Comp est le nom d'un composant dans Composants
--             dont fait partie �lt.

PROCEDURE Fusionner(Comp1, Comp2: IN TypeNom; Composants: IN OUT EnsembleFT);
-- Fusionner Comp1 et Comp2 en appelant le r�sultat Comp1 ou Comp2
-- arbitrairement.
-- Ant�c�dent: l'ensemble Composants existe,
--             les composants Comp1 et Comp2 sont disjoints.
-- Cons�quent: les composants Comp1 et Comp2 sont fusionn�s dans l'ensemble
--             des composants connect�s Composants, le r�sultat est appel�
--             Comp1 ou Comp2 arbitrairement.

FUNCTION Nom(�lt: Type�l�ment; Composants: EnsembleFT) RETURN TypeNom;
-- Retourne le nom de l'ensemble dont �lt est membre.
-- Ant�c�dent: �lt n'appartient qu'� un seul composant.
-- Cons�quent: retourne le nom du composant de Composants
--             dont �lt est membre.

PROCEDURE Vider(Composants: IN OUT EnsembleFT);
-- Vider l'ensemble Composants.
-- Ant�c�dent: l'ensemble Composants existe.
-- Cons�quent: l'ensemble Composants est vide.

PRIVATE
SUBTYPE Pointeur IS Natural RANGE 0..N;
TYPE TypeComp IS RECORD
                   NombreD�l�ments: Pointeur;
                   Premier�l�ment: Pointeur;
                 END RECORD;
TYPE Type�lts IS RECORD
                   NomEns: TypeNom;
                   Prochain�lt: Pointeur;
                 END RECORD;
TYPE TableComp IS ARRAY(1..N) OF TypeComp;
TYPE Table�lts IS ARRAY(1..N) OF Type�lts;
TYPE EnsembleFT IS RECORD
                   Composants: TableComp;
                   �l�ments: Table�lts;
                 END RECORD;

END EnsemblesFT;
