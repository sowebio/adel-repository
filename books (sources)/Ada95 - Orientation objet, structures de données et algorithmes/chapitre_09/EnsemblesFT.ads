--          Copyright © 1998 Philippe J. Gabrini
PACKAGE EnsemblesFT IS

N: CONSTANT Natural := 15;
TYPE EnsembleFT IS PRIVATE;

SUBTYPE TypeNom IS Natural RANGE 1..N;
SUBTYPE TypeÉlément IS Natural RANGE 1..N;

PROCEDURE Initialiser(Comp: IN TypeNom; Élt: IN TypeÉlément;
                      Composants: IN OUT EnsembleFT);
-- Initialiser Comp à un ensemble ne comprenant que Élt.
-- Antécédent: l'ensemble Composants existe.
-- Conséquent: Comp est le nom d'un composant dans Composants
--             dont fait partie Élt.

PROCEDURE Fusionner(Comp1, Comp2: IN TypeNom; Composants: IN OUT EnsembleFT);
-- Fusionner Comp1 et Comp2 en appelant le résultat Comp1 ou Comp2
-- arbitrairement.
-- Antécédent: l'ensemble Composants existe,
--             les composants Comp1 et Comp2 sont disjoints.
-- Conséquent: les composants Comp1 et Comp2 sont fusionnés dans l'ensemble
--             des composants connectés Composants, le résultat est appelé
--             Comp1 ou Comp2 arbitrairement.

FUNCTION Nom(Élt: TypeÉlément; Composants: EnsembleFT) RETURN TypeNom;
-- Retourne le nom de l'ensemble dont Élt est membre.
-- Antécédent: Élt n'appartient qu'à un seul composant.
-- Conséquent: retourne le nom du composant de Composants
--             dont Élt est membre.

PROCEDURE Vider(Composants: IN OUT EnsembleFT);
-- Vider l'ensemble Composants.
-- Antécédent: l'ensemble Composants existe.
-- Conséquent: l'ensemble Composants est vide.

PRIVATE
SUBTYPE Pointeur IS Natural RANGE 0..N;
TYPE TypeComp IS RECORD
                   NombreDÉléments: Pointeur;
                   PremierÉlément: Pointeur;
                 END RECORD;
TYPE TypeÉlts IS RECORD
                   NomEns: TypeNom;
                   ProchainÉlt: Pointeur;
                 END RECORD;
TYPE TableComp IS ARRAY(1..N) OF TypeComp;
TYPE TableÉlts IS ARRAY(1..N) OF TypeÉlts;
TYPE EnsembleFT IS RECORD
                   Composants: TableComp;
                   Éléments: TableÉlts;
                 END RECORD;

END EnsemblesFT;
