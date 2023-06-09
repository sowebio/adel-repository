--          Copyright © 1997 Philippe J. Gabrini
WITH EnsemblesFT, Ensembles.Statiques;
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PROCEDURE TesterKruskal IS

TailleEnsemble: CONSTANT Natural := 32;
SUBTYPE Intervalle IS Natural RANGE 0..TailleEnsemble;
TYPE TypeArête IS RECORD
                    Ident: Natural := 0;
                    Coût: Natural := 0;
                    S1, S2: Intervalle := 0;
                  END RECORD;

FUNCTION Comparés(E1, E2: TypeArête) RETURN Integer;
PROCEDURE Afficher(E: IN TypeArête);
PROCEDURE MontrerClef(E: IN TypeArête);
PACKAGE EnsemblesBase IS NEW Ensembles(Taille_Ensemble_Abstrait => TailleEnsemble,
                                       Type_Élément => TypeArête,
                                       Comparaison => Comparés,
                                       AfficherÉlément => Afficher,
                                       AfficherClef => MontrerClef);
FUNCTION Convertie(Élt: TypeArête) RETURN Intervalle;
PACKAGE EnsArêtes IS NEW EnsemblesBase.Statiques(Type_Clef => Intervalle,
                                                      Clef => Convertie);

PROCEDURE Kruskal(Sommets: IN Natural; Arêtes: IN EnsArêtes.Ensemble;
                  Recouvrement: IN OUT EnsArêtes.Ensemble) IS
EnsembleArêtes: EnsArêtes.Ensemble;	-- file de priorité des arêtes
Composants: EnsemblesFT.EnsembleFT;
NombreComposants, NouveauComposant: Natural;
Élt: TypeArête;
Comp1, Comp2: Natural;

BEGIN
  -- initialiser
  NouveauComposant := 0;
  NombreComposants := Sommets;
  FOR Sommet1 IN 1..Sommets LOOP
  -- mettre chaque sommet tout seul dans un composant
    NouveauComposant := NouveauComposant + 1;
    EnsemblesFT.Initialiser(NouveauComposant, Sommet1, Composants);
  END LOOP;
  EnsArêtes.Affecter(EnsembleArêtes, Arêtes);	-- copier ensemble des arêtes
  WHILE NombreComposants > 1 LOOP
    Min(EnsembleArêtes, Élt); -- arête avec Coût minimum
    EnsArêtes.Exclure(EnsembleArêtes, Élt);
    Comp1 := EnsemblesFT.Nom(Élt.S1, Composants);
    Comp2 := EnsemblesFT.Nom(Élt.S2, Composants);
    IF Comp1 /= Comp2 THEN -- arête entre différents composants
      EnsemblesFT.Fusionner(Comp1, Comp2, Composants);
      NombreComposants := NombreComposants - 1;
      EnsArêtes.Inclure(Recouvrement, Élt); -- la garder
    END IF;
  END LOOP;
END Kruskal;

BEGIN
  NULL;
END TesterKruskal;
