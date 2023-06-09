--          Copyright � 1997 Philippe J. Gabrini
WITH EnsemblesFT, Ensembles.Statiques;
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PROCEDURE TesterKruskal IS

TailleEnsemble: CONSTANT Natural := 32;
SUBTYPE Intervalle IS Natural RANGE 0..TailleEnsemble;
TYPE TypeAr�te IS RECORD
                    Ident: Natural := 0;
                    Co�t: Natural := 0;
                    S1, S2: Intervalle := 0;
                  END RECORD;

FUNCTION Compar�s(E1, E2: TypeAr�te) RETURN Integer;
PROCEDURE Afficher(E: IN TypeAr�te);
PROCEDURE MontrerClef(E: IN TypeAr�te);
PACKAGE EnsemblesBase IS NEW Ensembles(Taille_Ensemble_Abstrait => TailleEnsemble,
                                       Type_�l�ment => TypeAr�te,
                                       Comparaison => Compar�s,
                                       Afficher�l�ment => Afficher,
                                       AfficherClef => MontrerClef);
FUNCTION Convertie(�lt: TypeAr�te) RETURN Intervalle;
PACKAGE EnsAr�tes IS NEW EnsemblesBase.Statiques(Type_Clef => Intervalle,
                                                      Clef => Convertie);

PROCEDURE Kruskal(Sommets: IN Natural; Ar�tes: IN EnsAr�tes.Ensemble;
                  Recouvrement: IN OUT EnsAr�tes.Ensemble) IS
EnsembleAr�tes: EnsAr�tes.Ensemble;	-- file de priorit� des ar�tes
Composants: EnsemblesFT.EnsembleFT;
NombreComposants, NouveauComposant: Natural;
�lt: TypeAr�te;
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
  EnsAr�tes.Affecter(EnsembleAr�tes, Ar�tes);	-- copier ensemble des ar�tes
  WHILE NombreComposants > 1 LOOP
    Min(EnsembleAr�tes, �lt); -- ar�te avec Co�t minimum
    EnsAr�tes.Exclure(EnsembleAr�tes, �lt);
    Comp1 := EnsemblesFT.Nom(�lt.S1, Composants);
    Comp2 := EnsemblesFT.Nom(�lt.S2, Composants);
    IF Comp1 /= Comp2 THEN -- ar�te entre diff�rents composants
      EnsemblesFT.Fusionner(Comp1, Comp2, Composants);
      NombreComposants := NombreComposants - 1;
      EnsAr�tes.Inclure(Recouvrement, �lt); -- la garder
    END IF;
  END LOOP;
END Kruskal;

BEGIN
  NULL;
END TesterKruskal;
