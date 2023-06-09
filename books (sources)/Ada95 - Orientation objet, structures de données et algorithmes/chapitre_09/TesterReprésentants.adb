--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
WITH Ensembles.Dynamiques;
-- Trouver les représentants distincts d'un ensemble de sous-ensembles
-- tel que l'a défini P. Hall en 1935.

PROCEDURE TesterReprésentants IS
N: CONSTANT Natural := 25;
SUBTYPE Intervalle IS Natural RANGE 1..N;
Maximum: CONSTANT Natural := 25;

FUNCTION Comparés(E1, E2: IN Intervalle) RETURN Integer;
PROCEDURE Afficher(E: IN Intervalle);
PROCEDURE MontrerClef(E: IN Intervalle);
PACKAGE EnsemblesBase IS NEW Ensembles(Type_Élément => Intervalle,
                                       Comparaison => Comparés,
                                       AfficherÉlément => Afficher,
                                       AfficherClef => MontrerClef);
SUBTYPE Type_Clef IS Intervalle;
FUNCTION Convertie(Élt: IN Intervalle) RETURN Type_Clef;
PACKAGE EnsemblesRep IS NEW EnsemblesBase.Dynamiques(Type_Clef => Intervalle,
                                                    Clef => Convertie);
SUBTYPE IntervallePaire IS Natural RANGE 0..Maximum;
TYPE TypePaire IS RECORD
                    Premier, Second: IntervallePaire;
                  END RECORD;
FUNCTION ComparésP(E1, E2: IN TypePaire) RETURN Integer;
PROCEDURE AfficherP(E: IN TypePaire);
PROCEDURE MontrerClefP(E: IN TypePaire);
PACKAGE EnsBasePaires IS NEW Ensembles(Type_Élément => TypePaire,
                                       Comparaison => ComparésP,
                                       AfficherÉlément => AfficherP,
                                       AfficherClef => MontrerClefP);
FUNCTION ConvertieP(Élt: IN TypePaire) RETURN IntervallePaire;
PACKAGE EnsemblesPaires IS NEW EnsBasePaires.Dynamiques(Type_Clef => IntervallePaire,
                                                       Clef => ConvertieP);

TYPE TableEnsembles IS ARRAY (1..16) OF EnsemblesRep.Ensemble;
    
PROCEDURE RepDistincts(NbEnsembles: IN Natural; Ens: IN OUT TableEnsembles) IS

Index, Minimum, NuméroEnsemble, Valeur, NbDansEns: Natural;
Continuer: Boolean;
Représentants: EnsemblesPaires.Ensemble;
PasReprésentants: EnsemblesRep.Ensemble;
Paire: TypePaire;

BEGIN
  FOR Index IN 1..NbEnsembles LOOP
    EnsemblesRep.Inclure(PasReprésentants, Index);	-- tous les sous-ensembles
  END LOOP;
  LOOP
    Minimum := Maximum + 1;
    FOR Index IN 1..NbEnsembles LOOP
      -- choisir ensemble sans représentant avec la plus petite cardinalité
      IF EnsemblesRep.Membre(Index, PasReprésentants) THEN
        NbDansEns := EnsemblesRep.Cardinalité(Ens(Index));
        IF NbDansEns < Minimum THEN
          Minimum := NbDansEns;
          NuméroEnsemble := Index;
        END IF;
      END IF;
    END LOOP;
    -- choisir un membre dans l'ensemble avec le plus petit nombre d'éléments
    Index := 1;
    Continuer := True;
    WHILE (Index <= Maximum) AND Continuer LOOP
      IF EnsemblesRep.Membre(Index, Ens(NuméroEnsemble)) THEN
        Valeur := Index;
        Continuer := False;
      END IF;
      Index := Index + 1;
    END LOOP;
    -- créer paire
    Paire.Premier := NuméroEnsemble;
    Paire.Second := Valeur;
    EnsemblesPaires.Inclure(Représentants, Paire);
    -- garder nouveau représentant
    EnsemblesRep.Exclure(PasReprésentants, NuméroEnsemble);
    -- éliminer ensemble choisi
    FOR Index IN 1..NbEnsembles LOOP
      -- éliminer valeur du reste de EnsemblesRep
      IF EnsemblesRep.Membre(Index, PasReprésentants) THEN
        EnsemblesRep.Exclure(Ens(Index), Valeur);
      END IF;
    END LOOP;
    EXIT WHEN EnsemblesRep.Vide(PasReprésentants) OR (Minimum = 0);
  END LOOP;
  -- Minimum est nul si des sous-ensembles de EnsemblesRep sans représentant
  -- sont vides
  IF EnsemblesRep.Vide(PasReprésentants) THEN
    AfficherEnsemble(Représentants);
  ELSE
    Ada.Text_IO.Put(Item => " pas de représentants distincts possibles ");
  END IF;
END RepDistincts;

NbEnsembles: Natural;
BEGIN
  NbEnsembles := 5;
END TesterReprésentants;
