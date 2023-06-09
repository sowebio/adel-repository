--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
WITH Ensembles.Dynamiques;
-- Trouver les repr�sentants distincts d'un ensemble de sous-ensembles
-- tel que l'a d�fini P. Hall en 1935.

PROCEDURE TesterRepr�sentants IS
N: CONSTANT Natural := 25;
SUBTYPE Intervalle IS Natural RANGE 1..N;
Maximum: CONSTANT Natural := 25;

FUNCTION Compar�s(E1, E2: IN Intervalle) RETURN Integer;
PROCEDURE Afficher(E: IN Intervalle);
PROCEDURE MontrerClef(E: IN Intervalle);
PACKAGE EnsemblesBase IS NEW Ensembles(Type_�l�ment => Intervalle,
                                       Comparaison => Compar�s,
                                       Afficher�l�ment => Afficher,
                                       AfficherClef => MontrerClef);
SUBTYPE Type_Clef IS Intervalle;
FUNCTION Convertie(�lt: IN Intervalle) RETURN Type_Clef;
PACKAGE EnsemblesRep IS NEW EnsemblesBase.Dynamiques(Type_Clef => Intervalle,
                                                    Clef => Convertie);
SUBTYPE IntervallePaire IS Natural RANGE 0..Maximum;
TYPE TypePaire IS RECORD
                    Premier, Second: IntervallePaire;
                  END RECORD;
FUNCTION Compar�sP(E1, E2: IN TypePaire) RETURN Integer;
PROCEDURE AfficherP(E: IN TypePaire);
PROCEDURE MontrerClefP(E: IN TypePaire);
PACKAGE EnsBasePaires IS NEW Ensembles(Type_�l�ment => TypePaire,
                                       Comparaison => Compar�sP,
                                       Afficher�l�ment => AfficherP,
                                       AfficherClef => MontrerClefP);
FUNCTION ConvertieP(�lt: IN TypePaire) RETURN IntervallePaire;
PACKAGE EnsemblesPaires IS NEW EnsBasePaires.Dynamiques(Type_Clef => IntervallePaire,
                                                       Clef => ConvertieP);

TYPE TableEnsembles IS ARRAY (1..16) OF EnsemblesRep.Ensemble;
    
PROCEDURE RepDistincts(NbEnsembles: IN Natural; Ens: IN OUT TableEnsembles) IS

Index, Minimum, Num�roEnsemble, Valeur, NbDansEns: Natural;
Continuer: Boolean;
Repr�sentants: EnsemblesPaires.Ensemble;
PasRepr�sentants: EnsemblesRep.Ensemble;
Paire: TypePaire;

BEGIN
  FOR Index IN 1..NbEnsembles LOOP
    EnsemblesRep.Inclure(PasRepr�sentants, Index);	-- tous les sous-ensembles
  END LOOP;
  LOOP
    Minimum := Maximum + 1;
    FOR Index IN 1..NbEnsembles LOOP
      -- choisir ensemble sans repr�sentant avec la plus petite cardinalit�
      IF EnsemblesRep.Membre(Index, PasRepr�sentants) THEN
        NbDansEns := EnsemblesRep.Cardinalit�(Ens(Index));
        IF NbDansEns < Minimum THEN
          Minimum := NbDansEns;
          Num�roEnsemble := Index;
        END IF;
      END IF;
    END LOOP;
    -- choisir un membre dans l'ensemble avec le plus petit nombre d'�l�ments
    Index := 1;
    Continuer := True;
    WHILE (Index <= Maximum) AND Continuer LOOP
      IF EnsemblesRep.Membre(Index, Ens(Num�roEnsemble)) THEN
        Valeur := Index;
        Continuer := False;
      END IF;
      Index := Index + 1;
    END LOOP;
    -- cr�er paire
    Paire.Premier := Num�roEnsemble;
    Paire.Second := Valeur;
    EnsemblesPaires.Inclure(Repr�sentants, Paire);
    -- garder nouveau repr�sentant
    EnsemblesRep.Exclure(PasRepr�sentants, Num�roEnsemble);
    -- �liminer ensemble choisi
    FOR Index IN 1..NbEnsembles LOOP
      -- �liminer valeur du reste de EnsemblesRep
      IF EnsemblesRep.Membre(Index, PasRepr�sentants) THEN
        EnsemblesRep.Exclure(Ens(Index), Valeur);
      END IF;
    END LOOP;
    EXIT WHEN EnsemblesRep.Vide(PasRepr�sentants) OR (Minimum = 0);
  END LOOP;
  -- Minimum est nul si des sous-ensembles de EnsemblesRep sans repr�sentant
  -- sont vides
  IF EnsemblesRep.Vide(PasRepr�sentants) THEN
    AfficherEnsemble(Repr�sentants);
  ELSE
    Ada.Text_IO.Put(Item => " pas de repr�sentants distincts possibles ");
  END IF;
END RepDistincts;

NbEnsembles: Natural;
BEGIN
  NbEnsembles := 5;
END TesterRepr�sentants;
