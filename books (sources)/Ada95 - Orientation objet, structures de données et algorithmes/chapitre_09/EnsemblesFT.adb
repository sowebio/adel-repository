--          Copyright � 1998 Philippe J. Gabrini
PACKAGE BODY EnsemblesFT IS

PROCEDURE Initialiser(Comp: IN TypeNom; �lt: IN Type�l�ment;
                      Composants: IN OUT EnsembleFT) IS
-- Initialiser Comp � un ensemble ne contenant que �lt.
BEGIN
  Composants.�l�ments(�lt).NomEns := Comp;
  Composants.�l�ments(�lt).Prochain�lt := 0;
  Composants.Composants(Comp).NombreD�l�ments := 1;
  Composants.Composants(Comp).Premier�l�ment := �lt;
END Initialiser;

PROCEDURE Fusionner(Comp1, Comp2: IN TypeNom; Composants: IN OUT EnsembleFT) IS
-- Fusionner Comp1 et Comp2 en appelant le r�sultat Comp1 ou Comp2
-- arbitrairement.

  PROCEDURE Fondre(E1, E2: IN TypeNom) IS
  -- Fusionner E1 dans E2, E1 et E2 sont membres de Composants
  Index: Natural RANGE 0..N;
  BEGIN
    Index := Composants.Composants(E1).Premier�l�ment;
    LOOP	-- attacher les �l�ments de E1 au nom E2
      Composants.�l�ments(Index).NomEns := E2;
      IF Composants.�l�ments(Index).Prochain�lt /= 0 THEN
        -- prochain �l�ment
        Index := Composants.�l�ments(Index).Prochain�lt;
      END IF;
      EXIT WHEN Composants.�l�ments(Index).Prochain�lt = 0;
    END LOOP;
    -- ajouter la liste E2 � la liste E1 et appeler le r�sultat E2
    Composants.�l�ments(Index).NomEns := E2;	-- dernier �l�ment de E1
    Composants.�l�ments(Index).Prochain�lt :=
                        Composants.Composants(E2).Premier�l�ment;
    Composants.Composants(E2).Premier�l�ment :=
                        Composants.Composants(E1).Premier�l�ment;
    Composants.Composants(E2).NombreD�l�ments :=
                        Composants.Composants(E2).NombreD�l�ments + 
    				    Composants.Composants(E1).NombreD�l�ments;
    Composants.Composants(E1).NombreD�l�ments := 0;
    -- E1 n'existe plus
    Composants.Composants(E1).Premier�l�ment := 0;
  END Fondre;

BEGIN
  IF Composants.Composants(Comp1).NombreD�l�ments 
       > Composants.Composants(Comp2).NombreD�l�ments THEN
    Fondre(Comp2, Comp1); -- fusionner Comp2 dans Comp1
  ELSE
    Fondre(Comp1, Comp2); -- fusionner Comp1 dans Comp2
  END IF;
END Fusionner;

FUNCTION Nom(�lt: Type�l�ment; Composants: EnsembleFT) RETURN TypeNom IS
-- Retourne le nom de l'ensemble dont �lt est membre.
BEGIN 
  RETURN Composants.�l�ments(�lt).NomEns;
END Nom;

PROCEDURE Vider(Composants: IN OUT EnsembleFT) IS
-- Vider ensemble Composants
BEGIN
  FOR Index IN 1..N LOOP
	Composants.Composants(Index).NombreD�l�ments := 0;
	Composants.Composants(Index).Premier�l�ment := 0;
	Composants.�l�ments(Index).NomEns := 1;
	Composants.�l�ments(Index).Prochain�lt := 0;
  END LOOP;
END Vider;

END EnsemblesFT;
