--          Copyright © 1998 Philippe J. Gabrini
PACKAGE BODY EnsemblesFT IS

PROCEDURE Initialiser(Comp: IN TypeNom; Élt: IN TypeÉlément;
                      Composants: IN OUT EnsembleFT) IS
-- Initialiser Comp à un ensemble ne contenant que Élt.
BEGIN
  Composants.Éléments(Élt).NomEns := Comp;
  Composants.Éléments(Élt).ProchainÉlt := 0;
  Composants.Composants(Comp).NombreDÉléments := 1;
  Composants.Composants(Comp).PremierÉlément := Élt;
END Initialiser;

PROCEDURE Fusionner(Comp1, Comp2: IN TypeNom; Composants: IN OUT EnsembleFT) IS
-- Fusionner Comp1 et Comp2 en appelant le résultat Comp1 ou Comp2
-- arbitrairement.

  PROCEDURE Fondre(E1, E2: IN TypeNom) IS
  -- Fusionner E1 dans E2, E1 et E2 sont membres de Composants
  Index: Natural RANGE 0..N;
  BEGIN
    Index := Composants.Composants(E1).PremierÉlément;
    LOOP	-- attacher les éléments de E1 au nom E2
      Composants.Éléments(Index).NomEns := E2;
      IF Composants.Éléments(Index).ProchainÉlt /= 0 THEN
        -- prochain élément
        Index := Composants.Éléments(Index).ProchainÉlt;
      END IF;
      EXIT WHEN Composants.Éléments(Index).ProchainÉlt = 0;
    END LOOP;
    -- ajouter la liste E2 à la liste E1 et appeler le résultat E2
    Composants.Éléments(Index).NomEns := E2;	-- dernier élément de E1
    Composants.Éléments(Index).ProchainÉlt :=
                        Composants.Composants(E2).PremierÉlément;
    Composants.Composants(E2).PremierÉlément :=
                        Composants.Composants(E1).PremierÉlément;
    Composants.Composants(E2).NombreDÉléments :=
                        Composants.Composants(E2).NombreDÉléments + 
    				    Composants.Composants(E1).NombreDÉléments;
    Composants.Composants(E1).NombreDÉléments := 0;
    -- E1 n'existe plus
    Composants.Composants(E1).PremierÉlément := 0;
  END Fondre;

BEGIN
  IF Composants.Composants(Comp1).NombreDÉléments 
       > Composants.Composants(Comp2).NombreDÉléments THEN
    Fondre(Comp2, Comp1); -- fusionner Comp2 dans Comp1
  ELSE
    Fondre(Comp1, Comp2); -- fusionner Comp1 dans Comp2
  END IF;
END Fusionner;

FUNCTION Nom(Élt: TypeÉlément; Composants: EnsembleFT) RETURN TypeNom IS
-- Retourne le nom de l'ensemble dont Élt est membre.
BEGIN 
  RETURN Composants.Éléments(Élt).NomEns;
END Nom;

PROCEDURE Vider(Composants: IN OUT EnsembleFT) IS
-- Vider ensemble Composants
BEGIN
  FOR Index IN 1..N LOOP
	Composants.Composants(Index).NombreDÉléments := 0;
	Composants.Composants(Index).PremierÉlément := 0;
	Composants.Éléments(Index).NomEns := 1;
	Composants.Éléments(Index).ProchainÉlt := 0;
  END LOOP;
END Vider;

END EnsemblesFT;
