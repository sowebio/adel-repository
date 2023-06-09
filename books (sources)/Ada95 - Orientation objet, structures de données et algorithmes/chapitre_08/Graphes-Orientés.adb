--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Unchecked_Deallocation;
PACKAGE BODY Graphes.Orientés IS

PROCEDURE Libérer IS NEW Ada.Unchecked_Deallocation(Arc, PointeurArc);

PROCEDURE Détruire(G: IN OUT Graphe) IS
-- Vider graphe G
Présent, Précédent: PointeurArc;
BEGIN
  FOR Index IN Nom_Sommet LOOP
    Présent := G.Sommets(Index).Arcs;
    Précédent := NULL;
    WHILE Présent /= NULL LOOP -- libérer espace
      Précédent := Présent;
      Présent := Présent.Suivant;
      Libérer(Précédent);
    END LOOP;
    G.Sommets(Index).Arcs := NULL;
  END LOOP;
  G.N := 0;
END Détruire;

FUNCTION NombreSommets(G: Graphe) RETURN Natural IS
-- Retourner le nombre de sommets dans G
BEGIN
  RETURN G.N;
END NombreSommets;

PROCEDURE InsérerSommet(G: IN OUT Graphe; Élt: IN Type_Élément) IS
-- Insérer (ou mettre à jour) un nouveau sommet dans le graphe G
-- avec valeur Élt.
BEGIN
  IF G.N < Nombre_Sommets_Maxi AND NOT G.Sommets(Nom(Élt)).Présent THEN
    G.N := G.N + 1;
  END IF;
  G.Sommets(Nom(Élt)).Présent := True;
  G.Sommets(Nom(Élt)).Valeur := Élt;
END InsérerSommet;

PROCEDURE InsérerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_Élément; 
                     Poids: IN Integer) IS
-- Insérer un nouvel arc avec pondération Poids dans le graphe G
-- entre les sommets Sommet1 et Sommet2.
NouvelArc, Courant, Précédent: PointeurArc;
BEGIN
  IF G.Sommets(Nom(Sommet1)).Présent AND G.Sommets(Nom(Sommet2)).Présent THEN
    Courant := G.Sommets(Nom(Sommet1)).Arcs;	-- premier arc
    Précédent := NULL;
    WHILE (Courant /= NULL) AND THEN (Courant.Clef < Nom(Sommet2)) LOOP
      -- trouver endroit
      Précédent := Courant;
      Courant := Courant.Suivant;
    END LOOP;
    IF (Courant /= NULL) AND THEN (Courant.Clef = Nom(Sommet2)) THEN
      -- déjà là
      Courant.ValeurPoids := Poids;
    ELSE
      NouvelArc := NEW Arc'(Nom(Sommet2), Poids, Courant);
      IF Précédent = NULL THEN
        G.Sommets(Nom(Sommet1)).Arcs := NouvelArc;
      ELSE
        Précédent.Suivant := NouvelArc;
      END IF;
    END IF;
  END IF;
END InsérerArc;

PROCEDURE SupprimerSommet(G: IN OUT Graphe; Sommet: IN Type_Élément) IS
-- Supprimer le sommet Sommet du graphe G.
Courant, Précédent: PointeurArc;
BEGIN
  IF G.Sommets(Nom(Sommet)).Présent THEN
    Courant := G.Sommets(Nom(Sommet)).Arcs;
    WHILE Courant /= NULL LOOP	              -- éliminer liste arcs
      Précédent := Courant;
      Courant := Courant.Suivant;
      Libérer(Précédent);
    END LOOP;
    G.Sommets(Nom(Sommet)).Arcs := NULL;
    G.Sommets(Nom(Sommet)).Présent := False;	-- marquer absent
    G.N := G.N - 1;
    FOR Index IN Nom_Sommet LOOP	    -- chercher les arcs touchant Sommet
      IF G.Sommets(Index).Arcs /= NULL THEN
        Courant := G.Sommets(Index).Arcs;
        Précédent := NULL;
        WHILE (Courant /= NULL) AND THEN (Courant.Clef /= Nom(Sommet)) LOOP
          Précédent := Courant;
          Courant := Courant.Suivant;
        END LOOP;
        IF (Précédent /= NULL) AND (Courant /= NULL) THEN
          Précédent.Suivant := Courant.Suivant;
          Libérer(Courant);
        ELSIF Courant /= NULL THEN
          G.Sommets(Index).Arcs := Courant.Suivant;
          Libérer(Courant);
        END IF;
      END IF;
    END LOOP;
  END IF;
END SupprimerSommet;

PROCEDURE SupprimerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_Élément) IS
-- Supprimer l'arc entre les sommets Sommet1 et Sommet2 dans le graphe G.
Courant, Précédent: PointeurArc;
BEGIN
  Courant := G.Sommets(Nom(Sommet1)).Arcs;
  Précédent := NULL;
  WHILE (Courant /= NULL) AND THEN (Courant.Clef /= Nom(Sommet2)) LOOP 
    -- chercher arc
    Précédent := Courant;
    Courant := Courant.Suivant;
  END LOOP;
  IF (Courant /= NULL) AND (Précédent /= NULL) THEN	-- éliminer
    Précédent.Suivant := Courant.Suivant;
    Libérer(Courant);
  ELSIF Courant /= NULL THEN	                      -- premier élément
    G.Sommets(Nom(Sommet1)).Arcs := Courant.Suivant;
    Libérer(Courant);
  END IF;
END SupprimerArc;

PROCEDURE TrouverSommet(G: IN Graphe; Sommet: IN OUT Type_Élément;
                        Trouvé: OUT Boolean) IS
-- Chercher le sommet Sommet dans le graphe G et retourner la valeur
-- du sommet.
BEGIN
  Trouvé := False;
  IF  G.Sommets(Nom(Sommet)).Présent THEN
    Sommet := G.Sommets(Nom(Sommet)).Valeur;
    Trouvé := True;
  END IF;
END TrouverSommet;

FUNCTION Poids(G: Graphe; Sommet1, Sommet2: Type_Élément) RETURN Integer IS
-- Retourne le poids associé à l'arc entre les deux sommets
-- Sommet1 et Sommet2 dans le graphe G.
Courant: PointeurArc;
BEGIN
  Courant := G.Sommets(Nom(Sommet1)).Arcs;
  WHILE (Courant /= NULL) AND THEN (Courant.Clef /= Nom(Sommet2)) LOOP 
    -- chercher arc
    Courant := Courant.Suivant;
  END LOOP;
  IF Courant /= NULL THEN
    RETURN Courant.ValeurPoids;
  ELSE
    RETURN 0;
  END IF;
END Poids;

PROCEDURE TrouverAdjacent(G: IN Graphe;
                          Sommet: IN Type_Élément; N: IN Natural;
                          SommetAdjacent: OUT Type_Élément;
                          Succès: OUT Boolean) IS
-- Retourne le Nième sommet adjacent au sommet Sommet dans le graphe G.
Voisin: Natural := 0;
Courant: PointeurArc;
BEGIN
  Courant := G.Sommets(Nom(Sommet)).Arcs;
  WHILE Courant /= NULL LOOP
    Voisin := Voisin + 1;
    IF Voisin = N THEN	                              -- trouvé
      SommetAdjacent := G.Sommets(Courant.Clef).Valeur;
      Succès := True;
      RETURN;
    END IF;
    Courant := Courant.Suivant;
  END LOOP;
  Succès := False;
END TrouverAdjacent;

END Graphes.Orientés;
