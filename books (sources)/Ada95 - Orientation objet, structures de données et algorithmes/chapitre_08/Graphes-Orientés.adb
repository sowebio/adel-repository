--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Unchecked_Deallocation;
PACKAGE BODY Graphes.Orient�s IS

PROCEDURE Lib�rer IS NEW Ada.Unchecked_Deallocation(Arc, PointeurArc);

PROCEDURE D�truire(G: IN OUT Graphe) IS
-- Vider graphe G
Pr�sent, Pr�c�dent: PointeurArc;
BEGIN
  FOR Index IN Nom_Sommet LOOP
    Pr�sent := G.Sommets(Index).Arcs;
    Pr�c�dent := NULL;
    WHILE Pr�sent /= NULL LOOP -- lib�rer espace
      Pr�c�dent := Pr�sent;
      Pr�sent := Pr�sent.Suivant;
      Lib�rer(Pr�c�dent);
    END LOOP;
    G.Sommets(Index).Arcs := NULL;
  END LOOP;
  G.N := 0;
END D�truire;

FUNCTION NombreSommets(G: Graphe) RETURN Natural IS
-- Retourner le nombre de sommets dans G
BEGIN
  RETURN G.N;
END NombreSommets;

PROCEDURE Ins�rerSommet(G: IN OUT Graphe; �lt: IN Type_�l�ment) IS
-- Ins�rer (ou mettre � jour) un nouveau sommet dans le graphe G
-- avec valeur �lt.
BEGIN
  IF G.N < Nombre_Sommets_Maxi AND NOT G.Sommets(Nom(�lt)).Pr�sent THEN
    G.N := G.N + 1;
  END IF;
  G.Sommets(Nom(�lt)).Pr�sent := True;
  G.Sommets(Nom(�lt)).Valeur := �lt;
END Ins�rerSommet;

PROCEDURE Ins�rerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_�l�ment; 
                     Poids: IN Integer) IS
-- Ins�rer un nouvel arc avec pond�ration Poids dans le graphe G
-- entre les sommets Sommet1 et Sommet2.
NouvelArc, Courant, Pr�c�dent: PointeurArc;
BEGIN
  IF G.Sommets(Nom(Sommet1)).Pr�sent AND G.Sommets(Nom(Sommet2)).Pr�sent THEN
    Courant := G.Sommets(Nom(Sommet1)).Arcs;	-- premier arc
    Pr�c�dent := NULL;
    WHILE (Courant /= NULL) AND THEN (Courant.Clef < Nom(Sommet2)) LOOP
      -- trouver endroit
      Pr�c�dent := Courant;
      Courant := Courant.Suivant;
    END LOOP;
    IF (Courant /= NULL) AND THEN (Courant.Clef = Nom(Sommet2)) THEN
      -- d�j� l�
      Courant.ValeurPoids := Poids;
    ELSE
      NouvelArc := NEW Arc'(Nom(Sommet2), Poids, Courant);
      IF Pr�c�dent = NULL THEN
        G.Sommets(Nom(Sommet1)).Arcs := NouvelArc;
      ELSE
        Pr�c�dent.Suivant := NouvelArc;
      END IF;
    END IF;
  END IF;
END Ins�rerArc;

PROCEDURE SupprimerSommet(G: IN OUT Graphe; Sommet: IN Type_�l�ment) IS
-- Supprimer le sommet Sommet du graphe G.
Courant, Pr�c�dent: PointeurArc;
BEGIN
  IF G.Sommets(Nom(Sommet)).Pr�sent THEN
    Courant := G.Sommets(Nom(Sommet)).Arcs;
    WHILE Courant /= NULL LOOP	              -- �liminer liste arcs
      Pr�c�dent := Courant;
      Courant := Courant.Suivant;
      Lib�rer(Pr�c�dent);
    END LOOP;
    G.Sommets(Nom(Sommet)).Arcs := NULL;
    G.Sommets(Nom(Sommet)).Pr�sent := False;	-- marquer absent
    G.N := G.N - 1;
    FOR Index IN Nom_Sommet LOOP	    -- chercher les arcs touchant Sommet
      IF G.Sommets(Index).Arcs /= NULL THEN
        Courant := G.Sommets(Index).Arcs;
        Pr�c�dent := NULL;
        WHILE (Courant /= NULL) AND THEN (Courant.Clef /= Nom(Sommet)) LOOP
          Pr�c�dent := Courant;
          Courant := Courant.Suivant;
        END LOOP;
        IF (Pr�c�dent /= NULL) AND (Courant /= NULL) THEN
          Pr�c�dent.Suivant := Courant.Suivant;
          Lib�rer(Courant);
        ELSIF Courant /= NULL THEN
          G.Sommets(Index).Arcs := Courant.Suivant;
          Lib�rer(Courant);
        END IF;
      END IF;
    END LOOP;
  END IF;
END SupprimerSommet;

PROCEDURE SupprimerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_�l�ment) IS
-- Supprimer l'arc entre les sommets Sommet1 et Sommet2 dans le graphe G.
Courant, Pr�c�dent: PointeurArc;
BEGIN
  Courant := G.Sommets(Nom(Sommet1)).Arcs;
  Pr�c�dent := NULL;
  WHILE (Courant /= NULL) AND THEN (Courant.Clef /= Nom(Sommet2)) LOOP 
    -- chercher arc
    Pr�c�dent := Courant;
    Courant := Courant.Suivant;
  END LOOP;
  IF (Courant /= NULL) AND (Pr�c�dent /= NULL) THEN	-- �liminer
    Pr�c�dent.Suivant := Courant.Suivant;
    Lib�rer(Courant);
  ELSIF Courant /= NULL THEN	                      -- premier �l�ment
    G.Sommets(Nom(Sommet1)).Arcs := Courant.Suivant;
    Lib�rer(Courant);
  END IF;
END SupprimerArc;

PROCEDURE TrouverSommet(G: IN Graphe; Sommet: IN OUT Type_�l�ment;
                        Trouv�: OUT Boolean) IS
-- Chercher le sommet Sommet dans le graphe G et retourner la valeur
-- du sommet.
BEGIN
  Trouv� := False;
  IF  G.Sommets(Nom(Sommet)).Pr�sent THEN
    Sommet := G.Sommets(Nom(Sommet)).Valeur;
    Trouv� := True;
  END IF;
END TrouverSommet;

FUNCTION Poids(G: Graphe; Sommet1, Sommet2: Type_�l�ment) RETURN Integer IS
-- Retourne le poids associ� � l'arc entre les deux sommets
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
                          Sommet: IN Type_�l�ment; N: IN Natural;
                          SommetAdjacent: OUT Type_�l�ment;
                          Succ�s: OUT Boolean) IS
-- Retourne le Ni�me sommet adjacent au sommet Sommet dans le graphe G.
Voisin: Natural := 0;
Courant: PointeurArc;
BEGIN
  Courant := G.Sommets(Nom(Sommet)).Arcs;
  WHILE Courant /= NULL LOOP
    Voisin := Voisin + 1;
    IF Voisin = N THEN	                              -- trouv�
      SommetAdjacent := G.Sommets(Courant.Clef).Valeur;
      Succ�s := True;
      RETURN;
    END IF;
    Courant := Courant.Suivant;
  END LOOP;
  Succ�s := False;
END TrouverAdjacent;

END Graphes.Orient�s;
