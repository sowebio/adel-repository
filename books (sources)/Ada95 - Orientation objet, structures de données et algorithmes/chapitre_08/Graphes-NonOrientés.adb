--          Copyright � 1998 Philippe J. Gabrini
PACKAGE BODY Graphes.NonOrient�s IS

PROCEDURE D�truire(G: IN OUT Graphe) IS
-- �liminer tous les sommets et les arcs du graphe G.
BEGIN
  FOR Rang�e IN Nom_Sommet LOOP
    G.Sommets(Rang�e).Pr�sent := False;		-- pas de sommet
    FOR Colonne IN Nom_Sommet LOOP
      G.Ar�tes(Rang�e, Colonne) := 0;		  -- pas d'ar�te
    END LOOP;
  END LOOP;
  G.N := 0;
END D�truire;

FUNCTION NombreSommets(G: IN Graphe) RETURN Natural IS
-- Compte du nombre de sommets du graphe G.
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
-- Ins�rer une nouvelle ar�te avec pond�ration Poids dans le graphe G
-- entre les sommets Sommet1 et Sommet2.
BEGIN
  G.Ar�tes(Nom(Sommet1), Nom(Sommet2)) := Poids;
END Ins�rerArc;

PROCEDURE SupprimerSommet(G: IN OUT Graphe; Sommet: IN Type_�l�ment) IS
-- Supprimer le sommet Sommet du graphe G.
BEGIN
  FOR Index IN Nom_Sommet LOOP
    G.Ar�tes(Nom(Sommet), Index) := 0;
    G.Ar�tes(Index, Nom(Sommet)) := 0;
  END LOOP;
  G.Sommets(Nom(Sommet)).Pr�sent := False;
  G.N := G.N - 1;
END SupprimerSommet;

PROCEDURE SupprimerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_�l�ment) IS
-- Supprimer l'ar�te entre les sommets Sommet1 et Sommet2 dans le graphe G.
BEGIN
  G.Ar�tes(Nom(Sommet1), Nom(Sommet2)) := 0;
  G.Ar�tes(Nom(Sommet2), Nom(Sommet1)) := 0;	-- pour graphes non dirig�s
END SupprimerArc;

PROCEDURE TrouverSommet(G: IN Graphe; Sommet: IN OUT Type_�l�ment;
                        Trouv�: OUT Boolean) IS
-- Chercher le Sommet dans le graphe G et retourner la valeur du Sommet.
BEGIN
  IF G.Sommets(Nom(Sommet)).Pr�sent THEN
    Sommet := G.Sommets(Nom(Sommet)).Valeur;
    Trouv� := True;
  ELSE
    Trouv� := False;
  END IF;
END TrouverSommet;

FUNCTION Poids(G: IN Graphe; Sommet1, Sommet2: IN Type_�l�ment)
              RETURN Integer IS
-- Retourne le poids associ� � l'ar�te entre les deux sommets
-- Sommet1 et Sommet2 dans le graphe G.
BEGIN
  RETURN G.Ar�tes(Nom(Sommet1), Nom(Sommet2));
END Poids;

PROCEDURE TrouverAdjacent(G: IN Graphe;
                          Sommet: IN Type_�l�ment; N: IN Natural;
                          SommetAdjacent: OUT Type_�l�ment;
                          Succ�s: OUT Boolean) IS
-- Retourne le Ni�me sommet adjacent au sommet Sommet dans le graphe G.
Voisin: Natural := 0;
BEGIN
  FOR LeSommet IN Nom_Sommet LOOP
    IF G.Ar�tes(Nom(Sommet), LeSommet) /= 0 THEN	-- compter sommets
      Voisin := Voisin + 1;
      IF Voisin = N THEN 
        SommetAdjacent := G.Sommets(LeSommet).Valeur;
        Succ�s := True;
        RETURN;
      END IF;
    END IF;
  END LOOP;
  Succ�s := False;
END TrouverAdjacent;

END Graphes.NonOrient�s;
