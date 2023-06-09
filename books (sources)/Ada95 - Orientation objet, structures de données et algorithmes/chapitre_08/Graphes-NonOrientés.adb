--          Copyright © 1998 Philippe J. Gabrini
PACKAGE BODY Graphes.NonOrientés IS

PROCEDURE Détruire(G: IN OUT Graphe) IS
-- Éliminer tous les sommets et les arcs du graphe G.
BEGIN
  FOR Rangée IN Nom_Sommet LOOP
    G.Sommets(Rangée).Présent := False;		-- pas de sommet
    FOR Colonne IN Nom_Sommet LOOP
      G.Arêtes(Rangée, Colonne) := 0;		  -- pas d'arête
    END LOOP;
  END LOOP;
  G.N := 0;
END Détruire;

FUNCTION NombreSommets(G: IN Graphe) RETURN Natural IS
-- Compte du nombre de sommets du graphe G.
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
-- Insérer une nouvelle arête avec pondération Poids dans le graphe G
-- entre les sommets Sommet1 et Sommet2.
BEGIN
  G.Arêtes(Nom(Sommet1), Nom(Sommet2)) := Poids;
END InsérerArc;

PROCEDURE SupprimerSommet(G: IN OUT Graphe; Sommet: IN Type_Élément) IS
-- Supprimer le sommet Sommet du graphe G.
BEGIN
  FOR Index IN Nom_Sommet LOOP
    G.Arêtes(Nom(Sommet), Index) := 0;
    G.Arêtes(Index, Nom(Sommet)) := 0;
  END LOOP;
  G.Sommets(Nom(Sommet)).Présent := False;
  G.N := G.N - 1;
END SupprimerSommet;

PROCEDURE SupprimerArc(G: IN OUT Graphe; Sommet1, Sommet2: IN Type_Élément) IS
-- Supprimer l'arête entre les sommets Sommet1 et Sommet2 dans le graphe G.
BEGIN
  G.Arêtes(Nom(Sommet1), Nom(Sommet2)) := 0;
  G.Arêtes(Nom(Sommet2), Nom(Sommet1)) := 0;	-- pour graphes non dirigés
END SupprimerArc;

PROCEDURE TrouverSommet(G: IN Graphe; Sommet: IN OUT Type_Élément;
                        Trouvé: OUT Boolean) IS
-- Chercher le Sommet dans le graphe G et retourner la valeur du Sommet.
BEGIN
  IF G.Sommets(Nom(Sommet)).Présent THEN
    Sommet := G.Sommets(Nom(Sommet)).Valeur;
    Trouvé := True;
  ELSE
    Trouvé := False;
  END IF;
END TrouverSommet;

FUNCTION Poids(G: IN Graphe; Sommet1, Sommet2: IN Type_Élément)
              RETURN Integer IS
-- Retourne le poids associé à l'arête entre les deux sommets
-- Sommet1 et Sommet2 dans le graphe G.
BEGIN
  RETURN G.Arêtes(Nom(Sommet1), Nom(Sommet2));
END Poids;

PROCEDURE TrouverAdjacent(G: IN Graphe;
                          Sommet: IN Type_Élément; N: IN Natural;
                          SommetAdjacent: OUT Type_Élément;
                          Succès: OUT Boolean) IS
-- Retourne le Nième sommet adjacent au sommet Sommet dans le graphe G.
Voisin: Natural := 0;
BEGIN
  FOR LeSommet IN Nom_Sommet LOOP
    IF G.Arêtes(Nom(Sommet), LeSommet) /= 0 THEN	-- compter sommets
      Voisin := Voisin + 1;
      IF Voisin = N THEN 
        SommetAdjacent := G.Sommets(LeSommet).Valeur;
        Succès := True;
        RETURN;
      END IF;
    END IF;
  END LOOP;
  Succès := False;
END TrouverAdjacent;

END Graphes.NonOrientés;
