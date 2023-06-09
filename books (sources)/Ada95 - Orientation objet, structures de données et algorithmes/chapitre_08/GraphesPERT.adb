--          Copyright © 1999 Philippe J. Gabrini
WITH Graphes.Orientés;
WITH Ada.Text_IO, Ada.Integer_Text_IO;

PROCEDURE GraphesPERT IS
N: CONSTANT Natural := 25;
SUBTYPE Intervalle IS Natural RANGE 1..N;

PROCEDURE Afficher(Élt: IN Intervalle);
PACKAGE GraphesBase IS NEW Graphes(Type_Élément => Intervalle,
                                   AfficherClef => Afficher,
                                   AfficherSommet => Afficher);
FUNCTION Même(Élt: Intervalle) RETURN Intervalle;
PACKAGE GraphesEntiers IS NEW GraphesBase.Orientés(Nom_Sommet => Intervalle, Nom => Même);

TYPE TypeTemps IS (Tôt, Tard);
TYPE TableTemps IS ARRAY (Intervalle, TypeTemps) OF Integer;
     
PROCEDURE TrouverCheminCritique(G: IN OUT GraphesEntiers.Graphe;
                                Temps: IN OUT TableTemps;
                                Sommets, Arcs: IN Natural) IS
-- À partir d'un graphe donné, trouver les sommets critiques.
Sommet, Arc: Natural;
Destination: Integer;
Trouvé: Boolean;
BEGIN
  -- Calculer les valeurs tôt pour chaque sommet
  Sommet := 1;
  WHILE Sommet < Sommets LOOP
    Arc := 1;
    Trouvé := True;
    WHILE (Arc <= Arcs) AND Trouvé LOOP
      GraphesEntiers.TrouverAdjacent(G, Sommet, Arc, Destination, Trouvé);
      IF Trouvé THEN
        Temps(Destination, Tôt) := Max(Temps(Destination, Tôt),
           Temps(Sommet, Tôt) + GraphesEntiers.Poids(G, Sommet, Destination));
        Arc := Arc + 1;
      END IF;
    END LOOP;
    Sommet := Sommet + 1;
  END LOOP;
  -- Calculer les valeurs tard pour chaque sommet
  Temps(Sommet, Tard) := Temps(Sommet, Tôt);
  Sommet := Sommets;
  WHILE Sommet >= 1 LOOP
    Arc := 1;
    Trouvé := True;
    WHILE (Arc <= N) AND Trouvé LOOP
      GraphesEntiers.TrouverAdjacent(G, Sommet, Arc, Destination, Trouvé);
      IF Trouvé THEN
	      Temps(Sommet, Tard) := Min(Temps(Sommet, Tard),
		               Temps(Destination, Tard) 
                       - GraphesEntiers.Poids(G, Sommet, Destination));
        Arc := Arc + 1;
      END IF;
    END LOOP;
    Sommet := Sommet - 1;
  END LOOP;
  -- Sortir sommets critiques
  Ada.Text_IO.Put(Item => "Chemin critique: ");
  FOR Sommet IN 1..Sommets LOOP
    IF Temps(Sommet, Tôt) = Temps(Sommet, Tard) THEN
      Ada.Text_IO.Put(Item => "-");
      Ada.Integer_Text_IO.Put(Item => Sommet, Width => 3);
	    Ada.Text_IO.Put(Item => "-");
    END IF;
  END LOOP;
  Ada.Text_IO.New_Line;
END TrouverCheminCritique;
BEGIN
  NULL;
END GraphesPERT;
