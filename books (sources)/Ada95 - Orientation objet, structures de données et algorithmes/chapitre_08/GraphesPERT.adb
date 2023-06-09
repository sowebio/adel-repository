--          Copyright � 1999 Philippe J. Gabrini
WITH Graphes.Orient�s;
WITH Ada.Text_IO, Ada.Integer_Text_IO;

PROCEDURE GraphesPERT IS
N: CONSTANT Natural := 25;
SUBTYPE Intervalle IS Natural RANGE 1..N;

PROCEDURE Afficher(�lt: IN Intervalle);
PACKAGE GraphesBase IS NEW Graphes(Type_�l�ment => Intervalle,
                                   AfficherClef => Afficher,
                                   AfficherSommet => Afficher);
FUNCTION M�me(�lt: Intervalle) RETURN Intervalle;
PACKAGE GraphesEntiers IS NEW GraphesBase.Orient�s(Nom_Sommet => Intervalle, Nom => M�me);

TYPE TypeTemps IS (T�t, Tard);
TYPE TableTemps IS ARRAY (Intervalle, TypeTemps) OF Integer;
     
PROCEDURE TrouverCheminCritique(G: IN OUT GraphesEntiers.Graphe;
                                Temps: IN OUT TableTemps;
                                Sommets, Arcs: IN Natural) IS
-- � partir d'un graphe donn�, trouver les sommets critiques.
Sommet, Arc: Natural;
Destination: Integer;
Trouv�: Boolean;
BEGIN
  -- Calculer les valeurs t�t pour chaque sommet
  Sommet := 1;
  WHILE Sommet < Sommets LOOP
    Arc := 1;
    Trouv� := True;
    WHILE (Arc <= Arcs) AND Trouv� LOOP
      GraphesEntiers.TrouverAdjacent(G, Sommet, Arc, Destination, Trouv�);
      IF Trouv� THEN
        Temps(Destination, T�t) := Max(Temps(Destination, T�t),
           Temps(Sommet, T�t) + GraphesEntiers.Poids(G, Sommet, Destination));
        Arc := Arc + 1;
      END IF;
    END LOOP;
    Sommet := Sommet + 1;
  END LOOP;
  -- Calculer les valeurs tard pour chaque sommet
  Temps(Sommet, Tard) := Temps(Sommet, T�t);
  Sommet := Sommets;
  WHILE Sommet >= 1 LOOP
    Arc := 1;
    Trouv� := True;
    WHILE (Arc <= N) AND Trouv� LOOP
      GraphesEntiers.TrouverAdjacent(G, Sommet, Arc, Destination, Trouv�);
      IF Trouv� THEN
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
    IF Temps(Sommet, T�t) = Temps(Sommet, Tard) THEN
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
