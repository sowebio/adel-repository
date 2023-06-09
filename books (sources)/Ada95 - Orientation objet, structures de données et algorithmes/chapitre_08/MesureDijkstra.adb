--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO, Ada.Calendar, EnsemblesDiscrets;
WITH Ada.Numerics.Discrete_Random;
PROCEDURE MesureDijkstra IS
PACKAGE ES_Durée IS NEW Ada.Text_IO.Fixed_IO(Num => Duration);
      
NombreSommets: CONSTANT Natural := 200;
NombreEssais: CONSTANT Natural := 10;
NombreArcsMax: CONSTANT Natural := NombreSommets * NombreSommets;
Infinité: CONSTANT Natural := 32000;
SUBTYPE IntervalleN IS Natural RANGE 1..NombreSommets;
SUBTYPE IntervalleNPlus IS Natural RANGE 0..NombreSommets;
PACKAGE EnsNoeuds IS NEW EnsemblesDiscrets(Type_Élément => IntervalleN);
PACKAGE Aléatoire IS NEW Ada.Numerics.Discrete_Random(IntervalleN);
SUBTYPE IntervalleArc IS Natural RANGE 0..NombreArcsMax;

TYPE Sorte IS (matrice, liste, floyd);
SUBTYPE IntervalleEssais IS Natural RANGE 0..NombreEssais;
TYPE MatriceC IS ARRAY (1..NombreSommets, 1..NombreSommets) OF Natural; -- pour Floyd
TYPE Contrôle IS ARRAY(IntervalleN) OF Natural;
 
TYPE Sommet IS RECORD
                 Clef: IntervalleNPlus;
                 -- autre information associée au sommet
               END RECORD;
TYPE Arc IS RECORD
              Poids: Natural;
              -- autre information associée à l'arc
            END RECORD;
TYPE MatriceArcs IS ARRAY (IntervalleN, IntervalleN) OF Arc;
TYPE VecteurSommets IS ARRAY (IntervalleN) OF Sommet;
TYPE Graphe IS RECORD
                 NombreNoeuds: IntervalleN;
                 NombreArcs: IntervalleArc;
                 Sommets: VecteurSommets;
                 Arcs: MatriceArcs;
               END RECORD;
    --*************************************--
TYPE ArcCh;
TYPE PtrArc IS ACCESS ArcCh;
TYPE ArcCh IS RECORD
                Poids: Natural;
                SommetFin: Natural;
                Suivant: PtrArc;
              END RECORD;
TYPE MatriceArcsCh IS ARRAY (IntervalleN) OF PtrArc;
TYPE SommetCh IS ACCESS Sommet;
TYPE VecteurSommetsCh IS ARRAY (IntervalleN) OF SommetCh;
TYPE GrapheCh IS RECORD
                   NombreNoeuds: IntervalleN;
                   NombreArcs: IntervalleArc;
                   Sommets: VecteurSommetsCh;
                   Arcs: MatriceArcsCh;
                 END RECORD;
     --************************************--
TYPE VecteurNoeuds IS ARRAY (IntervalleN) OF Natural;
TYPE MatriceNoeuds IS ARRAY (IntervalleN) OF VecteurNoeuds;
TYPE TypProcDijkstra IS ACCESS PROCEDURE(G: IN Graphe; Som: IN Sommet;
                   Poids, Chemins: OUT VecteurNoeuds);

PROCEDURE Dijkstra(G: IN Graphe; Cime: IN Sommet;
                   Poids, Chemins: OUT VecteurNoeuds) IS
-- Étant donné un graphe G et un sommet donné Cime, cette procédure
-- retourne un tableau des poids des chemins minimum du sommet
-- spécifié au sommet indiqué par la position de l'index et un
-- tableau Chemins que l'on peut utiliser pour retrouver le chemin
-- de Cime à tout autre sommet, si un tel chemin existe.

  PROCEDURE TrouverSommetMini(Poids: IN VecteurNoeuds;
                              EnsembleNoeuds: IN EnsNoeuds.TypEnsemble;
                              NombreNoeuds: IN IntervalleN; 
                              Petit: OUT Natural) IS
  -- Retourne dans Petit le noeud qui n'est pas dans l'ensemble et
  -- qui a la plus petite valeur dans Poids.
  Min: Natural;
  BEGIN
    Petit := NombreSommets+1;
    Min := Infinité;
    FOR Essai IN 1..NombreNoeuds LOOP	-- vérifier tous les sommets
      IF NOT EnsNoeuds.Membre(Essai, EnsembleNoeuds) THEN
        IF Poids(Essai) < Min THEN
          Petit := Essai;
          Min := Poids(Essai);
        END IF;
      END IF;
    END LOOP;
  END TrouverSommetMini;

EnsembleNoeuds: EnsNoeuds.TypEnsemble;
Mini: Natural;
PoidsTest: Natural;

BEGIN  -- Dijkstra
  FOR Noeud1 IN 1..G.NombreNoeuds LOOP	-- initialiser
    Poids(Noeud1) := G.Arcs(Cime.Clef, Noeud1).Poids;
    Chemins(Noeud1) := Cime.Clef;
  END LOOP;
  EnsNoeuds.Vider(EnsembleNoeuds);
  Chemins(Cime.Clef) := 0;
  EnsNoeuds.Inclure(EnsembleNoeuds,Cime.Clef);
  FOR Noeud1 IN 1..G.NombreNoeuds LOOP -- nouveaux chemins les plus courts
    IF Noeud1 /= Cime.Clef THEN        -- pour sommets non dans ensemble
      TrouverSommetMini(Poids, EnsembleNoeuds, G.NombreNoeuds, Mini);
      IF (Mini-1) /= NombreSommets THEN  -- éviter de déborder
        EnsNoeuds.Inclure(EnsembleNoeuds, Mini);
        FOR Noeud2 IN 1..G.NombreNoeuds LOOP
          IF NOT EnsNoeuds.Membre(Noeud2, EnsembleNoeuds) THEN
            IF (Infinité - Poids(Mini))
                        >= G.Arcs(Mini,Noeud2).Poids THEN
              PoidsTest := Poids(Mini) + G.Arcs(Mini, Noeud2).Poids;
            ELSE
              PoidsTest := Infinité;
            END IF;
            IF PoidsTest < Poids(Noeud2) THEN  -- valeur plus petite
              Poids(Noeud2) := PoidsTest;
              Chemins(Noeud2) := Mini;
            END IF;
          END IF;
        END LOOP;
      END IF;
    END IF;
  END LOOP;
END Dijkstra;

PROCEDURE DijkstraTous(G: IN Graphe;
                       TousPoids, TousChemins: OUT MatriceNoeuds) IS
-- Étant donné un graphe G, cette procédure applique l'algorithme de
-- Dijkstra à partir de tout sommet source possible et retourne une
-- matrice de poids, TousPoids, pour toutes les paires de sommets et
-- une matrice de chemins, TousChemins, correspondant aux poids minimum.
BEGIN
  FOR Noeud1 IN 1..G.NombreNoeuds LOOP
    Dijkstra(G, G.Sommets(Noeud1), TousPoids(Noeud1), TousChemins(Noeud1));
  END LOOP;
END DijkstraTous;

PROCEDURE DijkstraCh(G: IN GrapheCh; Cime: IN SommetCh;
                     Poids, Chemins: OUT VecteurNoeuds) IS
-- Étant donné un graphe G et un sommet donné Cime, cette procédure
-- retourne un tableau des poids des chemins minimum du sommet
-- spécifié au sommet indiqué par la position de l'index et un
-- tableau Chemins que l'on peut utiliser pour retrouver le chemin
-- de Cime à tout autre sommet, si un tel chemin existe.

  PROCEDURE TrouverSommetMini(Poids: IN VecteurNoeuds;
                              EnsembleNoeuds: IN EnsNoeuds.TypEnsemble;
                              NombreNoeuds : IN IntervalleN;
                              Bas: OUT Natural) IS
  Min: Natural;
  BEGIN
    Bas := NombreSommets+1;
    Min := Infinité;
    FOR Essai IN 1..NombreNoeuds LOOP  -- vérifier tous sommets
      IF NOT EnsNoeuds.Membre(Essai, EnsembleNoeuds) THEN
        IF Poids(Essai) < Min THEN
          Bas := Essai;
          Min := Poids(Essai);
        END IF;
      END IF;
    END LOOP;
  END TrouverSommetMini;

EnsembleNoeuds : EnsNoeuds.TypEnsemble;
PoidsTest, Mini: Natural;
Ptr: PtrArc;

BEGIN -- DijkstraCh
  FOR Noeud1 IN 1..G.NombreNoeuds LOOP  -- initialiser
   Poids(Noeud1) := Infinité;
   Chemins(Noeud1)   := Cime.Clef;
  END LOOP;
  EnsNoeuds.Vider(EnsembleNoeuds);
  Ptr := G.Arcs(Cime.Clef);
  WHILE Ptr /= NULL LOOP                -- initialiser poids
    IF Ptr.SommetFin > 0 THEN
      Poids(Ptr.SommetFin) := Ptr.Poids;
    END IF;
    Ptr := Ptr.Suivant;
  END LOOP;
  Chemins(Cime.Clef) := 0;
  EnsNoeuds.Inclure(EnsembleNoeuds, Cime.Clef);
  FOR Noeud1 IN 1..G.NombreNoeuds LOOP  -- vérifier tous noeuds
    IF Noeud1 /= Cime.Clef THEN
      TrouverSommetMini(Poids, EnsembleNoeuds, G.NombreNoeuds, Mini);
      IF (Mini-1) /= NombreSommets THEN -- éviter débordement
        EnsNoeuds.Inclure(EnsembleNoeuds, Mini);
		    Ptr := G.Arcs(Mini);
		    WHILE ((Ptr /= NULL) AND THEN (Ptr.SommetFin /= 0)) LOOP
	        -- vérifier toutes les arêtes
	        IF NOT EnsNoeuds.Membre(Ptr.SommetFin, EnsembleNoeuds) THEN
		        IF (Infinité - Poids(Mini)) >= Ptr.Poids THEN
		          PoidsTest := Poids(Mini) + Ptr.Poids;
		        ELSE
		          PoidsTest := Infinité;
		        END IF;
		        IF PoidsTest < Poids(Ptr.SommetFin) THEN
		          Poids(Ptr.SommetFin) := PoidsTest;
		          Chemins(Ptr.SommetFin) := Mini;
		        END IF;
		      END IF;
		      Ptr := Ptr.Suivant;         -- prochain arc
		    END LOOP;
      END IF;
    END IF;
  END LOOP;
END DijkstraCh;

PROCEDURE DijkstraChTous(G: IN GrapheCh;
                       TousPoids, TousChemins: OUT MatriceNoeuds) IS
-- Étant donné un graphe G, cette procédure applique l'algorithme de
-- Dijkstra à partir de tout sommet source possible et retourne une
-- matrice de poids, TousPoids, pour toutes les paires de sommets et
-- une matrice de chemins, TousChemins, correspondant aux poids minimum.
BEGIN
  NULL;
END DijkstraChTous;

BEGIN
--* Faire tous les essais
    NULL;
END MesureDijkstra;
