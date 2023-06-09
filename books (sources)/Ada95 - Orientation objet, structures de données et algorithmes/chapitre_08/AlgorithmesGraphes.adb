--          Copyright © 1998 Philippe J. Gabrini
WITH Graphes.Orientés, TDAFile, EnsemblesDiscrets;
WITH Ada.Text_IO, Ada.Integer_Text_IO;

PROCEDURE AlgorithmesGraphes IS
Infinité: CONSTANT Integer := 32000;
N: CONSTANT Natural := 25;
SUBTYPE Intervalle IS Natural RANGE 1..N;

PACKAGE FilesG IS NEW TDAFile(TypeÉlément => Natural);
PACKAGE EnsemblesG IS NEW EnsemblesDiscrets(Type_Élément => Intervalle);

PROCEDURE Afficher(Élt: IN Intervalle);
PACKAGE GraphesBase IS NEW Graphes(Type_Élément => Intervalle,
                                   AfficherClef => Afficher,
                                   AfficherSommet => Afficher);
FUNCTION Même(Élt: Intervalle) RETURN Intervalle;
PACKAGE GraphesEntiers IS NEW GraphesBase.Orientés(Nom_Sommet => Intervalle, Nom => Même);

TYPE Vecteur IS ARRAY (1..N) OF Integer;
TYPE MatriceC IS ARRAY (1..N, 1..N) OF Natural; -- pour plus court chemin
TYPE MatriceB IS ARRAY (1..N, 1..N) OF Boolean; -- for Warshall
     
PROCEDURE Afficher(Élt: IN Intervalle) IS
BEGIN
  Ada.Integer_Text_IO.Put(Élt, 5);
END Afficher;

FUNCTION Même(Élt: Intervalle) RETURN Intervalle IS
BEGIN
  RETURN Élt;
END Même;

PROCEDURE Prim(Coût: IN GraphesEntiers.Graphe;
               CoûtBas, Proche: IN OUT Vecteur;
               N: IN Natural) IS
-- Donne les arêtes d'un arbre de recouvrement à coût minimum pour le graphe
-- Coût avec les sommets {1,2,...,N}.  L'arbre commence au sommet 1 et croît
-- à partir de là.
Bas: Natural;       -- indices
Min: Integer;               -- coût le plus bas trouvé
BEGIN
  FOR UnSommet IN 2..N LOOP	-- initialiser
    CoûtBas(UnSommet) := GraphesEntiers.Poids(Coût, 1, UnSommet);
    Proche(UnSommet) := 1;
  END LOOP;
  FOR UnSommet IN 2..N LOOP	-- trouver sommet Bas le plus proche du sommet 1
    Min := CoûtBas(2);
    Bas := 2;
    FOR Voisin IN 3..N LOOP
      IF CoûtBas(Voisin) < Min THEN
        Min := CoûtBas(Voisin);
        Bas := Voisin;
      END IF;
    END LOOP;
    -- montrer arête
    Ada.Integer_Text_IO.Put(Item => CoûtBas(Bas), Width => 5);
	  Ada.Integer_Text_IO.Put(Item => Bas, Width => 4);
    Ada.Integer_Text_IO.Put(Item => Proche(Bas), Width => 4);
    Ada.Text_IO.New_Line;
    CoûtBas(Bas) := Infinité;   -- Bas est ajouté à l'arbre
    FOR Voisin IN 2..N LOOP     -- ajuster coûts
      IF (GraphesEntiers.Poids(Coût, Bas, Voisin) < CoûtBas(Voisin))
                                   AND (CoûtBas(Voisin) < Infinité) THEN
        CoûtBas(Voisin) := GraphesEntiers.Poids(Coût, Bas, Voisin);
        Proche(Voisin) := Bas;
      END IF;
    END LOOP;
  END LOOP;
END Prim;

PROCEDURE TriTopo(G: IN GraphesEntiers.Graphe; Sommet: IN Natural;
                  Visité: IN OUT EnsemblesG.TypEnsemble) IS
-- Tri topologique d'un graphe orienté acyclique.  Cette procédure
-- produit une liste des sommets du graphe en ordre inverse de 
-- l'ordre topologique. Sommet est un numéro de sommet, et Visité 
-- un ensemble de tous les sommets visités.
Suivant, Nombre: Integer;
Trouvé: Boolean;
BEGIN
  EnsemblesG.Inclure(Visité, Sommet);
  Nombre := 1;
  GraphesEntiers.TrouverAdjacent(G, Sommet, Nombre, Suivant, Trouvé);
  -- appliquer TriTopo à tous les sommets adjacents à Sommet
  WHILE Trouvé AND (Nombre <= N) LOOP
    IF NOT EnsemblesG.Membre(Suivant, Visité) THEN
      TriTopo(G, Suivant, Visité);
    END IF;
    Nombre := Nombre + 1;
    GraphesEntiers.TrouverAdjacent(G, Sommet, Nombre, Suivant, Trouvé);
  END LOOP;
  Ada.Integer_Text_IO.Put(Item => Sommet, Width => 4);
END TriTopo;

PROCEDURE TraverserEnProfondeur(G: IN GraphesEntiers.Graphe) IS
-- Appliquer la méthode en profondeur pour traverser un graphe G qui
-- peut ne pas être un graphe connexe.

  PROCEDURE Approfondir(Sommet: IN Natural;
                        Visité: IN OUT EnsemblesG.TypEnsemble) IS
  -- Traverser récursivement tous les sommets accessibles à partir de Sommet.
  Adjacent: Integer;
  Présent: Boolean;
  BEGIN
    EnsemblesG.Inclure(Visité, Sommet);
    FOR AutreSommet IN 1..N LOOP -- essayer tous les sommets possibles
    -- appliquer la méthode en profondeur à tous les sommets adjacents à Sommet
      GraphesEntiers.TrouverAdjacent(G, Sommet, AutreSommet, Adjacent, Présent);
      IF Présent AND THEN NOT EnsemblesG.Membre(Adjacent, Visité) THEN
        Approfondir(Adjacent, Visité);
        -- montrer arête
        Ada.Integer_Text_IO.Put(Item => Sommet, Width => 2); Ada.Text_IO.Put(Item => "--");
        Ada.Integer_Text_IO.Put(Item => Adjacent, Width => 2); Ada.Text_IO.New_Line;
      END IF;
    END LOOP;
  END Approfondir;

LeSommet: Natural;
Trouvé: Boolean;
Visité: EnsemblesG.TypEnsemble;
BEGIN
  EnsemblesG.Vider(Visité);
  FOR Sommet IN 1..N LOOP	-- traverser tous les sommets possibles
    LeSommet := Sommet;
    GraphesEntiers.TrouverSommet(G, LeSommet, Trouvé);
    IF Trouvé AND NOT EnsemblesG.Membre(LeSommet, Visité) THEN
      -- le sommet existe et n'est pas encore visité
      Ada.Text_IO.Put(Item => "Noeud ");
      Ada.Integer_Text_IO.Put(Item => Sommet, Width => 4); Ada.Text_IO.New_Line;
      Approfondir(LeSommet, Visité);
    END IF;
  END LOOP;
END TraverserEnProfondeur;

PROCEDURE TraverserEnLargeur(G: IN OUT GraphesEntiers.Graphe) IS
-- Visiter tous les sommets du graphe G en utilisant la méthode en largeur.
-- Le graphe G peut ne pas être un graphe connexe.

  PROCEDURE Élargir(Sommet: IN Natural;
                    Visité: IN OUT EnsemblesG.TypEnsemble;
                    Q: IN OUT FilesG.File) IS
  -- Traverser tous les sommets accessibles de Sommet en utilisant
  -- la méthode de traversée en largeur.
  Adjacent, Suivant, UnSommet: Integer;
  Présent: Boolean;
  BEGIN
    EnsemblesG.Inclure(Visité, Sommet);    -- commencer par Sommet
    FilesG.Enfiler(Q, Sommet);
    WHILE FilesG.Longueur(Q) /= 0 LOOP
      FilesG.Défiler(Q, UnSommet);
      Suivant := 1;
      GraphesEntiers.TrouverAdjacent(G, UnSommet, Suivant, Adjacent, Présent);
      -- visiter tous les sommets adjacents à UnSommet
      WHILE Présent AND (Suivant <= N) LOOP
        IF NOT EnsemblesG.Membre(Adjacent, Visité) THEN
          EnsemblesG.Inclure(Visité, Adjacent);
          FilesG.Enfiler(Q, Adjacent);
          -- afficher arête
          Ada.Integer_Text_IO.Put(Item => UnSommet, Width => 2);
          Ada.Text_IO.Put(Item => "--");
          Ada.Integer_Text_IO.Put(Item => Adjacent, Width => 2); Ada.Text_IO.New_Line;
        END IF;
        Suivant := Suivant + 1;
        GraphesEntiers.TrouverAdjacent(G, UnSommet, Suivant, Adjacent, Présent);
      END LOOP;
    END LOOP;
  END Élargir;

Visité: EnsemblesG.TypEnsemble;
LeSommet: Natural;
Trouvé: Boolean;
Q: FilesG.File;
BEGIN
  EnsemblesG.Vider(Visité);
  FilesG.Vider(Q);        -- initialiser
  FOR Sommet IN 1..N LOOP -- essayer tous les sommets possibles
    LeSommet := Sommet;
    GraphesEntiers.TrouverSommet(G, LeSommet, Trouvé);
    IF Trouvé AND NOT EnsemblesG.Membre(LeSommet, Visité) THEN
      -- si sommet existe et pas visité
      Ada.Text_IO.Put(Item => "Noeud ");
      Ada.Integer_Text_IO.Put(Item => LeSommet, Width => 4); Ada.Text_IO.New_Line;
      Élargir(LeSommet, Visité, Q);
    END IF;
  END LOOP;
END TraverserEnLargeur;

PROCEDURE Warshall(G: IN MatriceB; Fermeture: OUT MatriceB; N: IN Natural) IS
-- Trouver la fermeture transitive de la matrice associée G.
BEGIN
  FOR Sommet IN 1..N LOOP	-- initialiser en copiant la matrice associée
    FOR Voisin IN 1..N LOOP
      Fermeture(Voisin, Sommet) := G(Voisin, Sommet);
    END LOOP;
  END LOOP;
  FOR Sommet IN 1..N LOOP
    FOR Voisin IN 1..N LOOP
      IF Fermeture(Voisin, Sommet) THEN
        FOR Suivant IN 1..N LOOP
          IF Fermeture(Sommet, Suivant) THEN
            Fermeture(Voisin, Suivant) := True;
          END IF;
        END LOOP;
      END IF;
    END LOOP;
  END LOOP;
END Warshall;

PROCEDURE Floyd(G: IN MatriceC; CheminCourt, PointChemin: OUT MatriceC; 
                N: IN Natural) IS
-- Résolution du problème des plus courts chemins de toutes les paires 
-- d'un graphe G en utilisant l'algorithme de Floyd. Le graphe G est 
-- représenté par une matrice où chaque élément représente une longueur
-- de chemin entre les sommets correspondant à ses indices ou a une valeur
-- Infinité s'il n'y a pas de chemin.  PointChemin(Sommet, Voisin)
-- montrera un point intermédiaire du chemin Floyd(Sommet, Voisin)
-- et peut être utilisé pour donner le plus court chemin complet.
BEGIN
  FOR Sommet IN 1..N LOOP      -- initialiser en faisant une copie
    FOR Voisin IN 1..N LOOP
      CheminCourt(Sommet, Voisin) := G(Sommet, Voisin);
      PointChemin(Sommet, Voisin) := 0;
    END LOOP;
    CheminCourt(Sommet, Sommet) := 0; -- longueur du chemin à soi-même = 0
  END LOOP;
  FOR Sommet IN 1..N LOOP
    FOR Voisin IN 1..N LOOP
      FOR Suivant IN 1..N LOOP
        IF CheminCourt(Voisin, Sommet) + CheminCourt(Sommet, Suivant)
                                     < CheminCourt(Voisin, Suivant) THEN
          -- conserver chemin plus court
          CheminCourt(Voisin, Suivant) :=
                CheminCourt(Voisin, Sommet) + CheminCourt(Sommet, Suivant);
          PointChemin(Voisin, Suivant) := Sommet;
        END IF;
      END LOOP;
    END LOOP;
  END LOOP;
END Floyd;

PROCEDURE AfficherChemin(Chemin: IN MatriceC; Sommet, Voisin: IN Natural;
                         Premier: IN OUT Boolean) IS
-- Afficher le chemin le plus court entre Sommet et Voisin
Suivant: Natural;
BEGIN
  Suivant := Chemin(Sommet, Voisin);
  IF Suivant = 0 THEN
    RETURN;
  END IF;
  AfficherChemin(Chemin, Sommet, Suivant, Premier);
  IF Premier THEN
    Ada.Text_IO.Put(Item => " par");
    Premier := False;
  ELSE
    Ada.Text_IO.Put(Item => " et");
  END IF;
  Ada.Integer_Text_IO.Put(Item => Suivant, Width => 4);
  AfficherChemin(Chemin, Suivant, Voisin, Premier);
END AfficherChemin;

BEGIN
  Ada.Text_IO.Put(Item => "Début ...(Prim.dat)..."); Ada.Text_IO.New_Line;
END AlgorithmesGraphes;
