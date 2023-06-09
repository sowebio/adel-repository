--          Copyright � 1998 Philippe J. Gabrini
WITH Graphes.Orient�s, TDAFile, EnsemblesDiscrets;
WITH Ada.Text_IO, Ada.Integer_Text_IO;

PROCEDURE AlgorithmesGraphes IS
Infinit�: CONSTANT Integer := 32000;
N: CONSTANT Natural := 25;
SUBTYPE Intervalle IS Natural RANGE 1..N;

PACKAGE FilesG IS NEW TDAFile(Type�l�ment => Natural);
PACKAGE EnsemblesG IS NEW EnsemblesDiscrets(Type_�l�ment => Intervalle);

PROCEDURE Afficher(�lt: IN Intervalle);
PACKAGE GraphesBase IS NEW Graphes(Type_�l�ment => Intervalle,
                                   AfficherClef => Afficher,
                                   AfficherSommet => Afficher);
FUNCTION M�me(�lt: Intervalle) RETURN Intervalle;
PACKAGE GraphesEntiers IS NEW GraphesBase.Orient�s(Nom_Sommet => Intervalle, Nom => M�me);

TYPE Vecteur IS ARRAY (1..N) OF Integer;
TYPE MatriceC IS ARRAY (1..N, 1..N) OF Natural; -- pour plus court chemin
TYPE MatriceB IS ARRAY (1..N, 1..N) OF Boolean; -- for Warshall
     
PROCEDURE Afficher(�lt: IN Intervalle) IS
BEGIN
  Ada.Integer_Text_IO.Put(�lt, 5);
END Afficher;

FUNCTION M�me(�lt: Intervalle) RETURN Intervalle IS
BEGIN
  RETURN �lt;
END M�me;

PROCEDURE Prim(Co�t: IN GraphesEntiers.Graphe;
               Co�tBas, Proche: IN OUT Vecteur;
               N: IN Natural) IS
-- Donne les ar�tes d'un arbre de recouvrement � co�t minimum pour le graphe
-- Co�t avec les sommets {1,2,...,N}.  L'arbre commence au sommet 1 et cro�t
-- � partir de l�.
Bas: Natural;       -- indices
Min: Integer;               -- co�t le plus bas trouv�
BEGIN
  FOR UnSommet IN 2..N LOOP	-- initialiser
    Co�tBas(UnSommet) := GraphesEntiers.Poids(Co�t, 1, UnSommet);
    Proche(UnSommet) := 1;
  END LOOP;
  FOR UnSommet IN 2..N LOOP	-- trouver sommet Bas le plus proche du sommet 1
    Min := Co�tBas(2);
    Bas := 2;
    FOR Voisin IN 3..N LOOP
      IF Co�tBas(Voisin) < Min THEN
        Min := Co�tBas(Voisin);
        Bas := Voisin;
      END IF;
    END LOOP;
    -- montrer ar�te
    Ada.Integer_Text_IO.Put(Item => Co�tBas(Bas), Width => 5);
	  Ada.Integer_Text_IO.Put(Item => Bas, Width => 4);
    Ada.Integer_Text_IO.Put(Item => Proche(Bas), Width => 4);
    Ada.Text_IO.New_Line;
    Co�tBas(Bas) := Infinit�;   -- Bas est ajout� � l'arbre
    FOR Voisin IN 2..N LOOP     -- ajuster co�ts
      IF (GraphesEntiers.Poids(Co�t, Bas, Voisin) < Co�tBas(Voisin))
                                   AND (Co�tBas(Voisin) < Infinit�) THEN
        Co�tBas(Voisin) := GraphesEntiers.Poids(Co�t, Bas, Voisin);
        Proche(Voisin) := Bas;
      END IF;
    END LOOP;
  END LOOP;
END Prim;

PROCEDURE TriTopo(G: IN GraphesEntiers.Graphe; Sommet: IN Natural;
                  Visit�: IN OUT EnsemblesG.TypEnsemble) IS
-- Tri topologique d'un graphe orient� acyclique.  Cette proc�dure
-- produit une liste des sommets du graphe en ordre inverse de 
-- l'ordre topologique. Sommet est un num�ro de sommet, et Visit� 
-- un ensemble de tous les sommets visit�s.
Suivant, Nombre: Integer;
Trouv�: Boolean;
BEGIN
  EnsemblesG.Inclure(Visit�, Sommet);
  Nombre := 1;
  GraphesEntiers.TrouverAdjacent(G, Sommet, Nombre, Suivant, Trouv�);
  -- appliquer TriTopo � tous les sommets adjacents � Sommet
  WHILE Trouv� AND (Nombre <= N) LOOP
    IF NOT EnsemblesG.Membre(Suivant, Visit�) THEN
      TriTopo(G, Suivant, Visit�);
    END IF;
    Nombre := Nombre + 1;
    GraphesEntiers.TrouverAdjacent(G, Sommet, Nombre, Suivant, Trouv�);
  END LOOP;
  Ada.Integer_Text_IO.Put(Item => Sommet, Width => 4);
END TriTopo;

PROCEDURE TraverserEnProfondeur(G: IN GraphesEntiers.Graphe) IS
-- Appliquer la m�thode en profondeur pour traverser un graphe G qui
-- peut ne pas �tre un graphe connexe.

  PROCEDURE Approfondir(Sommet: IN Natural;
                        Visit�: IN OUT EnsemblesG.TypEnsemble) IS
  -- Traverser r�cursivement tous les sommets accessibles � partir de Sommet.
  Adjacent: Integer;
  Pr�sent: Boolean;
  BEGIN
    EnsemblesG.Inclure(Visit�, Sommet);
    FOR AutreSommet IN 1..N LOOP -- essayer tous les sommets possibles
    -- appliquer la m�thode en profondeur � tous les sommets adjacents � Sommet
      GraphesEntiers.TrouverAdjacent(G, Sommet, AutreSommet, Adjacent, Pr�sent);
      IF Pr�sent AND THEN NOT EnsemblesG.Membre(Adjacent, Visit�) THEN
        Approfondir(Adjacent, Visit�);
        -- montrer ar�te
        Ada.Integer_Text_IO.Put(Item => Sommet, Width => 2); Ada.Text_IO.Put(Item => "--");
        Ada.Integer_Text_IO.Put(Item => Adjacent, Width => 2); Ada.Text_IO.New_Line;
      END IF;
    END LOOP;
  END Approfondir;

LeSommet: Natural;
Trouv�: Boolean;
Visit�: EnsemblesG.TypEnsemble;
BEGIN
  EnsemblesG.Vider(Visit�);
  FOR Sommet IN 1..N LOOP	-- traverser tous les sommets possibles
    LeSommet := Sommet;
    GraphesEntiers.TrouverSommet(G, LeSommet, Trouv�);
    IF Trouv� AND NOT EnsemblesG.Membre(LeSommet, Visit�) THEN
      -- le sommet existe et n'est pas encore visit�
      Ada.Text_IO.Put(Item => "Noeud ");
      Ada.Integer_Text_IO.Put(Item => Sommet, Width => 4); Ada.Text_IO.New_Line;
      Approfondir(LeSommet, Visit�);
    END IF;
  END LOOP;
END TraverserEnProfondeur;

PROCEDURE TraverserEnLargeur(G: IN OUT GraphesEntiers.Graphe) IS
-- Visiter tous les sommets du graphe G en utilisant la m�thode en largeur.
-- Le graphe G peut ne pas �tre un graphe connexe.

  PROCEDURE �largir(Sommet: IN Natural;
                    Visit�: IN OUT EnsemblesG.TypEnsemble;
                    Q: IN OUT FilesG.File) IS
  -- Traverser tous les sommets accessibles de Sommet en utilisant
  -- la m�thode de travers�e en largeur.
  Adjacent, Suivant, UnSommet: Integer;
  Pr�sent: Boolean;
  BEGIN
    EnsemblesG.Inclure(Visit�, Sommet);    -- commencer par Sommet
    FilesG.Enfiler(Q, Sommet);
    WHILE FilesG.Longueur(Q) /= 0 LOOP
      FilesG.D�filer(Q, UnSommet);
      Suivant := 1;
      GraphesEntiers.TrouverAdjacent(G, UnSommet, Suivant, Adjacent, Pr�sent);
      -- visiter tous les sommets adjacents � UnSommet
      WHILE Pr�sent AND (Suivant <= N) LOOP
        IF NOT EnsemblesG.Membre(Adjacent, Visit�) THEN
          EnsemblesG.Inclure(Visit�, Adjacent);
          FilesG.Enfiler(Q, Adjacent);
          -- afficher ar�te
          Ada.Integer_Text_IO.Put(Item => UnSommet, Width => 2);
          Ada.Text_IO.Put(Item => "--");
          Ada.Integer_Text_IO.Put(Item => Adjacent, Width => 2); Ada.Text_IO.New_Line;
        END IF;
        Suivant := Suivant + 1;
        GraphesEntiers.TrouverAdjacent(G, UnSommet, Suivant, Adjacent, Pr�sent);
      END LOOP;
    END LOOP;
  END �largir;

Visit�: EnsemblesG.TypEnsemble;
LeSommet: Natural;
Trouv�: Boolean;
Q: FilesG.File;
BEGIN
  EnsemblesG.Vider(Visit�);
  FilesG.Vider(Q);        -- initialiser
  FOR Sommet IN 1..N LOOP -- essayer tous les sommets possibles
    LeSommet := Sommet;
    GraphesEntiers.TrouverSommet(G, LeSommet, Trouv�);
    IF Trouv� AND NOT EnsemblesG.Membre(LeSommet, Visit�) THEN
      -- si sommet existe et pas visit�
      Ada.Text_IO.Put(Item => "Noeud ");
      Ada.Integer_Text_IO.Put(Item => LeSommet, Width => 4); Ada.Text_IO.New_Line;
      �largir(LeSommet, Visit�, Q);
    END IF;
  END LOOP;
END TraverserEnLargeur;

PROCEDURE Warshall(G: IN MatriceB; Fermeture: OUT MatriceB; N: IN Natural) IS
-- Trouver la fermeture transitive de la matrice associ�e G.
BEGIN
  FOR Sommet IN 1..N LOOP	-- initialiser en copiant la matrice associ�e
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
-- R�solution du probl�me des plus courts chemins de toutes les paires 
-- d'un graphe G en utilisant l'algorithme de Floyd. Le graphe G est 
-- repr�sent� par une matrice o� chaque �l�ment repr�sente une longueur
-- de chemin entre les sommets correspondant � ses indices ou a une valeur
-- Infinit� s'il n'y a pas de chemin.  PointChemin(Sommet, Voisin)
-- montrera un point interm�diaire du chemin Floyd(Sommet, Voisin)
-- et peut �tre utilis� pour donner le plus court chemin complet.
BEGIN
  FOR Sommet IN 1..N LOOP      -- initialiser en faisant une copie
    FOR Voisin IN 1..N LOOP
      CheminCourt(Sommet, Voisin) := G(Sommet, Voisin);
      PointChemin(Sommet, Voisin) := 0;
    END LOOP;
    CheminCourt(Sommet, Sommet) := 0; -- longueur du chemin � soi-m�me = 0
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
  Ada.Text_IO.Put(Item => "D�but ...(Prim.dat)..."); Ada.Text_IO.New_Line;
END AlgorithmesGraphes;
