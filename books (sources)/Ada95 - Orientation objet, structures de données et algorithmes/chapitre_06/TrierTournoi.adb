PROCEDURE TrierTournoi(Table: IN OUT TypeNonTri�;
                       Nombre�l�ments: IN Natural) IS
-- Appliquer le tri du tournoi � Table; Table contient des donn�es
-- initialement non tri�es, et tri�es � la fin de la proc�dure.
-- Un arbre est initialis� avec les donn�es de Table, l'�l�ment
-- le plus grand est sorti, l'arbre est r�ajust�, et le processus
-- est r�p�t� jusqu'� ce que l'arbre soit vide. 

TYPE TypeNoeud IS RECORD
                    Index: Natural;
                    Valeur: Type�l�ment;
                  END RECORD;
TYPE TypeArbre IS ARRAY (1..2*Maximum-1) OF TypeNoeud;
   
  PROCEDURE CalculerNombreFeuilles(Nombre�l�ments: IN Natural;
                                   NombreFeuilles: OUT Natural) IS
  -- Calculer le nombre de feuilles de l'arbre minimum qui peut 
  -- contenir Nombre�l�ments �l�ments.
  Nombre: Natural;
  BEGIN
    Nombre := Nombre�l�ments;
    NombreFeuilles := 1;
    LOOP
      NombreFeuilles := NombreFeuilles * 2;
      Nombre := Nombre / 2;
      EXIT WHEN Nombre = 0;
	  END LOOP;
  END CalculerNombreFeuilles;
 
  PROCEDURE ConstruireNoeuds(Arbre: IN OUT TypeArbre; Feuille: IN Boolean;
                             Gauche, Droite: IN Natural) IS
  -- Construire les noeuds parents d'un niveau de noeuds de l'arbre.
  -- Les feuilles contiennent les valeurs des �l�ments, tandis que les
  -- noeuds internes contiennent les indices d'autres noeuds de l'arbre.
  Position: Natural;
  BEGIN 
    IF Feuille THEN   -- comparer niveau feuille
	    Position := Gauche;
      WHILE Position <= Droite LOOP
        IF Arbre(Position).Valeur > Arbre(Position+1).Valeur THEN
           Arbre(Position / 2).Index := Position;
        ELSE 
           Arbre(Position / 2).Index := Position+1;
        END IF;
		    Position := Position + 2;
      END LOOP;
    ELSE             -- comparer niveau non feuille 
	    Position := Gauche;
      WHILE Position <= Droite LOOP
        IF Arbre(Arbre(Position).Index).Valeur >
                   Arbre(Arbre(Position+1).Index).Valeur THEN
           Arbre(Position / 2) := Arbre(Position);
        ELSE 
           Arbre(Position / 2) := Arbre(Position+1);
        END IF;
		    Position := Position + 2;
      END LOOP;
    END IF; 
  END ConstruireNoeuds;
       
  PROCEDURE Cr�erArbre(Arbre: IN OUT TypeArbre; Nombre�l�ments: IN Natural;
                       NonTri�: IN OUT TypeNonTri�) IS
  -- Calculer le nombre de feuilles et la taille de l'arbre.  Copier
  -- les �l�ments du tableau NonTri� dans les feuilles de l'arbre, 
  -- puis construire l'arbre des feuilles jusqu'� la racine.
  
  Num�ro�l�ment: Natural;
  Feuilles: CONSTANT Boolean := True;
  Position, NombreFeuilles, TailleArbre: Natural;
  
  BEGIN
    CalculerNombreFeuilles(Nombre�l�ments, NombreFeuilles);
    TailleArbre := NombreFeuilles * 2 -1;
    FOR Position IN Nombre�l�ments+1..NombreFeuilles LOOP
      -- remplir de marqueurs
      NonTri�(Position) := Marqueur;
    END LOOP;
    Position := NombreFeuilles;	-- copier donn�es dans les feuilles
    FOR Num�ro IN 1..NombreFeuilles LOOP
      Arbre(Position).Valeur := NonTri�(Num�ro);
      Arbre(Position).Index := 0;
      Position := Position + 1;
    END LOOP;
    -- construire premier niveau de noeuds au dessus des feuilles
    ConstruireNoeuds(Arbre, Feuilles, NombreFeuilles, TailleArbre-1);
    Num�ro�l�ment := NombreFeuilles;
    WHILE Num�ro�l�ment > 1 LOOP -- construire prochain niveau de noeuds
      ConstruireNoeuds(Arbre, NOT Feuilles, Num�ro�l�ment/2, Num�ro�l�ment-2);
      Num�ro�l�ment := Num�ro�l�ment / 2;
    END LOOP; 
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put(Item => "Cr�erArbre: ");
    Ada.Text_IO.New_Line;
    FOR Num�ro IN 1..NombreFeuilles-1 LOOP
      Ada.Integer_Text_IO.Put(Item => Arbre(Num�ro).Index, Width => 8);
      Ada.Text_IO.Put(Item => " ");
      IF Num�ro MOD 8 = 0 THEN  Ada.Text_IO.New_Line; END IF;
    END LOOP;
	  Ada.Text_IO.New_Line;
    FOR Num�ro IN NombreFeuilles..TailleArbre LOOP
      Sortir(Item => Arbre(Num�ro).Valeur, Largeur => 8);
      Ada.Text_IO.Put(Item => " ");
      IF (Num�ro-NombreFeuilles+1) MOD 8 = 0 THEN  Ada.Text_IO.New_Line; END IF;
    END LOOP;
    Ada.Text_IO.New_Line;
  END Cr�erArbre;
   
  PROCEDURE AjusterArbre(Arbre: IN OUT TypeArbre; Position: IN Natural) IS
  -- L'�l�ment � Position vient d'�tre �limin�; ajuster les noeuds
  -- sup�rieurs d'Arbre en remontant le voisin du noeud supprim�.
     
  Place: Natural;
  
  BEGIN
    IF (Position MOD 2) = 0 THEN			   -- remplacer parent par
      Arbre(Position / 2).Index := Position+1; -- voisin droit
    ELSE 									                     --   ou  
      Arbre(Position / 2).Index := Position-1; -- voisin gauche
    END IF;
    Place := Position / 2;
    WHILE Place > 1 LOOP
      -- remonter l'indice de la plus grande valeur en comparant 
      -- des paires de voisins (pair-impair)
      IF (Place MOD 2) = 0 THEN	-- Place pair, Place+1 impair
        IF Arbre(Arbre(Place).Index).Valeur >
                   Arbre(Arbre(Place+1).Index).Valeur THEN
           Arbre(Place / 2).Index := Arbre(Place).Index;
        ELSE  
           Arbre(Place / 2).Index := Arbre(Place+1).Index; 
        END IF;
      ELSE 						-- Place-1 pair, Place impair
        IF Arbre(Arbre(Place-1).Index).Valeur >
                   Arbre(Arbre(Place).Index).Valeur THEN
           Arbre(Place / 2).Index := Arbre(Place-1).Index;
        ELSE  
           Arbre(Place / 2).Index := Arbre(Place).Index;
        END IF;
      END IF;
      Place := Place / 2;		-- remonter dans l'arbre
    END LOOP; 
  END AjusterArbre;
 
Position: Integer;
Arbre: TypeArbre;

BEGIN
  Cr�erArbre(Arbre, Nombre�l�ments, Table);
  FOR Num�ro IN 1..Nombre�l�ments LOOP
    -- copier arbre dans tableau Table
    Position := Arbre(1).Index;
    Table(Num�ro) := Arbre(Position).Valeur;
    Arbre(Position).Valeur := Marqueur;	-- supprimer �l�ment trait�
    AjusterArbre(Arbre, Position);
  END LOOP;
END TrierTournoi;
