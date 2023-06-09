PROCEDURE TrierTournoi(Table: IN OUT TypeNonTrié;
                       NombreÉléments: IN Natural) IS
-- Appliquer le tri du tournoi à Table; Table contient des données
-- initialement non triées, et triées à la fin de la procédure.
-- Un arbre est initialisé avec les données de Table, l'élément
-- le plus grand est sorti, l'arbre est réajusté, et le processus
-- est répété jusqu'à ce que l'arbre soit vide. 

TYPE TypeNoeud IS RECORD
                    Index: Natural;
                    Valeur: TypeÉlément;
                  END RECORD;
TYPE TypeArbre IS ARRAY (1..2*Maximum-1) OF TypeNoeud;
   
  PROCEDURE CalculerNombreFeuilles(NombreÉléments: IN Natural;
                                   NombreFeuilles: OUT Natural) IS
  -- Calculer le nombre de feuilles de l'arbre minimum qui peut 
  -- contenir NombreÉléments éléments.
  Nombre: Natural;
  BEGIN
    Nombre := NombreÉléments;
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
  -- Les feuilles contiennent les valeurs des éléments, tandis que les
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
       
  PROCEDURE CréerArbre(Arbre: IN OUT TypeArbre; NombreÉléments: IN Natural;
                       NonTrié: IN OUT TypeNonTrié) IS
  -- Calculer le nombre de feuilles et la taille de l'arbre.  Copier
  -- les éléments du tableau NonTrié dans les feuilles de l'arbre, 
  -- puis construire l'arbre des feuilles jusqu'à la racine.
  
  NuméroÉlément: Natural;
  Feuilles: CONSTANT Boolean := True;
  Position, NombreFeuilles, TailleArbre: Natural;
  
  BEGIN
    CalculerNombreFeuilles(NombreÉléments, NombreFeuilles);
    TailleArbre := NombreFeuilles * 2 -1;
    FOR Position IN NombreÉléments+1..NombreFeuilles LOOP
      -- remplir de marqueurs
      NonTrié(Position) := Marqueur;
    END LOOP;
    Position := NombreFeuilles;	-- copier données dans les feuilles
    FOR Numéro IN 1..NombreFeuilles LOOP
      Arbre(Position).Valeur := NonTrié(Numéro);
      Arbre(Position).Index := 0;
      Position := Position + 1;
    END LOOP;
    -- construire premier niveau de noeuds au dessus des feuilles
    ConstruireNoeuds(Arbre, Feuilles, NombreFeuilles, TailleArbre-1);
    NuméroÉlément := NombreFeuilles;
    WHILE NuméroÉlément > 1 LOOP -- construire prochain niveau de noeuds
      ConstruireNoeuds(Arbre, NOT Feuilles, NuméroÉlément/2, NuméroÉlément-2);
      NuméroÉlément := NuméroÉlément / 2;
    END LOOP; 
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put(Item => "CréerArbre: ");
    Ada.Text_IO.New_Line;
    FOR Numéro IN 1..NombreFeuilles-1 LOOP
      Ada.Integer_Text_IO.Put(Item => Arbre(Numéro).Index, Width => 8);
      Ada.Text_IO.Put(Item => " ");
      IF Numéro MOD 8 = 0 THEN  Ada.Text_IO.New_Line; END IF;
    END LOOP;
	  Ada.Text_IO.New_Line;
    FOR Numéro IN NombreFeuilles..TailleArbre LOOP
      Sortir(Item => Arbre(Numéro).Valeur, Largeur => 8);
      Ada.Text_IO.Put(Item => " ");
      IF (Numéro-NombreFeuilles+1) MOD 8 = 0 THEN  Ada.Text_IO.New_Line; END IF;
    END LOOP;
    Ada.Text_IO.New_Line;
  END CréerArbre;
   
  PROCEDURE AjusterArbre(Arbre: IN OUT TypeArbre; Position: IN Natural) IS
  -- L'élément à Position vient d'être éliminé; ajuster les noeuds
  -- supérieurs d'Arbre en remontant le voisin du noeud supprimé.
     
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
  CréerArbre(Arbre, NombreÉléments, Table);
  FOR Numéro IN 1..NombreÉléments LOOP
    -- copier arbre dans tableau Table
    Position := Arbre(1).Index;
    Table(Numéro) := Arbre(Position).Valeur;
    Arbre(Position).Valeur := Marqueur;	-- supprimer élément traité
    AjusterArbre(Arbre, Position);
  END LOOP;
END TrierTournoi;
