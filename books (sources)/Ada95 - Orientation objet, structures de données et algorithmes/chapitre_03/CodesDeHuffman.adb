--          Copyright © 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO;
PROCEDURE CodesdeHuffman IS

MaxBits: CONSTANT Natural := 20;				-- nombre maximum de bits dans les codes
MaxPos: CONSTANT Natural := MaxBits + 1;
MaxSymboles: CONSTANT Natural := 26;			-- nombre maximum de codes différents
MaxNoeuds: CONSTANT Natural := MaxSymboles*2-1; -- nombre maximum de noeuds

TYPE TypeDescendant IS (Left, Right);
SUBTYPE TypeIndex IS Natural RANGE 0..MaxNoeuds;
SUBTYPE Bit IS Character RANGE '0'..'1';
TYPE VecteurBits IS ARRAY (1..MaxBits) OF Bit;
TYPE TypeCode IS RECORD
                   Bits: VecteurBits;	            -- code du symbole
                   Début: Natural RANGE 1..MaxPos;-- premier bit du code
                 END RECORD;
TYPE TypeNoeud IS RECORD					                -- élément de l'arbre
                    Fréquence: Natural;
                    Parent: TypeIndex;
                    Descendant: TypeDescendant;
                  END RECORD;

Alphabet: ARRAY (1..MaxSymboles) OF Character;    -- symboles
Codes: ARRAY (1..MaxSymboles) OF TypeCode;        -- codes
Noeuds: ARRAY (1..MaxNoeuds) OF TypeNoeud;        -- noeuds de l'arbre
N: Natural RANGE 0..MaxSymboles;                  -- indices des symboles
Noeud, Index1, Index2: TypeIndex;          -- indices de l'arbre
Code: TypeCode;
Petit1, Petit2, Longueur: Natural;                -- plus petites fréquences
F1: Ada.Text_IO.File_Type;
Nom: String(1..25);

BEGIN
  -- Initialization
  FOR Noeud IN 1..MaxNoeuds LOOP    -- vider arbre
    Noeuds(Noeud).Fréquence := 0;
    Noeuds(Noeud).Parent := 0;
  END LOOP;
  FOR Index IN 1..MaxSymboles LOOP  -- vider alphabet
    Alphabet(Index) := ' ';
  END LOOP;
  Ada.Text_IO.Put(Item => "Donnez le nom du fichier de données");
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Get_Line(Item => Nom, Last => Longueur);
  Ada.Text_IO.Open(File => F1, Mode => Ada.Text_IO.In_File, Name => Nom(1..Longueur));
  Ada.Text_IO.Set_Input(File => F1);
  -- Lire alphabet et fréquences
  Ada.Integer_Text_IO.Get(Item => N);
  FOR Index IN 1..N LOOP
	  Ada.Integer_Text_IO.Get(Item => Noeuds(Index).Fréquence);
	  Ada.Text_IO.Get(Item => Alphabet(Index));
  END LOOP;
  Ada.Text_IO.Set_Input(File => Ada.Text_IO.Standard_Input);
  Ada.Text_IO.Close(F1);
  -- Construire l'arbre
  FOR Libre IN N+1..2*N-1 LOOP	-- Libre indique le prochain noeud libre
    -- chercher deux symboles non utilisés ayant les plus petites fréquences
    Index1 := 0; Index2 := 0;
    Petit1 := Natural'Last;
    Petit2 := Natural'Last;
    FOR Noeud IN 1..Libre-1 LOOP
      IF Noeuds(Noeud).Parent = 0 THEN             -- noeud non encore utilisé
        IF Noeuds(Noeud).Fréquence < Petit1 THEN   -- nouveau plus petit
          Petit2 := Petit1;
          Petit1 := Noeuds(Noeud).Fréquence;
          Index2 := Index1;
          Index1 := Noeud;
        ELSIF Noeuds(Noeud).Fréquence < Petit2 THEN -- second plus petit
          Petit2 := Noeuds(Noeud).Fréquence;
          Index2 := Noeud;
        END IF;
      END IF;
    END LOOP;
    -- nouveau noeud a Index1 comme fils gauche et Index2 comme fils droit 
    Noeuds(Index1).Parent := Libre;
    Noeuds(Index1).Descendant := Left;
    Noeuds(Index2).Parent := Libre;
    Noeuds(Index2).Descendant := Right;
    Noeuds(Libre).Fréquence := 
                      Noeuds(Index1).Fréquence + Noeuds(Index2).Fréquence;
  END LOOP;
  -- extraire les codes de l'arbre
  FOR Index IN 1..N LOOP
    Code.Début := MaxPos;
    Noeud := Index;
    WHILE Noeuds(Noeud).Parent /= 0 LOOP        -- monter dans l'arbre
      IF Noeuds(Noeud).Descendant = Left THEN   -- descendant gauche: 0
        Code.Début := Code.Début - 1;
        Code.Bits(Code.Début) := '0';
      ELSE                                      -- descendant droit: 1
        Code.Début := Code.Début - 1;
        Code.Bits(Code.Début) := '1';
      END IF;
      Noeud := Noeuds(Noeud).Parent;
    END LOOP;
    Codes(Index) := Code;                       -- garder nouveau code
  END LOOP;
  -- afficher résultats
  FOR Index IN 1..N LOOP
    Ada.Text_IO.Put(Item => Alphabet(Index)); Ada.Text_IO.Put(Item => ' ');
    Ada.Integer_Text_IO.Put(Item => Noeuds(Index).Fréquence, Width => 3);
    Ada.Text_IO.Put(Item => ' ');
    FOR IndexBit IN Codes(Index).Début..MaxBits LOOP  -- afficher code
      Ada.Text_IO.Put(Item => Codes(Index).Bits(IndexBit));
    END LOOP;
    Ada.Text_IO.New_Line;
  END LOOP;
END CodesdeHuffman;
