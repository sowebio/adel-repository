----------------------------------------------------------------------------
--   Fichier:  Calculatrice.adb
--   Objectif: Calculatrice acceptant et calculant la valeur
--             d'expressions avec parenth�ses.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright � 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Ada.Text_IO, Ada.Integer_Text_IO, PJG.Piles;
PROCEDURE Calculatrice IS

PACKAGE POp�randes IS NEW PJG.Piles(Integer);
PACKAGE POp�rateurs IS NEW PJG.Piles(Character);

Pile_Op�randes: POp�randes.Type_Pile;
Pile_Op�rateurs: POp�rateurs.Type_Pile;

Op�rateur: Character;
Op�rande: Integer;
Fin_Ligne: Boolean;
Trouv�_Op�rande: Boolean := False;

Erreur_Op�rateur: EXCEPTION;

PROCEDURE Calculer IS
Gauche, Droite: Integer;
Op�rateur: Character;
BEGIN
  POp�randes.D�sempiler(Pile_Op�randes, Droite);
  POp�randes.D�sempiler(Pile_Op�randes, Gauche);
  POp�rateurs.D�sempiler(Pile_Op�rateurs, Op�rateur);
  CASE Op�rateur IS
    WHEN '+' => POp�randes.Empiler(Pile_Op�randes, Gauche + Droite);
    WHEN '-' => POp�randes.Empiler(Pile_Op�randes, Gauche - Droite);
    WHEN '*' => POp�randes.Empiler(Pile_Op�randes, Gauche * Droite);
    WHEN '/' => POp�randes.Empiler(Pile_Op�randes, Gauche / Droite);
    WHEN OTHERS => RAISE Erreur_Op�rateur;
  END CASE;
END Calculer;

FUNCTION Priorit�(Op�rateur: Character) RETURN Natural IS
BEGIN
  CASE Op�rateur IS
    WHEN '+' | '-' => RETURN 1;
    WHEN '*' | '/' => RETURN 2;
    WHEN '$' | '(' => RETURN 0;
    WHEN OTHERS => RAISE Erreur_Op�rateur;
  END CASE;
END Priorit�;

BEGIN  -- Calculatrice
  POp�rateurs.Empiler(Pile_Op�rateurs, '$');
  Ada.Text_IO.Put("Donnez une expression: ");
  LOOP  -- prochain caract�re non espace
    LOOP
      Ada.Text_IO.Look_Ahead(Op�rateur, Fin_Ligne);
      IF Fin_Ligne THEN            -- fin de ligne = fin de l'expression
        Op�rateur := '.';
      END IF;
      EXIT WHEN Op�rateur /= ' ';  -- sortir boucle si trouv� non espace
      Ada.Text_IO.Get(Op�rateur);  -- sinon sauter espace
    END LOOP;
    -- Traiter op�rateur ou op�rande
    IF Op�rateur IN '0'..'9' THEN  -- c'est un op�rande
      IF Trouv�_Op�rande THEN      -- op�rande impossible imm�diatement apr�s un autre
        Ada.Text_IO.Put ("Op�rateur manquant");
        EXIT;
      END IF;
      Ada.Integer_Text_IO.Get(Op�rande);  -- lire l'op�rande
      POp�randes.Empiler(Pile_Op�randes, Op�rande);
      Trouv�_Op�rande := True;     -- noter qu'on a un op�rande
    ELSE                           -- pas un op�rande
      Trouv�_Op�rande := False;    -- le noter
      EXIT WHEN Op�rateur = '.';    -- fin de l'expression
      Ada.Text_IO.Get(Op�rateur);         -- lire l'op�rateur
      CASE Op�rateur IS
        WHEN '+' | '-' | '*' | '/' =>
             WHILE Priorit�(Op�rateur) 
                   <= Priorit�(POp�rateurs.Sommet(Pile_Op�rateurs)) LOOP
               Calculer;
             END LOOP;
             POp�rateurs.Empiler(Pile_Op�rateurs, Op�rateur);
        WHEN '(' =>                -- empiler parenth�ses gauches
             POp�rateurs.Empiler(Pile_Op�rateurs, Op�rateur);
        WHEN ')' =>                -- vider pile jusqu'� '('
             WHILE Priorit�(POp�rateurs.Sommet(Pile_Op�rateurs)) 
                   > Priorit�('(') LOOP
               Calculer;
             END LOOP;
             POp�rateurs.D�sempiler(Pile_Op�rateurs, Op�rateur);
             IF Op�rateur /= '(' THEN
               Ada.Text_IO.Put("Parenth�se gauche manquante");
               EXIT;
             END IF;
        WHEN OTHERS =>
             Ada.Text_IO.Put("Op�rateur invalide '");
             Ada.Text_IO.Put(Op�rateur);
             Ada.Text_IO.Put("'");
             EXIT;
      END CASE;
    END IF;
  END LOOP;
  Ada.Integer_Text_IO.Put(POp�randes.Taille(Pile_Op�randes), Width => 1);
  Ada.Text_IO.New_Line; -- test h�ritage implicite
  IF Op�rateur = '.' THEN
    WHILE Priorit�(POp�rateurs.Sommet(Pile_Op�rateurs)) > Priorit�('$') LOOP
      Calculer;
    END LOOP;
    IF POp�rateurs.Sommet(Pile_Op�rateurs) = '$' THEN
      Ada.Integer_Text_IO.Put(POp�randes.Sommet(Pile_Op�randes), Width => 1);
    ELSE
      Ada.Text_IO.Put("Parenth�se droite manquante");
    END IF;
  END IF;
  Ada.Text_IO.New_Line;
END Calculatrice;
