----------------------------------------------------------------------------
--   Fichier:  Calculatrice.adb
--   Objectif: Calculatrice acceptant et calculant la valeur
--             d'expressions avec parenthèses.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Ada.Text_IO, Ada.Integer_Text_IO, PJG.Piles;
PROCEDURE Calculatrice IS

PACKAGE POpérandes IS NEW PJG.Piles(Integer);
PACKAGE POpérateurs IS NEW PJG.Piles(Character);

Pile_Opérandes: POpérandes.Type_Pile;
Pile_Opérateurs: POpérateurs.Type_Pile;

Opérateur: Character;
Opérande: Integer;
Fin_Ligne: Boolean;
Trouvé_Opérande: Boolean := False;

Erreur_Opérateur: EXCEPTION;

PROCEDURE Calculer IS
Gauche, Droite: Integer;
Opérateur: Character;
BEGIN
  POpérandes.Désempiler(Pile_Opérandes, Droite);
  POpérandes.Désempiler(Pile_Opérandes, Gauche);
  POpérateurs.Désempiler(Pile_Opérateurs, Opérateur);
  CASE Opérateur IS
    WHEN '+' => POpérandes.Empiler(Pile_Opérandes, Gauche + Droite);
    WHEN '-' => POpérandes.Empiler(Pile_Opérandes, Gauche - Droite);
    WHEN '*' => POpérandes.Empiler(Pile_Opérandes, Gauche * Droite);
    WHEN '/' => POpérandes.Empiler(Pile_Opérandes, Gauche / Droite);
    WHEN OTHERS => RAISE Erreur_Opérateur;
  END CASE;
END Calculer;

FUNCTION Priorité(Opérateur: Character) RETURN Natural IS
BEGIN
  CASE Opérateur IS
    WHEN '+' | '-' => RETURN 1;
    WHEN '*' | '/' => RETURN 2;
    WHEN '$' | '(' => RETURN 0;
    WHEN OTHERS => RAISE Erreur_Opérateur;
  END CASE;
END Priorité;

BEGIN  -- Calculatrice
  POpérateurs.Empiler(Pile_Opérateurs, '$');
  Ada.Text_IO.Put("Donnez une expression: ");
  LOOP  -- prochain caractère non espace
    LOOP
      Ada.Text_IO.Look_Ahead(Opérateur, Fin_Ligne);
      IF Fin_Ligne THEN            -- fin de ligne = fin de l'expression
        Opérateur := '.';
      END IF;
      EXIT WHEN Opérateur /= ' ';  -- sortir boucle si trouvé non espace
      Ada.Text_IO.Get(Opérateur);  -- sinon sauter espace
    END LOOP;
    -- Traiter opérateur ou opérande
    IF Opérateur IN '0'..'9' THEN  -- c'est un opérande
      IF Trouvé_Opérande THEN      -- opérande impossible immédiatement après un autre
        Ada.Text_IO.Put ("Opérateur manquant");
        EXIT;
      END IF;
      Ada.Integer_Text_IO.Get(Opérande);  -- lire l'opérande
      POpérandes.Empiler(Pile_Opérandes, Opérande);
      Trouvé_Opérande := True;     -- noter qu'on a un opérande
    ELSE                           -- pas un opérande
      Trouvé_Opérande := False;    -- le noter
      EXIT WHEN Opérateur = '.';    -- fin de l'expression
      Ada.Text_IO.Get(Opérateur);         -- lire l'opérateur
      CASE Opérateur IS
        WHEN '+' | '-' | '*' | '/' =>
             WHILE Priorité(Opérateur) 
                   <= Priorité(POpérateurs.Sommet(Pile_Opérateurs)) LOOP
               Calculer;
             END LOOP;
             POpérateurs.Empiler(Pile_Opérateurs, Opérateur);
        WHEN '(' =>                -- empiler parenthèses gauches
             POpérateurs.Empiler(Pile_Opérateurs, Opérateur);
        WHEN ')' =>                -- vider pile jusqu'à '('
             WHILE Priorité(POpérateurs.Sommet(Pile_Opérateurs)) 
                   > Priorité('(') LOOP
               Calculer;
             END LOOP;
             POpérateurs.Désempiler(Pile_Opérateurs, Opérateur);
             IF Opérateur /= '(' THEN
               Ada.Text_IO.Put("Parenthèse gauche manquante");
               EXIT;
             END IF;
        WHEN OTHERS =>
             Ada.Text_IO.Put("Opérateur invalide '");
             Ada.Text_IO.Put(Opérateur);
             Ada.Text_IO.Put("'");
             EXIT;
      END CASE;
    END IF;
  END LOOP;
  Ada.Integer_Text_IO.Put(POpérandes.Taille(Pile_Opérandes), Width => 1);
  Ada.Text_IO.New_Line; -- test héritage implicite
  IF Opérateur = '.' THEN
    WHILE Priorité(POpérateurs.Sommet(Pile_Opérateurs)) > Priorité('$') LOOP
      Calculer;
    END LOOP;
    IF POpérateurs.Sommet(Pile_Opérateurs) = '$' THEN
      Ada.Integer_Text_IO.Put(POpérandes.Sommet(Pile_Opérandes), Width => 1);
    ELSE
      Ada.Text_IO.Put("Parenthèse droite manquante");
    END IF;
  END IF;
  Ada.Text_IO.New_Line;
END Calculatrice;
