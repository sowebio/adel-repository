----------------------------------------------------------------------------
--   Objectif: Calculatrice acceptant et calculant la valeur
--             d'expressions avec parenthèses.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Ada.Text_IO, Ada.Integer_Text_IO, PJG.Listes.Piles;
PROCEDURE Calculatrice IS

PACKAGE Listes_Opérandes IS NEW PJG.Listes(Integer);
PACKAGE Listes_Opérateurs IS NEW PJG.Listes(character);
PACKAGE Piles_Opérandes IS NEW Listes_Opérandes.Piles;
PACKAGE Piles_Opérateurs IS NEW Listes_Opérateurs.Piles;
USE Piles_Opérandes, Piles_Opérateurs; -- pas d'ambiguïté

Pile_Opérandes: Piles_Opérandes.Type_Pile;
Pile_Opérateurs: Piles_Opérateurs.Type_Pile;

Opérateur: Character;
Opérande: Integer;
Fin_Ligne: Boolean;
Trouvé_Opérande: Boolean := False;

Erreur_Opérateur: EXCEPTION;

PROCEDURE Calculer IS
Gauche, Droite: Integer;
Opérateur: Character;
BEGIN
  Désempiler(Pile_Opérandes, Droite);
  Désempiler(Pile_Opérandes, Gauche);
  Désempiler(Pile_Opérateurs, Opérateur);
  CASE Opérateur IS
    WHEN '+' => Empiler(Pile_Opérandes, Gauche + Droite);
    WHEN '-' => Empiler(Pile_Opérandes, Gauche - Droite);
    WHEN '*' => Empiler(Pile_Opérandes, Gauche * Droite);
    WHEN '/' => EMPILER(Pile_Opérandes, Gauche / Droite);
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
  Empiler(Pile_Opérateurs, '$');
  Ada.Text_IO.Put("Donnez une expression: ");
  LOOP  -- prochain caractère non espace
    LOOP
      Ada.Text_IO.Look_Ahead(Opérateur, Fin_Ligne);
      IF Fin_Ligne THEN            -- fin de ligne = fin de l'expression
        Opérateur := '.';
      END IF;
      EXIT WHEN Opérateur /= ' ';  -- sortir boucle si trouvé non espace
      Ada.Text_IO.Get(Opérateur);              -- sinon sauter espace
    END LOOP;
    -- Traiter opérateur ou opérande
    IF Opérateur IN '0'..'9' THEN               -- c'est un opérande
      IF Trouvé_Opérande THEN                   -- opérande impossible
        Ada.Text_IO.Put ("Opérateur manquant"); -- immédiatement après un autre
        EXIT;
      END IF;
      Ada.Integer_Text_IO.Get(Opérande);               -- lire l'opérande
      Empiler(Pile_Opérandes, Opérande);
      Trouvé_Opérande := True;     -- noter qu'on a un opérande
    ELSE                           -- pas un opérande
      Trouvé_Opérande := False;    -- le noter
      EXIT WHEN Opérateur = '.';    -- fin de l'expression
      Ada.Text_IO.Get(Opérateur);              -- lire l'opérateur
      CASE Opérateur IS
        WHEN '+' | '-' | '*' | '/' =>
             WHILE Priorité(Opérateur) <= Priorité(Sommet(Pile_Opérateurs)) LOOP
               Calculer;
             END LOOP;
             Empiler(Pile_Opérateurs, Opérateur);
        WHEN '(' =>                -- empiler parenthèses gauches
             Empiler(Pile_Opérateurs, Opérateur);
        WHEN ')' =>                -- vider pile jusqu'à '('
             WHILE Priorité(Sommet(Pile_Opérateurs)) > Priorité('(') LOOP
               Calculer;
             END LOOP;
             Désempiler(Pile_Opérateurs, Opérateur);
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
  IF Opérateur = '.' THEN
    WHILE Priorité(Sommet(Pile_Opérateurs)) > Priorité('$') LOOP
      Calculer;
    END LOOP;
    IF Sommet(Pile_Opérateurs) = '$' THEN
      Ada.Integer_Text_IO.Put(Sommet(Pile_Opérandes), Width => 1);
    ELSE
      Ada.Text_IO.Put("Parenthèse droite manquante");
    END IF;
  END IF;
  Ada.Text_IO.New_Line;
END Calculatrice;
