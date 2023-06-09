----------------------------------------------------------------------------
--   Fichier:  Calculatrice.adb
--   Objectif: Calculatrice acceptant et calculant la valeur
--             d'expressions avec parenthèses.
--   Auteur:   Philippe J. Gabrini (gabrini.philippe@uqam.ca)
--
--          Copyright © 1998 Philippe J. Gabrini
----------------------------------------------------------------------------
WITH Ada.Text_IO, Ada.Integer_Text_IO, Piles;
PROCEDURE Calculatrice IS

PACKAGE Piles_Opérandes IS NEW Piles(Integer);
PACKAGE Piles_Opérateurs IS NEW Piles(Character);
USE Piles_Opérandes, Piles_Opérateurs; -- un raccourci d'écriture non ambigu

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
    WHEN '/' => Empiler(Pile_Opérandes, Gauche / Droite);
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
      -- obtenir la valeur du prochain caractère sans avancer dans l'entrée
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
      Ada.Integer_Text_IO.Get(Opérande); -- lecture effective de l'opérande
      Empiler(Pile_Opérandes, Opérande);
      Trouvé_Opérande := True;     -- noter qu'on a un opérande
    ELSE                           -- pas un opérande
      Trouvé_Opérande := False;    -- le noter
      EXIT WHEN Opérateur = '.';   -- fin de l'expression
      Ada.Text_IO.Get(Opérateur);  -- lecture effective de l'opérateur
      CASE Opérateur IS
        WHEN '+' | '-' | '*' | '/' =>
             WHILE Priorité(Opérateur) <= Priorité(Sommet(Pile_Opérateurs)) LOOP
               Calculer;        -- effectuer l'opération prioritaire sur la pile
             END LOOP;
             Empiler(Pile_Opérateurs, Opérateur);
        WHEN '(' =>                -- empiler parenthèses gauches
             Empiler(Pile_Opérateurs, Opérateur);
        WHEN ')' =>                -- vider pile jusqu'à '('
             WHILE Priorité(Sommet(Pile_Opérateurs)) > Priorité('(') LOOP
               Calculer;        -- effectuer l'opération entre parenthèses
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
  IF Opérateur = '.' THEN -- vider la pile
    WHILE Priorité(Sommet(Pile_Opérateurs)) > Priorité('$') LOOP
      Calculer;           -- effectuer l'opération sur la pile
    END LOOP;
    IF Sommet(Pile_Opérateurs) = '$' THEN
      Ada.Integer_Text_IO.Put(Sommet(Pile_Opérandes), Width => 1);
    ELSE
      Ada.Text_IO.Put("Parenthèse droite manquante");
    END IF;
  END IF;
  Ada.Text_IO.New_Line;
END Calculatrice;
