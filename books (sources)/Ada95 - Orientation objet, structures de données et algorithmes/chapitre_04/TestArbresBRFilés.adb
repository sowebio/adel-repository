--          Copyright � 1998 Philippe J. Gabrini
WITH Ada.Text_IO, Ada.Integer_Text_IO, ArbresBinRechFil�s;
PROCEDURE TestArbresBRFil�s IS
-- Ce programme de conduite fournit un menu de choix � l'utilisateur
-- permettant de tester toutes les proc�dures du TDA arbre binaire de recherche fil�.

PACKAGE Enfilage IS NEW ArbresBinRechFil�s(Type�l�ment => Character, �l�ments�gaux => "=",
                              Inf�rieur => "<", Afficher�l�ment => Ada.Text_IO.Put);
Arbre, Courant : Enfilage.Arbre_Bin_Rech_Fil�;
�l�ment: Character;

PROCEDURE AfficherMenu IS
-- Afficher la liste des options.
BEGIN
  Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "1. Supprimer arbre"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "2. Ins�rer noeud"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "3. Supprimer noeud"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "4. Trouver noeud"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "5. Aller au pr�d�cesseur"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "6. Aller au successeur"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "7. Travers�e en avant"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "8. Travers�e en arri�re"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "9. Afficher valeur courante"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "10. Afficher l'arbre au complet"); Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "11. Terminer"); Ada.Text_IO.New_Line; Ada.Text_IO.New_Line;
  Ada.Text_IO.Put(Item => "Donnez votre choix (de 1 � 11) : ");
END AfficherMenu;

FUNCTION Choix RETURN Natural IS
-- Demande � l'utilisateur un nombre entre 1 et 12.  Tout nombre en dehors
-- de cet intervalle provoque une nouvelle demande.  La fonction ne retourne 
-- qu'une valeur valide.
ChoixLu: Natural;
BEGIN
  AfficherMenu;
  LOOP
    Ada.Integer_Text_IO.Get(Item => ChoixLu);
    EXIT WHEN ChoixLu IN 1..11;
    Ada.Text_IO.Put(Item => "S.V.P. donnez un nombre entre 1 et 11 => ");
  END LOOP;
  RETURN ChoixLu;
END Choix;

BEGIN
  LOOP
    CASE Choix IS
      WHEN 1 => Enfilage.D�truireArbre(Arbre);
      WHEN 2 => Ada.Text_IO.Put(Item => "Donnez la valeur � ins�rer > ");
                Ada.Text_IO.Get(Item => �l�ment);
                Ada.Text_IO.Put(Item => �l�ment);
                Enfilage.Ins�rerNoeud(Arbre, �l�ment);
      WHEN 3 => Ada.Text_IO.Put(Item => "Donnez la valeur � supprimer > ");
                Ada.Text_IO.Get(Item => �l�ment);
                Ada.Text_IO.Put(Item => �l�ment);
                Enfilage.SupprimerNoeud(Arbre, �l�ment);
      WHEN 4 => Ada.Text_IO.Put(Item => "Donnez la valeur � trouver > ");
                Ada.Text_IO.Get(Item => �l�ment);
                Ada.Text_IO.Put(Item => �l�ment);
                Courant := Enfilage.Noeud(Arbre, �l�ment);
                IF Enfilage.ArbreVide(Courant) THEN
                  Ada.Text_IO.New_Line;
                  Ada.Text_IO.Put_Line(Item => 
                              "Valeur non trouv�e, on prend la racine");
                  Courant := Arbre;
                END IF;
      WHEN 5 => Courant := Enfilage.Pr�d�cesseur(Courant);
                IF Enfilage.ArbreVide(Courant) THEN
                  Ada.Text_IO.Put(Item => 
                         "Noeud sans pr�d�cesseur, on prend la racine");
                  Courant := Arbre;
                ELSE
                  Ada.Text_IO.Put(Item => "Noeud courant: ");
                  Enfilage.AfficherNoeud(Courant);
                END IF;
                Ada.Text_IO.New_Line;
      WHEN 6 => Courant := Enfilage.Successeur(Courant);
                IF Enfilage.ArbreVide(Courant) THEN
                  Ada.Text_IO.Put(Item => 
                           "Noeud sans successeur, on prend la racine");
                  Courant := Arbre;
                ELSE
                  Ada.Text_IO.Put(Item => "Noeud courant: ");
                  Enfilage.AfficherNoeud(Courant);
                END IF;
                Ada.Text_IO.New_Line;
      WHEN 7 => Enfilage.TraverserAvant(Courant);
      WHEN 8 => Enfilage. TraverserArri�re(Courant);
      WHEN 9 => Ada.Text_IO.Put(Item => "Noeud courant: ");
                Enfilage.AfficherNoeud(Courant); Ada.Text_IO.New_Line;
      WHEN 10 => Enfilage.AfficherArbre(Arbre);
      WHEN OTHERS => EXIT;
    END CASE;
  END LOOP;
END TestArbresBRFil�s;
