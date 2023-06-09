PACKAGE Personnes IS

TYPE Personne IS TAGGED PRIVATE;
  
FUNCTION Nom(Qui: Personne) RETURN String; 
FUNCTION Pr�nom(Qui: Personne) RETURN String; 
-- Ant�c�dent: Qui est d�finie.
-- Cons�quent: retourne le champ d�sir�.
  
PROCEDURE DonnerNom(Nom: IN String; Qui: IN OUT Personne);
-- Ant�c�dent: Qui est d�finie.
-- Cons�quent: Nom devient le nom de Qui.
  
PROCEDURE DonnerPr�nom(Pr�nom: IN String; Qui: IN OUT Personne);
-- Ant�c�dent: Qui est d�finie.
-- Cons�quent: Pr�nom devient le pr�nom de Qui.
  
PROCEDURE Put(Qui: IN Personne);
-- Ant�c�dent: Qui est d�finie.
-- Cons�quent: Le nom et le pr�nom de Qui sont affich�s.
  
PRIVATE
  TYPE Personne IS TAGGED RECORD
    Nom: String(1..25) := (OTHERS => ' ');
    Pr�nom: String(1..25) := (OTHERS => ' ');
  END RECORD;

END Personnes;
