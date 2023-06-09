PACKAGE Personnes IS

TYPE Personne IS TAGGED PRIVATE;
  
FUNCTION Nom(Qui: Personne) RETURN String; 
FUNCTION Prénom(Qui: Personne) RETURN String; 
-- Antécédent: Qui est définie.
-- Conséquent: retourne le champ désiré.
  
PROCEDURE DonnerNom(Nom: IN String; Qui: IN OUT Personne);
-- Antécédent: Qui est définie.
-- Conséquent: Nom devient le nom de Qui.
  
PROCEDURE DonnerPrénom(Prénom: IN String; Qui: IN OUT Personne);
-- Antécédent: Qui est définie.
-- Conséquent: Prénom devient le prénom de Qui.
  
PROCEDURE Put(Qui: IN Personne);
-- Antécédent: Qui est définie.
-- Conséquent: Le nom et le prénom de Qui sont affichés.
  
PRIVATE
  TYPE Personne IS TAGGED RECORD
    Nom: String(1..25) := (OTHERS => ' ');
    Prénom: String(1..25) := (OTHERS => ' ');
  END RECORD;

END Personnes;
