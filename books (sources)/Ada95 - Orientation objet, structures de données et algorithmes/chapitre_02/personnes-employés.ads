PACKAGE Personnes.Employ�s IS
  
TYPE Employ� IS NEW Personne WITH PRIVATE;
    
FUNCTION Code(Qui: Employ�) RETURN String;        
-- Retourne le code de l'employ�.
                      
PROCEDURE DonnerCode(Qui: IN OUT Employ�; Code: IN String);
-- Ant�c�dent: Qui est d�fini.
-- Cons�quent: Qui poss�de un Code.

PROCEDURE Put(Qui: IN Employ�);
-- Ant�c�dent: Qui est d�fini.
-- Cons�quent: Qui est affich�.

PRIVATE

  TYPE Employ� IS NEW Personne WITH RECORD
    Code: String(1..5);
  END RECORD;

END Personnes.Employ�s;
