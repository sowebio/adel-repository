PACKAGE Personnes.Employés IS
  
TYPE Employé IS NEW Personne WITH PRIVATE;
    
FUNCTION Code(Qui: Employé) RETURN String;        
-- Retourne le code de l'employé.
                      
PROCEDURE DonnerCode(Qui: IN OUT Employé; Code: IN String);
-- Antécédent: Qui est défini.
-- Conséquent: Qui possède un Code.

PROCEDURE Put(Qui: IN Employé);
-- Antécédent: Qui est défini.
-- Conséquent: Qui est affiché.

PRIVATE

  TYPE Employé IS NEW Personne WITH RECORD
    Code: String(1..5);
  END RECORD;

END Personnes.Employés;
