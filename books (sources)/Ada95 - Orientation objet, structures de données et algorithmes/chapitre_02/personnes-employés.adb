WITH Ada.Text_IO;
PACKAGE BODY Personnes.Employés IS
  
FUNCTION Code(Qui: Employé) RETURN String IS        
BEGIN
  RETURN Qui.Code;
END Code;
                      
PROCEDURE DonnerCode(Qui: IN OUT Employé; Code: IN String) IS
BEGIN
  Qui.Code := Code;
END DonnerCode;

PROCEDURE Put(Qui: IN Employé) IS
BEGIN 
  Put(Qui => Personne(Qui));  -- conversion d'employé à personne
  Ada.Text_IO.Put(Item => "Code: ");
  Ada.Text_IO.Put_Line(Item => Qui.Code);
END Put;

END Personnes.Employés;
