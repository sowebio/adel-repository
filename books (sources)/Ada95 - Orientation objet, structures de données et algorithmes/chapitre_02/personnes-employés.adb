WITH Ada.Text_IO;
PACKAGE BODY Personnes.Employ�s IS
  
FUNCTION Code(Qui: Employ�) RETURN String IS        
BEGIN
  RETURN Qui.Code;
END Code;
                      
PROCEDURE DonnerCode(Qui: IN OUT Employ�; Code: IN String) IS
BEGIN
  Qui.Code := Code;
END DonnerCode;

PROCEDURE Put(Qui: IN Employ�) IS
BEGIN 
  Put(Qui => Personne(Qui));  -- conversion d'employ� � personne
  Ada.Text_IO.Put(Item => "Code: ");
  Ada.Text_IO.Put_Line(Item => Qui.Code);
END Put;

END Personnes.Employ�s;
