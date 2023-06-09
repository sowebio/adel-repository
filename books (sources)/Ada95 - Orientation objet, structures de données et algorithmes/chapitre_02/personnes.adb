WITH Ada.Text_IO;
WITH Ada.Integer_Text_IO;
PACKAGE BODY Personnes IS

FUNCTION Nom(Qui: Personne) RETURN String IS
BEGIN
  RETURN Qui.Nom;
END Nom; 
     
FUNCTION Pr�nom(Qui: Personne) RETURN String IS
BEGIN
  RETURN Qui.Pr�nom;
END Pr�nom; 
       
PROCEDURE DonnerNom(Nom: IN String; Qui: IN OUT Personne) IS
BEGIN
  Qui.Nom(1..Nom'Length) := Nom;
END DonnerNom; 
  
PROCEDURE DonnerPr�nom(Pr�nom: IN String; Qui: IN OUT Personne) IS
BEGIN
  Qui.Pr�nom(1..Pr�nom'Length) := Pr�nom;
END DonnerPr�nom; 
  
PROCEDURE Put(Qui: IN Personne) IS
BEGIN
  Ada.Text_IO.Put(Item => "Nom: ");
  Ada.Text_IO.Put_Line(Item => Qui.Nom);
  Ada.Text_IO.Put(Item => "Pr�nom: ");
  Ada.Text_IO.Put_Line(Item => Qui.Pr�nom);
END Put;

END Personnes;
