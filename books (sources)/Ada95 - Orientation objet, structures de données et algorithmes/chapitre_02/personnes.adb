WITH Ada.Text_IO;
WITH Ada.Integer_Text_IO;
PACKAGE BODY Personnes IS

FUNCTION Nom(Qui: Personne) RETURN String IS
BEGIN
  RETURN Qui.Nom;
END Nom; 
     
FUNCTION Prénom(Qui: Personne) RETURN String IS
BEGIN
  RETURN Qui.Prénom;
END Prénom; 
       
PROCEDURE DonnerNom(Nom: IN String; Qui: IN OUT Personne) IS
BEGIN
  Qui.Nom(1..Nom'Length) := Nom;
END DonnerNom; 
  
PROCEDURE DonnerPrénom(Prénom: IN String; Qui: IN OUT Personne) IS
BEGIN
  Qui.Prénom(1..Prénom'Length) := Prénom;
END DonnerPrénom; 
  
PROCEDURE Put(Qui: IN Personne) IS
BEGIN
  Ada.Text_IO.Put(Item => "Nom: ");
  Ada.Text_IO.Put_Line(Item => Qui.Nom);
  Ada.Text_IO.Put(Item => "Prénom: ");
  Ada.Text_IO.Put_Line(Item => Qui.Prénom);
END Put;

END Personnes;
