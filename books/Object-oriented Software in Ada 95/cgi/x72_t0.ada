---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:53 PM BST  --
---------------------------------------------------------------------
function Laser ( Mes:in String ) return String is
begin
  return "";
end Laser;

function Ink_Jet ( Mes:in String ) return String is
begin
  return "";
end Ink_Jet;

function About ( Mes:in String ) return String is
begin
  return "";
end About;

with Class_Input_Manager, Class_Menu, Class_Menu_Title,
     Laser, Ink_Jet, About;
use  Class_Input_Manager, Class_Menu, Class_Menu_Title;
procedure Main is
begin
  Window_Prologue;
  declare
    Menu_Bar     : Menu_Title;
    Printer_Type : aliased Menu;
  begin
    Framework( Printer_Type,
      "Laser",   null, Laser'access,
      "Ink jet", null, Ink_Jet'access );
    Framework( Menu_Bar,
      "About",   null, About'access,
      "Print",   Printer_Type'Unchecked_Access, null );
    Window_Start;
  end;
  Window_Epilogue;
end Main;
