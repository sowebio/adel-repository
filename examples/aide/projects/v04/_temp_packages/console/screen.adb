WITH Ada.Text_IO;
WITH Ada.Integer_Text_IO;
PACKAGE BODY Screen IS
------------------------------------------------------------------
--|                                                              
--| body of screen-handling package
--|                                                              
--| Author: Michael B. Feldman, The George Washington University 
--| Last Modified: July 1995                                     
--|                                                              
------------------------------------------------------------------

  PROCEDURE Beep IS
  BEGIN
    Ada.Text_IO.Put (Item => ASCII.BEL);
    Ada.Text_IO.Flush;
  END Beep;

  PROCEDURE ClearScreen IS
  BEGIN
    Ada.Text_IO.Put (Item => ASCII.ESC);
    Ada.Text_IO.Put (Item => "[2J");
    Ada.Text_IO.Flush;
  END ClearScreen;

  PROCEDURE MoveCursor (Column : Width; Row : Depth) IS
  BEGIN
    Ada.Text_IO.Flush;
    Ada.Text_IO.Put (Item => ASCII.ESC);
    Ada.Text_IO.Put ("[");
    Ada.Integer_Text_IO.Put (Item => Row, Width => 1);
    Ada.Text_IO.Put (Item => ';');
    Ada.Integer_Text_IO.Put (Item => Column, Width => 1);
    Ada.Text_IO.Put (Item => 'f');
  END MoveCursor;  

END Screen;
