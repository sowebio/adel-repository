pragma Source_Reference (000001, "x80_exp.ada");
---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:59 PM BST  --
---------------------------------------------------------------------
with Ada.Text_Io;
use  Ada.Text_Io;
package Class_Line is
  type Line_Status is ( Text_Line, File_Name, Unknown );
  type Line is private;
  procedure Get_Line( The:in out Line; Fd:in Ada.Text_Io.File_Type );
  procedure Put_Line( The:in Line; Fd:in Ada.Text_Io.File_Type );
  procedure Get_Fd( The:in out Line; Fd:in out Ada.Text_Io.File_Type );
  function  Status( The:in Line ) return Line_Status;
private
  Max_Line : constant := 200;
  subtype Line_Index is Integer range 0 .. Max_Line+1;
  subtype Line_Range is Line_Index range 1 .. Max_Line;
  subtype Line_Array is String( Line_Range );
  type Line is record
    Chs : Line_Array;       -- Characters of line
    Len : Line_Index;       -- Positions used
    Open: Boolean := False; -- Output file open
  end record;
  Name     : Line_Array;    -- File name from file
  Name_Pos : Line_Index;    -- Characters in name
end Class_Line;
