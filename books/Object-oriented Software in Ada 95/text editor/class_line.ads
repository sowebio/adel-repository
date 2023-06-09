pragma Source_Reference (000055, "x80_ed.ada");

--====================================================================

--[class_line.ads] Specification
with Ada.Finalization;
use  Ada.Finalization;
package Class_Line is
  type Line is new Controlled with private;

  procedure Debug( The:in Line );
  procedure Initialize( The:in out Line );
  procedure Finalize( The:in out Line );
  procedure Adjust( The:in out Line );
  procedure Clear( The:in out Line );

  procedure Start( The:in out Line );       -- Iterator for line
  function  End_Of_Line( The:in Line) return Boolean;
  function  Get_Char( The:in Line ) return Character;
  procedure Next_Ch( The:in out Line );

  procedure Add(The:in out Line; Where:in Natural; Ch:in Character);
  procedure Del( The:in out Line; Where:in Natural );

  function  Deliver_Current_Col( The:in Line ) return Natural;
  function  Deliver_Cur_Len( The:in Line ) return Natural;
  function  Deliver_Max_Line_Size( The:in Line ) return Natural;

private
  Max_Chs : constant := 79;
  type    Line_Iter_Index is range 0 .. Max_Chs+1;
  subtype Line_Index      is Line_Iter_Index range 0 .. Max_Chs;
  subtype Line_Range      is Line_Iter_Index range 1 .. Max_Chs;
  type    Line_Array      is array ( Line_Range ) of Character;

  --TYPE O_Line IS RECORD
  type Line is new Controlled with record
    Chs        : Line_Array;          -- Line of characters
    Iter_Pos   : Line_Iter_Index := 0;-- Iterator position
    Cur_Len    : Line_Index := 0;     -- Position of last ch
    Col        : Line_Range := 1;     -- Last operation here
  end record;
  --TYPE Line IS ACCESS O_Line;          -- Pointer to a Line

end Class_Line;
