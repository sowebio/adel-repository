pragma Source_Reference (000210, "x80_ed.ada");

--====================================================================

--[class_store.ads] Specification
with Class_Line; use Class_Line;
with Ada.Finalization;
use  Ada.Finalization;
package Class_Store is

  type Store is new Limited_Controlled with private;

  procedure Debug( The:in Store );
  procedure Initialize( The:in out Store );
  procedure Finalize( The:in out Store );
  procedure Clear( The:in out Store );

  procedure Add( The:in out Store; Row:in Natural;
    Column:in Natural; Ch:in Character );
  procedure Del( The:in out Store; Row:in Natural;
    Column:in Natural );
  procedure Add( The:in out Store; Ch:in Character );
  procedure Del( The:in out Store );

  procedure Left_Right( The:in out Store; Dir:in Character );
  procedure Up_Down( The:in out Store; Dir:in Character;
    No_Lines:in Natural );

  function  Deliver_Line(The:in Store; Row:in Natural) return Line;

  function  No_Lines( The:in Store ) return Natural;
  procedure Set_Position( The:in out Store; Row:in Natural;
    Column:in Natural );
  procedure Set_Last_Line( The:in out Store; Row:in Natural );
  procedure Deliver_Row_Column( The:in Store; Row:out Natural;
    Column:out Natural );
  function  Deliver_Store_Size( The:in Store ) return Natural;
  function  Deliver_Max_Line_Size
    ( The:in Store; Row:in Natural ) return Natural;

private
  Max_Lines : constant := 120;
  type    Store_Index    is range 0 .. Max_Lines;
  subtype Store_Range    is Store_Index range 1 .. Max_Lines;
  type    Store_As_Array is array ( Store_Index ) of Line;

  type Store is new Limited_Controlled with record
    Lines     : Store_As_Array;    -- Store as array
    No_Lines  : Store_Index := 0;  -- Lines stores
    Row       : Natural := 0;      -- Current row
    Col       : Natural := 0;      -- Current column
  end record;
end Class_Store;
