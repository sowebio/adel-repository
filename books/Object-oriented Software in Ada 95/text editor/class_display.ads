pragma Source_Reference (000841, "x80_ed.ada");

--====================================================================

--[class_display.ads] Specification
with Ada.Finalization, Pack_Constants, Class_Line, Class_Store;
use  Ada.Finalization, Pack_Constants, Class_Line, Class_Store;
package Class_Display is

  type Mode    is ( No_Echo, Echo );
  type Display is new Limited_Controlled with private;

  procedure Debug( The:in Display );
  procedure Initialize( The:in out Display );
  procedure Finalize( The:in out Display );
  procedure Clear( The:in out Display );

  procedure Init( The:in out Display; S:in Store);
  procedure Refresh( The:in out Display );

  procedure Add( The:in out Display; B:in Store; Ch:in Character );
  procedure Del( The:in out Display; S:in Store );
  procedure Position( The:in out Display; S:in Store );

  procedure Status( The:in out Display; S:in Store );
  function  Deliver_Display_Size( The:in Display ) return Natural;

private
  Display_Lines : constant := Lines_On_Screen-1;
  type    Display_Index is new Integer   range 0..Display_Lines;
  subtype Display_Range is Display_Index range 1..Display_Lines;

  procedure Display_Line( The:in out Display; I:in Display_Index );
  procedure Minimal_Refresh( The:in out Display;
    Old_Abs_Line:in Natural );

  type Display_As_Array is array ( Display_Range ) of Line;
  type Display is new Limited_Controlled with record
    Display       : Display_As_Array;    -- Display
    No_Lines      : Display_Index := 0;  -- Active lines
    Abs_Line      : Natural := 0;        -- of 1st display line
    Row           : Natural := 0;        -- current row
    Col           : Natural := 0;        -- current column
  end record;

end Class_Display;
