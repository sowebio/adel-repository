pragma Source_Reference (001095, "x80_ed.ada");

--====================================================================

--[class_file.ads] Specification
with Ada.Finalization, Class_Store, Pack_Constants, Class_User, Class_Display;
use  Ada.Finalization, Class_Store, Pack_Constants, Class_User, Class_Display;
package Class_File is

  type File is new Limited_Controlled with private;

  procedure Initialize( The:in out File );
  procedure Finalize( The:in out File );
  procedure Register( The:in out File; Str:in String );
  function  Is_Active( The:in File ) return Boolean;
  procedure Set_Active( The:in out File );
  procedure Set_Not_Active( The:in out File );
  procedure Read ( The:in out File; S:in out Store );
  procedure Write( The:in out File; S:in out Store; U:in User );

private
  type    File_Index  is range 0 .. Max_Answer;
  subtype File_Range  is File_Index  range 1 .. Max_Answer;
  type    State_File  is ( Active, Not_Active );
  type File is new Limited_Controlled with record
    State         : State_File := Not_Active;
    Lines_In_File : Natural := 0;
    File          : String( 1 .. Max_Answer );
    File_Length   : File_Index := 0;
  end record;
end Class_File;
