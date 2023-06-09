---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:51:03 PM BST  --
---------------------------------------------------------------------
package Unix_If is
  function Get_Env( Str:in String ) return String;
end Unix_If;

with Interfaces.C, Interfaces.C.Strings;
use  Interfaces.C, Interfaces.C.Strings;
package body Unix_If is

  function Get_Env( Str:in String ) return String is
    function Getenv( Str:in Char_Array ) return Chars_Ptr;
    pragma Import (C, Getenv, "getenv");
    Res : Chars_Ptr;
  begin
    Res := Getenv( To_C( Str, Append_Nul=>True ) );
    if Res = Null_Ptr then
      return "";
    else
      return Value(Res);
    end if;
  end Get_Env;

end Unix_If;

with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Unix_If;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Unix_If;
procedure Main is
begin
  New_Line;
  Put( "Content-type: text/plain" ); New_Line(2);
  Put( "<HTML>  " ); New_Line;
  Put( "<HEAD>  " ); New_Line;
  Put( "</HEAD> " ); New_Line;
  Put( "<BODY>  " ); New_Line;
  Put( "<P>" );      New_Line;
  Put( "The data sent to the form processing program " ); New_Line;
  Put( "in the environment variable QUERY_STRING is:" ); New_Line;
  Put( "<P>" ); New_Line;
  Put( Get_Env( "QUERY_STRING" ) ); New_Line;
  Put( "<P>" ); New_Line;
  Put( "</BODY>  " ); New_Line;
  Put( "</HTML>  " ); New_Line;
end Main;

