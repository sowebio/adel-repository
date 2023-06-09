---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:51:02 PM BST  --
---------------------------------------------------------------------
--(class_description.ADS) Implementation Instantiation

--
-- Split the components of a CGI result string into
--  individual sub strings
--
-- For example the string
--   name=Your+name&action=%2B10%25&log=~mas/log
--
-- is composed of three named elements:
--
--     Element    String associated with element
--     name       Your name
--     action     +10%
--     log        /usr/staff/mas/log
--
-- (C) M.A.Smith University of Brighton
-- Permission is granted to use this code
--   provided this declaration and copyright notice remains intact.
-- 4 January 1996
--
--
--
-- S p e c i f i c a t i o n


with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Strings.Unbounded;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Strings.Unbounded;
package Class_Parse is
  type Parse is private;
  procedure Set( The:in out Parse; Mes:in String );
  function  Get_Item( The:in Parse; Key: in String; Pos:in Integer:=1;
    Map:in Boolean :=False ) return String;
private
  Sep : constant Character := '&';
  type Parse is record
    Str : Unbounded_String;            -- String to parse
    Len : Integer;                     -- Length
  end record;
end Class_Parse;



--
-- Split the components of a CGI result string into
--  individual sub strings
--
-- For example the string
--   name=Your+name&action=%2B10%25&log=~mas/log
--
-- is composed of three named elements:
--
--     Element    String associated with element
--     name       Your name
--     action     +10%
--     log        /usr/staff/mas/log
--
-- (C) M.A.Smith University of Brighton
-- Permission is granted to use this code
--   provided this declaration and copyright notice remains intact.
-- 4 January 1996
--
--
-- I m p l e m e n t a t i o n

package body Class_Parse is

  function Remove_Escape(From:in String) return String;
  function Hex( First, Second :in Character ) return Character;

  procedure Set( The:in out Parse; Mes:in String ) is
  begin
    The.Str := To_Unbounded_String(Mes);
    The.Len := Mes'Length;
  end Set;

  function Get_Item( The:in Parse; Key: in String; Pos:in Integer:=1;
      Map:in Boolean :=False ) return String is
    Cur_Tag   : Integer := 1;
    I,J       : Integer;
    Start     : Integer;
    Parse_Str : String (1 .. The.Len) := To_String( The.Str );
  begin
    I := 1;
    while I < The.Len-Key'Length loop
      if Parse_Str(I .. I+Key'Length-1) = Key then
        if Parse_Str(I+Key'Length) = '=' then
          if Cur_Tag = Pos then
            Start := I+Key'Length+1; J := Start;
            while J <= The.Len and then Parse_Str(J) /= Sep loop
              if J <= The.Len then J := J + 1; end if;
            end loop;
            return Remove_Escape( Parse_Str( Start .. J-1 ) );
          else
            Cur_Tag := Cur_Tag + 1;
          end if;
        end if;
      end if;
      I := I + 1;
    end loop;
    return "";
  end Get_Item;

  function Remove_Escape(From:in String) return String is
    Res : String( 1 .. From'Length );
    Ch  : Character;
    I,J : Integer;
  begin
    I := From'First; J := 0;
    while I <= From'Last loop
      Ch := From(I);
      case Ch is
        when '%' =>
          Ch := Hex(From(I+1), From(I+2) );
          I:= I+2;
        when '+' =>
          Ch := ' ';
        when others =>
          null;
      end case;
      I := I + 1;
      J := J + 1; Res(J) := Ch;
    end loop;
    return Res(1..J);
  end Remove_Escape;

  function Hex( First, Second :in Character ) return Character is
    type Mod256 is mod 256;
    A_Ch : Mod256;
    function Hex_Value( Ch:in Character ) return Mod256 is
    begin
      if Ch in '0' .. '9' then
        return Character'Pos(Ch)-Character'Pos('0');
      end if;
      if Ch in 'A' .. 'F' then
        return Character'Pos(Ch)-Character'Pos('A')+10;
      end if;
      return 0;
    end Hex_Value;
  begin
    return Character'Val(
      ( Hex_Value(First) and 16#FF#) * 16 +
      ( Hex_Value(Second) and 16#FF#) );
  end Hex;

end Class_Parse;

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

with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Class_Parse, Unix_If, Ada.Strings.Unbounded;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Class_Parse, Unix_If, Ada.Strings.Unbounded;
procedure Main is
  List         : Parse;
  Query_String : Unbounded_String;      -- String to parse

begin
  Query_String := To_Unbounded_String( Get_Env( "QUERY_STRING" ) );
  if ( To_String( Query_String ) = "" ) then
    Query_String := To_Unbounded_String(
      "tag=one&"             &
      "name=mike&"           &
      "action=%2B10%25&"     &
      "tag=two&"             &
      "log=~mas/log&"        &
      "tag=three" );
  end if;

  Set( List, To_String(Query_String) ) ;

  Put("name   = "); Put( Get_Item( List, "name" )) ; New_Line;
  Put("action = "); Put( Get_Item( List, "action" )) ; New_Line;
  Put("log    = "); Put( Get_Item( List, "log" )) ; New_Line;
  for I in 1 .. 4 loop
    Put("tag(" ); Put( I, Width=>1 ); Put( ")  ");
    Put( Get_Item( List, "tag", I ) ); New_Line;
  end loop;
end Main;
