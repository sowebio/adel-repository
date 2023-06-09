---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:47 PM BST  --
---------------------------------------------------------------------
--[class_string.ads] Specification
package Class_Bounded_String is
  type Bounded_String is private;

  function To_Bounded_String(Str:in String)
    return Bounded_String;

  function To_String(The:in Bounded_String) return String;

  function "&" (F:in Bounded_String; S:in Bounded_String)
    return Bounded_String;
  function "&" (F:in Bounded_String; S:in String)
    return Bounded_String;
  function "&" (F:in String; S:in Bounded_String)
    return Bounded_String;

  function Slice( The:in Bounded_String;
                  Low:in Positive; High:in Natural )
    return String;

  function "="  ( F:in Bounded_String; S:in Bounded_String )
    return Boolean;

  function ">"  ( F:in Bounded_String; S:in Bounded_String )
    return Boolean;
  function ">=" ( F:in Bounded_String; S:in Bounded_String )
    return Boolean;
  function "<"  ( F:in Bounded_String; S:in Bounded_String )
    return Boolean;
  function "<=" ( F:in Bounded_String; S:in Bounded_String )
    return Boolean;
private
  Max_String: constant := 80;
  subtype Str_Range is Natural range 0 .. Max_String;
  type A_Bounded_String( Length: Str_Range := 0 ) is record
    Chrs: String( 1 .. Length );  -- Stored string
  end record;
  type Bounded_String is record
    V_Str : A_Bounded_String;
  end record;
end Class_Bounded_String;

--[class_string.adb] Implementation
package body Class_Bounded_String is

  function To_Bounded_String( Str:in String )
      return Bounded_String is
  begin
    return (V_Str=>(Str'Length, Str));
  end To_Bounded_String;

  function To_String(The:in Bounded_String) return String is
  begin
    return The.V_Str.Chrs( 1 .. The.V_Str.Length );
  end To_String;

  function "&" ( F:in Bounded_String; S:in Bounded_String )
      return Bounded_String is
  begin
    return (V_Str=>(F.V_Str.Chrs'Length + S.V_Str.Chrs'Length,
        F.V_Str.Chrs & S.V_Str.Chrs));
  end "&";

  function "&" ( F:in Bounded_String; S:in String )
      return Bounded_String is
  begin
    return (V_Str=>(F.V_Str.Chrs'Length + S'Length,
        F.V_Str.Chrs & S ) );
  end "&";

  function "&" ( F:in String; S:in Bounded_String )
      return Bounded_String is
  begin
    return ( V_Str=>(F'Length + S.V_Str.Chrs'Length,
        F & S.V_Str.Chrs ) );
  end "&";

  function Slice( The:in Bounded_String;
                   Low:in Positive; High:in Natural)
    return String is
  begin
    if Low <= High and then High <= The.V_Str.Length then
      return The.V_Str.Chrs( Low .. High );
    end if;
    return "";
  end Slice;

  function "="  ( F:in Bounded_String; S:in Bounded_String )
      return Boolean is
  begin
    return F.V_Str.Chrs = S.V_Str.Chrs;
  end "=";

  function ">"  ( F:in Bounded_String; S:in Bounded_String )
      return Boolean is
  begin
    return F.V_Str.Chrs > S.V_Str.Chrs;
  end ">";

  function ">=" ( F:in Bounded_String; S:in Bounded_String )
      return Boolean is
  begin
    return F.V_Str.Chrs >= S.V_Str.Chrs;
  end ">=";

  function "<"  ( F:in Bounded_String; S:in Bounded_String )
      return Boolean is
  begin
    return F.V_Str.Chrs < S.V_Str.Chrs;
  end "<";

  function "<=" ( F:in Bounded_String; S:in Bounded_String )
      return Boolean is
  begin
    return F.V_Str.Chrs <= S.V_Str.Chrs;
  end "<=";

end Class_Bounded_String;

--[class_container.ads] Specification
package Class_Container is
  procedure Main1;
  procedure Main2;
end Class_Container;

--[class_container.adb] Implementation
with Ada.Text_Io, Class_Bounded_String;
use  Ada.Text_Io, Class_Bounded_String;
package body Class_Container is

  procedure Main1 is
    Town, County, Address : Bounded_String;
  begin
    Town   := To_Bounded_String( "Brighton" );
    County := To_Bounded_String( "East Sussex" );

    Address := Town & " " & County;

    Put( To_String(Address) ); New_Line;
    Put( Slice( County & " UK", 6, 14 ) );
    New_Line;

  end Main1;

  procedure Main2 is
    Text: Bounded_String;
    procedure Check(B:in Boolean; Str1,Str2:in String) is
    begin
      if B then Put(Str1); else Put(Str2); end if;
      Put(" ");
    end Check;
  begin
    Put("=    /=   >    >=   >=   <    <=   <=    "); New_Line;
    Check(To_Bounded_String("ABC") =  To_Bounded_String("ABC") , "Pass",  "Fail");
    Check(To_Bounded_String("ABC") /= To_Bounded_String("AB ") , "Pass",  "Fail");
    Check(To_Bounded_String("ABC") >  To_Bounded_String("ABBB"), "Pass",  "Fail");
    Check(To_Bounded_String("ABC") >= To_Bounded_String("ABBB"), "Pass",  "Fail");
    Check(To_Bounded_String("ABC") >= To_Bounded_String("ABC") , "Pass",  "Fail");
    Check(To_Bounded_String("ABBB")<  To_Bounded_String("ABC") , "Pass",  "Fail");
    Check(To_Bounded_String("ABBB")<= To_Bounded_String("ABC") , "Pass",  "Fail");
    Check(To_Bounded_String("ABC") <= To_Bounded_String("ABC") , "Pass",  "Fail");
    New_Line;

    Put("/=    =   <=   <    <    >=   >    >     "); New_Line;
    Check(To_Bounded_String("ABC") /= To_Bounded_String("ABC") , "Fail",  "Pass");
    Check(To_Bounded_String("ABC") =  To_Bounded_String("AB ") , "Fail",  "Pass");
    Check(To_Bounded_String("ABC") <= To_Bounded_String("ABBB"), "Fail",  "Pass");
    Check(To_Bounded_String("ABC") <  To_Bounded_String("ABBB"), "Fail",  "Pass");
    Check(To_Bounded_String("ABC") <  To_Bounded_String("ABC") , "Fail",  "Pass");
    Check(To_Bounded_String("ABBB")>= To_Bounded_String("ABC") , "Fail",  "Pass");
    Check(To_Bounded_String("ABBB")>  To_Bounded_String("ABC") , "Fail",  "Pass");
    Check(To_Bounded_String("ABC") >  To_Bounded_String("ABC") , "Fail",  "Pass");
    New_Line;

    Put("Equality test"); New_Line;
    for I in 1 .. 6 loop
      Text := To_Bounded_String("ABCDEFGH-ABCDEFGH");
      if Slice(Text,I,4) = Slice(Text,9+I,13) then
        Put("Pass");
      else
        Put("Fail");
      end if;
      Put(" ");
    end loop;
    New_Line;

  end Main2;

end Class_Container;

with Ada.Text_Io, Class_Bounded_String;
use  type Class_Bounded_String.Bounded_String;
procedure Main3 is
  Town  : Class_Bounded_String.Bounded_String :=
    Class_Bounded_String.To_Bounded_String("Brighton");
  County: Class_Bounded_String.Bounded_String :=
    Class_Bounded_String.To_Bounded_String("E Sussex");
begin
  Ada.Text_Io.Put(
    Class_Bounded_String.To_String( Town & " " & County )
    );
end Main3;

--[main.adb] Procedure
with Class_Container, Main3;
use  Class_Container;
procedure Main is
begin
  Main1; Main2; Main3;
end Main;
