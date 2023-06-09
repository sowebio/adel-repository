---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:47 PM BST  --
---------------------------------------------------------------------
--[class_rational.ads] Specification
package Class_Rational is
  type Rational is private;

  function "+" ( F:in Rational; S:in Rational ) return Rational;
  function "-" ( F:in Rational; S:in Rational ) return Rational;
  function "*" ( F:in Rational; S:in Rational ) return Rational;
  function "/" ( F:in Rational; S:in Rational ) return Rational;

  function Rat_Const( F:in Integer;
                      S:in Integer:=1 ) return Rational;
  function  Image( The:in Rational ) return String;
private
  function Sign( The:in Rational ) return Rational;
  function Simplify( The:in Rational ) return Rational;
  type Rational is record
    Above : Integer := 0;      -- Numerator
    Below : Integer := 1;      -- Denominator
  end record;
end Class_Rational;

--[class_rational.adb] Implementation
package body Class_Rational is

  function "+" (F:in Rational; S:in Rational) return Rational is
    Res : Rational;
  begin
    Res.Below := F.Below * S.Below;
    Res.Above := F.Above * S.Below + S.Above * F.Below;
    return Simplify(Res);
  end "+";

  function "-" (F:in Rational; S:in Rational) return Rational is
    Res : Rational;
  begin
    Res.Below := F.Below * S.Below;
    Res.Above := F.Above * S.Below - S.Above * F.Below;
    return Simplify(Res);
  end "-";

  function "*" (F:in Rational; S:in Rational) return Rational is
    Res : Rational;
  begin
    Res.Above := F.Above * S.Above;
    Res.Below := F.Below * S.Below;
    return Simplify(Res);
  end "*";

  function "/" (F:in Rational; S:in Rational) return Rational is
    Res : Rational;
  begin
    Res.Above := F.Above * S.Below;
    Res.Below := F.Below * S.Above;
    return Simplify(Res);
  end "/";

  function Rat_Const( F:in Integer; S:in Integer:=1 ) return Rational is
  begin
    if F = 0 then
      return Rational'(0,1);
    else
      return Simplify( Sign( Rational'( F, S ) ) );
    end if;
  end Rat_Const;

  function Image( The:in Rational ) return String is
    Above : Integer := The.Above;
    Below : Integer := The.Below;

    function Trim( Str:in String ) return String is
    begin
      return Str( Str'First+1 .. Str'Last );
    end Trim;

    function To_String( Above, Below : in Integer )
             return String is
    begin
      if Above = 0 then            -- No fraction
        return "";
      elsif Above >= Below then    -- Whole number
        return Trim( Integer'Image(Above/Below) ) & " " &
               To_String( Above rem below, Below );
      else
        return Trim( Integer'Image( Above ) ) & "/" &
               Trim( Integer'Image( Below ) );
      end if;
    end To_String;

  begin
   if Above = 0 then
     return "0";                                 -- Zero
   elsif Above < 0 then
     return "-" & To_String( abs Above, Below ); -- -ve
   else
     return To_String( Above, Below );           -- +ve
   end if;
  end Image;

  function Sign( The:in Rational ) return Rational is
  begin
    if The.Below >= 0 then            --   -a/b or a/b
      return The;
    else                              --   a/-b or -a/-b
      return Rational'( -The.Above, -The.Below );
    end if;
  end Sign;

  function Simplify( The:in Rational ) return Rational is
    Res: Rational := The;
    D  : Positive;                    -- Divisor to reduce with
  begin
    if Res.Below = 0 then             -- Invalid treat as 0
      Res.Above := 0; Res.Below := 1;
    end if;
    D := 2;                           -- Divide by 2, 3, 4 ...
    while D < Res.Below loop
      while Res.Below rem D = 0 and then Res.Above rem D = 0 loop
        Res.Above := Res.Above / D;
        Res.Below := Res.Below / D;
      end loop;
      D := D + 1;
    end loop;
    return Res;
  end Simplify;

end Class_Rational;

--[Main.adb] Procedure
with Ada.Text_Io, Class_Rational;
use  Ada.Text_Io, Class_Rational;
procedure Main1 is
  A,B : Rational;
begin
  A := Rat_Const( 1, 2 );
  B := Rat_Const( 1, 3 );

  Put( "a     = " ); Put( Image(A) );   New_Line;
  Put( "b     = " ); Put( Image(B) );   New_Line;
  Put( "a + b = " ); Put( Image(A+B) ); New_Line;
  Put( "a - b = " ); Put( Image(A-B) ); New_Line;
  Put( "b - a = " ); Put( Image(B-A) ); New_Line;
  Put( "a * b = " ); Put( Image(A*B) ); New_Line;
  Put( "a / b = " ); Put( Image(A/B) ); New_Line;
end Main1;

--[Main.adb] Procedure
with Ada.Text_Io, Class_Rational;
use  Ada.Text_Io, Class_Rational;
procedure Main2 is
  A,B : Rational;
begin
  A := Rat_Const(1, 2);
  B := Rat_Const(1, 3);

  Put( "a     = " ); Put( Image(A) );   Put(" <1/2> "); New_Line;
  Put( "b     = " ); Put( Image(B) );   Put("  <1/3> "); New_Line;
  Put( "a + b = " ); Put( Image(A+B) ); Put(" <5/6> "); New_Line;
  Put( "a - b = " ); Put( Image(A-B) ); Put(" <1/6> "); New_Line;
  Put( "b - a = " ); Put( Image(B-A) ); Put(" <-1/6> "); New_Line;
  Put( "a * b = " ); Put( Image(A*B) ); Put(" <1/6> "); New_Line;
  Put( "a / b = " ); Put( Image(A/B) ); Put(" <1 1/2> "); New_Line;

  New_Line;
  A := Rat_Const(3, 2);
  B := Rat_Const(4, 5);
  Put( "a     = " ); Put( Image(A) );   Put(" <3/2> "); New_Line;
  Put( "b     = " ); Put( Image(B) );   Put(" <4/5> "); New_Line;
  Put( "a + b = " ); Put( Image(A+B) ); Put(" <2 3/10> "); New_Line;
  Put( "a - b = " ); Put( Image(A-B) ); Put(" <7/10> "); New_Line;
  Put( "b - a = " ); Put( Image(B-A) ); Put(" <-7/10> "); New_Line;
  Put( "a * b = " ); Put( Image(A*B) ); Put(" <1 1/5> "); New_Line;
  Put( "a / b = " ); Put( Image(A/B) ); Put(" <1 7/8> "); New_Line;
end Main2;

--[Main.adb] Procedure
with Ada.Text_Io, Class_Rational;
use  Ada.Text_Io, Class_Rational;
procedure Main3 is
  A : Rational;
begin
  -- put( 8/16 ) is ambiguous;
  New_Line;
  Put( " 0, 0      = " ); A := Rat_Const(0, 0);   Put( Image(A) ); New_Line;
  Put( " 0, 5      = " ); A := Rat_Const(0, 5);   Put( Image(A) ); New_Line;
  Put( " 5, 0      = " ); A := Rat_Const(5, 0);   Put( Image(A) ); New_Line;
  Put( " 8, 16     = " ); A := Rat_Const(8, 16);  Put( Image(A) ); New_Line;
  Put( " 8, 8      = " ); A := Rat_Const(8, 8);   Put( Image(A) ); New_Line;
  Put( " -17, 8    = " ); A := Rat_Const(-17, 8); Put( Image(A) ); New_Line;
  Put( " -17, -8   = " ); A := Rat_Const(-17, -8);Put( Image(A) ); New_Line;
  Put( " 17, -8    = " ); A := Rat_Const(17, -8); Put( Image(A) ); New_Line;
  Put( " 7, 97     = " ); A := Rat_Const(7, 97);  Put( Image(A) ); New_Line;
  Put( " 40, 20    = " ); A := Rat_Const(40, 20); Put( Image(A) ); New_Line;
  Put( " 20, 40    = " ); A := Rat_Const(20, 40); Put( Image(A) ); New_Line;
  Put( " 648, 972  = " ); A := Rat_Const(648,972);Put( Image(A) ); New_Line;
end Main3;

--[Main.adb] Procedure
with Ada.Text_Io, Class_Rational;
use  Ada.Text_Io, Class_Rational;
procedure Main4 is
  A : Rational;
begin
  A := Rat_Const(0, 0);
  for I in 1 .. 5 loop
    A := A + Rat_Const(1,I);
  end loop;
  Put("Sum of 1/1 + 1/2 + ... 1/5 is "); Put( Image(A) ); New_Line;
end Main4;

--[Main.adb] Procedure
with Ada.Text_Io, Class_Rational;
use  Ada.Text_Io, Class_Rational;
procedure Main5 is
  A : Rational;
  Up_To : constant := 7;
begin
  A := Rat_Const(1, 1);
  for I in 1 .. Up_To loop
    A := A + Rat_Const(1, I);
  end loop;
  for I in reverse 1 .. Up_To loop
    A := A - Rat_Const(1, I);
  end loop;
  Put("Sum  check answer should be 1 <=> "); Put( Image(A) ); New_Line;
  A := Rat_Const(1);
  for I in 1 .. Up_To loop
    A := A * Rat_Const(1, I);
  end loop;
  for I in reverse 1 .. Up_To loop
    A := A / Rat_Const(1,I);
  end loop;
  Put("Mult check answer should be 1 <=> "); Put( Image(A) ); New_Line;
end Main5;


with Ada.Text_Io, Class_Rational;
use  type Class_Rational.Rational;
procedure Main6 is
  A,B : Class_Rational.Rational;
begin
  A := Class_Rational.Rat_Const( 1, 2 );
  B := Class_Rational.Rat_Const( 1, 3 );

  Ada.Text_Io.Put( "a + b = " ); 
  Ada.Text_Io.Put( Class_Rational.Image(A+B) );
  Ada.Text_Io.New_Line;
end Main6;

--[main.adb] Procedure
with Ada.Text_Io, Main1, Main2, Main3, Main4, Main5, Main6;
use  Ada.Text_Io;
procedure Main is
begin
  Put("Example Account 1 "); New_Line; Main1;
  Put("Example Account 2 "); New_Line; Main2;
  Put("Example Account 3 "); New_Line; Main3;
  Put("Example Account 4 "); New_Line; Main4;
  Put("Example Account 5 "); New_Line; Main5;
  Put("Example Account 6 "); New_Line; Main6;
end Main;
