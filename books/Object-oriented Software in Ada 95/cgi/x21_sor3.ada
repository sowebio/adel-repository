---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:45 PM BST  --
---------------------------------------------------------------------
--[class_sort.ads] Specification
generic
  type T         is private;          -- Any non limited type
  type Vec_Range is (<>);             -- Any discrete type
  type Vec       is array( Vec_Range ) of T;
  with function  ">"( First, Second:in T ) return Boolean is <>;
procedure Sort( Items:in out Vec );

--[class_sort.adb] Implementation
procedure Sort( Items:in out Vec ) is
  Swaps : Boolean := True;
  Tmp   : T;
begin
  while Swaps loop
    Swaps := False;
    for I in Items'First .. Vec_Range'Pred(Items'Last) loop
      if Items( I ) > Items( Vec_Range'Succ(I) ) then
        Swaps := True;
        Tmp := Items( Vec_Range'Succ(I) );
        Items( Vec_Range'Succ(I) ) := Items( I );
        Items( I ) := Tmp;
      end if;
    end loop;
  end loop;
end Sort;

--[main.adb] Procedure
with Ada.Text_Io, Sort;
use  Ada.Text_Io;
procedure Main1 is

  type Chs_Range is range 1 .. 6;
  type Chs       is array( Chs_Range ) of Character;

  procedure Sort_Chs is new Sort (
    T         => Character,
    Vec_Range => Chs_Range,
    Vec       => Chs,
    ">"       => ">" );
  Some_Characters : Chs := ( 'q', 'w', 'e', 'r', 't', 'y' );
begin
  Sort_Chs( Some_Characters );
  for I in Chs_Range loop
    Put( Some_Characters( I ) ); Put( " " );
  end loop;
  New_Line;
end Main1;


--[main2.adb] Procedure
with Ada.Text_Io, Sort;
use  Ada.Text_Io;
procedure Main2 is
  type Chs_Range is ( Red, Blue, Green );
  type Chs is array( Chs_Range ) of Character;

  procedure Sort_Chs is new Sort (
    T         => Character,
    Vec_Range => Chs_Range,
    Vec       => Chs,
    ">"       => ">" );
  Some_Items : Chs := ( 'q', 'w', 'e' );
begin
  Sort_Chs( Some_Items );
  for I in Chs_Range loop
    Put( Some_Items( I ) ); Put( " " );
  end loop;
  New_Line;
end Main2;


--[main3.adb] Procedure
with Ada.Text_Io, Sort;
use  Ada.Text_Io;
procedure Main3 is
  Max_Chs : constant := 7;
  type Height_Cm is range 0 .. 300;
  type Person is record
    Name   : String( 1 .. Max_Chs );  -- Name as a String
    Height : Height_Cm := 0;          -- Height in cm.
  end record;
  type People_Range is (First, Second, Third, Forth );
  type People       is array( People_Range ) of Person;

  function Cmp_Height(First, Second:in Person) return Boolean is
  begin
    return First.Height > Second.Height;
  end Cmp_Height;

  function Cmp_Name( First, Second:in Person ) return Boolean is
  begin
    return First.Name > Second.Name;
  end Cmp_Name;

  procedure Sort_People_Height is new Sort (
    T         => Person,
    Vec_Range => People_Range,
    Vec       => People,
    ">"       => Cmp_Height );

  procedure Sort_People_Name is new Sort (
    T         => Person,
    Vec_Range => People_Range,
    Vec       => People,
    ">"       => Cmp_Name );

  Friends : People := ( ("Paul   ", 146 ), ("Carol  ", 147 ),
    ("Mike   ", 183 ), ("Corinna", 171 ) );
begin
  Sort_People_Name( Friends );                    -- Name order
  Put( "The first in ascending name order is   " );
  Put( Friends( First ).Name ); New_Line;
  Sort_People_Height( Friends );                  -- Height order
  Put( "The first in ascending height order is " );
  Put( Friends( First ).Name ); New_Line;
end Main3;

--[main4.adb] Procedure
procedure Main4 is
begin
  null;
end Main4;

--[main.adb] Procedure
with Ada.Text_Io, Main1, Main2, Main3, Main4;
use  Ada.Text_Io;
procedure Main is
begin
  Put("Example 1 "); New_Line; Main1;
  Put("Example 2 "); New_Line; Main2;
  Put("Example 3 "); New_Line; Main3;
  Put("Example 4 "); New_Line; Main4;
end Main;
