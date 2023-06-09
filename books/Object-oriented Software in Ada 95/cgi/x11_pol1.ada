---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:51:02 PM BST  --
---------------------------------------------------------------------
--[class_description.ADS] Implementation Instantiation
with Ada.Strings.Bounded; use Ada.Strings.Bounded;
package B_String is new Generic_Bounded_Length( 80 );



--[class_room.ads] Specification
with B_String; use B_String;
package Class_Room is
  type Room   is tagged private;

  procedure Initialize( The:in out Room; No:in Positive;
                        Mes:in String );
  function  Where( The:in Room ) return Positive;
  function  Describe( The:in Room ) return String;
private
  type Room is tagged record
    Desc  : Bounded_String;     -- Description of room
    Number: Positive;           -- Room number
  end record;
end Class_Room;



--[class_room.adb] Implementation
with Ada.Integer_Text_Io;
use  Ada.Integer_Text_Io;
package body Class_Room is

  procedure Initialize( The:in out Room;
                        No:in Positive; Mes:in String ) is
  begin
    The.Desc := To_Bounded_String( Mes );
    The.Number := No;
  end Initialize;

  function  Where( The:in Room ) return Positive is
  begin
    return The.Number;
  end Where;

  function  Describe( The:in Room ) return String is
    Num : String( 1 .. 4 );    -- Room number as string
  begin
    Put( Num, The.Number );
    return Num & " " & To_String(The.Desc);
  end Describe;

end Class_Room;






--[class_office.ads] Specification
with Class_Room; 
use  Class_Room;
package Class_Office is
  type Office is new Room with private;

  procedure Initialize( The:in out Office; No:in Positive;
                        Desc:in String; People:in Natural );
  function  Deliver_No_Of_People(The:in Office) return Natural;
  function  Describe( The:in Office ) return String;
private
  type Office is new Room with record
    People : Natural := 0;            -- Occupants
  end record;
end Class_Office;




--[class_office.adb] Implementation
with Ada.Integer_Text_Io;
use  Ada.Integer_Text_Io;
package body Class_Office is

  procedure Initialize( The:in out Office; No:in Positive;
                        Desc:in String; People:in Natural ) is
  begin
    Initialize( The, No, Desc );
    The.People := People;
  end Initialize;

  function Deliver_No_Of_People( The:in Office ) return Natural is
  begin
    return The.People;
  end Deliver_No_Of_People;

  function  Describe( The:in Office ) return String is
    No : String( 1 .. 4 );    -- the.people as string
  begin
    Put( No, The.People );
    return Describe( Room(The) ) &
      " occupied by" & No & " people";
  end Describe;

end Class_Office;





package Class_Room.Build is
  type P_Room is access all Room'Class;

  function  Build_Room( No:in Positive;
                        Desc:in String ) return P_Room;
end Class_Room.Build;

package body Class_Room.Build is

  function  Build_Room( No:in Positive;
                        Desc:in String ) return P_Room is
    P : P_Room;
  begin
    P := new Room; Initialize( P.all, No, Desc );
    return P;
  end Build_Room;

end Class_Room.Build;






with Class_Room, Class_Room.Build;
use  Class_Room, Class_Room.Build;
package Class_Office.Build is

  function  Build_Office( No:in Positive; Desc:in String;
                          People:in Natural ) return P_Room;
end Class_Office.Build;


package body Class_Office.Build is

  type P_Office is access all Office;

  function  Build_Office( No:in Positive; Desc:in String;
                          People:in Natural ) return P_Room is
    P : P_Office;
  begin
    P := new Office; Initialize( P.all, No, Desc, People );
    return P.all'access;
  end Build_Office;
end Class_Office.Build;





--[class_building.ads] Specification
with Class_Room, Class_Room.Build;
use  Class_Room, Class_Room.Build;
package Class_Building is

  type Building is tagged private;

  procedure Add( The:in out Building; Desc:in P_Room );
  function About(The:in Building; No:in Positive) return String;

private
  Max_Rooms : constant := 15;
  type    Rooms_Index is range 0 .. Max_Rooms;
  subtype Rooms_Range is Rooms_Index range 1 .. Max_Rooms;
  type    Rooms_Array is array (Rooms_Range) of P_Room;

  type Building is tagged record
    Last        : Rooms_Index := 0;  -- Last slot allocated
    Description : Rooms_Array;       -- Rooms in building
  end record;
end Class_Building;





--[class_building.adb] Implementation
package body Class_Building is

  procedure Add( The:in out Building; Desc:in P_Room ) is
  begin
    if The.Last < Max_Rooms then
      The.Last := The.Last + 1;
      The.Description( The.Last ) := Desc;
    else
      raise Constraint_Error;
    end if;
  end Add;

  function About(The:in Building; No:in Positive) return String is
  begin
    for I in 1 .. The.Last loop
      if Where(The.Description(I).all) = No then
        return Describe(The.Description(I).all);
      end if;
    end loop;
    return "Sorry room not known";
  end About;
end Class_Building;







--[pack_procedures.ads] Specification
with Class_Room, Class_Room.Build,
     Class_Office, Class_Office.Build, Class_Building;
use  Class_Room, Class_Room.Build,
     Class_Office, Class_Office.Build, Class_Building;
procedure Set_Up( Watts:in out Building ) is
begin
  Add( Watts, Build_Office( 414, "4th Floor west wing", 2 ) );
  Add( Watts, Build_Room  ( 422, "4th Floor east wing" ) );
end Set_Up;




with Ada.Text_Io, Ada.Integer_Text_Io, Class_Building, Set_Up;
use  Ada.Text_Io, Ada.Integer_Text_Io, Class_Building;
procedure Main is
  Watts   : Building;                      -- Watts Building
  Room_No : Positive;                      -- Queried room
begin
  Set_Up( Watts );                         -- Populate building
  loop
    begin
      Put( "Inquiry about room: " );       -- Ask
      exit when End_Of_File;
      Get( Room_No ); Skip_Line;           -- User response
      Put( About( Watts, Room_No ) );
      New_Line;                            -- Display answer
    exception
      when Data_Error =>
        Put("Please retype the number");   -- Ask again
        New_Line; Skip_Line;
    end;
  end loop;
end Main;


