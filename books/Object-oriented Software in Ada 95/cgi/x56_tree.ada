---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:48 PM BST  --
---------------------------------------------------------------------
--[class_pic.ads] Specification
with Ada.Strings.Unbounded, Ada.Finalization;
use  Ada.Strings.Unbounded, Ada.Finalization;
generic
  type Index is private;                  -- Index for record
  type Data  is private;                  -- Data for record
  with function  ">"( F:in Index; S:in Index ) return Boolean;
package Class_Pic is
  Not_There, Mainists, Per_Error : exception; -- Raised Exceptions
  type Pic  is new Limited_Controlled with private;
  procedure Initialize( The:in out Pic );
  procedure Initialize( The:in out Pic; Id:in String );
  procedure Finalize( The:in out Pic );
  procedure Discard( The:in out Pic );
  procedure Set_Name( The:in out Pic; Id:in String );
  function  Get_Name( The:in Pic ) return String;

  procedure Add( The:in out Pic; I:in Index; D:in Data );
  procedure Extract( The:in out Pic; I:in Index; D:in out Data );
  procedure Update( The:in out Pic; I:in Index; D:in out Data );
private
  type Leaf;                     -- Index + Data
  type Subtree is access Leaf;   --
  type Pic is new Limited_Controlled with record
    Tree   : Subtree := null;    --  Storage
    Obj_Id : Unbounded_String;     --  Name of object
  end record;

  function  Find( The:in Subtree; I:in Index) return Subtree;
  procedure Release_Storage( The:in out Subtree );

end Class_Pic;

--[class_pic.adb] Implementation
with Unchecked_Deallocation, Sequential_Io;
package body Class_Pic is

  type Element is record    --
    S_Index: Index;         -- The Index
    S_Data : Data;          -- The Data
  end record;

  type Leaf is record       --
    Left   : Subtree;       -- Possible left node
    Rec    : Element;       -- Index + data
    Right  : Subtree;       -- Possible right node;
  end record;

  package Io is new Sequential_Io( Element );

  procedure Initialize( The:in out Pic ) is
  begin
    The.Tree := null;    -- No storage
  end Initialize;

  procedure Initialize( The:in out Pic; Id:in String ) is
    Per : Io.File_Type;    -- File descriptor
    Cur : Element;         -- Persistent data record element
  begin
    Set_Name( The, Id );                  -- Name object
    Io.Open( Per, Io.In_File, Id );       -- Open saved state
    while not Io.End_Of_File( Per ) loop  -- Restore saved state
      Io.Read( Per, Cur );
      Add( The, Cur.S_Index, Cur.S_Data );
    end loop;
    Io.Close( Per );
  exception                               -- Return real exception
    when others => raise Per_Error;      --  as sub code
  end Initialize;

  procedure Finalize( The:in out Pic ) is
    Per : Io.File_Type;    -- File descriptor
    procedure Rec_Finalize( The:in Subtree ) is -- Save state
    begin
      if The /= null then                   -- Subtree save as
        Io.Write( Per, The.Rec );           --  Item
        Rec_Finalize( The.Left );           --  LHS
        Rec_Finalize( The.Right );          --  RHS
      end if;
    end Rec_Finalize;
  begin
    if To_String(The.Obj_Id) /= "" then     -- If save state
      Io.Create( Per, Io.Out_File,
        To_String( The.Obj_Id ) );
      Rec_Finalize( The.Tree );
      Io.Close( Per );
    end if;
    Release_Storage( The.Tree );
  exception                              -- Return real exception
    when others => raise Per_Error;     --  as sub code
  end Finalize;

  procedure Discard( The:in out Pic ) is
  begin
    Set_Name( The, "" );                 -- No name
    Release_Storage( The.Tree );         -- Release storage
  end Discard;

  procedure Set_Name( The:in out Pic; Id:in String ) is
  begin
    The.Obj_Id := To_Unbounded_String(Id); -- Set object name
  end Set_Name;

  function  Get_Name( The:in Pic ) return String is
  begin
    return To_String( The.Obj_Id );     -- Name of object
  end Get_Name;

  procedure Add( The:in out Pic; I:in Index; D:in Data ) is
    procedure Add_S(The:in out Subtree; I:in Index; D:in Data) is
    begin
      if The = null then
        The := new Leaf'( null, Element'(I,D), null );
      else
        if I = The.Rec.S_Index then     -- Index all ready exists
          raise Mainists;
        elsif I > The.Rec.S_Index then  -- Try on RHS
          Add_S( The.Right, I, D );
        else                            -- LHS
          Add_S( The.Left, I, D );
        end if;
      end if;
    end Add_S;
  begin
    Add_S( The.Tree, I, D );
  end Add;

  procedure Extract(The:in out Pic; I:in Index; D:in out Data) is
    Node_Is : Subtree;
  begin
    Node_Is := Find( The.Tree, I );     -- Find node with iey
    D := Node_Is.Rec.S_Data;            -- return data
  end Extract;

  procedure Update(The:in out Pic; I:in Index; D:in out Data) is
    Node_Is : Subtree;
  begin
    Node_Is := Find( The.Tree, I );     -- Find node with iey
    Node_Is.Rec.S_Data := D;            -- Update data
  end Update;

  function Find( The:in Subtree; I:in Index) return Subtree is
  begin
    if The = null then raise Not_There; end if;
    if I = The.Rec.S_Index then
      return The;                           -- Found
    else
      if I > The.Rec.S_Index
          then return Find( The.Right, I );   -- Try RHS
      else return Find( The.Left,  I );   -- Try LHS
      end if;
    end if;
  end Find;

  procedure Dispose is
    new Unchecked_Deallocation( Leaf, Subtree );

  procedure Release_Storage( The:in out Subtree ) is
  begin
    if The /= null then             -- Not empty
      Release_Storage( The.Left );  -- Free LHS
      Release_Storage( The.Right ); -- Free RHS
      Dispose( The );               -- Dispose of item
    end if;
    The := null;                    -- Subtree root NULL
  end Release_Storage;

end Class_Pic;

--[pack_types.ads] Specification
-- Subtype as get demands a String
package Pack_Types is
  subtype  Country is String(1 .. 12); -- Country 
  subtype  Idc     is String(1 .. 6); -- International Dialling Code
end Pack_Types;

--[class_tel_list.ADS] Implementation Instantiation
with Class_Pic, Pack_Types;
use  Pack_Types;
pragma Elaborate_All( Class_Pic );
package Class_Tel_List is new
  Class_Pic( Country, Idc, ">" );

--[pack_procedures.adb] Implementation
with Ada.Text_Io, Class_Tel_List;
use  Ada.Text_Io, Class_Tel_List;
procedure Main1 is
  Tel_List : Pic;
begin
  Put("Creating Telephone list"); New_Line;
  Set_Name( Tel_List, "tel_list.per" );
  Add( Tel_List, "Canada      ", "+1    " );
  Add( Tel_List, "USA         ", "+1    " );
  Add( Tel_List, "Netherlands ", "+31   " );
  Add( Tel_List, "Belgium     ", "+32   " );
  Add( Tel_List, "France      ", "+33   " );
  Add( Tel_List, "Gibraltar   ", "+350  " );
  Add( Tel_List, "Ireland     ", "+353  " );
  Add( Tel_List, "Switzerland ", "+41   " );
  Add( Tel_List, "UK          ", "+44   " );
  Add( Tel_List, "Denmark     ", "+45   " );
  Add( Tel_List, "Norway      ", "+47   " );
  Add( Tel_List, "Germany     ", "+49   " );
  Add( Tel_List, "Australia   ", "+61   " );
  Add( Tel_List, "Japan       ", "+81   " );
end Main1;



with Ada.Text_Io, Pack_Types, Class_Tel_List;
use  Ada.Text_Io, Pack_Types, Class_Tel_List;
procedure Main2 is
  Tel_List : Pic;
  Action   : Character;
  Name     : Country;
  Tel      : Idc;
begin
  Initialize( Tel_List, "tel_list.per" );
  while not End_Of_File loop
    begin
      Get( Action );                       -- Action to perform
      case Action is
        when '+' =>                        -- Add
          Get( Name ); Get( Tel );
          Add( Tel_List, Name, Tel );
        when '=' =>                        -- Extract
          Get( Name );
          Extract( Tel_List, Name, Tel );
          Put( "IDC for " ); Put( Name );
          Put( " is "); Put( Tel ); New_Line;
        when '*' =>                        -- Update
          Get( Name ); Get( Tel );
          Update( Tel_List, Name, Tel );
        when others =>                     -- Invalid action
          null;
      end case;
    exception
      when Not_There =>                    -- Not there
        Put("Name not in directory"); New_Line;
      when Mainists =>                       -- Exists
        Put("Name already in directory"); New_Line;
    end;
    Skip_Line;
  end loop;
end Main2;



with Ada.Text_Io, Pack_Types, Class_Tel_List;
use  Ada.Text_Io, Pack_Types, Class_Tel_List;
procedure Main3 is
  Tel_List : Pic;
  Name     : Country;
  Tel      : Idc;
begin
  Add( Tel_List, "Canada      ", "+1    " );
  Add( Tel_List, "USA         ", "+1    " );
  Add( Tel_List, "Netherlands ", "+31   " );
  Add( Tel_List, "Belgium     ", "+32   " );
  Add( Tel_List, "France      ", "+33   " );
  Add( Tel_List, "Gibraltar   ", "+350  " );
  Name := "France      ";
  Extract( Tel_List, Name, Tel );
  Put( "IDC for " ); Put( Name ); Put( " is "); Put( Tel ); New_Line;
  Name := "USA         ";
  Extract( Tel_List, Name, Tel );
  Put( "IDC for " ); Put( Name ); Put( " is "); Put( Tel ); New_Line;
end Main3;



with Ada.Text_Io, Class_Tel_List;
use  Ada.Text_Io, Class_Tel_List;
procedure Main4 is
  Tel_List : Pic;
begin
  Add( Tel_List, "Canada      ", "+1    " );
  Put("Object identity is [" & Get_Name(Tel_List) & "]"); New_Line;
  Set_Name( Tel_List, "tel_list.per" );
  Put("Object identity is [" & Get_Name(Tel_List) & "]"); New_Line;
  Discard( Tel_List );
end Main4;




--[main.adb] Procedure
with Ada.Text_Io, Main1, Main2, Main3, Main4;
use  Ada.Text_Io;
procedure Main is
begin
  Put("Example Account 1 "); New_Line; Main1;
  Put("Example Account 2 "); New_Line; Main2;
  Put("Example Account 3 "); New_Line; Main3;
  Put("Example Account 4 "); New_Line; Main4;
end Main;
