---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:50 PM BST  --
---------------------------------------------------------------------
with Ada.Finalization, Unchecked_Deallocation;
use  Ada.Finalization;
generic
  type T is private;                  -- Any type
package Class_List is
  type List is new Controlled with private;

  procedure Initialize( The:in out List );
  procedure Initialize( The:in out List; Data:in T );
  procedure Finalize( The:in out List );
  procedure Adjust( The:in out List );
  function "="  ( F:in List; S:in List ) return Boolean;
private
  procedure Release_Storage( The:in out List );

  type Node;                       -- Tentative declaration
  type P_Node is access all Node;  -- Pointer to Node

  type Node is record
    Prev    : P_Node;              -- Previous Node
    Item    : T;                   -- The physical item
    Next    : P_Node;              -- Next Node
  end record;

  procedure Dispose_Node is
    new Unchecked_Deallocation( Node, P_Node );
  type List is new Controlled with record
    First_Node : aliased P_Node := null;   -- First item in list
    Last_Node  : aliased P_Node := null;   -- First item in list
  end record;

end Class_List;

--[class_list.adb] Implementation
--WITH Simple_io; USE  Simple_io;
package body Class_List is

  procedure Initialize( The:in out List ) is
  begin
    The.First_Node := null;   -- Empty list
    The.Last_Node  := null;   -- Empty list
  end Initialize;

  procedure Initialize( The:in out List; Data:in T ) is
  begin
    The.First_Node := new Node'(null, Data, null);
    The.Last_Node  := The.First_Node;
  end Initialize;

  procedure Finalize( The:in out List ) is
  begin
    if The.First_Node /= null then
      Release_Storage( The );
      The.First_Node := null;
    end if;
  end Finalize;

  procedure Adjust( The:in out List ) is
    Cur : P_Node := The.First_Node;  -- Original list
    Lst : P_Node := null;            -- Last created node
    Prv : P_Node := null;            -- Previously created node
    Fst : P_Node := null;            -- The first node

  begin
    while Cur /= null loop
      Lst := new Node'( Prv, Cur.Item, null );
      if Fst =  null then Fst := Lst; end if;
      if Prv /= null then Prv.Next := Lst; end if;
      Prv := Lst;
      Cur := Cur.Next;               -- Next node
    end loop;
    The.First_Node := Fst;           -- Update
    The.Last_Node  := Lst;
  end Adjust;

  function "="  ( F:in List; S:in List ) return Boolean is
    F_Node : P_Node := F.First_Node;  -- First list
    S_Node : P_Node := S.First_Node;  -- Second list
  begin
    while F_Node /= null and S_Node /= null loop
      if F_Node.Item /= S_Node.Item then
        return False;                 -- Different items
      end if;
      F_Node := F_Node.Next; S_Node := S_Node.Next;
    end loop;
    return F_Node = S_Node;           -- Both NULL if equal
  end "=";

  procedure Release_Storage( The:in out List ) is
    Cur : P_Node := The.First_Node; -- Pointer to curr node
    Tmp : P_Node;                   -- Node to dispose
  begin
    while Cur /= null loop          -- For each item in list
      Tmp := Cur;                   -- Item to dispose
      Cur := Cur.Next;              -- Next node
      Dispose_Node( Tmp );          -- Dispose of item
    end loop;
  end Release_Storage;

end Class_List;

--[class_list/iterator.ads] Specification
--WITH Ada.Finalization; USE  Ada.Finalization;
generic
package Class_List.Iterator is

  type List_Iter is limited private;

  procedure Initialize( The:in out List_Iter );
  procedure Finalize( The:in out List_Iter );
  procedure First( The:in out List_Iter; L:in out List );
  procedure Last( The:in out List_Iter; L:in out List );

  function  Deliver( The:in List_Iter) return T;
  procedure Insert( The:in out List_Iter; Data:in T );
  procedure Delete( The:in out List_Iter );
  function  Is_End( The:in List_Iter ) return Boolean;
  procedure Next( The:in out List_Iter );
  procedure Prev( The:in out List_Iter );
private
  type P_P_Node is access all P_Node;
  type List_Iter is record
    Cur_List_First: P_P_Node := null;    -- First in chain
    Cur_List_Last : P_P_Node := null;    -- Last in chain
    Cur_Node      : P_Node   := null;    -- Current item
  end record;
end Class_List.Iterator;

--[class_list/iterator.adb] Implementation
--WITH Simple_io; USE Simple_io;
package body Class_List.Iterator is

  procedure Initialize( The:in out List_Iter ) is
  begin
    The.Cur_Node       := null;   -- Iterator not setup
    The.Cur_List_First := null;
    The.Cur_List_Last  := null;
  end Initialize;

  procedure Finalize( The:in out List_Iter ) is
  begin
    null;
  end Finalize;

  procedure First( The:in out List_Iter; L:in out List ) is
  begin
    The.Cur_Node      := L.First_Node;       -- Set to first
    The.Cur_List_First:= L.First_Node'Unchecked_Access;
    The.Cur_List_Last := L.Last_Node'Unchecked_Access;
  end First;

  procedure Last( The:in out List_Iter; L:in out List ) is
  begin
    The.Cur_Node      := L.Last_Node;        -- Set to last
    The.Cur_List_First:= L.First_Node'Unchecked_Access;
    The.Cur_List_Last := L.Last_Node'Unchecked_Access;
  end Last;

  function Deliver( The:in List_Iter ) return T is
  begin
    return The.Cur_Node.Item;  -- The current item
  end Deliver;

  procedure Insert( The:in out List_Iter; Data:in T ) is
    Tmp   : P_Node;
    Cur   : P_Node   := The.Cur_Node;   -- Current element
    First : P_P_Node := The.Cur_List_First;
    Last  : P_P_Node := The.Cur_List_Last;
  begin
    if Cur = null then            -- Empty or last item
      if First.all = null then    --  Empty list
        Tmp := new Node'( null, Data, null );
        First.all := Tmp;
        Last.all  := Tmp;
        The.Cur_Node := Tmp;
      else                        --  Last
        Tmp := new Node'( Last.all, Data, null );
        Last.all.Next := Tmp;
        Last.all      := Tmp;
        The.Cur_Node := Tmp;
      end if;
    else
      Tmp := new Node'( Cur.Prev, Data, Cur );
      if Cur.Prev = null then      -- First item
        First.all := Tmp;
      else
        Cur.Prev.Next := Tmp;
      end if;
      Cur.Prev := Tmp;
    end if;
  end Insert;

  procedure Delete( The:in out List_Iter) is
    Cur   : P_Node   := The.Cur_Node;   -- Current element
    First : P_P_Node := The.Cur_List_First;
    Last  : P_P_Node := The.Cur_List_Last;
  begin
    if Cur /= null then             -- Something to delete
      if Cur.Prev /= null then      -- Fix forward pointer;
        Cur.Prev.Next := Cur.Next;  --  Not first in chain
      else
        First.all := Cur.Next;      --  First in chain
        if First.all = null then
          Last.all := null;         --   Empty list
        end if;
      end if;
      if Cur.Next /= null then      -- Fix backward pointer;
        Cur.Next.Prev := Cur.Prev;  --  Not last in chain
      else
        Last.all := Cur.Prev;       --  Last in chain
        if Last.all = null then
          First.all := null;        --   Empty list
        end if;
      end if;
      if Cur.Next /= null then      -- Fix current pointer
        The.Cur_Node := Cur.Next;   --  next
      elsif Cur.Prev /= null then
        The.Cur_Node := Cur.Prev;   --  previous
      else
        The.Cur_Node := null;       --  none empty list
      end if;
      Dispose_Node( Cur );          -- Release storage
    end if;
  end Delete;

  function  Is_End( The:in List_Iter ) return Boolean is
  begin
    return The.Cur_Node = null;               -- True if end
  end Is_End;

  procedure Next( The:in out List_Iter ) is
  begin
    if The.Cur_Node /= null then               --
      The.Cur_Node  := The.Cur_Node.Next;      -- Next
    end if;
  end Next;

  procedure Prev( The:in out List_Iter ) is
  begin
    if The.Cur_Node /= null then               --
      The.Cur_Node  := The.Cur_Node.Prev;      -- Previous
    end if;
  end Prev;

end Class_List.Iterator;

--[class_set.ads] Specification
with Class_List, Class_List.Iterator;
pragma Elaborate_All( Class_List, Class_List.Iterator );
generic
  type T is private;
  with procedure Put( Item:in T ) is <>;
  with function  ">" (First,Second:in T ) return Boolean is <>;
  with function  "<" (First,Second:in T ) return Boolean is <>;
package Class_Set is
  type Set is private;
  procedure Put( The:in Set );
  function "+"( F:in Set; S:in Set ) return Set;
  function Set_Const( Item: in T )   return Set;
  function Members( The:in Set )     return Positive;
private
  package Class_List_T          is new Class_List(T);
  package Class_List_T_Iterator is new Class_List_T.Iterator;
  type Set is new Class_List_T.List with record
    Elements : Natural := 0;             -- Elements in set
  end record;
end Class_Set;

--[class_set.adb] Implementation
with Ada.Text_Io;
use  Ada.Text_Io;
package body Class_Set is
  use Class_List_T, Class_List_T_Iterator;

  procedure Put( The:in Set ) is
    It    : List_Iter;
    C_The : List := List(The);
  begin
    Put("("); First( It, C_The );
    for I in 1 .. The.Elements loop
      Put( Deliver(It) ); Next( It );
      if I /= The.Elements then Put(","); end if;
    end loop;
    Put(")");
  end Put;

  function "+"  ( F:in Set; S:in Set ) return Set is
    Res_It    : List_Iter;
    F_It,S_It : List_Iter;
    Res       : Set;
    F_List, S_List: List;
  begin
    F_List := List(F); S_List := List(S);
    First( F_It, List(F_List) );
    First( S_It, List(S_List) );
    First( Res_It, List(Res) );

    while (not Is_End(F_It)) or (not Is_End(S_It)) loop
      if Is_End(F_It) then
        Next(Res_It); Insert(Res_It, Deliver(S_It));
        Next(S_It);
      elsif Is_End(S_It) then
        Next(Res_It); Insert(Res_It, Deliver(F_It));
        Next(F_It);
      elsif Deliver(F_It) < Deliver(S_It) then
        Next(Res_It); Insert(Res_It, Deliver(F_It));
        Next(F_It);
      elsif Deliver(F_It) > Deliver(S_It) then
        Next(Res_It); Insert(Res_It, Deliver(S_It));
        Next(S_It);
      elsif Deliver(F_It) = Deliver(S_It) then
        Next(Res_It); Insert(Res_It, Deliver(F_It) );
        Next(F_It); Next(S_It);
      end if;
      Res.Elements := Res.Elements + 1;
    end loop;
    return Res;
  end "+";


  function Set_Const( Item: in T ) return Set is
    Res : Set;
  begin
    Initialize( Res, Item ); Res.Elements := 1;
    return Res;
  end Set_Const;

  function Members( The:in Set ) return Positive is
  begin
    return The.Elements;
  end Members;

end Class_Set;

-- =======================================================

-- =======================================================

package Pack_Types is
  type Filling is ( Cheese, Onion, Ham, Tomato );
end Pack_Types;

with Ada.Text_Io, Pack_Types;
use  Ada.Text_Io, Pack_Types;
procedure Put_Filling( C:in Filling ) is
begin
  Put( Filling'Image( C ) );
end Put_Filling;

with Pack_Types, Class_Set, Put_Filling;
use  Pack_Types;                          -- **** Object Ada
pragma Elaborate_All( Class_Set );
package Class_Set_Sandwich is
  new Class_Set( T => Pack_Types.Filling, Put => Put_Filling );

--[main1.adb] Procedure
with Pack_Types, Ada.Text_Io, Ada.Integer_Text_Io, Class_Set_Sandwich;
use  Pack_Types, Ada.Text_Io, Ada.Integer_Text_Io, Class_Set_Sandwich;
procedure Main1 is
  Sandwich : Class_Set_Sandwich.Set;
begin
  Sandwich := Sandwich + Set_Const(Cheese);
  Sandwich := Sandwich + Set_Const(Onion) ;
  Put("Contents of sandwich are : ");
  Put( Sandwich ); New_Line;
  Put("Number of ingredients is : ");
  Put( Members(Sandwich) ); New_Line;
  null;
end Main1;

-- =======================================================

with Ada.Integer_Text_Io;
procedure Put_Natural( N:in Natural ) is
begin
  Ada.Integer_Text_Io.Put( N, Width=>2 );
end Put_Natural;

with Class_Set, Put_Natural;
pragma Elaborate_All( Class_Set );
package Class_Set_Naturals is
  new Class_Set( T => Natural, Put => Put_Natural );

--[main2.adb] Procedure
with Ada.Text_Io, Class_Set_Naturals;
use  Ada.Text_Io, Class_Set_Naturals;
procedure Main2 is
  Set1 : Class_Set_Naturals.Set;
  Set2 : Class_Set_Naturals.Set;
  Set3 : Class_Set_Naturals.Set;
begin
  for I in 1 .. 5 loop
    Set1 := Set1 + Set_Const(I*2);
  end loop;
  Put( "Set 1 =       "); Put( Set1 ); New_Line;
  for I in 1 .. 5 loop
    Set2 := Set2 + Set_Const(I*2+1);
  end loop;
  Put( "Set 2 =       "); Put( Set2 ); New_Line;
  Put( "Set 1 + Set 2 ");
  Put( Set1+Set2 ); New_Line;
  for I in 4 .. 15 loop
    Set3 := Set3 + Set_Const(I);
  end loop;
  Put( "Set 3 =       "); Put( Set3 ); New_Line;
  Put( "Set 2 + Set 3 ");
  Put( Set2+Set3 ); New_Line;
end Main2;


--[main3.adb] Procedure
with Ada.Text_Io, Class_Set_Naturals;
use  Ada.Text_Io, Class_Set_Naturals;
procedure Main3 is
  Set1 : Class_Set_Naturals.Set;
  Set2 : Class_Set_Naturals.Set;
  Set3 : Class_Set_Naturals.Set;
begin
  for I in 1 .. 5 loop
    Set1 := Set1 + Set_Const(I*2);
  end loop;
  Put( "Set 1 =       "); Put( Set1 ); New_Line;
  for I in 1 .. 5 loop
    Set2 := Set2 + Set_Const(I*2+1);
  end loop;
  Put( "Set 2 =       "); Put( Set2 ); New_Line;
  Put( "Set 1 + Set 2 ");
  Put( Set1+Set2 ); New_Line;
  Set3 := Set1 + Set2;
  Put( Set3 ); New_Line;
  Set3 := Set3 +  Set_Const(100);
  Put( Set3 ); New_Line;
  Put( Set2 ); New_Line;
  Put( Set1 ); New_Line;
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
