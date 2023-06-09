---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:49 PM BST  --
---------------------------------------------------------------------
--[class_list.ads] Specification
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
  type Node;                       -- Tentative declaration
  type P_Node is access all Node;  -- Pointer to Node

  type Node is record
    Prev    : P_Node;              -- Previous Node
    Item    : T;                   -- The physical item
    Next    : P_Node;              -- Next Node
  end record;

  type List is new Controlled with record
    First_Node : aliased P_Node := null;   -- First item in list
    Last_Node  : aliased P_Node := null;   -- First item in list
  end record;

end Class_List;





--[class_list.adb] Implementation
package body Class_List is

  procedure Dispose_Node is
    new Unchecked_Deallocation( Node, P_Node );

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

end Class_List;



--[class_list/iterator.ads] Specification
--WITH Ada.Finalization; USE  Ada.Finalization;
generic
package Class_List.Iterator is

  type List_Iter is limited private;

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
package body Class_List.Iterator is

  procedure Dispose_Node is
    new Unchecked_Deallocation( Node, P_Node );

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


--[class_list_nat.ADS] Implementation Instantiation
with Class_List;
  pragma Elaborate_All( Class_List );
  package Class_List_Nat is new Class_List(Natural);


--[class_list/Nat_Iterator.ADS] Implementation Instantiation
with Class_List_Nat, Class_List.Iterator;
  pragma Elaborate_All( Class_List_Nat, Class_List.Iterator );
  package Class_List_Nat_Iterator is new Class_List_Nat.Iterator;




--[pack_procedures.ads] Specification
with Ada.Text_Io, Ada.Integer_Text_Io, Class_List_Nat, 
     Class_List_Nat_Iterator;
use  Ada.Text_Io, Ada.Integer_Text_Io, 
     Class_List_Nat, Class_List_Nat_Iterator;

procedure Main1 is
  Numbers    : List;            -- List of Natural numbers
  Numbers_It : List_Iter;       -- Iterator for list
begin
  First( Numbers_It, Numbers );
  Insert( Numbers_It, 50 );
  Insert( Numbers_It, 5 );
  Insert( Numbers_It, 40 );
  Last( Numbers_It, Numbers );
  Next( Numbers_It );
  Insert( Numbers_It, 100 );
  Prev( Numbers_It );
  Prev( Numbers_It );
  Insert( Numbers_It, 30 );
  First( Numbers_It, Numbers );
  while not Is_End( Numbers_It ) loop
    Put( Deliver( Numbers_It ) );
    Next( Numbers_It );
  end loop;
  New_Line;
end Main1;



with Ada.Text_Io, Ada.Integer_Text_Io, Class_List_Nat, 
     Class_List_Nat_Iterator;
use  Ada.Text_Io, Ada.Integer_Text_Io, 
     Class_List_Nat, Class_List_Nat_Iterator;
procedure Main2 is
  Numbers    : List;            -- List of Natural numbers
  Numbers_It : List_Iter;       -- Iterator for list

  procedure Insert( Pos:in Integer; Num:in Integer ) is
    Count : Integer := Pos;
  begin
    First( Numbers_It, Numbers );
    while Count >= 1 loop
      Next( Numbers_It ); Count := Count-1;
    end loop;
    Insert( Numbers_It, Num );
  end Insert;

  procedure Delete( Pos:in Integer ) is
    Count : Integer := Pos;
  begin
    First( Numbers_It, Numbers );
    while Count >= 1 loop
      Next( Numbers_It ); Count := Count-1;
    end loop;
    Delete( Numbers_It );
  end Delete;

  procedure Print is
  begin
    First( Numbers_It, Numbers );
    while not Is_End(Numbers_It) loop
      Put( Deliver(Numbers_It), Width=>3 );
      Next( Numbers_It );
    end loop;
    New_Line;
  end Print;

begin
  Insert( 0, 50 ); Print;
  Put(" 50 <Should be>"); New_Line;
  Delete( 0 ); Print;
  Put(" <Should be>"); New_Line;

  Insert( 0, 50 ); Print;
  Put(" 50 <Should be>"); New_Line;
  Insert( 0, 30 ); Print;
  Put(" 30 50 <Should be>"); New_Line;
  Insert( 1, 40 ); Print;
  Put(" 30 40 50 <Should be>"); New_Line;
  Insert( 2, 45 ); Print;
  Put(" 30 40 45 50 <Should be>"); New_Line;
  Insert( 2, 42 ); Print;
  Put(" 30 40 42 45 50 <Should be>"); New_Line;
  Insert( 5, 99 ); Print;
  Put(" 30 40 42 45 50 99<Should be>"); New_Line;

  Delete( 5 ); Print;
  Put(" 30 40 42 45 50 <Should be>"); New_Line;
  Delete( 0 ); Print;
  Put(" 40 42 45 50 <Should be>"); New_Line;
  Delete( 1 ); Print;
  Put(" 40 45 50 <Should be>"); New_Line;
  Delete( 2 ); Print;
  Put(" 40 45 <Should be>"); New_Line;
  Delete( 1 ); Print;
  Put(" 40 <Should be>"); New_Line;
  Delete( 0 ); Print;
  Put(" <Should be>"); New_Line;
end Main2;




with Ada.Text_Io, Ada.Integer_Text_Io, Class_List_Nat, 
     Class_List_Nat_Iterator;
use  Ada.Text_Io, Ada.Integer_Text_Io, 
     Class_List_Nat, Class_List_Nat_Iterator;
procedure Main3 is
  Numbers    : List;            -- List of Natural numbers
  Numbers_It : List_Iter;       -- Iterator for list
begin
  First( Numbers_It, Numbers );
  Insert( Numbers_It, 100 );
  Insert( Numbers_It, 50 );

  First( Numbers_It, Numbers );
  while not Is_End(Numbers_It) loop
    Put( Deliver(Numbers_It) ); Next( Numbers_It );
  end loop;
  New_Line;
end Main3;




with Ada.Text_Io, Ada.Integer_Text_Io, Class_List_Nat, 
     Class_List_Nat_Iterator;
use  Ada.Text_Io, Ada.Integer_Text_Io, 
     Class_List_Nat, Class_List_Nat_Iterator;
procedure Main4 is
  Numbers    : List;
  Numbers_It : List_Iter;
  Value      : Integer;
begin

  First( Numbers_It, Numbers );             -- Setup iterator
  for Number in 1 .. 10 loop
    First( Numbers_It, Numbers );           -- Set iterator first
    Insert( Numbers_It, Number );           -- Insert before
  end loop;

  First(Numbers_It,Numbers);                -- Set to start
  while not Is_End( Numbers_It ) loop       -- Not end of list
    Put( Deliver(Numbers_It) , Width=>3);   --  Print
    Next( Numbers_It );                     -- Next item
  end loop;
  New_Line;

  First(Numbers_It,Numbers);                -- Set to start
  for Number in 1 .. 10 loop
    delete( Numbers_It );                   -- Delete numbers
  end loop;

  Value := 1;
  While Value <= 10 loop
    Last( Numbers_It, Numbers );            -- Set iterator Last
    Next( Numbers_It );                     -- Move beyond last
    Insert( Numbers_It, Value );            -- Insert before
    value := Value + 1;                     -- Increment
  end loop;

  First(Numbers_It,Numbers);                -- Set to start
  while not Is_End( Numbers_It ) loop       -- Not end of list
    Put( Deliver(Numbers_It) , Width=>3);   --  Print
    Next( Numbers_It );                     -- Next item
  end loop;
  New_Line;


end Main4;




with Ada.Text_Io, Ada.Integer_Text_Io, Class_List_Nat, 
     Class_List_Nat_Iterator;
use  Ada.Text_Io, Ada.Integer_Text_Io, 
     Class_List_Nat, Class_List_Nat_Iterator;
procedure Main5 is
  Numbers    : List;
  Numbers_It : List_Iter;
  Num,In_List: Natural;
begin

  while not End_Of_File loop               -- While data
    while not End_Of_Line loop

      Get(Num);                            -- Read number

      First(Numbers_It,Numbers);           -- Iterator at start
      while not Is_End( Numbers_It ) loop  -- scan through list
        In_List := Deliver(Numbers_It);
        exit when In_List > Num;           -- Exit when larger no.
        Next( Numbers_It );                -- Next item
      end loop;

      Insert( Numbers_It, Num );           -- Insert before cur_node

    end loop;
    Skip_Line;                             -- Next line
  end loop;

  Put("Numbers sorted are: ");
  First(Numbers_It,Numbers);                -- Set at start
  while not Is_End( Numbers_It ) loop
    In_List := Deliver( Numbers_It );       -- Current number
    Put( In_List, width=>2 ); Put(" ");     --  Print
    Next( Numbers_It );                     -- Next number
  end loop;
  New_Line;
end Main5;


with Ada.Text_Io, Ada.Integer_Text_Io, Class_List_Nat, 
     Class_List_Nat_Iterator;
use  Ada.Text_Io, Ada.Integer_Text_Io, 
     Class_List_Nat, Class_List_Nat_Iterator;
procedure Main6 is
  Original: List;                      -- List of numbers
  Copy    : List;                      -- List of numbers
  Num_It  : List_Iter;                 -- Iterator
  Number  : Integer;
begin
  Number := 1;
  while Number <= 10 loop
    Last( Num_It, Original );          -- Set iterator last
    Next( Num_It );                    -- So can insert
    Insert( Num_It, Number );          --   after last item
    Number := Number + 1;
  end loop;

  Put("copy := original "); New_Line;
  Copy := Original;

  Put("copy = original ");
  if Copy = Original then Put("True"); else Put("False"); end if;
  New_Line;

  Put("Append 99 to copy "); New_Line;
  Last( Num_It, Copy ); Next( Num_It );
  Insert( Num_It, 99 );

  Put("copy = original ");
  if Copy = Original then Put("True"); else Put("False"); end if;
  New_Line;

  First(Num_It,Original);               -- Set to start
  while not Is_End( Num_It ) loop       -- Not end of list
    Put( Deliver(Num_It), Width=>4 );   --  Print
    Next( Num_It );                     -- Next item
  end loop;

  New_Line;
  First(Num_It,Copy);                   -- Set to start
  while not Is_End( Num_It ) loop       -- Not end of list
    Put( Deliver(Num_It), Width=>4 );   --  Print
    Next( Num_It );                     -- Next item
  end loop;
  New_Line;
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
