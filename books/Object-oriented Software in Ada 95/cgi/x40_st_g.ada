---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:48 PM BST  --
---------------------------------------------------------------------
--[class_stack.ads] Specification
generic
  type Stack_Element is private;        --

package Class_Stack is
  type Stack is limited private;        -- NO copying
  Stack_Error : exception;

  procedure Push( The:in out Stack; Item:in Stack_Element );
  procedure Pop(The:in out Stack; Item :out Stack_Element );
  procedure Reset( The:in out Stack );
private

  type Node;                            -- Mutually recursive def
  type P_Node is access Node;           -- Pointer to a Node
  pragma Controlled( P_Node );          -- We do deallocation

  type Node is record                   -- Node holds the data
    Item   : Stack_Element;             -- The stored item
    P_Next : P_Node;                    -- Next in list
  end record;

  type Stack is record
    P_Head : P_Node := null;            -- First node in list
  end record;
end Class_Stack;





--[class_stack.adb] Implementation

with Unchecked_Deallocation;
pragma Elaborate_All( Unchecked_Deallocation );
package body Class_Stack is

  procedure Dispose is
    new Unchecked_Deallocation( Node, P_Node );

  procedure Push( The:in out Stack; Item:in Stack_Element ) is
    Tmp : P_Node;                       -- Allocated node
  begin
    Tmp := new Node'(Item=>Item, P_Next=>The.P_Head);
    The.P_Head := Tmp;
  end Push;

  procedure Pop( The:in out Stack; Item :out Stack_Element ) is
    Tmp : P_Node;                        -- Free node
  begin
    if The.P_Head /= null then           -- if item then
      Tmp := The.P_Head;                 -- isolate top node
      Item := The.P_Head.Item;           -- extract item stored
      The.P_Head := The.P_Head.P_Next;   -- Relink
      Dispose( Tmp );                    -- return storage
    else
      raise Stack_Error;                 -- Failure
    end if;
  end Pop;

  procedure Reset( The:in out Stack ) is
    Tmp : Stack_Element;
  begin
    while The.P_Head /= null loop        -- Re-initialize stack
      Pop( The, Tmp );
    end loop;
  end Reset;

end Class_Stack;




--[class_stack_int.ADS] Specification
with Class_Stack;
  pragma Elaborate_All( Class_Stack );
  package Class_Stack_Int is new Class_Stack(Integer);

--[main.adb] Procedure
with Ada.Text_Io, Ada.Integer_Text_Io, Class_Stack_Int;
use  Ada.Text_Io, Ada.Integer_Text_Io, Class_Stack_Int;
procedure Main is
  Number_Stack : Stack;              -- Stack of numbers
  Action       : Character;          -- Action
  Number       : Integer;            -- Number processed
begin
  Reset( Number_Stack );             -- Reset stack to empty
  while not End_Of_File loop
    while not End_Of_Line loop
      begin
        Get( Action );
        case Action is               -- Process action
          when '+' =>
            Get( Number ); Push(Number_Stack,Number);
            Put("push number = "); Put(Number); New_Line;
          when '-' =>
            Pop(Number_Stack,Number);
            Put("Pop number  = "); Put(Number); New_Line;
          when others =>
            Put("Invalid action"); New_Line;
        end case;
      exception
        when Stack_Error =>
          Put("Stack_error"); New_Line;
        when Data_Error  =>
          Put("Not a number"); New_Line;
        when End_Error   =>
          Put("Unexpected end of file"); New_Line; exit;
      end;
    end loop;
    Skip_Line;
  end loop;

  Reset( Number_Stack );
end Main;
