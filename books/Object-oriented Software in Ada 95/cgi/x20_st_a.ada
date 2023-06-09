---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:45 PM BST  --
---------------------------------------------------------------------
--[class_stack.ads] Specification
package Class_Stack is
  type Stack is private;                -- Copying allowed
  Stack_Error: exception;               -- When error

  procedure Reset( The:in out Stack);
  procedure Push( The:in out Stack; Item:in Integer );
  procedure Pop(The:in out Stack; Item:out Integer );
private

  Max_Stack: constant := 3;
  type    Stack_Index is range 0 .. Max_Stack;
  subtype Stack_Range is Stack_Index range 1 .. Max_Stack;
  type    Stack_Array is array ( Stack_Range ) of Integer;

  type Stack is record
    Elements: Stack_Array;          -- Array of elements
    Tos     : Stack_Index := 0;     -- Index
  end record;

end Class_Stack;


--[class_stack.adb] Implementation
package body Class_Stack is

  procedure Push( The:in out Stack; Item:in Integer ) is
  begin
    if The.Tos /= Max_Stack then
      The.Tos := The.Tos + 1;               -- Next element
      The.Elements( The.Tos ) := Item;      -- Move in
    else
      raise Stack_Error;                    -- Failed
    end if;
  end Push;

  procedure Pop( The:in out Stack; Item :out Integer ) is
  begin
    if The.Tos > 0 then
      Item := The.Elements( The.Tos );      -- Top element
      The.Tos := The.Tos - 1;               -- Move down
    else
      raise Stack_Error;                    -- Failed
    end if;
  end Pop;

  procedure Reset( The:in out Stack ) is
  begin
    The.Tos := 0;  -- Set TOS to 0 (Non existing element)
  end Reset;

end Class_Stack;

--[main.adb] Procedure
with Ada.Text_Io, Ada.Integer_Text_Io, Class_Stack;
use  Ada.Text_Io, Ada.Integer_Text_Io, Class_Stack;
procedure Main is
  Number_Stack : Stack;              -- Stack of numbers
  Action       : Character;          -- Action
  Number       : Integer;            -- Number processed
begin
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
          Put("Not a number"); New_Line; Skip_Line;
        when End_Error   =>
          Put("Unexpected end of file"); New_Line; exit;
      end;
    end loop;
    Skip_Line;
  end loop;
  Reset( Number_Stack );
end Main;
