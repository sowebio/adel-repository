---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:46 PM BST  --
---------------------------------------------------------------------
--[class_stack.ads] Specification
generic
  type Stack_Element is private;  -- Can specify any type
  Max_Stack:in Positive := 3;     -- Has to be typed / not const
package Class_Stack is
  type Stack is private;
  Stack_Error: exception;

  procedure Reset( The:in out Stack);
  procedure Push( The:in out Stack; Item:in Stack_Element );
  procedure Pop( The:in out Stack; Item:out Stack_Element );
private

  type    Stack_Index is new Integer range 0 .. Max_Stack;
  subtype Stack_Range is Stack_Index
                      range 1 .. Stack_Index(Max_Stack);
  type    Stack_Array is array ( Stack_Range ) of Stack_Element;

  type Stack is record
    Elements: Stack_Array;          -- Array of elements
    Tos     : Stack_Index := 0;     -- Index
  end record;

end Class_Stack;

--[class_stack.adb] Implementation
package body Class_Stack is

  procedure Push( The:in out Stack; Item:in Stack_Element ) is
  begin
    if The.Tos /= Stack_Index(Max_Stack) then
      The.Tos := The.Tos + 1;               -- Next element
      The.Elements( The.Tos ) := Item;      -- Move in
    else
      raise Stack_Error;                    -- Failed
    end if;
  end Push;

  procedure Pop( The:in out Stack; Item :out Stack_Element ) is
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

--[class_stack_char.ADS] Implementation Instantiation
with Class_Stack;
pragma Elaborate_All( Class_Stack );
package Class_Stack_Char is new Class_Stack(Character,3);

--[class_stack_int.ADS] Implementation Instantiation
with Class_Stack;
pragma Elaborate_All( Class_Stack );
package Class_Stack_Int is new Class_Stack(Integer);

--[class_stack_stack_int.adb] Implementation Instantiation
with Class_Stack, Class_Stack_Int;
package Class_Stack_Stack_Int is
  new Class_Stack(Class_Stack_Int.Stack);

--[main1.adb] Procedure
with Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Class_Stack_Int;
use  Ada.Text_Io, Ada.Integer_Text_Io, Ada.Float_Text_Io, Class_Stack_Int;
procedure Main1 is
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
end Main1;

--[main2.adb] Procedure
with Ada.Text_Io, Ada.Float_Text_Io, Class_Stack_Int, Class_Stack_Stack_Int;
use  Ada.Text_Io, Ada.Float_Text_Io, Class_Stack_Int, Class_Stack_Stack_Int;
procedure Main2 is
  Number_Stack1      : Class_Stack_Int.Stack;
  Number_Stack2      : Class_Stack_Int.Stack;
  Stack_Number_Stack : Class_Stack_Stack_Int.Stack;
  Number             : Integer;
begin
  Push( Number_Stack1, 1 );
  Push( Number_Stack1, 2 );
  Push( Stack_Number_Stack, Number_Stack1 );
  Pop(  Stack_Number_Stack, Number_Stack2 );
  Pop(  Number_Stack2, Number );
  if Number = 2 then Put("ok "); else Put("Fail "); end if;
  Pop(  Number_Stack2, Number );
  if Number = 1 then Put("ok "); else Put("Fail "); end if;
  New_Line;
end Main2;


--[main.adb] Procedure
with Ada.Text_Io, Main1, Main2;
use  Ada.Text_Io;
procedure Main is
begin
  Put("Example 1"); New_Line; Main1;
  Put("Example 2"); New_Line; Main2;
end Main;
