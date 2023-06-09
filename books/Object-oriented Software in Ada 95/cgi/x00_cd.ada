---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:41 PM BST  --
---------------------------------------------------------------------
--[main.adb] Procedure
with Ada.Text_Io;                  -- With package Text_Io
use  Ada.Text_Io;                  -- Use  components

procedure Main1 is
  Count : Integer;                 -- Declaration of count
begin
  Count := 10;                     -- Set to 10
  while Count > 0 loop             -- loop while greater than 0
    if Count = 3 then              -- If 3 print Ignition
      Put("Ignition"); New_Line;
    end if;
    Put( Integer'Image( Count ) ); -- Print current count
    New_Line;
    Count := Count - 1;            -- Decrement by 1 count
    delay 1.0;                     -- Wait 1 second
  end loop;
  Put("Blast off"); New_Line;      -- Print Blast off

end Main1;

--[main.adb] Procedure
with Ada.Text_Io;
use  Ada.Text_Io;
procedure Main2 is
  type Count_Range is range 0 .. 10;
  Count : Count_Range := 10;           -- Declaration of count
begin
  for Count in reverse Count_Range loop
    if Count = 3 then                  -- If 3 print Ignition
      Put("Ignition"); New_Line;
    end if;
    Put( Count_Range'Image( Count ) ); -- Print current count
    New_Line;
    --DELAY 1.0;                       -- Wait 1 second
  end loop;
  Put("Blast off"); New_Line;          -- Print Blast off

end Main2;

--[main.adb] Procedure
with Ada.Text_Io, Main1, Main2;
use  Ada.Text_Io;
procedure Main is
begin
  Put("Example 1"); New_Line; Main1;
  Put("Example 2"); New_Line; Main2;
end Main;
