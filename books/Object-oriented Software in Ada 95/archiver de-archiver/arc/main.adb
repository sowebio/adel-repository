pragma Source_Reference (000001, "x80_arch.ada");
---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:57 PM BST  --
---------------------------------------------------------------------
--[cat.adb] Procedure
with Ada.Text_Io, Ada.Command_Line;
use  Ada.Text_Io, Ada.Command_Line;
procedure Main is
  Fd  : Ada.Text_Io.File_Type;          -- File descriptor
  Ch  : Character;                      -- Current character
begin
  if Argument_Count >= 1 then
    for I in 1 .. Argument_Count loop   -- Repeat for each file
      begin
        Open( File=>Fd, Mode=>In_File,  -- Open file
          Name=>Argument(I) );
        Put("@@ "); Put( Argument(I) );
        Put(" @@"); New_Line;
        while not End_Of_File(Fd) loop  -- For each Line
          Put("+");                     -- Marker
          while not End_Of_Line(Fd) loop-- For each character
            Get(Fd,Ch); Put(Ch);        -- Read / Write character
          end loop;
          Skip_Line(Fd); New_Line;      -- Next line / new line
        end loop;
        Close(Fd);                      -- Close file
      exception
        when Name_Error =>
          Put("arch: " & Argument(I) & " no such file" );
          New_Line;
        when Status_Error =>
          Put("arch: " & Argument(I) & " all ready open" );
          New_Line;
      end;
    end loop;
  else
    Put("Usage: arch file1 ... "); New_Line;
  end if;
end Main;
