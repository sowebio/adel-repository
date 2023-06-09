pragma Source_Reference (000101, "x80_exp.ada");


with Ada.Text_Io, Ada.Command_Line, Class_Line;
use  Ada.Text_Io, Ada.Command_Line, Class_Line;
procedure Main is
  I_Fd   : Ada.Text_Io.File_Type;        -- File descriptor
  O_Fd   : Ada.Text_Io.File_Type;        -- File descriptor
  A_Line : Class_Line.Line;
begin
  if Argument_Count >= 1 then
    for I in 1 .. Argument_Count loop    -- Repeat for each file
      begin
        Open( File=>I_Fd, Mode=>In_File, -- Open file
          Name=>Argument(I) );
        while not End_Of_File(I_Fd) loop -- For each Line
          Get_Line( A_Line, I_Fd );
          case Status(A_Line) is
            when Text_Line =>            -- Write to file
              Put_Line( A_Line, O_Fd );
            when File_Name =>            -- Get file name
              Get_Fd( A_Line, O_Fd );
            when Unknown =>              -- Ignore
              null;
          end case;
        end loop;
        Close(I_Fd);                     -- Close file
      exception
        when Name_Error =>
          Put("Exp: " & Argument(I) & " no such file" );
          New_Line;
        when Status_Error =>
          Put("Exp: " & Argument(I) & " all ready open" );
          New_Line;
        when others =>
          Put("Exp: " & Argument(I) & " unknow error" );
          New_Line;
      end;
    end loop;
  else
    Put("Usage: Exp file1 ... "); New_Line;
  end if;
end Main;
