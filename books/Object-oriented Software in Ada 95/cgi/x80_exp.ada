---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:59 PM BST  --
---------------------------------------------------------------------
with Ada.Text_Io;
use  Ada.Text_Io;
package Class_Line is
  type Line_Status is ( Text_Line, File_Name, Unknown );
  type Line is private;
  procedure Get_Line( The:in out Line; Fd:in Ada.Text_Io.File_Type );
  procedure Put_Line( The:in Line; Fd:in Ada.Text_Io.File_Type );
  procedure Get_Fd( The:in out Line; Fd:in out Ada.Text_Io.File_Type );
  function  Status( The:in Line ) return Line_Status;
private
  Max_Line : constant := 200;
  subtype Line_Index is Integer range 0 .. Max_Line+1;
  subtype Line_Range is Line_Index range 1 .. Max_Line;
  subtype Line_Array is String( Line_Range );
  type Line is record
    Chs : Line_Array;       -- Characters of line
    Len : Line_Index;       -- Positions used
    Open: Boolean := False; -- Output file open
  end record;
  Name     : Line_Array;    -- File name from file
  Name_Pos : Line_Index;    -- Characters in name
end Class_Line;

with Ada.Text_Io, Ada.Characters.Handling;
use  Ada.Text_Io, Ada.Characters.Handling;
package body Class_Line is
  procedure Get_Line( The:in out Line; Fd:in Ada.Text_Io.File_Type ) is
    Pos : Line_Index := 0;
    Ch  : Character;
  begin
    while not End_Of_Line( Fd ) loop
      Get( Fd, Ch );
      if Pos < Max_Line then
        Pos := Pos + 1;
        The.Chs(Pos) := Ch;
      end if;
    end loop;
    The.Len := Pos;
    Skip_Line( Fd );
  end Get_Line;

  procedure Put_Line( The:in Line; Fd:in Ada.Text_Io.File_Type ) is
  begin
    if The.Open then
      for I in 2 .. The.Len loop
        Put( Fd, The.Chs(I) );
      end loop;
      New_Line( Fd );
    end if;
  end Put_Line;

  function Status( The:in Line ) return Line_Status is
    Pos  : Line_Index := 0;
  begin
    if The.Len >= 1 and then The.Chs(1) = '+' then
      return Text_Line;
    end if;
    if The.Len >= 2 and then The.Chs(1..2) = "@@" then
      for I in 3 .. The.Len-2 loop
        if Is_Upper( The.Chs(I) ) or Is_Lower( The.Chs(I) ) or
            Is_Digit( The.Chs(I) ) or The.Chs(I) = '_' or
            The.Chs(I) = '.' then
          Pos := Pos + 1;
          Name(Pos) := The.Chs(I);
        end if;
      end loop;
      Name_Pos := Pos;
      Put( "Extracting file " & Name(1..Pos) ); New_Line;
      return File_Name;
    end if;
    return Unknown;
  end Status;

  procedure Get_Fd( The:in out Line; Fd:in out Ada.Text_Io.File_Type ) is
  begin
    if The.Open then                    -- Output file open
      Close( Fd ); The.Open := False;
    end if;
    Create( File=>Fd, Mode=>Out_File,   -- Create file
      Name=>Name(1..Name_Pos) );
    The.Open := True;
  exception
    when Name_Error =>
      Put("Exp: Can not create file " & Name(1..Name_Pos) );
      New_Line;
    when Status_Error =>
      Put("Exp: " & Name(1..Name_Pos) & " all ready open" );
      New_Line;
    when others =>
      Put("Exp: " & Name(1..Name_Pos) & " unknown error" );
      New_Line;
  end;

end Class_Line;


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
