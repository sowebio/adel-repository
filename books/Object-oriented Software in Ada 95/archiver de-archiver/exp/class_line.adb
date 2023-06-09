pragma Source_Reference (000029, "x80_exp.ada");

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
