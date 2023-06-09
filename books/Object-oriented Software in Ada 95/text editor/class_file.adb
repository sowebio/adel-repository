pragma Source_Reference (001125, "x80_ed.ada");

--[class_file.adb] Implementation
with Text_Io, Class_Line;
use  Text_Io, Class_Line;
package body Class_File is

  procedure Initialize( The:in out File ) is
  begin
    The.State       := Not_Active;    -- Not active
    The.File_Length := 0;             -- No file registered
    The.File        := (others=>' '); -- Blank file name
  end Initialize;

  procedure Finalize( The:in out File ) is
  begin
    if The.State = Active then
      null;                      -- Dilemma should do something
    end if;
  end Finalize;

  procedure Register( The:in out File; Str:in String ) is
  begin
    The.File( 1 .. Str'Length ) := Str;
    The.File_Length             := Str'Length;
    The.State                   := Active;
  end Register;

  function  Is_Active( The:in File ) return Boolean is
  begin
    return The.State = Active;
  end Is_Active;

  procedure Set_Active( The:in out File ) is
  begin
    The.State := Active;
  end Set_Active;

  procedure Set_Not_Active( The:in out File ) is
  begin
    The.State := Not_Active;
  end Set_Not_Active;

  procedure Read( The:in out File; S:in out Store ) is
    File_In : Text_Io.File_Type;   -- File handle
    Row     : Natural;             -- Processing row
    Max_Size: Natural;             -- Max lines in store
  begin
    Open( File => File_In, Mode => In_File,
      Name => The.File(1 .. Integer(The.File_Length)));
    Row := 1; Max_Size := Deliver_Store_Size(S);
    while not End_Of_File( File_In ) and Row <= Max_Size loop
      Set_Position( S, Row, 1 );
      declare
        Ch       : Character;    -- Character read from file
        Col      : Natural;      -- Current col position in line
        Line_Size: Natural;      -- Maximum line size
      begin
        Col := 1; Line_Size := Deliver_Max_Line_Size( S, Row );
        while not End_Of_Line( File_In ) loop
          Get( File_In, Ch );
          if Col <= Line_Size then
            Add( S, Ch ); Col := Col + 1;
          end if;
        end loop;
      end;
      Skip_Line( File_In ); Row := Row + 1;
    end loop;
    Close( File_In );
    Set_Position( S, 1, 1 ); Set_Last_Line( S, Row-1 );
    The.Lines_In_File := Row-1;
    The.State := Not_Active;              -- Not changed
  end Read;

  procedure Write( The:in out File; S:in out Store;
      U:in User ) is
    File_Out : Text_Io.File_Type;   -- File handle
    Row      : Natural := 1;
  begin
    if The.State = Active then
      if The.File(1) = ' ' then
        Register( The, Dialog( U, "File name: ") );
      end if;
      Create( File=> File_Out, Mode => Out_File,
        Name => The.File(1..Integer(The.File_Length)));
      Row := 1;
      while Row <= No_Lines(S) loop
        declare
          L  : Class_Line.Line;             -- Line to output
          Ch : Character;                  -- current character
        begin
          L := Deliver_Line( S, Row ); Start(L);
          while not End_Of_Line(L) loop
            Ch := Get_Char( L ); Put( File_Out, Ch );
            Next_Ch( L );
          end loop;
        end;
        New_Line( File_Out ); Row := Row + 1;
      end loop;
      Close( File_Out );
      The.State := Not_Active;
      -- ELSE
      --   RAISE Name_error;
    end if;
  end Write;

end Class_File;
