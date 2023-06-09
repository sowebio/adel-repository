pragma Source_Reference (000886, "x80_ed.ada");

--[class_display.adb] Implementation
with Ada.Text_Io, Ada.Integer_Text_Io,
     Pack_Constants, Pack_Md_Io, Pack_Ansi_Display; 
use  Pack_Constants, Pack_Md_Io, Pack_Ansi_Display;
package body Class_Display is

  procedure Debug( The:in Display ) is
  begin
    Ada.Text_Io.        Put("Display >");
    Ada.Text_Io.        Put("no_lines     :");
    Ada.Integer_Text_Io.Put(Integer(The.No_Lines),Width=>2);
    Ada.Text_Io.        Put(" current_row :");
    Ada.Integer_Text_Io.Put(Integer(The.Row),Width=>2);
    Ada.Text_Io.        Put(" current_col :");
    Ada.Integer_Text_Io.Put(Integer(The.Col),Width=>2);
    Ada.Text_Io.        Put(" abs_line    :");
    Ada.Integer_Text_Io.Put(Integer(The.Abs_Line),Width=>2);
    Ada.Text_Io.        New_Line;
  end Debug;

  procedure Initialize( The:in out Display ) is
  begin
    The.Row := 1;      The.Col := 1;
    The.Abs_Line := 1; The.No_Lines := 0;
    --O    FOR i IN Display_range LOOP
    --O      initialize( the.display( i ) );    -- Individual lines
    --O    END LOOP;
    Clear(The);
  end Initialize;

  procedure Finalize( The:in out Display ) is
  begin
    null;
  end Finalize;

  procedure Clear( The:in out Display ) is
  begin
    Clear;
  end Clear;

  procedure Init( The:in out Display; S:in Store) is
    Store_Row, Store_Col, Lines : Natural;
  begin
    Deliver_Row_Column( S, Store_Row, Store_Col );
    The.Abs_Line := Store_Row;         -- Shadowing screen from
    The.Row := Store_Row;              --
    The.Col := Store_Col;              -- Position on screen
    Lines := No_Lines(S)-Store_Row+1;  -- Current pos to end
    The.No_Lines := Display_Index(Natural'Min(Lines, Display_Lines));

    for I in 1 .. The.No_Lines loop
      The.Display(I) := Deliver_Line( S, Natural(I) );
    end loop;
  end Init;

  procedure Refresh( The:in out Display ) is
  begin
    Clear( The );
    for I in 1 .. The.No_Lines loop
      Cursor_Position( Natural(I), 1 );
      Display_Line( The, I );
    end loop;
    Cursor_Position( The.Row, The.Col );
  end Refresh;

  procedure Display_Line(The:in out Display; I:in Display_Index) is
  begin
    Start( The.Display(I) );
    while not End_Of_Line( The.Display(I) ) loop
      Put( Get_Char( The.Display(I) ) );
      Next_Ch( The.Display(I) );
    end loop;
    Clear_To_End_Of_Line;
  end Display_Line;

  -- Add the character to the current position in the display

  procedure Add(The:in out Display; B:in Store; Ch:in Character) is
    I : Display_Index;
  begin
    I := Display_Index( The.Row );
    if I > The.No_Lines then      -- Insert on empty line
      The.No_Lines := I;          --  the first time
    end if;
    The.Display(I):=Deliver_Line(B,Natural(I)+The.Abs_Line-1);
    Cursor_Position( Natural(I), 1 );
    Display_Line( The, I );
    The.Col := Deliver_Current_Col( The.Display(I) );

    Cursor_Position( Natural(I), The.Col );
  end Add;

  -- Delete the char at the current position from the display

  procedure Del( The:in out Display; S:in Store ) is
    I : Display_Index;
  begin
    I := Display_Index( The.Row );
    The.Display(I) :=
      Deliver_Line(S, Natural(I)+The.Abs_Line-1 );
    Cursor_Position( Natural(I), 1 );
    Display_Line( The, I );
    Position( The, S );
  end Del;

  -- Position the cursor in the correct position on the display

  procedure Position( The:in out Display; S:in Store ) is
    Row,Column   : Natural;     -- In store
    Change       : Boolean;     -- Change display
    Old_Abs_Line : Natural;     -- Old abs line at top of screen
  begin
    Deliver_Row_Column( S, Row, Column );
    Change := False;
    if Row < The.Abs_Line then                     -- Rack Down
      Old_Abs_Line := The.Abs_Line;
      The.Abs_Line := Row;
      Change := True;
    end if;

    if Row > The.Abs_Line + (Display_Lines-1) then  -- Rack up
      Old_Abs_Line := The.Abs_Line;
      The.Abs_Line := Row - (Display_Lines-1);
      Change := True;
    end if;

    if Change then                            -- change display
      declare
        Remaining_Lines : Natural;
      begin
        Remaining_Lines := No_Lines(S) - The.Abs_Line+1;
        Remaining_Lines := Natural'Min(Remaining_Lines, Display_Lines);
        The.No_Lines := Display_Range( Remaining_Lines );

        for I in 1 .. The.No_Lines loop
          The.Display(I) :=
            Deliver_Line(S, Natural(I) + The.Abs_Line-1 );
        end loop;

        Row := Row - The.Abs_Line + 1;
        Minimal_Refresh( The, Old_Abs_Line );    -- try and do
      end;
    else
      Row := Row - The.Abs_Line + 1;
    end if;

    The.Row := Row; The.Col := Column;
    Cursor_Position( The.Row, The.Col );
  end Position;

  -- Do a minimal refresh of the screen

  procedure Minimal_Refresh( The:in out Display;
      Old_Abs_Line:in Natural ) is
    Diff : Natural;
  begin
    if Ansi_Ins_Del and then
        ( Old_Abs_Line-(Display_Lines-1) <= The.Abs_Line and
        Old_Abs_Line+(Display_Lines-1) >= The.Abs_Line )
        then
      -- Some of the lines on the screen OK
      if The.Abs_Line < Old_Abs_Line then
        -- Rack display down, Insert new lines at top
        Diff := Old_Abs_Line-The.Abs_Line;
        Cursor_Position( 1, 1 );
        Insert_Line( Diff );
        for I in 1 .. Display_Index(Diff) loop
          Cursor_Position( Natural(I), 1 );
          Display_Line( The, I );
        end loop;
      else
        -- Rack display up, Insert new lines at bottom of display
        Diff := The.Abs_Line - Old_Abs_Line;
        Cursor_Position(1,1); Delete_Line( Diff );   -- Rack up
        Cursor_Position( Display_Lines-(Diff)+1, 1);
        for I in The.No_Lines-Display_Index(Diff)+1
            .. The.No_Lines loop
          Cursor_Position( Natural(I), 1 );
          Display_Line( The, I );
        end loop;
        for I in The.No_Lines+1 .. Display_Lines loop
          Cursor_Position( Natural(I), 1 );      -- Clear left
          Clear_To_End_Of_Line;                  -- on screen
        end loop;
      end if;
    else -- No lines on display valid do a total refresh
      Refresh(The);
    end if;
  end Minimal_Refresh;

  procedure Status( The:in out Display; S:in Store ) is
    Row,Column : Natural;
  begin
    Deliver_Row_Column( S, Row, Column );
    Cursor_Position( Lines_On_Screen, 1 ); Clear_To_End_Of_Line;
    Cursor_Position( Lines_On_Screen, 58 );
    Put("Line "); Put( Row ); Put("    ");
    Cursor_Position( Lines_On_Screen, 68 );
    Put("column "); Put( Column ); Put("  ");
    Cursor_Position( The.Row, The.Col);
  end Status;

  function Deliver_Display_Size(The:in Display) return Natural is
  begin
    return Display_Lines;
  end Deliver_Display_Size;

end Class_Display;
