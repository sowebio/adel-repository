pragma Source_Reference (000262, "x80_ed.ada");

--[class_store.adb] Implementation
with Pack_Constants, Ada.Text_Io, Ada.Integer_Text_Io;
use  Pack_Constants;
package body Class_Store is

  procedure Debug( The:in Store ) is
    use Ada.Text_Io, Ada.Integer_Text_Io;
  begin
    Put("Store   >");
    Put("no_lines     :"); Put(Integer(The.No_Lines), Width=>2);
    Put(" active row  :"); Put(Integer(The.Row), Width=>2);
    Put(" active col  :"); Put(Integer(The.Col), Width=>2);
    New_Line;
    Debug( The.Lines( Store_Index( The.Row ) ) );
  end Debug;

  procedure Initialize( The:in out Store ) is
  begin
    --O    FOR i IN Store_range LOOP
    --O      initialize( the.lines(i) );           -- Individual lines
    --O    END LOOP;
    Clear(The);                             -- clear Store
    Set_Last_Line( The, 1 );                -- Set the last line
  end Initialize;

  procedure Finalize( The:in out Store ) is
  begin
    null;
  end Finalize;

  procedure Clear( The:in out Store ) is
  begin
    for I in Store_Range loop
      Clear( The.Lines(I) );      -- Individual lines
    end loop;
    The.No_Lines := 0;            -- Lines stored
    The.Row      := 1;            -- Current Line
    The.Col      := 1;            -- Current char in line
  end Clear;

  procedure Add( The:in out Store; Row:in Natural;
      Column:in Natural; Ch:in Character ) is
    Row_Is : Store_Index := Store_Index( Row );
  begin
    if Row_Is > The.No_Lines and then Row <= Max_Lines then
      The.No_Lines := The.No_Lines + 1;
    end if;
    Add( The.Lines( Row_Is ), Column, Ch );
  end Add;

  procedure Del( The:in out Store; Row:in Natural;
      Column:in Natural ) is
  begin
    Del( The.Lines( Store_Index(Row) ), Column );
  end Del;

  procedure Add( The:in out Store; Ch:in Character ) is
  begin
    Add( The, The.Row, The.Col, Ch );
    The.Col :=
      Deliver_Current_Col( The.Lines(Store_Index(The.Row)));
  end Add;

  procedure Del( The:in out Store ) is
  begin
    Del( The, The.Row, Natural'Max(The.Col-1, 1) );    -- at position
    The.Col :=
      Deliver_Current_Col( The.Lines(Store_Index(The.Row)));
  end Del;

  procedure Left_Right( The:in out Store; Dir:in Character ) is
    Length  : Natural;
  begin
    if Dir = C_Left then                -- move ->
      if The.Col > 1 then               -- Can go left
        The.Col := The.Col-1;
      end if;
    else                                -- move ->
      Length :=
        Deliver_Cur_Len( The.Lines(Store_Index(The.Row)) );
      if The.Col <= Length then         -- Can go right
        The.Col := The.Col+1;
      end if;
    end if;
  end Left_Right;

  procedure Up_Down( The:in out Store; Dir:in Character;
      No_Lines:in Natural ) is
    Length  : Natural;
  begin
    if Dir = C_Up or else Dir = C_Page_Up then
      if Integer(The.Row) - No_Lines >= 1 then
        The.Row := The.Row - No_Lines;
      else
        The.Row := 1;
      end if;
    else
      if Integer(The.Row)+No_Lines<=Integer(The.No_Lines) then
        The.Row := The.Row + No_Lines;
      else
        The.Row := Natural(The.No_Lines);
        if The.Row < Max_Lines and then Dir = C_Down then
          The.Row := The.Row + 1;       -- Expand by 1 line
        end if;
      end if;
    end if;
    Length := Deliver_Cur_Len(The.Lines(Store_Index(The.Row)));
    The.Col := Natural'Max( Natural'Min( Length, The.Col ), 1 );
  end Up_Down;

  function Deliver_Line(The:in Store; Row:in Natural) return Line is
  begin
    return The.Lines( Store_Index(Row) );  -- The whole line
  end  Deliver_Line;

  function  No_Lines( The:in Store ) return Natural is
  begin
    return Natural(The.No_Lines);          -- Lines in buffer
  end No_Lines;

  procedure Set_Position( The:in out Store; Row:in Natural;
      Column:in Natural ) is
  begin
    The.Col  := Column;         -- The new col
    The.Row  := Row;            -- The new row
  end Set_Position;

  procedure Set_Last_Line( The:in out Store; Row:in Natural ) is
  begin
    The.No_Lines := Store_Index(Row);  -- New last line
  end Set_Last_Line;

  procedure Deliver_Row_Column( The:in Store; Row:out Natural;
      Column:out Natural ) is
  begin
    Row    := The.Row;           -- The current row
    Column := The.Col;           -- The current col
  end Deliver_Row_Column;

  function  Deliver_Store_Size( The:in Store ) return Natural is
  begin
    return Max_Lines;            -- Max size of buffer
  end Deliver_Store_Size;

  function  Deliver_Max_Line_Size
      ( The:in Store; Row:in Natural ) return Natural is
  begin
    return Deliver_Max_Line_Size(The.Lines(Store_Index(Row)));
  end Deliver_Max_Line_Size;

end Class_Store;
