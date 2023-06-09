pragma Source_Reference (000099, "x80_ed.ada");

--[class_line.adb] Implementation
with Ada.Text_Io, Ada.Integer_Text_Io;
package body Class_Line is

  procedure Debug( The:in Line ) is
    use  Ada.Text_Io, Ada.Integer_Text_Io;
  begin
    Put("Line    >");
    Put("last ch      :"); Put(Integer(The.Cur_Len),Width=>2);
    Put(" Iter pos    :"); Put(Integer(The.Iter_Pos),Width=>2);
    Put(" Active col  :"); Put(Integer(The.Col), Width=>2);
    New_Line;
  end Debug;

  procedure Initialize( The:in out Line ) is
  begin
    --the := NEW O_Line;               -- Dynamic Store
    Clear(The);
  end Initialize;

  procedure Finalize( The:in out Line ) is
  begin
    null;
  end Finalize;

  procedure Adjust( The:in out Line ) is
  begin
    null;
  end Adjust;

  procedure Clear( The:in out Line ) is
  begin
    The.Iter_Pos := 0;           -- Iterator
    The.Cur_Len := 0;            -- Empty Line
    The.Col := 1;                -- Current position
  end Clear;

  procedure Start( The:in out Line ) is
  begin
    The.Iter_Pos := 1;
  end Start;

  function  End_Of_Line( The:in Line) return Boolean is
  begin
    return The.Iter_Pos > The.Cur_Len;
  end End_Of_Line;

  function  Get_Char( The:in Line ) return Character is
  begin
    return The.Chs( The.Iter_Pos );
  end Get_Char;

  procedure Next_Ch( The:in out Line ) is
  begin
    if The.Iter_Pos <= The.Cur_Len then
      The.Iter_Pos := The.Iter_Pos + 1;
    end if;
  end Next_Ch;

  procedure Add(The:in out Line; Where:in Natural;
      Ch:in Character) is
    Add_At : Line_Index;
  begin
    Add_At := Line_Index( Where );
    if The.Cur_Len < The.Chs'Length and then
        Add_At <= The.Cur_Len+1
        then
      for I in reverse Add_At .. The.Cur_Len loop
        The.Chs(I+1) := The.Chs(I);       -- Make room
      end loop;
      The.Cur_Len := The.Cur_Len + 1;     -- Increase length
      The.Chs( Add_At ) := Ch;            -- Insert character
      if Add_At < The.Chs'Length then     -- New column
        The.Col := Add_At + 1;
      end if;
    end if;
  end Add;

  procedure Del( The:in out Line; Where:in Natural ) is
    Del_At : Line_Index;
  begin
    Del_At := Line_Index( Where );
    if Del_At <= The.Cur_Len then         -- Can delete
      The.Cur_Len := The.Cur_Len-1;       -- New length
      The.Col     := Del_At;              -- New current col
      for I in Del_At .. The.Cur_Len loop
        The.Chs(I) := The.Chs(I+1);       -- Delete ch
      end loop;
    end if;
    if Del_At > The.Cur_Len then          -- New column
      The.Col := Line_Index'Max(The.Cur_Len+1, 1 );
    end if;
  end Del;

  function  Deliver_Current_Col( The:in Line ) return Natural is
  begin
    return Natural( The.Col );            -- Current position
  end Deliver_Current_Col;

  function  Deliver_Cur_Len( The:in Line ) return Natural is
  begin
    return Natural( The.Cur_Len );        -- Chars in line
  end Deliver_Cur_Len;

  function  Deliver_Max_Line_Size( The:in Line ) return Natural is
  begin
    return Max_Chs;                         -- Max size of line
  end Deliver_Max_Line_Size;

end Class_Line;
