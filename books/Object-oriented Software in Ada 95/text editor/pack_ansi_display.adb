pragma Source_Reference (000722, "x80_ed.ada");

--[pack_ansi_display.adb] Implementation
with Pack_Md_Io;
use  Pack_Md_Io;
package body Pack_Ansi_Display is
  Prefix: constant String := Character'Val(27) & "[";

  procedure Clear is                      -- Clear screen
  begin
    Put( Prefix & "2J");
  end Clear;

  procedure Down( N:in Natural ) is      -- Cursor Down
  begin
    Put( Prefix ); Put( N );Put("B");
  end Down;

  procedure Up( N:in Natural ) is        -- Cursor Up
  begin
    Put( Prefix ); Put( N );Put("A");
  end Up;

  procedure Left( N:in Natural ) is      -- Cursor Left
  begin
    Put( Prefix ); Put( N );Put("D");
  end Left;

  procedure Right( N:in Natural ) is      -- Cursor Right
  begin
    Put( Prefix ); Put( N );Put("C");
  end Right;

  procedure Insert_Line( N:in Natural ) is
  begin
    Put( Prefix ); Put( N ); Put("L");
  end Insert_Line;

  procedure Delete_Line( N:in Natural ) is
  begin
    Put( Prefix ); Put( N ); Put("M");
  end Delete_Line;

  procedure Cursor_Position(Row:in Natural; Column:in Natural) is
  begin
    Put( Prefix ); Put(Row); Put(";"); Put(Column); Put("H");
  end Cursor_Position;

  procedure Clear_To_End_Of_Line is    -- Clear to end of line
  begin
    Put( Prefix & "K");
  end Clear_To_End_Of_Line;

  procedure Put( N:in Natural ) is     -- Write decimal number
  begin
    if N >= 10 then Put( N / 10 ); end if;
    Put( Character'Val(N rem 10 + Character'Pos('0') ) );
  end Put;

end Pack_Ansi_Display;
