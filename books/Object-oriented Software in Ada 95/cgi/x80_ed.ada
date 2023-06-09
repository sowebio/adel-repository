---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:57 PM BST  --
---------------------------------------------------------------------
-- The editor
--   To compile using the GNAT Ada 95 Compiler on Windows 95/NT, Linux
-- 
--   gnatchop /w x80_ed.ada          // Split into units
--   gcc /c io.c                     // Compile C interface
--   gnatmake ed.adb /largs io.o     // Compile program
-- 
-- Notes:
--   The file io.c is the commented out C code before Raw_io.adb
--   The current I/O system with Windows 95 can not cope with 
--     the fast reception of characters
--   ^C Will Kill the program use ^E to exit
--   ^D Will cause the program to terminate do not use
-- 
--   In the package Pack_constants
--     ANSI_IN_DEL currently must be set to FALSE for Windows 95/NT
--     as the ANSI escape sequences for Insert and Delete lines are
--     not supported.
--     Using this setting will slow down the refreshing of the screen.


--[pack_constants.ads] Specification
package Pack_Constants is
  Max_Answer     : constant := 80;
  Lines_On_Screen: constant := 25;
  Page_Rack      : constant := Lines_On_Screen/2;

  C_Left         : constant Character := Character'Val(012);
  C_Right        : constant Character := Character'Val(018);
  C_Up           : constant Character := Character'Val(021);
  C_Down         : constant Character := Character'Val(011);

  C_Page_Up      : constant Character := Character'Val(023);
  C_Page_Down    : constant Character := Character'Val(024);

  C_Quit         : constant Character := Character'Val(005);
  C_Debug        : constant Character := Character'Val(020);
  C_Refresh      : constant Character := Character'Val(025);
  C_Del          : constant Character := Character'Val(008);

  C_Open         : constant Character := Character'Val(001);
  C_Close        : constant Character := Character'Val(002);
  C_Set_Fn       : constant Character := Character'Val(006);

  C_Next         : constant Character := Character'Val(007);

  Ansi_Ins_Del   : constant Boolean   := False;
end Pack_Constants;

--====================================================================

--[class_line.ads] Specification
with Ada.Finalization;
use  Ada.Finalization;
package Class_Line is
  type Line is new Controlled with private;

  procedure Debug( The:in Line );
  procedure Initialize( The:in out Line );
  procedure Finalize( The:in out Line );
  procedure Adjust( The:in out Line );
  procedure Clear( The:in out Line );

  procedure Start( The:in out Line );       -- Iterator for line
  function  End_Of_Line( The:in Line) return Boolean;
  function  Get_Char( The:in Line ) return Character;
  procedure Next_Ch( The:in out Line );

  procedure Add(The:in out Line; Where:in Natural; Ch:in Character);
  procedure Del( The:in out Line; Where:in Natural );

  function  Deliver_Current_Col( The:in Line ) return Natural;
  function  Deliver_Cur_Len( The:in Line ) return Natural;
  function  Deliver_Max_Line_Size( The:in Line ) return Natural;

private
  Max_Chs : constant := 79;
  type    Line_Iter_Index is range 0 .. Max_Chs+1;
  subtype Line_Index      is Line_Iter_Index range 0 .. Max_Chs;
  subtype Line_Range      is Line_Iter_Index range 1 .. Max_Chs;
  type    Line_Array      is array ( Line_Range ) of Character;

  --TYPE O_Line IS RECORD
  type Line is new Controlled with record
    Chs        : Line_Array;          -- Line of characters
    Iter_Pos   : Line_Iter_Index := 0;-- Iterator position
    Cur_Len    : Line_Index := 0;     -- Position of last ch
    Col        : Line_Range := 1;     -- Last operation here
  end record;
  --TYPE Line IS ACCESS O_Line;          -- Pointer to a Line

end Class_Line;

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

--====================================================================

--[class_store.ads] Specification
with Class_Line; use Class_Line;
with Ada.Finalization;
use  Ada.Finalization;
package Class_Store is

  type Store is new Limited_Controlled with private;

  procedure Debug( The:in Store );
  procedure Initialize( The:in out Store );
  procedure Finalize( The:in out Store );
  procedure Clear( The:in out Store );

  procedure Add( The:in out Store; Row:in Natural;
    Column:in Natural; Ch:in Character );
  procedure Del( The:in out Store; Row:in Natural;
    Column:in Natural );
  procedure Add( The:in out Store; Ch:in Character );
  procedure Del( The:in out Store );

  procedure Left_Right( The:in out Store; Dir:in Character );
  procedure Up_Down( The:in out Store; Dir:in Character;
    No_Lines:in Natural );

  function  Deliver_Line(The:in Store; Row:in Natural) return Line;

  function  No_Lines( The:in Store ) return Natural;
  procedure Set_Position( The:in out Store; Row:in Natural;
    Column:in Natural );
  procedure Set_Last_Line( The:in out Store; Row:in Natural );
  procedure Deliver_Row_Column( The:in Store; Row:out Natural;
    Column:out Natural );
  function  Deliver_Store_Size( The:in Store ) return Natural;
  function  Deliver_Max_Line_Size
    ( The:in Store; Row:in Natural ) return Natural;

private
  Max_Lines : constant := 120;
  type    Store_Index    is range 0 .. Max_Lines;
  subtype Store_Range    is Store_Index range 1 .. Max_Lines;
  type    Store_As_Array is array ( Store_Index ) of Line;

  type Store is new Limited_Controlled with record
    Lines     : Store_As_Array;    -- Store as array
    No_Lines  : Store_Index := 0;  -- Lines stores
    Row       : Natural := 0;      -- Current row
    Col       : Natural := 0;      -- Current column
  end record;
end Class_Store;

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

-- All I/O is RAW
--     Write :chs are immediately written to the terminal
--     Read  :chs are immediately available to the program
--           Unfortunately input characters are echoed using
--           get_immediate in Ada.Text_IO

package Raw_Io is
  procedure Get_Immediate( Ch:out Character );
  procedure Put( Ch:in Character );
  procedure Put( Str:in String );
private
  First_Time : Boolean := True;
end Raw_Io;

-- BEGIN file io.c
-- /*
--  *   The C function to turn of echoing
--  *
--  *   Works with the GNAT implementation on Linux, Win95
--  *   Note:
--  *    Uses Unix API call to turn of echoing
--  *   Compile: gcc /c io.c
--  */
-- 
-- 
-- #include <termios.h>
-- #include <unistd.h>
-- 
-- void c_no_echo()
-- {
--   static tcflag_t c_lflag;
--   static int fd = STDIN_FILENO;
--   static struct termios termios_data;
--   tcgetattr( fd, &termios_data );
--   c_lflag = termios_data.c_lflag;
--   termios_data.c_lflag = termios_data.c_lflag & (~ECHO);
--   tcsetattr( fd, TCSANOW, &termios_data );
-- }
-- END file io.c


-- =========================================================================
-- 
-- Alternative BODY for Raw_io
--   All the I/O is performed by code written in C
--   Implemented in C code that following this Ada body
-- 

-- WITH Interfaces.C;
-- USE  Interfaces.C;
-- PACKAGE BODY raw_io IS
-- 
-- PROCEDURE get_immediate( ch:OUT Character ) IS
--    FUNCTION c_get_char RETURN Char;
--    PRAGMA import (C, c_get_char, "c_get_char");
-- BEGIN
--    ch := to_ada( c_get_char );
-- END get_immediate;
-- 
-- PROCEDURE put( ch:IN Character ) IS
--    PROCEDURE c_put_char( ch:IN Char );
--    PRAGMA import (C, c_put_char, "c_put_char");
-- BEGIN
--    c_put_char( to_c( ch ) );
-- END put;
-- 
-- PROCEDURE put( str:IN String ) IS
--    PROCEDURE c_put_str( str:IN Char_array );
--    PRAGMA import (C, c_put_str, "c_put_str");
-- BEGIN
--    c_put_str( to_c( str, append_nul=>TRUE ) );
-- END put;
-- 
-- END raw_io;
-- 
-- C Interface for above Ada body
-- 
-- #define UNIX     1       /* Using GNAT on a UNIX based system all IO in C*/
-- #define DOS      2       /* Using GNAT on a DOS based system */
-- #define WIN95    3       /* Using GNAT on a WIN 95/NT system */
-- #define UNIXs    WIN95   /* Using GNAT on a UNIX based system */
-- 
-- /* Notes:
--  *  ENVIRONMENT = UNIX  / Uses Unix API for all I/O
--  *              = DOS   / Uses DOS API (Via DJGPP C) for all I/O
--  *              = GNAT  / Uses Ada95 Input & Output procedures +
--  *                             Unix API call to turn of echoing of input
--  *              = UNIXs / Uses Ada95 Input & Output procedures +
--  *                             Unix API call to turn of echoing of input
--  */
-- 
-- #define ENVIRONMENT	WIN95    /* Environment for program */
-- 
-- 
-- #define ESC	'\033'
-- 
-- 
-- #if ENVIRONMENT == DOS
-- # include <pc.h>
-- # include <keys.h>
-- #endif
-- #include <stdio.h>
-- 
-- typedef enum { false, true } bool;
-- 
-- char c_get_char();
-- void c_put_char( char ch );
-- void c_put_str( char *str );
-- 
-- 
-- #if ENVIRONMENT == DOS
-- /*
--  * Make function keys and arrow keys return two characters
--  * E.G. Right arrow returns (char) 0, 'M'
--  *      Left  arrow         (char) 0, 'K'
--  */
-- 
-- char c_get_char()
-- {
--   int c;
--   static char the_ch;                 /* Remembered character */
--   static bool prev_char = false;      /* There is remembered ch */
--   if ( prev_char ) {
--     prev_char = false; return the_ch;
--   }
--   c = getkey();                        /* Get char no echo */
--   if ( c & 0x100 ) {                   /* Function / Arrow key */
--     prev_char = true; 
--     the_ch = (char) ( c & 0xFF );
--     return (char) 0;                   /* Marker */
--   }
--   return (char) (c & 0xFF);            /* Ordinary character */
-- }
-- #endif
-- 
-- #if ENVIRONMENT == UNIX
-- /*
--  * Set the terminal mode to /echo /icanon on first read
--  * reset when get ^E
--  *
--  */
-- 
-- #include <termios.h>
-- #include <unistd.h>
-- 
-- char c_get_char()
-- {
--   static bool first_time = true;
--   static tcflag_t c_lflag;
--   static int fd = STDOUT_FILENO;
--   static struct termios termios_data;
--   char c;
-- 
--   if ( first_time )
--   {
--     tcgetattr( fd, &termios_data );
--     c_lflag = termios_data.c_lflag;
--     termios_data.c_lflag = termios_data.c_lflag & ( ~(ECHO|ICANON) );
--     tcsetattr( fd, TCSANOW, &termios_data );
--     first_time = false;
--   }
--   c = getchar();
--   if ( c == '\005')
--   {
--     termios_data.c_lflag = c_lflag;
--     tcsetattr( fd, TCSANOW, &termios_data );
--   }
--   return (char) (c & 0xFF);            /* Ordinary character */
-- }
-- 
-- #endif
-- 
-- #if ENVIRONMENT == WIN95
-- 
-- /*
--  * Uses the C function c_no_echo to turn of echoing of input
--  *
--  */
-- 
-- #include <termios.h>
-- #include <unistd.h>
-- 
-- void c_no_echo()
-- {
--   static tcflag_t c_lflag;
--   static int fd = STDIN_FILENO;
--   static struct termios termios_data;
--   tcgetattr( fd, &termios_data );
--   c_lflag = termios_data.c_lflag;
--   termios_data.c_lflag = termios_data.c_lflag & (~ECHO);
--   tcsetattr( fd, TCSANOW, &termios_data );
-- }
-- 
-- #endif
-- 
-- #if ENVIRONMENT == UNIX || ENVIRONMENT == DOS
-- 
-- /*
--  * C function to write characters immediately to the terminal
--  */
-- 
-- void c_put_char( char ch )
-- {
--   fputc(ch, stdout); fflush( stdout );  /* Output ch */
-- }
-- 
-- void c_put_str( char *str )
-- {
--   while (*str) fputc(*str++, stdout);  /* Output String */
--   fflush( stdout );                    /* Flush buffer */
-- }
-- 
-- #endif
-- 
-- =========================================================================

with Interfaces.C, Ada.Text_Io;
use  Interfaces.C, Ada.Text_Io;
package body Raw_Io is

  procedure Get_Immediate( Ch:out Character) is
    procedure C_No_Echo;
    pragma Import (C, C_No_Echo, "c_no_echo");   -- Turn off echo
  begin
    if First_Time then
      C_No_Echo; First_Time := False;
    end if;
    Ada.Text_Io.Get_Immediate(Ch);
    if Character'Pos(Ch) = 10 then               -- Real Return ch
      Ch := Character'Val(13);
    end if;
  end Get_Immediate;

  procedure Put( Ch:in Character ) is            -- Raw write
  begin
    Ada.Text_Io.Put( Ch ); Ada.Text_Io.Flush;
  end Put;

  procedure Put( Str:in String ) is              -- Raw write
  begin
    Ada.Text_Io.Put( Str ); Ada.Text_Io.Flush;
  end Put;

end Raw_Io;


-- Machine dependent I/O
-- Currently assume input is from a terminal supporting
-- input of ANSI escape sequences

package Pack_Md_Io is
  procedure Put( Ch :in Character );           -- Put char
  procedure Put( Str:in String );              -- Put string
  procedure Get_Immediate( Ch:out Character ); -- no echo
end Pack_Md_Io;

--[pack_md_io.adb] Implementation
with Raw_Io, Pack_Constants;
use  Raw_Io, Pack_Constants;
package body Pack_Md_Io is
  procedure Put( Ch:in Character ) is
  begin
    Raw_Io.Put( Ch );
  end Put;

  procedure Put( Str:in String ) is
  begin
    Raw_Io.Put( Str );
  end Put;

  procedure Get_Immediate( Ch:out Character) is
    Esc: constant Character := Character'Val(27);
  begin
    Raw_Io.Get_Immediate( Ch );
    if Ch = Esc then                         -- ESC 
      Raw_Io.Get_Immediate( Ch );            -- [
      if Ch = '[' then
        Raw_Io.Get_Immediate( Ch );
        case Ch is
          when 'A'    => Ch := C_Up;         -- A - Up arrow
          when 'B'    => Ch := C_Down;       -- B - Down arrow
          when 'C'    => Ch := C_Right;      -- C - Right arrow
          when 'D'    => Ch := C_Left;       -- D - Left arrow
          when others => Ch := '?';          -- ? - Unknown
        end case;
      end if;
    end if;
  end Get_Immediate;

end Pack_Md_Io;

--====================================================================

--[pack_ansi_display.ads] Specification
package Pack_Ansi_Display is
  procedure Clear;                        -- Clear screen
  procedure Down ( N:in Natural );        -- Cursor Down
  procedure Up   ( N:in Natural );        -- Cursor Up
  procedure Left ( N:in Natural );        -- Cursor Left
  procedure Right( N:in Natural );        -- Cursor Right
  procedure Insert_Line( N:in Natural );  -- Insert Line(s)
  procedure Delete_Line( N:in Natural );  -- Delete Line(s)
  procedure Clear_To_End_Of_Line;         -- Clear to end of line
  procedure Cursor_Position (Row:in Natural; Column:in Natural);
  procedure Put  ( N:in Natural );        -- Write decimal number
private
end Pack_Ansi_Display;

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

--====================================================================

--[class_user.ads] Specification
package Class_User is
  type User is private;
  type Mode is ( No_Echo, Echo );

  function Get_Command( The:in User ) return Character;
  function Dialog(The:in User; Mes:in String) return String;
  function Get_Character(The:in User; M:in Mode)return Character;

private
  type User is record null; end record;

end Class_User;

--[class_user.adb] Implementation
with Pack_Constants, Pack_Md_Io, Pack_Ansi_Display;
use  Pack_Constants, Pack_Md_Io, Pack_Ansi_Display;

package body Class_User is

  function Get_Command( The:in User ) return Character is
    Ch : Character;
  begin
    Get_Immediate( Ch );
    return Ch;
  end Get_Command;

  function  Dialog( The:in User;
      Mes:in String) return String is
    Ch      : Character;
    Reading : Boolean;
    Str     : String( 1 .. Max_Answer );
    Str_Len : Integer := 1;
  begin
    Cursor_Position( Lines_On_Screen, 1 ); Clear_To_End_Of_Line;
    Put( Mes ); Reading := True; Str(1) := ' ';
    for I in Str'range loop
      Ch := Get_Character( The, No_Echo );
      if Ch = Ascii.Cr or else Ch = Ascii.Lf then
        Reading := False;
        exit;
      else
        Put( Ch ); Str(I) := Ch; Str_Len := I;
      end if;
    end loop;
    return Str( 1 .. Str_Len );
  end Dialog;

  function Get_Character( The:in User; M:in Mode ) return Character is
    Ch : Character;
  begin
    Get_Immediate( Ch );
    if M = Echo then Put( Ch ); end if;
    return Ch;
  end Get_Character;

end Class_User;

--====================================================================

--[class_display.ads] Specification
with Ada.Finalization, Pack_Constants, Class_Line, Class_Store;
use  Ada.Finalization, Pack_Constants, Class_Line, Class_Store;
package Class_Display is

  type Mode    is ( No_Echo, Echo );
  type Display is new Limited_Controlled with private;

  procedure Debug( The:in Display );
  procedure Initialize( The:in out Display );
  procedure Finalize( The:in out Display );
  procedure Clear( The:in out Display );

  procedure Init( The:in out Display; S:in Store);
  procedure Refresh( The:in out Display );

  procedure Add( The:in out Display; B:in Store; Ch:in Character );
  procedure Del( The:in out Display; S:in Store );
  procedure Position( The:in out Display; S:in Store );

  procedure Status( The:in out Display; S:in Store );
  function  Deliver_Display_Size( The:in Display ) return Natural;

private
  Display_Lines : constant := Lines_On_Screen-1;
  type    Display_Index is new Integer   range 0..Display_Lines;
  subtype Display_Range is Display_Index range 1..Display_Lines;

  procedure Display_Line( The:in out Display; I:in Display_Index );
  procedure Minimal_Refresh( The:in out Display;
    Old_Abs_Line:in Natural );

  type Display_As_Array is array ( Display_Range ) of Line;
  type Display is new Limited_Controlled with record
    Display       : Display_As_Array;    -- Display
    No_Lines      : Display_Index := 0;  -- Active lines
    Abs_Line      : Natural := 0;        -- of 1st display line
    Row           : Natural := 0;        -- current row
    Col           : Natural := 0;        -- current column
  end record;

end Class_Display;

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

--====================================================================

--[class_file.ads] Specification
with Ada.Finalization, Class_Store, Pack_Constants, Class_User, Class_Display;
use  Ada.Finalization, Class_Store, Pack_Constants, Class_User, Class_Display;
package Class_File is

  type File is new Limited_Controlled with private;

  procedure Initialize( The:in out File );
  procedure Finalize( The:in out File );
  procedure Register( The:in out File; Str:in String );
  function  Is_Active( The:in File ) return Boolean;
  procedure Set_Active( The:in out File );
  procedure Set_Not_Active( The:in out File );
  procedure Read ( The:in out File; S:in out Store );
  procedure Write( The:in out File; S:in out Store; U:in User );

private
  type    File_Index  is range 0 .. Max_Answer;
  subtype File_Range  is File_Index  range 1 .. Max_Answer;
  type    State_File  is ( Active, Not_Active );
  type File is new Limited_Controlled with record
    State         : State_File := Not_Active;
    Lines_In_File : Natural := 0;
    File          : String( 1 .. Max_Answer );
    File_Length   : File_Index := 0;
  end record;
end Class_File;

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

--====================================================================

--[ed.adb] Procedure
with --Ada.Text_Io, Ada.Integer_Text_Io,
     Ada.Io_Exceptions, Class_Store, Class_File, Class_Display, Pack_Constants,
     Pack_Ansi_Display, Class_User;
use  --Ada.Text_Io, Ada.Integer_Text_Io,
     Ada.Io_Exceptions, Class_Store, Class_File, Class_Display, Pack_Constants,
     Pack_Ansi_Display, Class_User;
procedure Ed is
  Max_Open  : constant := 3;           -- Maximum open files
  type Open is range 1 .. Max_Open;

  C       : Open;                      -- Current screen/file
  In_Store: array(Open) of Store;      -- In store copy of file
  On_Disk : array(Open) of File;       -- Disk file access
  Screen  : array(Open) of Display;    -- The Display
  Person  : User;                      -- The editors user


  function Yes( Str:in String ) return Boolean is
  begin
    return ( Str'Length = 1  and then
      (Str = "y" or  else Str = "Y") ) or else
      ( Str'Length = 3  and then
      (Str = "yes" or else Str = "YES") );
  end Yes;

  function No( Str:in String ) return Boolean is
  begin
    return ( Str'Length = 1  and then
      (Str = "n" or  else Str = "N") ) or else
      ( Str'Length = 2  and then
      (Str = "no" or else Str = "NO") );
  end No;

  procedure Open_File( The:in Open ) is    -- Read file into buffer
    Fail : Boolean := False;               -- Result of read
  begin
    loop                                   -- Repeat until read
      begin
        if Is_Active( On_Disk(C) ) and then -- Deleting current
            not Yes(Dialog( Person,
              "Delete this buffer [y/n] : ")) then
          exit;                            -- No
        end if;
        Clear( In_Store(C) );              -- Clear store
        Register(On_Disk(C),
          Dialog(Person, "File name: "));
        Read( On_Disk(C), In_Store(C) );   -- Read file into store
        Init( Screen(C), In_Store(C) );    -- Initialize screen
        Refresh( Screen(C) );              -- Display
        Set_Not_Active( On_Disk(C) );      -- File not active
        exit;
      exception
        when Name_Error => Fail := True;   -- Could not read
        when others     => Fail := True;   -- Anything else
      end;
    end loop;
    if Fail then                           -- If failed to read
      Clear( In_Store(C) );                --  clear changes
    end if;
  end Open_File;                           --

  procedure Close_File( The:in Open ) is   -- Write buffer
  begin
    loop
      begin
        if not No(Dialog(Person, "Save file [y/n] : ")) then
          Write( On_Disk(C), In_Store(C),
            Person );                 -- Write file back
          Clear( In_Store(C) );            -- clear data
          Init( Screen(C), In_Store(C) );  -- Set to empty
          Position(Screen(C), In_Store(C));-- set to start position
          Refresh( Screen(C) );            -- Blank screen
          Set_Not_Active( On_Disk(C) );    -- Now non active
        end if;
        exit;
      exception
        when Name_Error =>
          Register( On_Disk(C), " " );      -- Could not write
        when others     =>
          Register( On_Disk(C), " " );      -- Could not write
      end;
    end loop;
  end Close_File;

  procedure Commands is
  begin
--    New_Line;
--    Put(" Left      ^L         Right       ^R"); New_Line;
--    Put(" Up        ^U         Down        ^K"); New_Line;
--    Put(" Page Up   ^W         Page Down   ^X"); New_Line;
--    Put(" Quit      ^E         Debug info  ^T"); New_Line;
--    Put(" Refresh   ^Y         Del         ^H"); New_Line;
--    Put(" Opem file ^A         Close File  ^B"); New_Line;
--    Put(" Set file  ^F         Next Buffer ^G"); New_Line;
    null;
  end Commands;


  procedure Process_Command(Action:in Character) is
  begin
    case Action is
      when C_Open   => Open_File(C);       -- read file
      when C_Close  => Close_File(C);      -- write
      when C_Set_Fn =>
        Register(On_Disk(C),
          Dialog( Person, "Set file name: "));
      when C_Next   =>                     -- next screen
        C := C rem Max_Open + 1;
        Refresh( Screen(C) );
      when C_Left | C_Right =>             -- Move   -> <-
        Left_Right( In_Store(C), Action );
        Position( Screen(C), In_Store(C) );
      when C_Up | C_Down =>                -- Move   up down
        Up_Down( In_Store(C), Action, 1 );
        Position( Screen(C), In_Store(C) );
      when C_Page_Up | C_Page_Down =>      -- Move   page up down
        Up_Down(In_Store(C), Action, Page_Rack);
        Position( Screen(C), In_Store(C) );
      when C_Debug =>
        Clear; Debug( In_Store(C) ); Debug( Screen(C) );
        Commands;
      when C_Refresh =>                    -- Refresh screen
        Refresh( Screen(C) );
      when C_Del =>                        -- Delete Character
        Del(In_Store(C));
        Del(Screen(C), In_Store(C) );
        Set_Active( On_Disk(C) );
      when Character'Val(32) .. Character'Val(127) =>
        Add(In_Store(C), Action);
        Add(Screen(C), In_Store(C), Action);
        Set_Active( On_Disk(C) );
      when Ascii.Cr | Ascii.Lf =>          -- Ignore
        null;
      when others =>                       -- Insert ?
        Add(In_Store(C), '?');
        Add(Screen(C), In_Store(C), '?');
        Set_Active( On_Disk(C) );
    end case;
  end Process_Command;

begin                                    -- Editor is
  for C in Open loop                     -- In each current ...
    Init( Screen(C), In_Store(C) );      -- Initialize
  end loop;

  C := 1;                                -- Current screen #1
  Refresh( Screen(C) );                  -- Display cur screen
  Status( Screen(C), In_Store(C) );      -- Display status

  loop                                   -- Main loop
    declare
      Action : Character;
    begin
      Action := Get_Command( Person );   -- Editing command
      exit when Action = C_Quit;         -- Quit
      Process_Command( Action );         -- Do action
      Status( Screen(C), In_Store(C) );  -- Display status
    end;
  end loop;

  for C in Open loop                     -- Write changes
    if Is_Active( On_Disk(C) ) then      -- if needed
      Refresh( Screen(C) );              -- display file
      Close_File(C);                     -- Save file
    end if;
  end loop;

end Ed;
