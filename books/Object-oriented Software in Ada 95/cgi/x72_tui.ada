---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:54 PM BST  --
---------------------------------------------------------------------
-- The TUI
--   To compile using the GNAT Compiler on Linux, Windows 95/NT
--
--   gnatchop /w x72_tui.ada         // Split into units
--   gcc /c io.c                     // Compile C interface
--   gnatmake main.adb /largs io.o   // Compile program
--
-- Notes:
--   The file io.c is the commented out C code before Raw_io.adb
--   The current I/O system with Windows 95 can not cope with 
--     the fast reception of characters
--   ^C Will Kill the program use ^E to exit
--   ^D Will cause the program to terminate do not use

package Pack_Constants is
  Vdt_Max_X    : constant := 79;      -- Columns on VDT
  Vdt_Max_Y    : constant := 25;      -- Lines on VDT
  Window_Max_X : constant := 79;      -- MAX columns window
  Window_Max_Y : constant := 25;      -- MAX lines window

  C_Cursor     : constant Character := '*';
  C_Blank      : constant Character := ' ';
  C_Win_A      : constant Character := '#';
  C_Win_Pas    : constant Character := '+';
  C_Exit       : constant Character := Character'Val(05); --^E
  C_Where      : constant Character := Character'Val(255);
  C_Action     : constant Character := Character'Val(13); --cr
  C_Switch     : constant Character := Character'Val(09); --ht
  C_Menu       : constant Character := Character'Val(01); --^A
  C_Del        : constant Character := Character'Val(08); --^B
  C_No_Char    : constant Character := Character'Val(00);

  C_Left       : constant Character := Character'Val(12); --^L
  C_Right      : constant Character := Character'Val(18); --^R
  C_Up         : constant Character := Character'Val(21); --^U
  C_Down       : constant Character := Character'Val(04); --^D
end Pack_Constants;

-- All I/O is RAW
--     Write :chs are immediately written to the terminal
--     Read  :chs are immediately available to the program
--           Unfortunately input characters are echoed using
--           get_immediate in Ada.Text_IO

package Raw_Io is
  procedure Get_Immediate( Ch:out Character );
  procedure Put( Ch:in Character );
  procedure Put( Str:in String );
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


-- Alternative BODY for Raw_io
--   All the I/O is performed by code written in C
--   Implemented in io.c
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

with Interfaces.C, Ada.Text_Io;
use  Interfaces.C, Ada.Text_Io;
package body Raw_Io is

  First_Time : Boolean := True;

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


package Class_Screen is
  procedure Put( Ch :in Character );       -- Put char
  procedure Put( Str:in String );          -- Put string
  procedure Clear_Screen;                  -- Clear screen
  procedure Position_Cursor(Col:in Positive; Row:in Positive);
private
end Class_Screen;

--[pack_ansi_display.adb] Implementation
with Pack_Md_Io; use  Pack_Md_Io;
package body Class_Screen is
  Prefix: constant String := Character'Val(27) & "[";
  procedure Put( N:in Positive );          -- Write decimal number

  procedure Put( Ch :in Character ) is
  begin
    Pack_Md_Io.Put( Ch );
  end Put;

  procedure Put( Str:in String ) is
  begin
    Pack_Md_Io.Put( Str );
  end Put;

  procedure Clear_Screen is                 -- Clear screen
  begin
    Put( Prefix & "2J");
  end Clear_Screen;

  procedure Position_Cursor(Col:in Positive; Row:in Positive) is
  begin
    Put( Prefix ); Put(Row); Put(";"); Put(Col); Put("H");
  end Position_Cursor;

  procedure Put( N:in Positive ) is   -- Write decimal number
  begin
    if N >= 10 then Put( N / 10 ); end if;
    Put( Character'Val(N rem 10 + Character'Pos('0') ) );
  end Put;

end Class_Screen;

--=================================================================

with Ada.Finalization;
use  Ada.Finalization;
package Class_Root_Window is
  type Root_Window   is abstract tagged limited private;
  type P_Root_Window is access all Root_Window'Class;
  type Attribute is ( Top, Bottom, Left, Right, Abs_X, Abs_Y );

  procedure Send_To( The:in out Root_Window;
    Ch:in Character) is abstract;
  procedure Switch_To( The:in out Root_Window ) is abstract;
  procedure Switch_Away( The:in out Root_Window ) is abstract;
  function  About( The:in Root_Window;
    B:in Attribute) return Natural is abstract;
private
  type Root_Window is
    abstract new Limited_Controlled with null record;

end Class_Root_Window;


--===========================================================

with Ada.Finalization;
use  Ada.Finalization;
package Class_Input_Manager is
  type Input_Manager is abstract tagged limited private;
  procedure Window_Prologue;       -- Initialize window system
  procedure Window_Start;          -- Start taking user input
  procedure Window_Epilogue;       -- Clean up
private
  type Input_Manager is
    abstract new Limited_Controlled with null record;
end Class_Input_Manager;

with Ada.Finalization, Class_Root_Window;
use  Ada.Finalization, Class_Root_Window;
package Class_Window_Control is

  type Window_Control is abstract tagged limited private;
  procedure Add_To_List(P_W:in P_Root_Window; Ch:in Character);
  procedure Remove_From_List( P_W:in P_Root_Window );
  procedure Top( P_W:in P_Root_Window );
  procedure Find( P_W:out P_Root_Window; Ch:in Character );

  procedure Send_To_Top( Ch:in Character );
  procedure Switch_To_Top;
  procedure Switch_Away_From_Top;

  procedure Write_To( P_W:in P_Root_Window;
    X,Y:in Positive; Mes:in String );
  procedure Hide_Win( P_W:in P_Root_Window );
  procedure Window_Fatal( Mes:in String );
private
  type Window_Control is
    abstract new Limited_Controlled with null record;
  Max_Items : constant := 10;
  type Active_Window is record           -- Active window
    P_W : P_Root_Window;                 -- Window
    A_Ch: Character;                     -- Activate character
  end record;

  subtype Window_Index is Natural      range 0 .. Max_Items;
  subtype Window_Range is Window_Index range 1 .. Max_Items;
  type    Window_Array is array (Window_Range) of Active_Window;

  The_Last_Win: Window_Index := 0;       -- Last active window
  The_Windows : Window_Array;            -- All windows
end Class_Window_Control;

--===========================================================

with Pack_Constants, Pack_Md_Io, Class_Screen,
  Class_Window_Control, Class_Root_Window;
use  Pack_Constants, Pack_Md_Io, Class_Screen,
  Class_Window_Control, Class_Root_Window;
package body Class_Input_Manager is

  procedure Window_Prologue is
  begin
    Clear_Screen;
  end Window_Prologue;

  procedure Window_Start is
    P_W : P_Root_Window;                   -- A window
    Ch  : Character;                       -- Current Char
  begin
    loop
      Get_Immediate( Ch );                 -- From Keyboard
      exit when Ch = C_Exit;
      Find( P_W, Ch );                     -- Active window
      if P_W /= null then                  -- Window activation
        Switch_Away_From_Top;              --  No longer active
        Top( P_W );                        --  Make p_w top
        Switch_To_Top;                     --  & make active
        Send_To_Top( C_Where );            -- In selected window
      else                                 --
        Send_To_Top( Ch );                 -- Give to top window
      end if;
    end loop;
    Pack_Md_Io.Put( Character'Val(0) );    -- Capture output
  end Window_Start;

  procedure Window_Epilogue is
  begin
    null;
  end Window_Epilogue;

end Class_Input_Manager;

--=================================================================

with Class_Screen;
use  Class_Screen;
package body Class_Window_Control is

  procedure Add_To_List(P_W:in P_Root_Window; Ch:in Character) is
  begin
    if The_Last_Win < Max_Items then
      The_Last_Win := The_Last_Win + 1;
      The_Windows( The_Last_Win ) := ( P_W, Ch );
    else
      Window_Fatal("Cannot register window");
    end if;
  end Add_To_List;

  procedure Remove_From_List( P_W:in P_Root_Window ) is
  begin
    for I in 1 .. The_Last_Win loop                 -- Look at
      if The_Windows( I ).P_W = P_W then            -- Found
        for J in I .. The_Last_Win-1 loop           -- Delete
          The_Windows( J ) := The_Windows( J+1 );   --  move up
        end loop;
        The_Last_Win := The_Last_Win - 1; exit;     -- Finish
      end if;
    end loop;
  end Remove_From_List;

  procedure Top( P_W:in P_Root_Window ) is
  begin
    for I in 1 .. The_Last_Win loop               --
      if The_Windows( I ).P_W = P_W then          -- Found
        declare
          Tmp : Active_Window := The_Windows( I );
        begin
          for J in I .. The_Last_Win-1 loop       -- Move down
            The_Windows( J ) := The_Windows( J+1 );
          end loop;
          The_Windows( The_Last_Win ) := Tmp;     -- New top
        end;
        exit;
      end if;
    end loop;
  end Top;

  procedure Find( P_W:out P_Root_Window; Ch:in Character ) is
  begin
    P_W := null;
    for I in 1 .. The_Last_Win loop
      if The_Windows( I ).A_Ch = Ch then
        P_W := The_Windows( I ).P_W;
        exit;
      end if;
    end loop;
  end Find;

  procedure Send_To_Top( Ch:in Character ) is
  begin
    if The_Last_Win >= 1 then
      Send_To( The_Windows(The_Last_Win).P_W.all, Ch );
    end if;
  end Send_To_Top;

  procedure Switch_To_Top is
  begin
    if The_Last_Win >= 1 then
      Switch_To( The_Windows(The_Last_Win).P_W.all );
    end if;
  end Switch_To_Top;

  procedure Switch_Away_From_Top is
  begin
    if The_Last_Win >= 1 then
      Switch_Away( The_Windows(The_Last_Win).P_W.all );
    end if;
  end Switch_Away_From_Top;

  -- Of course this allow overlapping wondows

  procedure Write_To( P_W:in P_Root_Window;
      X,Y:in Positive; Mes:in String ) is
    Abs_X_Crd : Positive := About( P_W.all, Abs_X );
    Abs_Y_Crd : Positive := About( P_W.all, Abs_Y );
  begin
    Position_Cursor( Abs_X_Crd+X-1, Abs_Y_Crd+Y-1 );
    Class_Screen.Put( Mes );
  end Write_To;

  -- Of course this allow overlapping wondows

  procedure Hide_Win( P_W:in P_Root_Window ) is
    Abs_X_Crd : Positive := About( P_W.all, Abs_X );
    Abs_Y_Crd : Positive := About( P_W.all, Abs_Y );
    Width     : Positive := About( P_W.all, Top );
    Height    : Positive := About( P_W.all, Left );
    Spaces    : String( 1 .. Width ) := ( others => ' ' );
  begin
    for H in 1 .. Height loop
      Position_Cursor( Abs_X_Crd, Abs_Y_Crd+H-1 );
      Class_Screen.Put( Spaces );
    end loop;
  end Hide_Win;

  procedure Window_Fatal( Mes:in String ) is
  begin
    Position_Cursor( 1, 1 );
    Put( "Window fatal error: "& Mes );
  end Window_Fatal;

end Class_Window_Control;

--===========================================================

with Pack_Constants, Class_Root_Window, Class_Window_Control;
use  Pack_Constants, Class_Root_Window, Class_Window_Control;
package Class_Window is
  type Window   is new Root_Window with private;
  type P_Window is access all Window;

  type Mode    is ( Visible, Invisible );
  type P_Cbf   is access function(Str:in String) return String;

  procedure Initialize( The:in out Window );
  procedure Finalize( The:in out Window );

  -- Basic construction

  procedure Framework( The:in out Window;
                       Abs_X_Crd, Abs_Y_Crd: Positive;
                       Max_X_Crd, Max_Y_Crd: Positive;
                       Cb:in P_Cbf := null );
  procedure Create   ( The:in out Window;
                       Abs_X_Crd, Abs_Y_Crd: Positive;
                       Max_X_Crd, Max_Y_Crd: Positive );

  -- Call back function processing

  procedure Set_Call_Back( The:in out Window; Cb:in P_Cbf );
  function Call_Call_Back( The:in Window;
                           Str:in String ) return String;

  -- I/O to a window

  procedure Put( The:in out Window; Mes:in String );
  procedure Put( The:in out Window; Ch:in Character );
  procedure Put( The:in out Window; N:in Integer );

  procedure Position( The:in out Window; X,Y:in Positive );
  procedure Clear( The:in out Window );
  procedure New_Line( The:in out Window );
  procedure Refresh( The:in out Window );

  -- Look and Feel

  procedure Make_Window( The:in out Window; Mo:in Mode );
  procedure Mark_Border( The:in out Window;
                         A_Border:in Attribute;
                         Pos:in Positive; Ch:in Character );
  function About(The:in Window; B:in Attribute) return Natural;

  -- When window selected do

  procedure Switch_Away( The:in out Window );
  procedure Switch_To( The:in out Window );
  procedure Send_To( The:in out Window; Ch:in Character );

  -- Register window with poling system

  procedure Register( P_W:in P_Root_Window; Ch:in Character );
  procedure De_Register( P_W:in P_Root_Window );
private
  subtype Y_Cord is Positive range 1 .. Vdt_Max_Y;
  subtype X_Cord is Positive range 1 .. Vdt_Max_X;

  subtype Line_Index  is X_Cord range 1 .. Window_Max_X;
  subtype Line_Range  is Line_Index;
  subtype Line        is String( Line_Range );

  subtype Pane_Index  is Y_Cord range 1 .. Window_Max_Y;
  subtype Pane_Range  is Pane_Index;
  type    Pane_Array  is array ( Pane_Range ) of Line;

  type Window is new Root_Window with record
    Abs_X    : X_Cord := 1;    -- The position on the vdt
    Abs_Y    : Y_Cord := 1;    -- The position on the vdt
    C_X      : X_Cord := 1;    -- Current position in window
    C_Y      : Y_Cord := 1;    -- Current position in window
    Max_X    : X_Cord := 5;    -- X size of window (+Border)
    Max_Y    : Y_Cord := 5;    -- Y size of window (+Border)
    Pane     : Pane_Array;     -- Copy of window in memory
    Mode_Of  : Mode := Invisible;-- Invisible window by default
    Call_Back: P_Cbf := null;  -- Call back function
  end record;
end Class_Window;

--[class_window.adb] Implementation
package body Class_Window is

  procedure Put( The:in out Window;
    X,Y:in Positive; Mes:in String );

  procedure Initialize( The:in out Window ) is
  begin
    null;
  end Initialize;

  procedure Finalize( The:in out Window ) is
  begin
    Make_Window( The, Invisible );
    De_Register( The'Unchecked_Access );
  end Finalize;

  procedure Create( The:in out Window;
      Abs_X_Crd, Abs_Y_Crd: Positive;
      Max_X_Crd, Max_Y_Crd: Positive ) is
  begin
    if Max_X_Crd < 3 or else Max_X_Crd > Window_Max_X or else
        Max_Y_Crd < 3 or else Max_Y_Crd > Window_Max_Y or else
        Abs_X_Crd + Max_X_Crd - 1 > Vdt_Max_X or else
        Abs_Y_Crd + Max_Y_Crd - 1 > Vdt_Max_Y then
      Window_Fatal("Creation window parameter error");
    end if;
    declare
      Top_Bottom: String(1..Max_X_Crd)     := (others => '-');
      Spaces    : String(2 .. Max_X_Crd-1) := (others => ' ');
    begin
      Top_Bottom(1) := '+'; Top_Bottom(Max_X_Crd) := '+';
      The.Max_X := Max_X_Crd - 2;        -- For border
      The.Max_Y := Max_Y_Crd - 2;        -- For border
      The.Abs_Y := Abs_Y_Crd;            -- Abs position screen
      The.Abs_X := Abs_X_Crd;            --
      The.Pane(1)(1..Max_X_Crd) := Top_Bottom;  -- Clear / set up
      for Y in 2 .. Max_Y_Crd-1 loop
        The.Pane(Y)(1..Max_X_Crd):= '|'&Spaces&'|';
      end loop;
      The.Pane(Max_Y_Crd)(1..Max_X_Crd) := Top_Bottom;
      Position( The, 1, 1 );             -- Top left hand corner
    end;
  end Create;

  -- The window co-ordinates of 1 .. n , 1 .. m are
  --  stored into an array in position 2 .. n+1, 2 .. m+1
  --  this allows the border to be stored

  procedure Framework( The:in out Window;
      Abs_X_Crd, Abs_Y_Crd: Positive;
      Max_X_Crd, Max_Y_Crd: Positive;
      Cb:in P_Cbf := null ) is
  begin
    Create( The, Abs_X_Crd, Abs_Y_Crd, Max_X_Crd, Max_Y_Crd );
    Make_Window( The, Visible );
    if Cb /= null then
      Set_Call_Back( The, Cb );
      Register( The'Unchecked_Access, C_Switch );
    else
      Register( The'Unchecked_Access, C_No_Char );
    end if;
  end Framework;

  procedure Set_Call_Back( The:in out Window; Cb:in P_Cbf ) is
  begin
    The.Call_Back := Cb;
  end Set_Call_Back;

  function Call_Call_Back( The:in Window;
      Str:in String ) return String is
  begin
    if The.Call_Back /= null then
      return The.Call_Back(Str);
    end if;
    return "No call back function";
  end;

  procedure Put( The:in out Window; Mes:in String ) is
    Add : Natural;
  begin
    Add := Mes'Length;                   -- Length
    if Add + The.C_X > The.Max_X then    -- Actual characters
      Add := The.Max_X - The.C_X + 1;    --  to add
    end if;
    if Add >= 1 then                     -- There are some
      The.Pane(The.C_Y+1)(The.C_X+1 .. The.C_X+Add)
        := Mes( 1 .. Add );
      if The.Mode_Of = Visible then      -- Add to screen
        Put(The, The.C_X+1, The.C_Y+1, Mes( 1 .. Add) );
      end if;
      The.C_X := The.C_X + Add;
    else
      Put(The, The.C_X+1, The.C_Y+1, "" );
    end if;
  end Put;

  procedure Put( The:in out Window; Ch:in Character ) is
  begin
    Put( The, "" & Ch );           -- Convert to string
  end Put;

  procedure Put( The:in out Window; N:in Integer ) is
  begin
    Put( The, Integer'Image(N) );  -- Convert to string
  end Put;

  procedure Position( The:in out Window; X,Y:in Positive ) is
  begin
    if X <= The.Max_X and Y <= The.Max_Y then
      The.C_X := X; The.C_Y := Y;
    end if;
  end Position;

  procedure Clear( The:in out Window ) is
    Empty : String( 1 .. The.Max_X ) := (others => ' ');
  begin
    Position(The, 1, 1);            -- Top right hand corner
    for Y in 1 .. The.Max_Y loop    -- Clear text
      Put( The, Empty ); New_Line(The);
    end loop;
  end Clear;

  procedure New_Line( The:in out Window ) is
  begin
    if The.C_Y >= The.Max_Y then         -- Scroll text
      for Y in 2 .. The.Max_Y loop       --  Copy up
        The.Pane(Y) := The.Pane(Y+1);
      end loop;
      The.Pane(The.Max_Y+1)(2..The.Max_X+1):= (others=>' ');
      Refresh(The);                      --  refresh
    else
      The.C_Y := The.C_Y + 1;            -- Next line
    end if;
    The.C_X := 1;                        -- At start
  end New_Line;

  procedure Refresh( The:in out Window ) is
  begin
    if The.Mode_Of = Visible then             -- Visible
      for Y in 1 .. The.Max_Y+2 loop          -- Text
        Put( The, 1, Y,
          The.Pane(Y)(1 .. The.Max_X+2) ); -- include border
      end loop;
      Put( The, "" );                         -- Cursor
    end if;
  end Refresh;

  procedure Make_Window( The:in out Window; Mo:in Mode ) is
  begin
    if The.Mode_Of /= Mo then              -- Change so
      The.Mode_Of := Mo;                   -- Set new mode_of
      case Mo is
        when Invisible =>                  -- Clear from screen
          Hide_Win( The'Unchecked_Access );-- Hide window
        when Visible =>                    -- Redraw on screen
          Refresh( The );
      end case;
    end if;
  end Make_Window;

  procedure Mark_Border( The:in out Window;
      A_Border:in Attribute;
      Pos:in Positive; Ch:in Character ) is
    A_Y, A_X : Positive;
  begin
    case A_Border is
      when Top    => A_X := Pos; A_Y := 1;
      when Bottom => A_X := Pos; A_Y := The.Max_Y+2;
      when Left   => A_X := 1; A_Y := Pos;
      when Right  => A_X := The.Max_X+2; A_Y := Pos;
      when others => null;
    end case;
    if A_X <= The.Max_X+2 and then A_Y <= The.Max_Y+2 then
      The.Pane(A_Y)(A_X) := Ch;       -- Store
      if The.Mode_Of = Visible then   -- Update on screen
        Put( The, A_X, A_Y, Ch & "" );
        Put( The, "" );
      end if;
    end if;
  end Mark_Border;

  function About(The:in Window; B:in Attribute) return Natural is
  begin
    case B is
      when Top  | Bottom => return The.Max_X+2;
      when Left | Right  => return The.Max_Y+2;
      when Abs_X         => return The.Abs_X;
      when Abs_Y         => return The.Abs_Y;
      when others        => return 0;
    end case;
  end;

  procedure Switch_Away( The:in out Window ) is
  begin
    Mark_Border( The, Top, 1, C_Win_Pas );
  end Switch_Away;

  procedure Switch_To( The:in out Window ) is
  begin
    Mark_Border( The, Top, 1, C_Win_A );
  end Switch_To;

  procedure Send_To( The:in out Window; Ch:in Character ) is
  begin
    null;
  end Send_To;

  procedure Register( P_W:in P_Root_Window;
      Ch:in Character ) is
  begin
    Switch_Away_From_Top;           -- Register window focus
    Add_To_List( P_W, Ch );         -- Register window
    Switch_To_Top;                  -- Make focus
  end Register;

  procedure De_Register( P_W:in P_Root_Window ) is
  begin
    Top( P_W );                     -- Make top
    Switch_Away_From_Top;           --  prepare for demise
    Remove_From_List( P_W );        -- De register window
    Switch_To_Top;                  -- Make focus
  end De_Register;

  -- Write to Physical Screen

  procedure Put( The:in out Window;
      X,Y:in Positive; Mes:in String ) is
  begin
    Write_To( The'Unchecked_Access, X, Y, Mes );
  end Put;

end Class_Window;


--==================================================================

with Pack_Constants, Class_Root_Window, Class_Window;
use  Pack_Constants, Class_Root_Window, Class_Window;
package Class_Dialog is
  type Dialog is new Window with private;

  procedure Framework ( The:in out Dialog;
    Abs_X, Abs_Y:in Positive;
    Max_X: in Positive;
    Name:in String; Cb:in P_Cbf );

  procedure Send_To( The:in out Dialog; Ch:in Character );
private
  subtype Message is String( 1 ..  Window_Max_X );
  type Dialog is new Window with record
    Dialog_Pos: Positive := 1;  -- Position in input message
    Dialog_Len: Positive := 1;  -- Length of dialogue message
    Dialog_Mes: Message := ( others => ' '); -- Input message
  end record;
end Class_Dialog;

package body Class_Dialog is

  procedure Framework( The:in out Dialog;
      Abs_X, Abs_Y:in Positive;
      Max_X:in Positive;
      Name:in String; Cb:in P_Cbf ) is
    Dashes : String( 1 .. Max_X ) := (others=>'-');
  begin
    Create( The, Abs_X, Abs_Y, Max_X, 5 );
    The.Dialog_Len := Max_X-2;                  -- User input
    The.Dialog_Pos := 1;                        -- In Dialog
    Set_Call_Back( The, Cb );                   -- Call back fun
    Put( The, "Dialog| " ); Put( The, Name );   -- Dialog title
    Position( The, 1, 2 ); Put( The, Dashes );  -- Line
    Position( The, 1, 3 ); Put( The, C_Cursor );-- Cursor
    Make_Window( The, Visible );
    Register( The'Unchecked_Access, C_Switch ); -- Activation Chr
  end Framework;

  procedure Send_To( The:in out Dialog; Ch:in Character ) is
    Spaces : String(1 .. About(Window(The),Top)) := (others => ' ');
    Res    : String(1..0);
  begin
    case Ch is
      when C_Where =>
        Put( The, "" );
      when C_Action =>
        Res := Call_Call_Back( The,
          The.Dialog_Mes(1..The.Dialog_Pos-1) )(1..0);
        The.Dialog_Pos := 1;
        The.Dialog_Mes := ( others => ' ' );
        Position( The, 1, 3 );                   -- Start
        Put( The, C_Cursor & Spaces );           -- Clear
        Position( The, 2, 3 );                   -- Cursor
        Put( The, "" );                          -- Cursor
      when C_Del =>
        if The.Dialog_Pos > 1 then               -- Can delete
          The.Dialog_Pos := The.Dialog_Pos - 1;  -- Make avail.
          The.Dialog_Mes(The.Dialog_Pos):= ' ';  -- Remove
          Position( The, The.Dialog_Pos, 3 );
          Put( The, C_Cursor & " " );            -- Overwrite
          Position( The, The.Dialog_Pos, 3 );
          Put( The, "" );                        -- Cursor
        end if;
      when others =>
        if The.Dialog_Pos <= The.Dialog_Len then
          if Ch in ' ' .. '~' then               -- Add to
            The.Dialog_Mes( The.Dialog_Pos ) := Ch; -- Save ch
            Position( The, The.Dialog_Pos, 3 );
            Put( The, The.Dialog_Mes(The.Dialog_Pos) );
            Put( The, C_Cursor );
            The.Dialog_Pos := The.Dialog_Pos + 1;
          end if;
        end if;
    end case;
  end Send_To;
end Class_Dialog;

--==================================================================

with Class_Root_Window, Class_Window;
use  Class_Root_Window, Class_Window;
package Class_Menu is
  type Menu is new Window with private;
  type P_Menu is access all Menu;

  procedure Framework( The:in out Menu'Class;
    M1:in String:=""; W1:in P_Menu:=null; Cb1:in P_Cbf:=null;
    M2:in String:=""; W2:in P_Menu:=null; Cb2:in P_Cbf:=null;
    M3:in String:=""; W3:in P_Menu:=null; Cb3:in P_Cbf:=null;
    M4:in String:=""; W4:in P_Menu:=null; Cb4:in P_Cbf:=null;
    M5:in String:=""; W5:in P_Menu:=null; Cb5:in P_Cbf:=null;
    M6:in String:=""; W6:in P_Menu:=null; Cb6:in P_Cbf:=null );

  procedure Set_Up( The:in out Menu; Active:in Positive);
  procedure Menu_Spot( The:in out Menu; Ch:in Character );
  procedure Send_To( The:in out Menu; Ch:in Character );

  Max_Menu : constant Positive := 10;
  subtype Menu_Item is String( 1 .. Max_Menu );

  procedure Get_Menu_Name( The:in Menu; I:in Positive;
    N:out Menu_Item );
  procedure Get_Cur_Selected_Details( The:in P_Menu;
    W:out P_Menu; Cb:out P_Cbf );
private
  type    Direction is (D_Reverse, D_Forward);
  procedure Next( The:in out Menu; Dir:in Direction );

  type Menu_Desc is record  -- A menu is:
    Name: Menu_Item;        -- Name of menu item
    P_M : P_Menu;           -- Menu window
    Fun : P_Cbf;            -- Call back function
  end record;

  Max_Menu_Items : constant := 6;    -- Maximum menu items

  type    Menus_Index is range 0 .. Max_Menu_Items;
  subtype Menus_Range is Menus_Index range 1 .. Max_Menu_Items;
  type    Menus       is array ( Menus_Range ) of Menu_Desc;

  type Menu is new Window with record
    Number   : Menus_Index := 0;   -- Number of menu items
    Cur_Men  : Menus_Index := 1;   -- Currently selected item
    Menu_Set : Menus;              -- Components of a menu
  end record;
end Class_Menu;

with Pack_Constants;
use  Pack_Constants;
package body Class_Menu is

  -- The type is Menu'Class so a run time dispatch will
  -- take place when set_up is called

  procedure Framework( The:in out Menu'Class;
      M1:in String:=""; W1:in P_Menu:=null; Cb1:in P_Cbf:=null;
      M2:in String:=""; W2:in P_Menu:=null; Cb2:in P_Cbf:=null;
      M3:in String:=""; W3:in P_Menu:=null; Cb3:in P_Cbf:=null;
      M4:in String:=""; W4:in P_Menu:=null; Cb4:in P_Cbf:=null;
      M5:in String:=""; W5:in P_Menu:=null; Cb5:in P_Cbf:=null;
      M6:in String:=""; W6:in P_Menu:=null; Cb6:in P_Cbf:=null
                                                               ) is
    Spaces : Menu_Item := ( others => ' ' );
    Active : Menus_Index := 1;
    procedure Set_Up( Mi:in String; Wi:in P_Menu;
                      Cb:in P_Cbf; N:in Menus_Index ) is
    begin
      if Mi /= "" then Active := N; end if;   -- A menu item
      The.Menu_Set( N ) :=
        (" "&Mi&Spaces(1 .. Max_Menu-1-Mi'Length), Wi, Cb);
    end Set_Up;
  begin
    Set_Up( M1, W1, Cb1, 1 ); Set_Up( M2, W2, Cb2, 2 );
    Set_Up( M3, W3, Cb3, 3 ); Set_Up( M4, W4, Cb4, 4 );
    Set_Up( M5, W5, Cb5, 5 ); Set_Up( M6, W6, Cb6, 6 );
    The.Number := Active;
    Set_Up( The, Positive(Active) );
  end Framework;

  procedure Set_Up( The:in out Menu;
                    Active:in Positive ) is
    Me: Menu_Item;
  begin
    Create( The, 1, 1, (1+Max_Menu)*Active+1, 3 );
    for I in 1 .. Active loop            -- Display menu names
      Get_Menu_Name( The, I, Me );
      Put( The, Me ); Put( The, "|" );
      null;
    end loop;
    Menu_Spot( The, C_Cursor );          -- Mark current
  end Set_Up;

  procedure Menu_Spot( The:in out Menu; Ch:in Character ) is
  begin
    Position( The, (Max_Menu+1)*(Positive(The.Cur_Men)-1)+1, 1 );
    Put( The, Ch );
  end Menu_Spot;

  procedure Send_To( The:in out Menu; Ch:in Character ) is
  begin
    Menu_Spot( The, C_Blank );
    case Ch is
      when C_Right => Next( The, D_Forward );
      when C_Left  => Next( The, D_Reverse );
      when others  => null;
    end case;
    Menu_Spot( The, C_Cursor );
  end Send_To;

  procedure Next( The:in out Menu; Dir:in Direction ) is
  begin
    case Dir is
      when D_Forward =>
        The.Cur_Men := The.Cur_Men rem The.Number + 1;
      when D_Reverse =>
        if The.Cur_Men = 1
            then The.Cur_Men := The.Number;
        else The.Cur_Men := The.Cur_Men-1;
        end if;
    end case;
  end Next;

  procedure Get_Menu_Name( The:in Menu; I:in Positive;
                           N:out Menu_Item ) is
  begin
    N  := The.Menu_Set( Menus_Index(I) ).Name;
  end Get_Menu_Name;

  procedure Get_Cur_Selected_Details( The:in P_Menu;
      W:out P_Menu; Cb:out P_Cbf ) is
  begin
    W  := The.Menu_Set( The.Cur_Men ).P_M;
    Cb := The.Menu_Set( The.Cur_Men ).Fun;
  end Get_Cur_Selected_Details;

end Class_Menu;

--==================================================================

with Class_Root_Window, Class_Window, Class_Menu;
use  Class_Root_Window, Class_Window, Class_Menu;
package Class_Menu_Title is
  type Menu_Title is new Menu with private;
  type P_Menu_Title is access all Menu_Title;

  procedure Set_Up( The:in out Menu_Title; Active:in Positive );
  procedure Send_To( The:in out Menu_Title; Ch:in Character );
  procedure Switch_Away( The:in out Menu_Title );
private

  Max_Act_Menu : constant := 6;    -- Maximum depth of menus
  type    Act_Index is range 0 .. Max_Act_Menu;
  subtype Act_Range is Act_Index range 1 .. Max_Act_Menu;
  type    Act_Menus is array ( Act_Range ) of P_Menu;

  type Menu_Title is new Menu with record
    Act_Menu  : Act_Menus;        -- Stack of displayed menus
    Menu_Index: Act_Index := 0;   -- Top of menu stack
  end record;
end Class_Menu_Title;

with Pack_Constants;
use  Pack_Constants;
package body Class_Menu_Title is

  procedure Set_Up( The:in out Menu_Title; Active:in Positive ) is
    Me: Menu_Item;
  begin
    Create( The, 1, 1, (1+Max_Menu)*Active+1, 3 ); -- Fixed size
    Make_Window( The, Visible );
    The.Act_Menu( 1 ) := Menu(The)'Unchecked_Access;-- Title menu
    The.Menu_Index := 1;
    for I in 1 .. Active loop                      -- Get menu
      Get_Menu_Name( The, I, Me );                 --  name
      Put( The, Me ); Put( The, "|" );             --  write
    end loop;
    Register( The'Unchecked_Access, C_Menu );      -- Register
    Menu_Spot( The, C_Cursor );                    -- Cursor on
  end Set_Up;

  procedure Send_To( The:in out Menu_Title; Ch:in Character ) is
    Current, Next : P_Menu;
    Proc          : P_Cbf;
    Res           : String( 1..0 );
  begin
    Current := The.Act_Menu( The.Menu_Index );  -- Active menu
    Get_Cur_Selected_Details( Current, Next, Proc );
    case Ch is
      when C_Where =>
        Put( Current.all, "" );
      when C_Action =>
        if Next /= null and The.Menu_Index < Max_Act_Menu then
          Make_Window( Current.all, Invisible );     -- Hide cur.
          The.Menu_Index := The.Menu_Index + 1;      --
          The.Act_Menu( The.Menu_Index ) := Next;    -- New menu
          Make_Window( Next.all, Visible );          -- Reveal
        else
          if Proc /= null then                       -- Call
            Res := Proc("Action")(1 .. 0 );
          end if;
        end if;
      when others =>
        Send_To( Current.all , Ch );  -- Treat as normal menu
    end case;
  end Send_To;

  procedure Switch_Away( The:in out Menu_Title ) is
  begin
    Mark_Border( The, Top, 1, C_Win_Pas ); -- Now inactive
    if The.Menu_Index > 1 then          -- Not top level menu
      Make_Window( The.Act_Menu(The.Menu_Index).all, Invisible );
      The.Menu_Index := 1;
      Make_Window( The.Act_Menu( 1 ).all, Visible ); -- Top level
    end if;
  end Switch_Away;

end Class_Menu_Title;

--==================================================================

with Class_Window;
use  Class_Window;
package Pack_Globals is
  P_W1 : P_Window;    -- Window 1
  P_W2 : P_Window;    -- Window 2
end Pack_Globals;

with Class_Window, Pack_Globals;
use  Class_Window, Pack_Globals;
function Execute_Call_Back(Cb_Mes:in String) return String is
  Win : Window;
begin
  Put( P_W1.all, "Start [" & Cb_Mes & "]" ); New_Line( P_W1.all );
  Framework( Win, 1, 17, 16, 5 );
  for I in 1 .. 10 loop
    Put( Win, I ); Put( Win, " " );
    Put( Win, Cb_Mes ); New_Line( Win );
  end loop;
  Put( P_W1.all, "End   [" & Cb_Mes & "]" ); New_Line( P_W1.all );
  return "";
end Execute_Call_Back;

with Class_Window, Pack_Globals;
use  Class_Window, Pack_Globals;
function Write_Name( Cb_Mes:in String ) return String is
begin
  Put( P_W1.all, "Written by Mike Smith" ); New_Line( P_W1.all );
  Put( P_W1.all, "University of Brighton" ); New_Line( P_W1.all );
  New_Line( P_W2.all ); Put( P_W2.all, "Written by Mike Smith" );
  New_Line( P_W2.all ); Put( P_W2.all, "University of Brighton" );
  return "";
end Write_Name;

with Class_Window, Pack_Globals;
use  Class_Window, Pack_Globals;
function No_Help( Cb_Mes:in String ) return String is
begin
  Put( P_W1.all, " +------------------+" ); New_Line( P_W1.all );
  Put( P_W1.all, " | There is no Help |" ); New_Line( P_W1.all );
  Put( P_W1.all, " +------------------+" ); New_Line( P_W1.all );
  return "";
end No_Help;

with Class_Window, Pack_Globals;
use  Class_Window, Pack_Globals;
function Echo_Mess( Cb_Mes:in String ) return String is
begin
  Clear( P_W1.all );
  Put( P_W1.all, Cb_Mes ); New_Line( P_W1.all );
  return "";
end Echo_Mess;

with Class_Root_Window, Class_Input_Manager, Class_Window,
     Class_Dialog, Class_Menu, Class_Menu_Title, Pack_Globals,
     Execute_Call_Back, Write_Name, No_Help, Echo_Mess;
use  Class_Root_Window, Class_Input_Manager, Class_Window,
     Class_Dialog, Class_Menu, Class_Menu_Title, Pack_Globals;
procedure Main is
begin
  Window_Prologue;
  declare
    E,F            : Dialog;
    W1,W2          : aliased Window;
    M1             : aliased Menu_Title;
    M2,M3,M4,M5    : aliased Menu;


  begin
    Framework( W1,  1,  5, 30, 10 );
    Framework( W2, 70, 20, 10, 6 );

    Framework(M2, "Open",   null,       null,
      "Close",  null,       null,
      "Window", null,       Execute_Call_Back'access,
      "help",   null,       No_Help'access );

    Framework(M3, "up",     null,       null,
      "down",   null,       null,
      "left",   null,       null,
      "right",  null,       null,
      "loop",   M4'Unchecked_Access,  null,
      "About",  null,       Write_Name'access);

    Framework(M4, "Loop",   M3'Unchecked_Access,   null,
      "Help",   null,        No_Help'access,
      "About",  null,        Write_Name'access,
      "About",  null,        Write_Name'access );

    Framework(M5, "Mike",   null,        Write_Name'access,
      "A",      null,        Write_Name'access,
      "Smith",  null,        Write_Name'access,
      "Next",   M4'Unchecked_Access,   null );

    Framework(M1, "PC",    M2'Unchecked_Access,    null,
      "File",  M3'Unchecked_Access,    null,
      "Trans", M5'Unchecked_Access,    null );

    for I in 1 .. 7 loop
      Put(W1,"Mike Smith"); New_Line( W1 );
    end loop;

    for I in 1 .. 20 loop
      for B in Attribute loop
        Mark_Border( W2, B, I, ':' );
      end loop;
    end loop;

    Framework(E, 40, 6 , 20, "Diag 1", Echo_Mess'access );
    Framework(F, 40, 18, 22, "Diag 2", Execute_Call_Back'access );

    P_W1 := W1'Unchecked_Access;
    P_W2 := W2'Unchecked_Access;

    Window_Start;
  end;
  Window_Epilogue;
end Main;
