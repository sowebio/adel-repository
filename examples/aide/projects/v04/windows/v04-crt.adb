--+------------------+---------------------------------------------------------
--| o   o  ooo    oo | @filename v04-crt.adb
--| o   o o   o  o o | @brief    V04 library, CRT functions
--| o   o o   o o  o | @author   Stephane & Xavier Riviere, Martin Cattoen
--|  o o  o   o ooooo| @date     03/09/04
--|   o    ooo     o | Licence, contributors & copyrights listed in v04-lcc.txt
--+------------------+---------------------------------------------------------

pragma C_Pass_By_Copy (128);  -- For interfacing with Windows

with Ada.Text_Io;
with Ada.Integer_Text_Io;
with Interfaces.C;

package body V04.Crt is

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

--
-- Win32 types
--

type Win32_Bool   is new Interfaces.C.Int;
type Win32_Tchar  is new Interfaces.C.Char;
type Win32_Short  is new Interfaces.Unsigned_16;
type Win32_Dword  is new Interfaces.Unsigned_32;
type Win32_Handle is new Interfaces.Unsigned_32;

--
-- Screen handle
--

Win32_Output_Buffer : Win32_Handle;

--
-- Clear
--

type Win32_Lpdword is access all Win32_Dword;
pragma Convention (C, Win32_Lpdword);

Num_Bytes           : aliased Win32_Dword;
Num_Bytes_Access    : Win32_Lpdword := Num_Bytes'Access;

--
-- Cursor
--

type Win32_Coord is
record
        X : Win32_Short;
        Y : Win32_Short;
end record;
pragma Convention (C, Win32_Coord);

--
-- Color
--

type Nibble is mod 2 ** 4;
for  Nibble'Size use 4;

type Attribute is
     record
        Foreground : Nibble;
        Background : Nibble;
        Reserved   : Interfaces.Unsigned_8 := 0;
     end record;
for  Attribute use
     record
        Foreground at 0 range 0 .. 3;
        Background at 0 range 4 .. 7;
        Reserved   at 1 range 0 .. 7;
     end record;

for Attribute'Size use 16;
pragma Convention (C, Attribute);

Color_Value : constant array (Color_Type) of Nibble := (0,1,2,3,4,5,6,7,9,10,11,12,13,14,15);

Color_Type_Value : constant array (Nibble) of Color_Type := (Black,         -- 0;30
                                                             Blue,          -- 0;34
                                                             Green,         -- 0;32
                                                             Cyan,          -- 0;36
                                                             Red,           -- 0;31
                                                             Magenta,       -- 0;35
                                                             Brown,         -- 0;33
                                                             Gray,          -- 0;37
                                                             Black,         -- 1;30
                                                             Light_Blue,    -- 1;34
                                                             Light_Green,   -- 1;32
                                                             Light_Cyan,    -- 1;36
                                                             Light_Red,     -- 1;31
                                                             Light_Magenta, -- 1;35
                                                             Yellow,        -- 1;33
                                                             White);        -- 1;37

--
-- Imports
--

function Win32_Beep(Frequency, Period : Win32_Dword) return Win32_Bool;
pragma   Import(Stdcall, Win32_Beep, "Beep");

function Win32_Getstdhandle(Nstdhandle : Win32_Dword) return Win32_Handle;
pragma   Import(Stdcall, Win32_Getstdhandle, "GetStdHandle");

function Win32_Setconsolecp(Cp: Win32_Dword) return Win32_Bool;
pragma   Import(Stdcall, Win32_Setconsolecp, "SetConsoleCP");

function Win32_Setconsoleoutputcp (Cp: Win32_Dword) return Win32_Bool;
pragma   Import(Stdcall, Win32_Setconsoleoutputcp, "SetConsoleOutputCP");

function Win32_Getconsolecp return Win32_Dword;
pragma   Import(Stdcall, Win32_Getconsolecp, "GetConsoleCP");

function Win32_Setconsolecursorposition(Console : Win32_Handle;
                                        Pos     : Win32_Coord) return Win32_Bool;
pragma   Import(Stdcall, Win32_Setconsolecursorposition, "SetConsoleCursorPosition");

function Win32_Fillconsoleoutputcharacter(Console : Win32_Handle;
                                          Char    : Win32_Tchar;
                                          Length  : Win32_Dword;
                                          Start   : Win32_Coord;
                                          Written : Win32_Lpdword)  return Win32_Bool;
pragma   Import (Stdcall, Win32_Fillconsoleoutputcharacter, "FillConsoleOutputCharacterA");

function Win32_FillConsoleOutputAttribute (Console : Win32_Handle;
                                           Attrib  : Attribute;
                                           Length  : Win32_Dword;
                                           Start   :  Win32_Coord;
                                           Written : Win32_Lpdword) return Win32_Bool;
pragma Import (Stdcall, Win32_FillConsoleOutputAttribute, "FillConsoleOutputAttribute");

function Win32_Setconsoletextattribute (Console : Win32_Handle; Attr : Attribute) return Win32_Bool;
pragma   Import (StdCall, Win32_Setconsoletextattribute, "SetConsoleTextAttribute");

function GetCh return Integer;
pragma Import (C, GetCh, "_getch");

function KbHit return Integer;
pragma Import (C, KbHit, "_kbhit");

--
-- Constants
--

Win32_Beep_Error           : exception;
Win32_Cursor_Pos_Error     : exception;
Win32_Fill_Screen_Error    : exception;
Win32_Fill_Attribute_Error : exception;
Win32_Invalid_Handle_Error : exception;
Win32_Set_Attribute_Error  : exception;
Win32_Buffer_Info_Error    : exception;
Win32_Codepage_Error       : exception;

Win32_False                : constant Win32_Bool   := 0;
Win32_Screen_Size          : constant Win32_Dword  := 2000;
Win32_Invalid_Handle_Value : constant Win32_Handle := -1;
Win32_Std_Output_Handle    : constant Win32_Dword  := -11;

-------------------------------------------------------------------------------
-- Implementation
-------------------------------------------------------------------------------

procedure Beep ( Frequency : in Integer := 2000;
                 Period : in Integer := 100 ) is

begin

  if Win32_Beep(Win32_Dword(Frequency), Win32_Dword(Period)) = Win32_False then
     raise Win32_Beep_Error;
  end if;

end Beep;

-------------------------------------------------------------------------------
procedure Clear(Back : in Color_Type := Black) is

Home : Win32_Coord := (0, 0);
Attr : Attribute;

begin

   Attr.Foreground := Color_Value(Gray);
   Attr.Background := Color_Value(Back);

   if Win32_FillConsoleOutputAttribute (Console  => Win32_Output_Buffer,
                                        Attrib   => Attr,
                                        Length   => Win32_Screen_Size,
                                        Start    => Home,
                                        Written  => Num_Bytes_Access) = Win32_False then
       raise Win32_Fill_Attribute_Error;
   end if;

   if Win32_FillConsoleOutputCharacter (Console  => Win32_Output_Buffer,
                                        Char     => ' ',
                                        Length   => Win32_Screen_Size,
                                        Start    => Home,
                                        Written  => Num_Bytes_Access) = Win32_False then
      raise Win32_Fill_Screen_Error;
   end if;

   Cursor_Move(1,1);

end Clear;

-------------------------------------------------------------------------------
procedure Codepage_Set(Codepage: Integer:=Latin_1) is

begin

   if Win32_Setconsolecp(Win32_Dword(Codepage)) = Win32_False then
      raise Win32_Codepage_Error;
   end if;

   if Win32_Setconsoleoutputcp(Win32_Dword(Codepage)) = Win32_False then
      raise Win32_Codepage_Error;
   end if;

end Codepage_Set;

-------------------------------------------------------------------------------
function Codepage_Get return Integer is

begin

   return Integer(Win32_Getconsolecp);

end Codepage_Get;

-------------------------------------------------------------------------------
procedure New_Line(Lines : Line := 1) is

begin

   Ada.Text_Io.New_Line(Ada.Text_Io.Count(Lines));

end New_Line;

-------------------------------------------------------------------------------
procedure Cursor_Move(Row : Line; Col : Column) is

New_Pos : Win32_Coord;

begin
   if (Row > 0) and (Col > 0) then

      New_Pos.Y := Win32_Short(Row) - 1;
      New_Pos.X := Win32_Short(Col) - 1;

      if Win32_Setconsolecursorposition (Console => Win32_Output_Buffer,
                                         Pos => New_Pos) = Win32_False then
         raise Win32_Cursor_Pos_Error;
      end if;

   end if;

end Cursor_Move;

-------------------------------------------------------------------------------
procedure Color_Set(Fore : in Color_Type := Gray ;
                    Back : in Color_Type := Black) is

Attr : Attribute;

begin

   Attr.Foreground := Color_Value(Fore);
   Attr.Background := Color_Value(Back);

   if Win32_Setconsoletextattribute (Win32_Output_Buffer, Attr) = Win32_False then
      raise Win32_Set_Attribute_Error;
   end if;

end Color_Set;

-------------------------------------------------------------------------------
procedure put(text : string ;
              fore : in color_type := gray ;
              back : in color_type := Black;
              Period : in Duration  := 0.0) is
begin
   color_set(fore, back);


   if Period = 0.0 then

      Ada.Text_Io.Put(Text);

   else

      for I in 1..Text'length loop

         Ada.Text_Io.Put ( Text (I) );
         delay Period;

      end loop;

   end if;

   color_set(gray, black);
end put;

-------------------------------------------------------------------------------
procedure Put(Number : Integer ;
              Fore   : in Color_Type := Gray ;
              Back   : in Color_Type := Black) is
begin

   Color_Set(Fore, Back);
   Ada.Integer_Text_Io.Put(Number);
   Color_Set(Gray, Black);

end Put;

-------------------------------------------------------------------------------
procedure Put_Line(Text : String ;
                   Fore : in Color_Type := Gray ;
                   Back : in Color_Type := Black) is
begin

   Color_Set(Fore, Back);
   Ada.Text_Io.Put(Text);
   Color_Set(Gray, Black);
   New_Line;

end Put_Line;

-------------------------------------------------------------------------------
procedure Put_Line(Number : Integer ;
                   Fore   : in Color_Type := Gray ;
                   Back : in Color_Type := Black) is
begin

   Color_Set(Fore, Back);
   Ada.Integer_Text_Io.Put(Number);
   New_Line;

end Put_Line;

-------------------------------------------------------------------------------
procedure Put_Lc(Row  : Line := 1 ;
                 Col  : Column := 1 ;
                 Text : String ;
                 Fore : in Color_Type := Gray ;
                 Back : in Color_Type := Black ;
                 Period : in Duration := 0.0 ) is
begin

   Color_Set(Fore, Back);

   if Period = 0.0 then

      Cursor_Move(Row, Col);
      Ada.Text_Io.Put(Text);

   else

      for I in 1..Text'Length loop

         Cursor_Move(Row, (Col + I) - 1 );
         Ada.Text_Io.Put ( Text (I) );
         delay Period;

      end loop;

   end if;

   Color_Set(Gray, Black);

end Put_Lc;

-------------------------------------------------------------------------------
procedure Put_Lc(Row    : Line := 1 ;
                 Col    : Column := 1 ;
                 Number : Integer ;
                 Fore : in Color_Type := Gray ;
                 Back : in Color_Type := Black) is

begin

   Color_Set(Fore, Back);
   Cursor_Move(Row, Col);
   Ada.Integer_Text_Io.Put(Number);
   Color_Set(Gray, Black);

end Put_Lc;

-------------------------------------------------------------------------------
function Key_Read return Integer is

Key : Integer;

begin

   --- Initial read
   Key := GetCh;
   if Key = 16#00E0# then
      Key := 0;
   end if;

   if Key = Crt.K_Null then

      --- Extended read
      Key := GetCh;
      if Key = 16#00E0# then
        Key := 0;
      end if;

      if Key /= Crt.K_Null then
         Key := Key + 256;
      end if;

   end if;

   return(Key);

end Key_Read;

-------------------------------------------------------------------------------
function Key_Available return Boolean is
begin
   if KbHit = 0 then
      return False;
   else
      return True;
   end if;
end Key_Available;

-------------------------------------------------------------------------------
-- Initialization
-------------------------------------------------------------------------------

begin

  Win32_Output_Buffer := Win32_Getstdhandle(Win32_Std_Output_Handle);

  if Win32_Output_Buffer = Win32_Invalid_Handle_Value then
     raise Win32_Invalid_Handle_Error;
  end if;

-------------------------------------------------------------------------------
end V04.Crt;
-------------------------------------------------------------------------------
