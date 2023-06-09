--+------------------+---------------------------------------------------------
--| o   o  ooo    oo | @filename v04-crt.ads
--| o   o o   o  o o | @brief    V04 library, CRT functions
--| o   o o   o o  o | @author   Stephane & Xavier Riviere, Martin Cattoen
--|  o o  o   o ooooo| @date     03/09/04
--|   o    ooo     o | Licence, contributors & copyrights listed in v04-lcc.txt
--+------------------+---------------------------------------------------------

package v04.crt is

Max_Line   : constant Integer := 24;
Max_Column : constant Integer := 79;

Oem        : constant Integer := 850;
Latin_1    : constant Integer := 1252;

--
-- Key codes (Direct)
--

K_Null            : constant Integer := 0;
K_BackSpace       : constant Integer := 8;
K_Tab             : constant Integer := 9;
K_Return          : constant Integer := 13;
K_Escape          : constant Integer := 27;

K_Left            : constant Integer := 331;
K_Right           : constant Integer := 333;
K_Up              : constant Integer := 328;
K_Down            : constant Integer := 336;
K_Home            : constant Integer := 327;
K_End             : constant Integer := 335;
K_PageUp          : constant Integer := 329;
K_PageDown        : constant Integer := 337;
K_Insert          : constant Integer := 338;
K_Delete          : constant Integer := 339;

K_F1              : constant Integer := 315;
K_F2              : constant Integer := 316;
K_F3              : constant Integer := 317;
K_F4              : constant Integer := 318;
K_F5              : constant Integer := 319;
K_F6              : constant Integer := 320;
K_F7              : constant Integer := 321;
K_F8              : constant Integer := 322;
K_F9              : constant Integer := 323;
K_F10             : constant Integer := 324;
K_F11             : constant Integer := 389;
K_F12             : constant Integer := 390;

--
-- Key codes (Shift)
--

K_Shift_F1        : constant Integer := 340;
K_Shift_F2        : constant Integer := 341;
K_Shift_F3        : constant Integer := 342;
K_Shift_F4        : constant Integer := 343;
K_Shift_F5        : constant Integer := 344;
K_Shift_F6        : constant Integer := 345;
K_Shift_F7        : constant Integer := 346;
K_Shift_F8        : constant Integer := 347;
K_Shift_F9        : constant Integer := 348;
K_Shift_F10       : constant Integer := 349;
K_Shift_F11       : constant Integer := 391;
K_Shift_F12       : constant Integer := 392;

--
-- Key codes (Ctrl)
--

K_Ctrl_Left       : constant Integer := 371;
K_Ctrl_Right      : constant Integer := 372;
K_Ctrl_Up         : constant Integer := 397;
K_Ctrl_Down       : constant Integer := 401;
K_Ctrl_Home       : constant Integer := 375;
K_Ctrl_End        : constant Integer := 373;
K_Ctrl_PageUp     : constant Integer := 390;
K_Ctrl_PageDown   : constant Integer := 374;
K_Ctrl_Insert     : constant Integer := 402;
K_Ctrl_Delete     : constant Integer := 403;

K_Ctrl_A          : constant Integer := 1;
K_Ctrl_B          : constant Integer := 2;
K_Ctrl_C          : constant Integer := 3;
K_Ctrl_D          : constant Integer := 4;
K_Ctrl_E          : constant Integer := 5;
K_Ctrl_F          : constant Integer := 6;
K_Ctrl_G          : constant Integer := 7;
K_Ctrl_H          : constant Integer := 8;
K_Ctrl_I          : constant Integer := 9;
K_Ctrl_J          : constant Integer := 10;
K_Ctrl_K          : constant Integer := 11;
K_Ctrl_L          : constant Integer := 12;
K_Ctrl_M          : constant Integer := 13;
K_Ctrl_N          : constant Integer := 14;
K_Ctrl_O          : constant Integer := 15;
K_Ctrl_P          : constant Integer := 16;
K_Ctrl_Q          : constant Integer := 17;
K_Ctrl_R          : constant Integer := 18;
K_Ctrl_S          : constant Integer := 19;
K_Ctrl_T          : constant Integer := 20;
K_Ctrl_U          : constant Integer := 21;
K_Ctrl_V          : constant Integer := 22;
K_Ctrl_W          : constant Integer := 23;
K_Ctrl_X          : constant Integer := 24;
K_Ctrl_Y          : constant Integer := 25;
K_Ctrl_Z          : constant Integer := 26;

K_Ctrl_F1         : constant Integer := 350;
K_Ctrl_F2         : constant Integer := 351;
K_Ctrl_F3         : constant Integer := 352;
K_Ctrl_F4         : constant Integer := 353;
K_Ctrl_F5         : constant Integer := 354;
K_Ctrl_F6         : constant Integer := 355;
K_Ctrl_F7         : constant Integer := 356;
K_Ctrl_F8         : constant Integer := 357;
K_Ctrl_F9         : constant Integer := 358;
K_Ctrl_F10        : constant Integer := 359;
K_Ctrl_F11        : constant Integer := 393;
K_Ctrl_F12        : constant Integer := 394;

--
-- Key codes (alt)
--

K_Alt_F1          : constant Integer := 360;
K_Alt_F2          : constant Integer := 361;
K_Alt_F3          : constant Integer := 362;
K_Alt_F4          : constant Integer := 363;
K_Alt_F5          : constant Integer := 364;
K_Alt_F6          : constant Integer := 365;
K_Alt_F7          : constant Integer := 366;
K_Alt_F8          : constant Integer := 367;
K_Alt_F9          : constant Integer := 368;
K_Alt_F10         : constant Integer := 369;
K_Alt_F11         : constant Integer := 395;
K_Alt_F12         : constant Integer := 396;

K_Alt_Left        : constant Integer := 411;
K_Alt_Right       : constant Integer := 413;
K_Alt_Up          : constant Integer := 408;
K_Alt_Down        : constant Integer := 416;
K_Alt_Home        : constant Integer := 407;
K_Alt_End         : constant Integer := 415;
K_Alt_PageUp      : constant Integer := 409;
K_Alt_PageDown    : constant Integer := 417;
K_Alt_Insert      : constant Integer := 418;
K_Alt_Delete      : constant Integer := 419;

--
-- Key codes (Ctrl Alt)
--

K_Ctrl_Alt_Backspace : constant Integer := 270;

--
-- Types
--

type Color_Type is (Black, Blue, Green, Cyan, Red, Magenta, Brown, Gray,
                    Light_Blue, Light_Green, Light_Cyan, Light_Red,
                    Light_Magenta, Yellow, White);

subtype Line    is Integer range 1..Max_Line;
subtype Column  is Integer range 1..Max_Column;

-------------------------------------------------------------------------------
-- Specifications
-------------------------------------------------------------------------------

--
-- Basic functions
--

-- @i{Description} : Send a beep.
-- @i{Frequency} : Tone frequency (in Hertz)
-- @i{Period} : Tone duration (in millisecond)
procedure Beep ( Frequency : in Integer := 2000;
                 Period : in Integer := 100);

-- @i{Description} : Clear the screen.
-- @i{Back} : Background color.
procedure Clear(Back : in color_type := black);

-- @i{Description} : Send a new line.
-- @i{Lines} : Number of lines.
procedure New_Line(Lines : Line := 1);

-- @i{Description} : Move the cursor at the specified coordinates.
-- @i{Row} : Row coordinate.
-- @i{Col} : Column coordinate.
procedure Cursor_Move(Row : Line; Col : Column);

-- @i{Description} : Set the current color.
-- @i{Fore} : Foreground color.
-- @i{Back} : Background color.
procedure Color_Set(Fore : in Color_Type := Gray;
                    Back : in Color_Type := Black);

--
-- put functions
--

-- @i{Description} : Put a text on the console with given colors attributes.
-- @i{Fore} : Foreground color.
-- @i{Back} : Background color.
-- @i{Period} : Wait duration between character output

procedure put(text : string;
              fore : in color_type := gray;
              back : in color_type := Black;
              Period : in Duration  := 0.0);

-- @i{Description} : Put a number on the console with given colors attributes.
-- @i{Fore} : Foreground color.
-- @i{Back} : Background color.
procedure Put(Number : integer;
              Fore   : in Color_Type := Gray;
              Back   : in Color_Type := Black);

-- @i{Description} : Put a text on the console with given colors attributes followed by a new line.
-- @i{Fore} : Foreground color.
-- @i{Back} : Background color.
procedure Put_Line(Text : String;
                   Fore : in Color_Type := Gray;
                   Back : in Color_Type := Black);

-- @i{Description} : Put a number on the console with given colors attributes followed by a new line.
-- @i{Fore} : Foreground color.
-- @i{Back} : Background color.
procedure Put_Line(Number : integer;
                   Fore : in Color_Type := Gray;
                   Back : in Color_Type := Black);

-- @i{Description} : Put a number on the console at given coordinates with specified colors attributes followed by a new line.
-- @i{Row} : Row coordinate.
-- @i{Col} : Column coordinate.
-- @i{Fore} : Foreground color.
-- @i{Back} : Background color.
procedure Put_Lc(Row    : Line := 1;
                 Col    : Column :=1;
                 Number : Integer;
                 Fore   : in Color_Type := Gray ;
                 Back   : in Color_Type := Black);

-- @i{Description} : Put a text on the console at given coordinates with specified colors attributes followed by a new line.
-- @i{Row} : Row coordinate.
-- @i{Col} : Column coordinate.
-- @i{Fore} : Foreground color.
-- @i{Back} : Background color.
-- @i{Period} : Wait duration between character output
Procedure Put_Lc(Row  : Line := 1;
                 Col  : Column :=1;
                 Text : String;
                 Fore : in Color_Type := Gray;
                 Back : in Color_Type := Black;
                 Period : in Duration := 0.0 );

--
-- Misc functions
--

-- @i{Description} : Set the codepage character, works with Windows NT OS family only.
-- @i{Codepage} : Codepage identifier, currently @code{Oem} or @code{Latin_1}.
procedure Codepage_Set(Codepage: integer:= Latin_1);

-- @i{Description} : Get the codepage character, works with Windows NT OS family only.
-- @i{Return} : Codepage identifier, currently @code{Oem} or @code{Latin_1}.
function  Codepage_Get return integer;

--
-- Keyboard functions
--

-- @i{Description} : Read the keyboard.
-- @i{Return} : The last key pressed.
function Key_Read return Integer;

-- @i{Description} : Check key availability.
-- @i{Return} : @code{True} if a key is available, @code{False} otherwise.
function Key_Available return Boolean;

-------------------------------------------------------------------------------
end v04.crt;
-------------------------------------------------------------------------------
