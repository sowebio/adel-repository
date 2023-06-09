--+------------------+---------------------------------------------------------
--|                  | @filename visual.ads
--|                  | @brief    Draw in text mode. Visual files can be used as resources by others programs
--|                  | @author   Martin Cattoen
--|                  | @date     16/08/04
--|                  | @rev      1.31
--+------------------+---------------------------------------------------------

with V04.Crt; use V04;

package Visual is

   pragma Elaborate_Body;

   --
   -- Constants
   --

   -- Screen depth
   MAX_LINE          : constant Natural        := Crt.Max_Line;
   MAX_COLUMN        : constant Natural        := Crt.Max_Column;

   -- Cursor
   FORE_COLOR_CURSOR : constant Crt.Color_Type := Crt.White;
   BACK_COLOR_CURSOR : constant Crt.Color_Type := Crt.Black;
   CHARACTER_CURSOR  : constant Character      := Character'Val(177);

   -- Definite colors
   MAGIC_COLOR       : constant Crt.Color_Type := Crt.Black;

   -- Exceptions
   Open_Visual_File_Error : exception;

   -- Keys (functions)

   KEY_CHOICE_MACRO          : constant Integer := Crt.K_Shift_F1;
   KEY_CHOICE_COLOR_FORE     : constant Integer := Crt.K_Ctrl_F;
   KEY_INSERT_CODE_CHARACTER : constant Integer := Crt.K_Ctrl_I;
   KEY_INITIALIZE_FILE       : constant Integer := Crt.K_Ctrl_N;
   KEY_OPEN_FILE             : constant Integer := Crt.K_Ctrl_O;
   KEY_SAVE_FILE             : constant Integer := Crt.K_Ctrl_S;
   KEY_APPLY_COLOR_BACK      : constant Integer := Crt.K_Delete;
   KEY_APPLY_COLOR_FORE      : constant Integer := Crt.K_Delete;
   KEY_SELECT_COPY           : constant Integer := Crt.K_Alt_F5;
   KEY_COPY                  : constant Integer := Crt.K_F5;
   KEY_PASTE                 : constant Integer := Crt.K_Alt_F7;
   KEY_MODE_INSERT_REPLACE   : constant Integer := Crt.K_Insert;
   KEY_EXIT_VISUAL           : constant Integer := Crt.K_Escape;

   -- Keys (colors)

   KEY_BLACK                 : constant Integer := Crt.K_F1;
   KEY_BLUE                  : constant Integer := Crt.K_F2;
   KEY_GREEN                 : constant Integer := Crt.K_F3;
   KEY_CYAN                  : constant Integer := Crt.K_F4;
   KEY_RED                   : constant Integer := Crt.K_F5;
   KEY_MAGENTA               : constant Integer := Crt.K_F6;
   KEY_BROWN                 : constant Integer := Crt.K_F7;
   KEY_GRAY                  : constant Integer := Crt.K_F8;
   KEY_LIGHT_BLUE            : constant Integer := Crt.K_Ctrl_F2;
   KEY_LIGHT_GREEN           : constant Integer := Crt.K_Ctrl_F3;
   KEY_LIGHT_CYAN            : constant Integer := Crt.K_Ctrl_F4;
   KEY_LIGHT_RED             : constant Integer := Crt.K_Ctrl_F5;
   KEY_LIGHT_MAGENTA         : constant Integer := Crt.K_Ctrl_F6;
   KEY_YELLOW                : constant Integer := Crt.K_F9;
   KEY_WHITE                 : constant Integer := Crt.K_F10;

   --
   -- Types
   --

   subtype Line   is Natural range 1..MAX_LINE;
   subtype Column is Natural range 1..MAX_COLUMN;

   type T_Tab_Color is array ( Line, Column ) of Crt.Color_Type;
   type T_Tab_Char  is array ( Line, Column ) of Character;

   type Visual_File is
      record
         Tab_Fore : T_Tab_Color;
         Tab_Back : T_Tab_Color;
         Tab_Char : T_Tab_Char;
      end record;

   NULL_FILE : constant Visual_File :=  ( Tab_Fore => ( others => ( others => Crt.Gray ) ),
                                          Tab_Back => ( others => ( others => Crt.Black ) ),
                                          Tab_Char => ( others => ( others => ' ' ) )
                                        );

   -------------------------------------------------------------------------------
   -- Subprograms
   -------------------------------------------------------------------------------

   --
   -- Visual file management
   --

   -- @i{Description} : Load a visual file
   -- @i{Name} : The visual file name to load
   -- @i{Return} : The loaded visual file
   -- @i{Exceptions} : Raise Open_Visual_File_Error if Name is not available or file is not a Visual file
   function Open ( Name : in String ) return Visual_File;

   -- @i{Description} : Load a visual file
   -- @i{Name} : The visual file name to load
   -- @i{Success} : Tre if open succeed, false otherwise
   -- @i{Return} : The loaded visual file
   -- @i{Exceptions} : Raise Open_Visual_File_Error if Name is not available or file is not a Visual file
   function Open ( Name    : in String;
                   Success : access Boolean
                 ) return Visual_File;

   -- @i{Description} : Create a visual file
   -- @i{Name} : The visual file name to create
   -- @i{Item} : The visual file to create
   procedure Create ( Name : in String;
                      Item : in Visual_File
                    );

   --
   -- Visual and text files management
   --

   -- @i{Description} : Load a visual file from an external text file
   -- @i{From} : The text file name to load
   -- @i{Return} : The visual file
   function Text_File_To_Visual ( From : in String ) return Visual_File;

   -- @i{Description} : Write an external text file from a visual file
   -- @i{From} : The visual file to write
   -- @i{To} : The external test file
   procedure Visual_To_Text_File ( From : in Visual_File;
                                   To   : in String
                                 );

   -- @i{Description} : Display item to screen
   -- @i{Item} :
   -- @i{Min_C_Scr} :
   -- @i{Min_L_Scr} :
   -- @i{Max_C_Scr} :
   -- @i{Max_L_Scr} :
   -- @i{Min_C_Fil} :
   -- @i{Min_L_Fil} :
   -- @i{Put_Magic_Color} :
   procedure Clean_Screen ( Item            : in Visual_File;
                            Min_C_Scr       : in Column  := 1;
                            Min_L_Scr       : in Line    := 1;
                            Max_C_Scr       : in Column  := MAX_COLUMN;
                            Max_L_Scr       : in Line    := MAX_LINE;
                            Min_C_Fil       : in Column  := 1;
                            Min_L_Fil       : in Line    := 1;
                            Put_Magic_Color : in Boolean := True
                          );

   --
   -- Interaction with user
   --

   -- @i{Description} : Visual editor
   --
   -- @i{Colors}
   -- Black    : @key{F1}
   -- Blue     : @key{F2}
   -- Green    : @key{F3}
   -- Cyan     : @key{F4}
   -- Red      : @key{F5}
   -- Magenta  : @key{F6}
   -- Brown    : @key{F7}
   -- Gray     : @key{F8}
   -- Yellow   : @key{F9}
   -- White    : @key{F10}
   -- Light_Blue    : @key{C-F2}
   -- Light_Green   : @key{C-F3}
   -- Light_Cyan    : @key{C-F4}
   -- Light_Red     : @key{C-F5}
   -- Light_Magenta : @key{C-F6}
   -- @i{Main commands}
   -- @key{Esc} : Quit;    @key{F5} : Copy;   @key{F7} : Paste;
   -- @key{C-s} : Save;       @key{C-o} : Load;     @key{C-n} : New file;
   -- @key{C-f} : Shape color;   @key{Del} : Fill background color;
   -- @key{C-i} : Character code;      @key{C-m} : macro character code
   -- @i{Editing commands}
   -- @key{Up}, @key{Down}, @key{Right}, @key{Left} : Write;
   -- @key{C-Up}, @key{C-Down}, @key{C-Right}, @key{C-Left} : Move cursor;
   -- @key{M-Up}, @key{M-Down}, @key{M-Right}, @key{M-Left} : Write macro character;
   --
   procedure Processing;

   --
   -- Others
   --

   -- @i{Description} : Get character code
   -- @i{Code_Char} : Character code
   -- @i{Return} : ASCII.NUL if character code is not available
   function Code_Char return Character;

-------------------------------------------------------------------------------
end Visual;
-------------------------------------------------------------------------------
