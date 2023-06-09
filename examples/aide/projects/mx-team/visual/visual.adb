--+------------------+---------------------------------------------------------
--|                  | @filename visual.adb
--|                  | @brief    Draw in text mode. Files saved can be used by others programs
--|                  | @author   Martin Cattoen
--|                  | @date     16/08/04
--|                  | @rev      1.31
--+------------------+---------------------------------------------------------

-- Ada packages
with Text_IO;
with Ada.Command_Line;

 -- Special packages
with V04.Crt; use V04;

package body Visual is

   --
   -- Vars
   --

   type Mode_Type is  ( Insert, Replace );
   for  Mode_Type use ( Insert => 0, Replace => 1 );
   for  Mode_Type'Size use 1;

   Color_Fore   : Crt.Color_Type := Crt.Gray;
   Color_Back   : Crt.Color_Type := Crt.Black;
   Char_Macro   : Character := Character'Val ( Crt.K_Space );

   Default_Tab  : Visual_File := Null_File;

   Current_Line   : Line   := 1;
   Current_Column : Column := 1;

   --
   -- Types
   --

   type Copy_Type is
      record
         File  : Visual_File := Null_File;
         Min_C : Column := 1;
         Min_L : Line   := 1;
         Max_C : Column := MAX_COLUMN;
         Max_L : Line   := MAX_LINE - 1;
      end record;

   Copy_Buffer : Copy_Type;

   -------------------------------------------------------------------------------
   -- Subprograms
   -------------------------------------------------------------------------------

   -------------------------------------------------------------------------------
   -- Description : Open Argument ( 1 )
   -- Return :
   function Begining return Visual_File is
   begin

      if Ada.Command_Line.Argument_Count < 1 then
         raise Open_Visual_File_Error;
      end if;

      Initialize;
      Clean_Screen ( Open ( Ada.Command_Line.Argument ( 1 ) ), Max_L_Scr => MAX_LINE - 1 );
      return Open ( Ada.Command_Line.Argument ( 1 ) );

   exception
      when Open_Visual_File_Error =>
         Initialize;
         return Default_Tab;

   end Begining;

   -------------------------------------------------------------------------------
   -- Description : Put Character_Cursor, with colors association
   procedure Put_Cursor is
   begin

      Crt.Put_Lc ( Current_Line, Current_Column, "" & CHARACTER_CURSOR, FORE_COLOR_CURSOR, BACK_COLOR_CURSOR );

   end Put_Cursor;

   -------------------------------------------------------------------------------
   -- Description : Put Current_Line, Current_Column, and Character, Color_Fore, Color_Back of ( Current_Line, Current_Column )
   -- Mode :
   -- Fore :
   -- Back :
   -- Char :
   procedure Put_Color_Image ( Mode : in Mode_Type;
                               Fore : in Crt.Color_Type;
                               Back : in Crt.Color_Type;
                               Char : in Character
                             ) is
   begin

      for I in 40..79 loop
         Crt.Put_lc ( MAX_LINE, I, " ", Crt.Light_Green, Crt.Black );
      end loop;

      Crt.Put_Lc ( MAX_LINE, 28, Integer'Image ( Current_Line )
                     & Integer'Image ( Current_Column )
                     & ' '
                     & Char
                     & ' '
                     & Crt.Color_Type'Image ( Fore )
                     & ' '
                     & Crt.Color_Type'Image ( Back )
                     & ' '
                     & Mode_Type'Image ( Mode ), Crt.Light_Green );

   end Put_Color_Image;

   -------------------------------------------------------------------------------
   -- Description : Current characters and colors display
   -- Mode :
   -- To_Back :
   -- Old_Char :
   procedure Affichage ( Mode     : in Mode_Type := Replace;
                         To_Back  : in Boolean   := False;
                         Old_Char : in Character := ' '
                       ) is

   begin

      if Mode = Insert and then ( not To_Back ) then
         for I in reverse Current_Column .. MAX_COLUMN - 1 loop -- Reverse because if not, all characters
                                                                -- on this line will been 'Current_Column'.
                                                                -- I = I - 1 !

            Default_Tab.Tab_Fore ( Current_Line, I + 1 ) := Default_Tab.Tab_Fore ( Current_Line, I );
            Default_Tab.Tab_Back ( Current_Line, I + 1 ) := Default_Tab.Tab_Back ( Current_Line, I );
            Default_Tab.Tab_Char ( Current_Line, I + 1 ) := Default_Tab.Tab_Char ( Current_Line, I );

         end loop;

      elsif Mode = Insert and then To_Back then

         for I in Current_Column .. MAX_COLUMN - 1 loop

            Default_Tab.Tab_Fore ( Current_Line, I ) := Default_Tab.Tab_Fore ( Current_Line, I + 1 );
            Default_Tab.Tab_Back ( Current_Line, I ) := Default_Tab.Tab_Back ( Current_Line, I + 1 );
            Default_Tab.Tab_Char ( Current_Line, I ) := Default_Tab.Tab_Char ( Current_Line, I + 1 );

         end loop;

         Default_Tab.Tab_Char ( Current_Line, MAX_COLUMN ) := Char_Macro;

         if Current_Column > 1 then
            Default_Tab.Tab_Char ( Current_Line, Current_Column - 1 ) := Old_Char;
         end if;

      end if;

      Crt.Put_Lc ( Current_Line, Current_Column, "" & Default_Tab.Tab_Char ( Current_Line, Current_Column ), Color_Fore, Color_Back );
      Default_Tab.Tab_Fore ( Current_Line, Current_Column ) := Color_Fore;
      Default_Tab.Tab_Back ( Current_Line, Current_Column ) := Color_Back;

      if Mode = Insert then
         Clean_Screen ( Default_Tab, Current_Column, Current_Line, MAX_COLUMN, Current_Line );
      end if;

   end Affichage;

   -------------------------------------------------------------------------------
   -- Description : Characters display leaving color character untouched
   -- Item :
   -- X :
   -- Y :
   procedure Put_Char ( Item : in Visual_File := Default_Tab;
                        X    : Column := Current_Column;
                        Y    : Line   := Current_Line
                      ) is
   begin
      Crt.Put_Lc ( Current_Line, Current_Column, "" & Item.Tab_Char ( Y, X ),
                   Item.Tab_Fore ( Y, X ),
                   Item.Tab_Back ( Y, X ) );
   end Put_Char;

   -------------------------------------------------------------------------------
   -- Description : User get a code character
   -- Code_Char :
   -- Return : Character
   function Code_Char return Character is

      Key  : Integer;
      Char : Integer;

   begin

      for I in 1..3 loop
         loop

            Key  := Crt.Key_Read;
            case Key is

               when Character'Pos ( '0' ) .. Character'Pos ( '2' ) =>
                  if    I = 1 then
                     Char := ( Key - 48 ) * 100;
                  elsif I = 2 then
                     Char := ( ( Key - 48 ) * 10 ) + Char;
                  else
                     Char := ( Key - 48 ) + Char;
                  end if;
                  exit;

               when Character'Pos ( '3' ) .. Character'Pos ( '9' ) =>
                  if    I = 2 then
                     Char := ( ( Key - 48 ) * 10 ) + Char;
                  elsif I = 3 then
                     Char := ( Key - 48 ) + Char;
                  end if;
                  exit;

               when others =>
                  null;

            end case;

         end loop;
      end loop;

      if Char > 255 then
         return Character'Val ( Crt.K_Null );
      end if;

      return Character'Val ( Char );

   end Code_Char;

   -------------------------------------------------------------------------------
   -- Description : Macro initiate
   procedure Macro is
   begin

      Default_Tab.Tab_Char ( Current_Line, Current_Column ) := Char_Macro;

      if Default_Tab.Tab_Char ( Current_Line, Current_Column ) = Character'Val ( Crt.K_Null ) then
         Default_Tab.Tab_Char ( Current_Line, Current_Column ) := ' ';
      end if;

      Default_Tab.Tab_Fore ( Current_Line, Current_Column ) := Color_Fore;
      Default_Tab.Tab_Back ( Current_Line, Current_Column ) := Color_Back;
      Crt.Put_Lc ( Current_Line, Current_Column, "" & Default_Tab.Tab_Char ( Current_Line, Current_Column ), Color_Fore, Color_Back );

   end Macro;

   -------------------------------------------------------------------------------
   -- Description : initilize file in memory and clean screen
   -- Color_Back_Interne :
   -- Color_Fore_Interne :
   -- Initialize_Char :
   -- Initialize_Color_Fore :
   -- Initialize_Color_Back :
   procedure Initialize ( Color_Back_Interne    : in Crt.Color_Type := Crt.Black;
                          Color_Fore_Interne    : in Crt.Color_Type := Color_Fore;
                          Initialize_Char       : in Boolean        := False;
                          Initialize_Color_Fore : in Boolean        := True;
                          Initialize_Color_Back : in Boolean        := True
                        ) is
   begin

      for I in 1..MAX_COLUMN loop
         for J in 1..MAX_LINE - 1 loop

            if Initialize_Color_Fore then
               Default_Tab.Tab_Fore ( J, I ) := Color_Fore_Interne;
            end if;
            if Initialize_Color_Back then
               Default_Tab.Tab_Back ( J, I ) := Color_Back_Interne;
            end if;
            if Initialize_Char then
               Default_Tab.Tab_Char ( J, I ) := Char_Macro;
            end if;

         end loop;
      end loop;

      Crt.Clear;

      Crt.Put_Lc ( MAX_LINE, 3,  "-- Visual --", Crt.Yellow );

      Color_Back := Color_Back_Interne;
      Clean_Screen ( Default_Tab, Max_L_Scr => MAX_LINE - 1 );

   end Initialize;

   -------------------------------------------------------------------------------
   -- Description : Insert a character according to its ANSI code
   procedure Initialize ( Color_Back_Interne    : in Crt.Color_Type := Crt.Black;
                          Color_Fore_Interne    : in Crt.Color_Type := Color_Fore;
                          Initialize_Char       : in Boolean        := False;
                          Initialize_Color_Fore : in Boolean        := True;
                          Initialize_Color_Back : in Boolean        := True
                        );

   procedure Insert is
   begin

      Default_Tab.Tab_Char ( Current_Line, Current_Column ) := Code_Char;
      if Default_Tab.Tab_Char ( Current_Line, Current_Column ) = Character'Val ( Crt.K_Null ) then
         Default_Tab.Tab_Char ( Current_Line, Current_Column ) := ' ';
      end if;
      Default_Tab.Tab_Fore ( Current_Line, Current_Column ) := Color_Fore;
      Default_Tab.Tab_Back ( Current_Line, Current_Column ) := Color_Back;
      Crt.Put_Lc ( Current_Line, Current_Column, "" & Default_Tab.Tab_Char ( Current_Line, Current_Column ), Color_Fore, Color_Back );
      if Current_Column < 80 then
         Current_Column := Current_Column + 1;
      end if;
      Put_Cursor;

   end Insert;

   -------------------------------------------------------------------------------
   -- Description : Copy a screen area
   procedure Copy ( Copy :    out Copy_Type;
                    Mode : in     Mode_Type
                  ) is

      procedure Selection is

         Mem_Current_Line   : constant Line   := Current_Line;
         Mem_Current_Column : constant Column := Current_Column;

      begin

         if Current_Column < Copy.Min_C then
            Copy.Max_C := Copy.Min_C;
            Copy.Min_C := Current_Column;
         end if;

         if Current_Line < Copy.Min_L then
            Copy.Max_L := Copy.Min_L;
            Copy.Min_L := Current_Line;
         end if;


         if Copy.Min_C     >  1 then
            Clean_Screen ( Default_Tab, Copy.Min_C - 1, Copy.Min_L, Copy.Min_C - 1, Copy.Min_L );
         end if;

         if Copy.Min_L     >  1 then
            Clean_Screen ( Default_Tab, Copy.Min_C, Copy.Min_L - 1, Copy.Min_C, Copy.Min_L - 1 );
         end if;

         if Current_Column < 80 then
            Clean_Screen ( Default_Tab, Current_Column + 1, Current_Line, Current_Column + 1, Current_Line );
         end if;

         if Current_Line   < 24 then
            Clean_Screen ( Default_Tab, Current_Column, Current_Line + 1, Current_Column, Current_Line + 1 );
         end if;


         for I in Copy.Min_C..Current_Column loop
            for J in Copy.Min_L..Current_Line loop

               if Crt.Color_Type'Pos ( Default_Tab.Tab_Back ( J, I ) ) = Crt.Color_Type'Pos ( Crt.Black ) or else
                  Crt.Color_Type'Pos ( Default_Tab.Tab_Fore ( J, I ) ) = Crt.Color_Type'Pos ( Crt.Gray )
               then
                  Crt.Put_Lc ( J, I, "" & Default_Tab.Tab_Char ( J, I ), Crt.Brown, Crt.Gray );

               elsif Crt.Color_Type'Pos ( Default_Tab.Tab_Back ( J, I ) ) = Crt.Color_Type'Pos ( Crt.Black ) then
                  Crt.Put_Lc ( J, I, "" & Default_Tab.Tab_Char ( J, I ), Default_Tab.Tab_Fore ( J, I ), Crt.Gray );

               elsif Crt.Color_Type'Pos ( Default_Tab.Tab_Fore ( J, I ) ) = Crt.Color_Type'Pos ( Crt.Gray ) then
                  Crt.Put_Lc ( J, I, "" & Default_Tab.Tab_Char ( J, I ), Crt.Brown, Default_Tab.Tab_Back ( J, I ) );

               else
                  Crt.Put_Lc ( J, I, "" & Default_Tab.Tab_Char ( J, I ) );

               end if;

            end loop;
         end loop;

         Current_Line   := Mem_Current_Line;
         Current_Column := Mem_Current_Column;

      end Selection;

      Key : Integer;

   begin

      Copy.Min_C := Current_Column;
      Copy.Min_L := Current_Line;

      loop

         Key := Crt.Key_Read;

         if    Key = Crt.K_Up and Current_Line > 1 then
            Selection;
            Current_Line := Current_Line - 1;
            Selection;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Char;
            Put_Cursor;

         elsif Key = Crt.K_Down and Current_Line < 24 then
            Selection;
            Current_Line := Current_Line + 1;
            Selection;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Char;
            Put_Cursor;

         elsif Key = Crt.K_Left and Current_Column > 1 then
            Selection;
            Current_Column := Current_Column - 1;
            Selection;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Char;
            Put_Cursor;

         elsif Key = Crt.K_Right and Current_Column < 80 then
            Selection;
            Current_Column := Current_Column + 1;
            Selection;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Char;
            Put_Cursor;

         elsif Key = Key_Copy then
            exit;

         end if;

      end loop;

      Copy.Max_C := Current_Column;
      Copy.Max_L := Current_Line;

      if Current_Column < Copy.Min_C then
         Copy.Max_C := Copy.Min_C;
         Copy.Min_C := Current_Column;
      end if;

      if Current_Line < Copy.Min_L then
         Copy.Max_L := Copy.Min_L;
         Copy.Min_L := Current_Line;
      end if;

      Clean_Screen ( Default_Tab, Max_L_Scr => MAX_LINE - 1 );
      Copy.File := Default_Tab;

   end Copy;

   -------------------------------------------------------------------------------
   -- Description : Paste a screen area
   procedure Paste ( From : in     Copy_Type;
                     To   :    out Visual_File
                   ) is
   begin

      if   From.Min_C > From.Max_C
        or From.Min_L > From.Max_L
      then
         raise Constraint_Error;
      end if;

      for I in From.Min_C..From.Max_C loop

         if ( Current_Column + I ) - 1 > MAX_COLUMN then
            exit;
         end if;

         for J in From.Min_L..From.Max_L loop

            if ( Current_Line + J ) - 1 > MAX_LINE - 1 then
               exit;
            end if;

            To.Tab_Fore ( ( Current_Line + J ) - From.Min_L, ( Current_Column + I ) - From.Min_C ) :=
              From.File.Tab_Fore ( J, I );
            To.Tab_Back ( ( Current_Line + J ) - From.Min_L, ( Current_Column + I ) - From.Min_C ) :=
              From.File.Tab_Back ( J, I );
            To.Tab_Char ( ( Current_Line + J ) - From.Min_L, ( Current_Column + I ) - From.Min_C ) :=
              From.File.Tab_Char ( J, I );

         end loop;
      end loop;
   end;

   -------------------------------------------------------------------------------
   -- Description : Choose a background color
   -- Return :
   function Select_Color_Fore return Crt.Color_Type is
      Key   : Integer;
   begin
      Key := Crt.Key_Read;

      if    Key = Crt.K_F1 then
         return Crt.Black;

      elsif Key = Crt.K_F2 then
         return Crt.Blue;

      elsif Key = Crt.K_F3 then
         return Crt.Green;

      elsif Key = Crt.K_F4 then
         return Crt.Cyan;

      elsif Key = Crt.K_F5 then
         return Crt.Red;

      elsif Key = Crt.K_F6 then
         return Crt.Magenta;

      elsif Key = Crt.K_F7 then
         return Crt.Brown;

      elsif Key = Crt.K_F8 then
         return Crt.Gray;

      elsif Key = Crt.K_F9 then
         return Crt.Yellow;

      elsif Key = Crt.K_F10 then
         return Crt.White;

      elsif Key = Crt.K_Ctrl_F2 then
         return Crt.Light_Blue;

      elsif Key = Crt.K_Ctrl_F3 then
         return Crt.Light_Green;

      elsif Key = Crt.K_Ctrl_F4 then
         return Crt.Light_Cyan;

      elsif Key = Crt.K_Ctrl_F5 then
         return Crt.Light_Red;

      elsif Key = Crt.K_Ctrl_F6 then
         return Crt.Light_Magenta;

      else
         return Crt.Gray;

      end if;
   end Select_Color_Fore;

   -------------------------------------------------------------------------------
   -- Description : Get a file name
   -- Msg : Message string
   -- Return : THe file name entered by the user
   function Ask_Name_File ( Msg : in String ) return String is

      Result : String ( 1..50 );    -- Nom du fichier
      Length : Natural range 0..50; -- Taille du nom de fichier

   begin

      Crt.Put_Lc ( 2, 5, Msg, Crt.Light_Green );
      Text_IO.Get_Line ( Result, Length );

      return Result ( 1..Length );

   exception
      when others =>
         return "";

   end Ask_Name_File;

   -------------------------------------------------------------------------------
   -- Description : Create file and write item to it
   -- Name :
   -- Item :
   procedure Create ( Name : in String;
                      Item : in Visual_File
                    ) is

      File : Text_IO.File_Type;  -- Save file

   begin

      begin

         -- Create file
         Text_IO.Create ( File, Text_IO.Out_File, Name & ".vsl" );

      exception
         when Text_IO.Status_Error =>
            Text_IO.Open ( File, Text_IO.Out_File, Name & ".vsl" );
            Text_IO.Delete ( File );
            Text_IO.Create ( File, Text_IO.Out_File, Name & ".vsl" );

      end;

      Text_IO.Put ( File, Integer'Image ( MAX_LINE ) & Integer'Image ( MAX_COLUMN ) ); -- Put screen depth

      -- Write colors and characters
      for Colonne in 1..MAX_COLUMN loop
         for Ligne in 1..MAX_LINE loop

            Text_IO.Put ( File, Integer'Image ( Character'Pos ( Item.Tab_Char ( Ligne, Colonne ) ) ) );
            Text_IO.Put ( File, Integer'Image ( Crt.Color_Type'Pos ( Item.Tab_Fore ( Ligne, Colonne ) ) ) );
            Text_IO.Put ( File, Integer'Image ( Crt.Color_Type'Pos ( Item.Tab_Back ( Ligne, Colonne ) ) ) );

         end loop;
      end loop;

      -- Close file
      Text_IO.Close ( File );

   end Create;

   -------------------------------------------------------------------------------
   -- Description : Load a visual file
   -- Name :
   -- Return :
   function Open ( Name : in String ) return Visual_File is

      File    : Text_IO.File_Type;  -- External file
      Result  : Visual_File;        -- Interne file
      Buffer  : Integer;            -- Current number in the file

      Max_Line_In_File   : Natural;
      Max_Column_In_File : Natural;

      package Int_Io is new Text_IO.Integer_IO ( Integer );

   begin

      -- Open file
      Text_IO.Open ( File, Text_IO.In_File, Name & ".vsl" );

      -- Get screen depth
      Int_Io.Get ( File, Max_Line_In_File );
      Int_Io.Get ( File, Max_Column_In_File );

      -- Read colors and characters
      for Column in 1..Max_Column_In_File loop
         for Line in 1..Max_Line_In_File loop
            if    ( Text_IO.End_Of_File ( File ) ) then
               Text_IO.Close ( File );
               raise Open_Visual_File_Error;

            elsif ( Text_IO.End_Of_Page ( File ) ) then
               Text_IO.Skip_Page ( File );

            elsif ( Text_IO.End_Of_Line ( File ) ) then
               Text_IO.Skip_Line ( File );

            else

               Int_Io.Get ( File, Buffer );
               if         Column <= MAX_COLUMN
                 and then Line   <= MAX_LINE
               then
                  Result.Tab_Char ( Line, Column ) := Character'Val ( Buffer );
               end if;

               Int_Io.Get ( File, Buffer );
               Color_Fore := Crt.Color_Type'Val ( Buffer );

               Int_Io.Get ( File, Buffer );
               Color_Back := Crt.Color_Type'Val ( Buffer );

            end if;

            if         Column <= MAX_COLUMN
              and then Line   <= MAX_LINE
            then
               Result.Tab_Fore ( Line, Column ) := Color_Fore;
               Result.Tab_Back ( Line, Column ) := Color_Back;
            end if;

         end loop;
      end loop;

      Text_IO.Close ( File );

      return Result;

   exception
      when Text_IO.Name_Error | Text_IO.Data_Error =>
         raise;
      when others =>
         if Text_IO.Is_Open ( File ) then
            Text_IO.Close ( File );
         end if;
         raise Open_Visual_File_Error;

   end Open;

   -------------------------------------------------------------------------------
   -- Description : Load a visual file (Open_Visual_File_Error isn't raised)
   -- Name :
   -- Success :
   -- Return :
   function Open ( Name    : in String;
                   Success : access Boolean
                 ) return Visual_File is

   begin

      Success.all := True;
      return Open ( Name );

   exception
      when Open_Visual_File_Error =>
         Success.all := False;
         return Null_File;

   end Open;

   -------------------------------------------------------------------------------
   -- Description : Display item to screen
   -- Item :
   -- Min_C_Scr :
   -- Min_L_Scr :
   -- Max_C_Scr :
   -- Max_L_Scr :
   -- Min_C_Fil :
   -- Min_L_Fil :
   -- Put_Magic_Color :
   procedure Clean_Screen ( Item            : in Visual_File;             -- Visual file will print
                            Min_C_Scr       : in Column  := 1;            -- }
                            Min_L_Scr       : in Line    := 1;            --  } Screen zone
                            Max_C_Scr       : in Column  := MAX_COLUMN;   --  }
                            Max_L_Scr       : in Line    := MAX_LINE;     -- }
                            Min_C_Fil       : in Column  := 1;            -- }
                            Min_L_Fil       : in Line    := 1;            -- } Item zone
                            Put_Magic_Color : in Boolean := True
                          ) is

      Current_Line_Before   : constant Line   := Current_Line;
      Current_Column_Before : constant Column := Current_Column;

   begin

      if        Min_C_Scr > Max_C_Scr
        or else Min_L_Scr > Max_L_Scr
      then
         raise Constraint_Error;
      end if;

      for L in Min_L_Scr..Max_L_Scr loop
         for C in Min_C_Scr..Max_C_Scr loop

            Current_Line   := L;
            Current_Column := C;

            if not ( ( not Put_Magic_Color ) and then
                       Crt.Color_Type'Pos ( Item.Tab_Back ( Current_Line, Current_Column ) ) =
                       Crt.Color_Type'Pos ( Magic_Color ) )
            then
               if         ( C + Min_C_Fil ) - 1 <= MAX_COLUMN
                 and then ( L + Min_L_Fil ) - 1 <= MAX_LINE
               then
                  Put_Char ( Item, ( C + Min_C_Fil ) - 1, ( L + Min_L_Fil ) - 1 );
               end if;
            end if;

         end loop;
      end loop;

      Current_Line   := Current_Line_Before;
      Current_Column := Current_Column_Before;

      Color_Back := Item.Tab_Back ( Current_Line, Current_Column );
      Color_Fore := Item.Tab_Fore ( Current_Line, Current_Column );

   end Clean_Screen;

   -------------------------------------------------------------------------------
   -- Description : Create external text file full of the To'characters.
   -- From :
   -- To :
   procedure Visual_To_Text_File ( From : in Visual_File;
                                   To   : in String
                                 ) is

      Text_File : Text_IO.File_Type;
      Test      : Boolean := False;

   begin

      Text_IO.Create ( Text_File, Text_IO.Out_File, To );

      for I in 1..MAX_LINE loop
         for J in 1..MAX_COLUMN loop

            if J = 1 and then I /= 1 then
               Text_IO.New_Line ( Text_File );
            end if;

            for K in J..MAX_COLUMN loop
               if Default_Tab.Tab_Char ( I, K ) = ' ' then
                  Test := False;
               else
                  Test := True;
                  exit;
               end if;
            end loop;

            if Test then
               Text_IO.Put ( Text_File, From.Tab_Char ( I, J ) );
            else
               exit;
            end if;

         end loop;
      end loop;

      Text_IO.Close ( Text_File );

   end Visual_To_Text_File;

   -------------------------------------------------------------------------------
   -- Description : Visual_File from an external text file
   -- From :
   -- Return :
   function Text_File_To_Visual ( From : in String ) return Visual_File is

      Text_File : Text_IO.File_Type;
      Result    : Visual_File := Null_File;
      Test      : Boolean := True;

   begin

      Text_IO.Open ( Text_File, Text_IO.In_File, From );

      for I in 1..MAX_LINE loop
         for J in 1..MAX_COLUMN loop

            if Text_IO.End_Of_File ( Text_File ) then
               Text_IO.Close ( Text_File );
               return Result;

            elsif Text_IO.End_Of_Line ( Text_File ) then
               Text_IO.Skip_Line ( Text_File );
               Test := False;
               exit;

            end if;

            Text_IO.Get ( Text_File, Result.Tab_Char ( I, J ) );

         end loop;

         if Test then
            Text_IO.Skip_Line ( Text_File );
         end if;
         Test := True;

      end loop;

      Text_IO.Close ( Text_File );
      return Result;

   end Text_File_To_Visual;

   -------------------------------------------------------------------------------
   -- Description : Main processing
   procedure Processing is

      Key  : Integer;  -- Got key
      Mode : Mode_Type := Replace;
      Test : aliased Boolean;

   begin

      -- Rendre l'ecran vierge ou avec le fichier en argument ouvert
      Crt.Codepage_Set ( Crt.Oem );
      Default_Tab := Begining;
      Put_Cursor;
      Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

      -- Saisie des touches et reaction a celles-ci
      loop

         Key := Crt.Key_Read;

         -- Deplacement du curseur
         if    Key = Crt.K_Up and Current_Line > 1 then
            Affichage;
            Current_Line := Current_Line - 1;
            Default_Tab.Tab_Fore ( Current_Line, Current_Column ) := Color_Fore;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Down and Current_Line < MAX_LINE - 1 then
            Affichage;
            Current_Line := Current_Line + 1;
            Default_Tab.Tab_Fore ( Current_Line, Current_Column ) := Color_Fore;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Left and Current_Column > 1 then
            Affichage;
            Current_Column := Current_Column - 1;
            Default_Tab.Tab_Fore ( Current_Line, Current_Column ) := Color_Fore;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Right and Current_Column < MAX_COLUMN then
            Affichage;
            Current_Column := Current_Column + 1;
            Default_Tab.Tab_Fore ( Current_Line, Current_Column ) := Color_Fore;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Alt_Up and Current_Line > 1 then
            Macro;
            Current_Line := Current_Line - 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Alt_Down and Current_Line < MAX_LINE - 1 then
            Macro;
            Current_Line := Current_Line + 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Alt_Left and Current_Column > 1 then
            Macro;
            Current_Column := Current_Column - 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Alt_Right and Current_Column < MAX_COLUMN then
            Macro;
            Current_Column := Current_Column + 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Ctrl_Up and Current_Line > 1 then
            Put_Char;
            Current_Line := Current_Line - 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Ctrl_Down and Current_Line < MAX_LINE - 1 then
            Put_Char;
            Current_Line := Current_Line + 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Ctrl_Left and Current_Column > 1 then
            Put_Char;
            Current_Column := Current_Column - 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Ctrl_Right and Current_Column < MAX_COLUMN then
            Put_Char;
            Current_Column := Current_Column + 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Pageup then
            Put_Char;
            Current_Line := 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Pagedown then
            Put_Char;
            Current_Line := MAX_LINE - 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_Home then
            Put_Char;
            Current_Column := 1;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = Crt.K_End then
            Put_Char;
            Current_Column := MAX_COLUMN;
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         -- Selection d'un caractere
         elsif Key in 32..255 then
            Affichage ( Mode );
            Default_Tab.Tab_Char ( Current_Line, Current_Column ) := Character'Val ( Key );
            Affichage;
            if Current_Column < MAX_COLUMN then
               Current_Column := Current_Column + 1;
            end if;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Cursor;

         -- Retour en arriere
         elsif Key = Crt.K_BackSpace and Current_Column > 1 then
            Affichage;
            Current_Column := Current_Column - 1;
            if Mode /= Insert then
               Default_Tab.Tab_Char ( Current_Line, Current_Column ) := ' ';
            end if;
            Affichage ( Mode, To_Back => True, Old_Char => Default_Tab.Tab_Char ( Current_Line, Current_Column + 1 ) );
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Cursor;

         -- Saut de page
         elsif Key = Crt.K_Return and Current_Line < MAX_LINE - 1 then
            Affichage ( Mode );
            Current_Line := Current_Line + 1;
            Current_Column := 1;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Cursor;

            -- Selection de la couleur de fond
         elsif Key = KEY_BLACK then
            Color_Back := Crt.Black;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_BLUE then
            Color_Back := Crt.Blue;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_GREEN then
            Color_Back := Crt.Green;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_CYAN then
            Color_Back := Crt.Cyan;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_RED then
            Color_Back := Crt.Red;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_MAGENTA then
            Color_Back := Crt.Magenta;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_BROWN then
            Color_Back := Crt.Brown;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_GRAY then
            Color_Back := Crt.Gray;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_YELLOW then
            Color_Back := Crt.Yellow;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_WHITE then
            Color_Back := Crt.White;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_LIGHT_BLUE then
            Color_Back := Crt.Light_Blue;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_LIGHT_GREEN then
            Color_Back := Crt.Light_Green;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_LIGHT_CYAN then
            Color_Back := Crt.Light_Cyan;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_LIGHT_RED then
            Color_Back := Crt.Light_Red;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         elsif Key = KEY_LIGHT_MAGENTA then
            Color_Back := Crt.Light_Magenta;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         -- Le fond est de la couleur selectionnee
         elsif Key = KEY_APPLY_COLOR_BACK then
            Initialize ( Color_Back_Interne => Color_Back, Initialize_Color_Fore => False );
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         -- La forme est de la couleur selectionnee
         elsif Key = KEY_APPLY_COLOR_FORE then
            Initialize ( Color_Fore_Interne => Color_Fore, Initialize_Color_Back => False );
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         -- Sauvegarde
         elsif Key = KEY_SAVE_FILE then
            Create ( Ask_Name_File ( "Quel nom voulez vous donner au fichier ? " ), Default_Tab );
            Clean_Screen ( Default_Tab, Max_L_Scr => MAX_LINE - 1 );
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Cursor;

         -- Ouverture
         elsif Key = KEY_OPEN_FILE then
            Default_Tab := Open ( Ask_Name_File ( "Quel fichier voulez vous ouvrir ? " ), Test'Access );
            if not Test then
               Crt.Put_Lc ( 5, 10, "Fichier inexistant ou mauvais format.", Crt.Light_Red );
            end if;
            Clean_Screen ( Default_Tab, Max_L_Scr => MAX_LINE - 1 );
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );
            Put_Cursor;

         -- Fichier noir
         elsif Key = KEY_INITIALIZE_FILE then
            Initialize ( Initialize_Char => True );
            Put_Cursor;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         -- Insert a character
         elsif Key = KEY_INSERT_CODE_CHARACTER then
            Insert;
            if Current_Column > 1 then
              Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column - 1 ) );
            end if;

         -- Choisir la couleur de forme
         elsif Key = KEY_CHOICE_COLOR_FORE then
            Color_Fore := Select_Color_Fore;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         -- Choisir le caractere de la macro
         elsif Key = KEY_CHOICE_MACRO then
            Char_Macro := Code_Char;

         -- Copy
         elsif Key = KEY_SELECT_COPY then
            Copy ( Copy_Buffer, Mode );
            Put_Cursor;

         -- Paste
         elsif Key = KEY_PASTE then
            Paste ( Copy_Buffer, Default_Tab );
            Clean_Screen ( Default_Tab, Max_L_Scr => MAX_LINE - 1 );
            Put_Cursor;

         -- Change de mode
         elsif Key = KEY_MODE_INSERT_REPLACE then
            case Mode is
               when Replace =>
                  Mode := Insert;
               when Insert =>
                  Mode := Replace;
            end case;
            Put_Color_Image ( Mode, Color_Fore, Color_Back, Default_Tab.Tab_Char ( Current_Line, Current_Column ) );

         -- Quitter
         elsif Key = KEY_EXIT_VISUAL then
            exit;

         end if;

      end loop;

      Create ( "autosave", Default_Tab );

      -- Details de fin
      Crt.Beep;
      Crt.Clear;
      delay 0.25;

   end Processing;

-------------------------------------------------------------------------------
end Visual;
-------------------------------------------------------------------------------
