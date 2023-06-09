pragma Source_Reference (001231, "x80_ed.ada");

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
