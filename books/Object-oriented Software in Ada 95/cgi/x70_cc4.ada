---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:53 PM BST  --
---------------------------------------------------------------------
--[pack_types.ads] Specification
with Ada.Text_Io;
use  Ada.Text_Io;
package Pack_Types is
  type P_File_Type is access all Ada.Text_Io.File_Type;
  Eot  : constant Character := Character'Val(0);
  Cr   : constant Character := Character'Val(15);
  Queue_Size    : constant := 3;

  type    Queue_No    is new Integer   range 0 .. Queue_Size;
  type    Queue_Index is mod Queue_Size;
  subtype Queue_Range is Queue_Index;
  type    Queue_Array is array ( Queue_Range ) of Character;
end Pack_Types;

--[pack_threads.ads] Specification
with Pack_Types;
use  Pack_Types;
package Pack_Threads is
  protected type PT_Buffer is         -- Task type specification
    entry Put( Ch:in Character; No_More:in Boolean );
    entry Get( Ch:in out Character; No_More:out Boolean);
  private
    Elements    : Queue_Array;              -- Array of elements
    Head        : Queue_Index := 0;         -- Index
    Tail        : Queue_Index := 0;         -- Index
    No_In_Queue : Queue_No    := 0;         -- Number in queue
    Fin         : Boolean     := False;     -- Finish;
  end PT_Buffer;

  type P_PT_Buffer is access all PT_Buffer;

  task type Task_Read( P_Buffer:P_PT_Buffer;
                       Fd_In:P_File_Type) is
    entry Finish;
  end Task_Read;

  task type Task_Write( P_Buffer:P_PT_Buffer; 
                        Fd_Out:P_File_Type) is
    entry Finish;
  end Task_Write;
end Pack_Threads;

--[pack_threads.adb] Implementation
with Ada.Text_Io;
use  Ada.Text_Io;
package body Pack_Threads is

  protected body PT_Buffer is

    entry Put( Ch:in Character; No_More:in Boolean )
        when No_In_Queue < Queue_Size  is
    begin
      if No_More then                    -- Last
        Fin := True;                     -- Set flag
      else
        Elements( Tail ) := Ch;          -- Add to queue
        Tail := Tail+1;                  -- Next position
        No_In_Queue := No_In_Queue + 1;  --
      end if;
    end;

    entry Get(Ch:in out Character; No_More:out Boolean)
        when No_In_Queue > 0 or else Fin is
    begin
      if No_In_Queue > 0 then             -- Item available
        Ch := Elements( Head );           -- Get item
        Head := Head+1;                   -- Next position
        No_In_Queue := No_In_Queue - 1;   --
        No_More := False;                 -- Not end
      else
        No_More := True;                  -- End of items
      end if;
    end;

  end PT_Buffer;

  task body Task_Read is                  -- Task implementation
    Ch      : Character;
  begin
    while not End_Of_File( Fd_In.all ) loop
      while not End_Of_Line( Fd_In.all ) loop
        Get( Fd_In.all, Ch);              -- Get character
        P_Buffer.Put( Ch, False );        -- Add to buffer
        --DELAY 0.00001;                  -- Cause task switch
      end loop;
      Skip_Line( Fd_In.all );             -- Next line
      P_Buffer.Put( Cr, False );          -- New line
      --DELAY 0.00001;                    -- Cause task switch
    end loop;
    P_Buffer.Put( Eot, True );            -- End of characters

    accept Finish;
  exception
    when Tasking_Error =>
      Put("Exception in Task read"); New_Line;
  end Task_Read;

  task body Task_Write is                 -- Task implementation
    Last     : Boolean := False;          -- No more data
    Ch       : Character;                 -- Character read
  begin
    loop
      P_Buffer.Get( Ch, Last );           -- From buffer
      exit when Last;                     -- No more characters
      if Ch = Cr then
        New_Line( Fd_Out.all );           -- New line
      else
        Put( Fd_Out.all, Ch );            -- Character
      end if;
      --DELAY 0.00001;                    -- Cause task switch
    end loop;

    accept Finish;                        -- Finished
  exception
    when Tasking_Error =>
      Put("Exception in Task write"); New_Line;
  end Task_Write;
end Pack_Threads;

--[copy.adb] Procedure
with Ada.Text_Io, Pack_Threads, Pack_Types;
use  Ada.Text_Io, Pack_Threads, Pack_Types;
procedure Do_Copy(From:in String; To:in String) is
  type State is ( Open_File, Create_File );
  Fd_In   : P_File_Type := new Ada.Text_Io.File_Type;
  Fd_Out  : P_File_Type := new Ada.Text_Io.File_Type;
  Mode    : State := Open_File;
begin
  Open(  File=>Fd_In.all,  Mode=>In_File,  Name=>From);
  Mode := Create_File;
  Create(File=>Fd_Out.all, Mode=>Out_File, Name=>To);
  declare
    Buffers : P_PT_Buffer := new PT_Buffer;
    Reader  : Task_Read( Buffers, Fd_In );
    Writer  : Task_Write( Buffers, Fd_Out );
  begin
    Reader.Finish;  Close( Fd_In.all );   -- Finish reader task
    Writer.Finish;  Close( Fd_Out.all );  -- Finish writer task
  end;
exception
  when Name_Error =>
    case Mode is
      when Open_File =>
        Put("Problem opening file " & From ); New_Line;
      when Create_File =>
        Put("Problem creating file " & To ); New_Line;
    end case;
  when Tasking_Error =>
    Put("Task error in main program"); New_Line;
end Do_Copy;

--[main.adb] Procedure
with Ada.Text_Io, Ada.Command_Line, Do_Copy;
use  Ada.Text_Io, Ada.Command_Line;
procedure Copy is
begin
  if Argument_Count = 2 then
    Do_Copy ( Argument(1), Argument(2) );
  else
    Put("Usage: copy from to"); New_Line;
  end if;
end Copy;
