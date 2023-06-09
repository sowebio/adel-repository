---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:45 PM BST  --
---------------------------------------------------------------------
--[class_histogram.ads] Specification
package Class_Histogram is
  type Histogram is private;
  Def_Height : constant Positive := 14;

  procedure Reset( The:in out Histogram );
  procedure Add_To( The:in out Histogram; A_Ch:in Character );
  procedure Put(The:in Histogram; Height:in Positive:=Def_Height);
private
  type    Alphabet_Index is new Character range 'A' .. 'Z';
  type    Alphabet_Array is array (Alphabet_Index) of Natural;

  type Histogram is record
    Number_Of   : Alphabet_Array := ( others => 0 );
  end record;
end Class_Histogram;

--[class_histogram.adb] Implementation
with Ada.Text_Io, Ada.Float_Text_Io, Ada.Characters.Handling;
use  Ada.Text_Io, Ada.Float_Text_Io, Ada.Characters.Handling;
package body Class_Histogram is

  procedure Reset(The:in out Histogram) is
  begin
    The.Number_Of := ( others => 0 );  -- Reset counts to 0
  end Reset;

  procedure Add_To(The:in out Histogram; A_Ch:in Character) is
    Ch : Character;
  begin
    Ch := A_Ch;                        -- As write to ch
    if Is_Lower(Ch) then               -- Convert to upper case
      Ch := To_Upper( Ch );
    end if;
    if Is_Upper( Ch ) then             -- so record
      declare
        C : Alphabet_Index := Alphabet_Index(Ch);
      begin
        The.Number_Of(C) := The.Number_Of(C) + 1;
      end;
    end if;
  end Add_To;

  procedure Put(The:in Histogram;
                Height:in Positive:=Def_Height) is
    Frequency    : Alphabet_Array;        -- Copy to process
    Max_Height   : Natural := 0;          -- Observed max
  begin
    Frequency := The.Number_Of;           -- Copy data (Array)
    for Ch in Alphabet_Array'Range loop   -- Find max frequency
      if Frequency(Ch) > Max_Height then
        Max_Height:= Frequency(Ch);
      end if;
    end loop;

    if Max_Height > 0 then
      for Ch in Alphabet_Array'Range loop -- Scale to max height
        Frequency(Ch):=(Frequency(Ch)*Height)/(Max_Height);
      end loop;
    end if;

    for Row in reverse 1 .. Height loop  -- Each line
      Put( "  | " );                     -- start of line
      for Ch in Alphabet_Array'Range loop
        if Frequency(Ch) >= Row then
          Put('*');                      -- bar of hist >= col
        else
          Put(' ');                      -- bar of hist <  col
        end if;
      end loop;
      Put(" | "); New_Line;              -- end of line
    end loop;
    Put("  +----------------------------+"); New_Line;
    Put("    ABCDEFGHIJKLMNOPQRSTUVWXYZ " ); New_Line;
    Put("  *  = (approx) ");
    Put( Float(Max_Height) / Float(Height), Aft=>2, Exp=>0 );
    Put(" characters "); New_Line;
  end Put;
end Class_Histogram;

--[main.adb] Procedure
with Ada.Text_Io, Class_Histogram;
use  Ada.Text_Io, Class_Histogram;
procedure Main is
  Ch:Character;                     -- Current character
  Text_Histogram: Histogram;        -- Histogram object
begin
  Reset(Text_Histogram);            -- Reset to empty

  while not End_Of_File loop        -- For each line
    while not End_Of_Line loop      -- For each character
      Get(Ch);                      -- Get current character
      Add_To( Text_Histogram, Ch ); -- Add to histogram
    end loop;
    Skip_Line;                      -- Next line
  end loop;

  Put( Text_Histogram );            -- Print histogram

end Main;
