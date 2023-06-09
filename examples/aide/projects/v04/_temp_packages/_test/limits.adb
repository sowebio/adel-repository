--+------------------+---------------------------------------------------------
--| o   o  ooo    oo | @filename limits.adb
--| o   o o   o  o o | @brief    test os limits
--| o   o o   o o  o | @author   Stephane Riviere
--|  o o  o   o ooooo| @date     05/11/04
--|   o    ooo     o | Licence   GNAT licence (LGPL like)
--+------------------+---------------------------------------------------------

with v04.crt; use v04;
with Ada.Text_IO;

procedure Limits is

package Tio renames Ada.Text_IO;

-------------------------------------------------------------------------------
function byte2string (I : integer) return string is

s : constant string := integer'image(I);

begin
   if    s'length = 1 then
         return "000" & s(2..S'last);
   elsif s'length = 2 then
         return "00"  & s(2..S'last);
   elsif s'length = 3 then
         return "0"   & s(2..S'last);
   else
         return         s(2..S'last);
   end if;

end byte2string;

-------------------------------------------------------------------------------

Version_Number : constant String := "B.01";

-- File_Handle : Tio.File_Type;

THandle : array (1 .. 10000) of Tio.File_Type;
-- File_Counter : Integer := 0;

begin

   Crt.Clear;
   Crt.Put_Line( "Adisland  (R) File system limits tester. Version " & Version_Number);
   Crt.Put( "Copyright (C) Adisland France 2004, all rights reserved.");

--   while True loop
   for File_Counter in 1 .. 500 loop
     -- File_Counter := File_Counter + 1 ;

      Tio.Create(File => THandle(File_Counter), Mode => Tio.Out_File, Name => "Test." & byte2string(File_Counter));
      crt.put_lc(10,1,File_Counter);

--   for I in character'pos(character'first)..character'pos(character'last) loop


   end loop;

   while True loop
         Crt.Beep (20000, 1000);
   end loop;






--   crt.put_lc(1,1,"This is a string, by defaults settings, white over black");
--   crt.put_lc(2,1,1234567890);

--   crt.put_lc(3,1,"This is a black string over a light cyan background",crt.black,crt.light_cyan);
--   crt.put_lc(4,1,"This is a yellow string over a default black background",crt.yellow);

--   crt.new_line;

--            Crt.put_line("Code :" & Integer'Image(Key) & " => Control key");

-------------------------------------------------------------------------------
end Limits;
-------------------------------------------------------------------------------
