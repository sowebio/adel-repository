--+------------------+---------------------------------------------------------
--| o   o  ooo    oo | @filename v04-crt.adb
--| o   o o   o  o o | @brief    V04 library, CRT functions
--| o   o o   o o  o | @author   Stephane Riviere
--|  o o  o   o ooooo| @date     03/09/04
--|   o    ooo     o | Licence, contributors & copyrights listed in v04-lcc.txt
--+------------------+---------------------------------------------------------

with v04.crt; use v04;

procedure Crt_Test is

-------------------------------------------------------------------------------
function byte2string (I : integer) return string is

s : constant string := integer'image(I);

begin
   if    s'length = 2 then
         return "00" & s(2..S'last);
   elsif s'length = 3 then
         return "0"  & s(2..S'last);
   else
         return        s(2..S'last);
   end if;

end byte2string;

-------------------------------------------------------------------------------
procedure ascii_table is

begin

   crt.new_line;

   for I in character'pos(character'first)..character'pos(character'last) loop

       if (I rem 8) = 0 then
          crt.new_line;

       end if;

       if (I = 7)  or     -- Bell
          (I = 8)  or     -- Backspace
          (I = 9)  or     -- Tabulation
          (I = 10) or     -- Line feed
          (I = 13) then   -- Carriage return
          crt.put(byte2string(I) & " :    ");
       else
          crt.put(byte2string(I) & " : " & character'Val(I) & "  ");
       end if;

   end loop;

   crt.new_line;
   crt.new_line;

end ascii_table;

-------------------------------------------------------------------------------

Key : integer := Crt.K_Null;

begin

   crt.clear;
   crt.beep;

   crt.put_lc(1,1,"This is a string, by defaults settings, white over black");
   crt.put_lc(2,1,1234567890);

   crt.put_lc(3,1,"This is a black string over a light cyan background",crt.black,crt.light_cyan);
   crt.put_lc(4,1,"This is a yellow string over a default black background",crt.yellow);

   crt.color_set(crt.white, crt.black);

   crt.cursor_move(6,1);

   crt.put_line("Use Lucida Console font to see codepages switching.");
   crt.put_line("Use 80 lines console buffer to see all ascii tables.");
   crt.new_line;

   crt.codepage_set(crt.latin_1); --- Latin by default
   crt.put_line("Ascii table with Latin_1, page code No " & integer'image(crt.codepage_get));
   ascii_table;

   crt.codepage_set(crt.oem);
   crt.put_line("Ascii table with Oem, page code No " & integer'image(crt.codepage_get));
   ascii_table;

   crt.codepage_set(crt.latin_1);

   crt.put_line("Press any key to test, use 'q' or 'Q' to quit.");
   crt.new_line;

   while Key /= Character'Pos('Q') and Key /= Character'Pos('q') loop

      Key := Crt.Key_Read;

      if    Key < 32  then
            Crt.put_line("Code :" & Integer'Image(Key) & " => Control key");
            if Key = Crt.K_Escape then
               Crt.Beep;
            end if;
      elsif Key < 256 then
            Crt.put_line("Code :" & Integer'Image(Key) & " => Printable character : " & Character'Val(Key));
      else
            Crt.put_line("Code :" & Integer'Image(Key) & " => Extended key") ;
      end if;

   end loop;

-------------------------------------------------------------------------------
end Crt_test;
-------------------------------------------------------------------------------
