pragma Source_Reference (000414, "x80_ed.ada");

-- All I/O is RAW
--     Write :chs are immediately written to the terminal
--     Read  :chs are immediately available to the program
--           Unfortunately input characters are echoed using
--           get_immediate in Ada.Text_IO

package Raw_Io is
  procedure Get_Immediate( Ch:out Character );
  procedure Put( Ch:in Character );
  procedure Put( Str:in String );
private
  First_Time : Boolean := True;
end Raw_Io;
