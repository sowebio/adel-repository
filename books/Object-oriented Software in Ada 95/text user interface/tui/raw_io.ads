
-- All I/O is RAW
--     Write :chs are immediately written to the terminal
--     Read  :chs are immediately available to the program
--           Unfortunately input characters are echoed using
--           get_immediate in Ada.Text_IO

package Raw_io is
  procedure get_immediate( ch:out Character );
  procedure put( ch:in Character );
  procedure put( str:in String );
private
  first_time : Boolean := TRUE;
end Raw_io;
