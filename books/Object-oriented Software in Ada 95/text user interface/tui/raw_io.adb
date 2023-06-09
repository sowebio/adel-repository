
-- begin file io.c
-- /*
--  *   The C function to turn of echoing
--  *
--  *   Works with the GNAT implementation on Linux, Win95
--  *   Note:
--  *    Uses Unix API call to turn of echoing
--  *   Compile: gcc -c io.c
--  */
--
-- 
-- #include <termios.h>
-- #include <unistd.h>
-- 
-- void c_no_echo()
-- {
--   static tcflag_t c_lflag;
--   static int fd = STDIN_FILENO;
--   static struct termios termios_data;
--   tcgetattr( fd, &termios_data );
--   c_lflag = termios_data.c_lflag;
--   termios_data.c_lflag = termios_data.c_lflag & (~ECHO);
--   tcsetattr( fd, TCSANOW, &termios_data );
-- }
-- end file io.c


-- Alternative body for Raw_io
--   All the I/O is performed by code written in C
--   Implemented in io.c
--

-- with Interfaces.C;
-- use  Interfaces.C;
-- package body raw_io is
-- 
-- procedure get_immediate( ch:out Character ) is
--    function c_get_char return Char;
--    pragma import (C, c_get_char, "c_get_char");
-- begin
--    ch := to_ada( c_get_char );
-- end get_immediate;
-- 
-- procedure put( ch:in Character ) is
--    procedure c_put_char( ch:in Char );
--    pragma import (C, c_put_char, "c_put_char");
-- begin
--    c_put_char( to_c( ch ) );
-- end put;
-- 
-- procedure put( str:in String ) is
--    procedure c_put_str( str:in Char_array );
--    pragma import (C, c_put_str, "c_put_str");
-- begin
--    c_put_str( to_c( str, append_nul=>TRUE ) );
-- end put;
-- 
-- end raw_io;

with Interfaces.C, Ada.Text_io;
use  Interfaces.C, Ada.Text_io;
package body Raw_io is

  procedure get_immediate( ch:out Character) is
    procedure c_no_echo;
    pragma import (C, c_no_echo, "c_no_echo");   -- Turn off echo
  begin
    if first_time then
      c_no_echo; first_time := false;
    end if;
    Ada.Text_io.get_immediate(ch);
    if Character'Pos(ch) = 10 then               -- Real Return ch
      ch := Character'Val(13);
    end if;
  end get_immediate;

  procedure put( ch:in Character ) is            -- Raw write
  begin
    Ada.Text_io.put( ch ); Ada.Text_io.flush;
  end put;

  procedure put( str:in String ) is              -- Raw write
  begin
    Ada.Text_io.put( str ); Ada.Text_io.flush;
  end put;

end Raw_io;
