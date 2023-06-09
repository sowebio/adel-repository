pragma Source_Reference (000428, "x80_ed.ada");

-- BEGIN file io.c
-- /*
--  *   The C function to turn of echoing
--  *
--  *   Works with the GNAT implementation on Linux, Win95
--  *   Note:
--  *    Uses Unix API call to turn of echoing
--  *   Compile: gcc /c io.c
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
-- END file io.c


-- =========================================================================
-- 
-- Alternative BODY for Raw_io
--   All the I/O is performed by code written in C
--   Implemented in C code that following this Ada body
-- 

-- WITH Interfaces.C;
-- USE  Interfaces.C;
-- PACKAGE BODY raw_io IS
-- 
-- PROCEDURE get_immediate( ch:OUT Character ) IS
--    FUNCTION c_get_char RETURN Char;
--    PRAGMA import (C, c_get_char, "c_get_char");
-- BEGIN
--    ch := to_ada( c_get_char );
-- END get_immediate;
-- 
-- PROCEDURE put( ch:IN Character ) IS
--    PROCEDURE c_put_char( ch:IN Char );
--    PRAGMA import (C, c_put_char, "c_put_char");
-- BEGIN
--    c_put_char( to_c( ch ) );
-- END put;
-- 
-- PROCEDURE put( str:IN String ) IS
--    PROCEDURE c_put_str( str:IN Char_array );
--    PRAGMA import (C, c_put_str, "c_put_str");
-- BEGIN
--    c_put_str( to_c( str, append_nul=>TRUE ) );
-- END put;
-- 
-- END raw_io;
-- 
-- C Interface for above Ada body
-- 
-- #define UNIX     1       /* Using GNAT on a UNIX based system all IO in C*/
-- #define DOS      2       /* Using GNAT on a DOS based system */
-- #define WIN95    3       /* Using GNAT on a WIN 95/NT system */
-- #define UNIXs    WIN95   /* Using GNAT on a UNIX based system */
-- 
-- /* Notes:
--  *  ENVIRONMENT = UNIX  / Uses Unix API for all I/O
--  *              = DOS   / Uses DOS API (Via DJGPP C) for all I/O
--  *              = GNAT  / Uses Ada95 Input & Output procedures +
--  *                             Unix API call to turn of echoing of input
--  *              = UNIXs / Uses Ada95 Input & Output procedures +
--  *                             Unix API call to turn of echoing of input
--  */
-- 
-- #define ENVIRONMENT	WIN95    /* Environment for program */
-- 
-- 
-- #define ESC	'\033'
-- 
-- 
-- #if ENVIRONMENT == DOS
-- # include <pc.h>
-- # include <keys.h>
-- #endif
-- #include <stdio.h>
-- 
-- typedef enum { false, true } bool;
-- 
-- char c_get_char();
-- void c_put_char( char ch );
-- void c_put_str( char *str );
-- 
-- 
-- #if ENVIRONMENT == DOS
-- /*
--  * Make function keys and arrow keys return two characters
--  * E.G. Right arrow returns (char) 0, 'M'
--  *      Left  arrow         (char) 0, 'K'
--  */
-- 
-- char c_get_char()
-- {
--   int c;
--   static char the_ch;                 /* Remembered character */
--   static bool prev_char = false;      /* There is remembered ch */
--   if ( prev_char ) {
--     prev_char = false; return the_ch;
--   }
--   c = getkey();                        /* Get char no echo */
--   if ( c & 0x100 ) {                   /* Function / Arrow key */
--     prev_char = true; 
--     the_ch = (char) ( c & 0xFF );
--     return (char) 0;                   /* Marker */
--   }
--   return (char) (c & 0xFF);            /* Ordinary character */
-- }
-- #endif
-- 
-- #if ENVIRONMENT == UNIX
-- /*
--  * Set the terminal mode to /echo /icanon on first read
--  * reset when get ^E
--  *
--  */
-- 
-- #include <termios.h>
-- #include <unistd.h>
-- 
-- char c_get_char()
-- {
--   static bool first_time = true;
--   static tcflag_t c_lflag;
--   static int fd = STDOUT_FILENO;
--   static struct termios termios_data;
--   char c;
-- 
--   if ( first_time )
--   {
--     tcgetattr( fd, &termios_data );
--     c_lflag = termios_data.c_lflag;
--     termios_data.c_lflag = termios_data.c_lflag & ( ~(ECHO|ICANON) );
--     tcsetattr( fd, TCSANOW, &termios_data );
--     first_time = false;
--   }
--   c = getchar();
--   if ( c == '\005')
--   {
--     termios_data.c_lflag = c_lflag;
--     tcsetattr( fd, TCSANOW, &termios_data );
--   }
--   return (char) (c & 0xFF);            /* Ordinary character */
-- }
-- 
-- #endif
-- 
-- #if ENVIRONMENT == WIN95
-- 
-- /*
--  * Uses the C function c_no_echo to turn of echoing of input
--  *
--  */
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
-- 
-- #endif
-- 
-- #if ENVIRONMENT == UNIX || ENVIRONMENT == DOS
-- 
-- /*
--  * C function to write characters immediately to the terminal
--  */
-- 
-- void c_put_char( char ch )
-- {
--   fputc(ch, stdout); fflush( stdout );  /* Output ch */
-- }
-- 
-- void c_put_str( char *str )
-- {
--   while (*str) fputc(*str++, stdout);  /* Output String */
--   fflush( stdout );                    /* Flush buffer */
-- }
-- 
-- #endif
-- 
-- =========================================================================

with Interfaces.C, Ada.Text_Io;
use  Interfaces.C, Ada.Text_Io;
package body Raw_Io is

  procedure Get_Immediate( Ch:out Character) is
    procedure C_No_Echo;
    pragma Import (C, C_No_Echo, "c_no_echo");   -- Turn off echo
  begin
    if First_Time then
      C_No_Echo; First_Time := False;
    end if;
    Ada.Text_Io.Get_Immediate(Ch);
    if Character'Pos(Ch) = 10 then               -- Real Return ch
      Ch := Character'Val(13);
    end if;
  end Get_Immediate;

  procedure Put( Ch:in Character ) is            -- Raw write
  begin
    Ada.Text_Io.Put( Ch ); Ada.Text_Io.Flush;
  end Put;

  procedure Put( Str:in String ) is              -- Raw write
  begin
    Ada.Text_Io.Put( Str ); Ada.Text_Io.Flush;
  end Put;

end Raw_Io;
