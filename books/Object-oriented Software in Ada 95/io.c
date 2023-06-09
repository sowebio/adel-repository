/*///////////////////////////////////////////////////////////////////////////
// I / O     I n t e r f a c e   f o r   A d a 9 5   P r o g r a m s       //
///////////////////////////////////////////////////////////////////////////*/

#define UNIX     1       /* Using GNAT on a UNIX based system all IO in C*/
#define DOS      2       /* Using GNAT on a DOS based system */
#define WIN95    3       /* Using GNAT on a WIN 95/NT system */
#define UNIXs    WIN95   /* Using GNAT on a UNIX based system */

/* Notes:
 *  ENVIRONMENT = UNIX  - Uses Unix API for all I/O
 *              = DOS   - Uses DOS API (Via DJGPP C) for all I/O
 *              = GNAT  - Uses Ada95 Input & Output procedures +
 *                             Unix API call to turn of echoing of input
 *              = UNIXs - Uses Ada95 Input & Output procedures +
 *                             Unix API call to turn of echoing of input
 */

#define ENVIRONMENT	WIN95    /* Environment for program */

/*///////////////////////////////////////////////////////////////////////////
// D o   N o t    C h a n g e   b e y o n d     h e r e                    //
///////////////////////////////////////////////////////////////////////////*/

#define ESC	'\033'


#if ENVIRONMENT == DOS
# include <pc.h>
# include <keys.h>
#endif
#include <stdio.h>

typedef enum { false, true } bool;

char c_get_char();
void c_put_char( char ch );
void c_put_str( char *str );


#if ENVIRONMENT == DOS
/*
 * Make function keys and arrow keys return two characters
 * E.G. Right arrow returns (char) 0, 'M'
 *      Left  arrow         (char) 0, 'K'
 */

char c_get_char()
{
  int c;
  static char the_ch;                 /* Remembered character */
  static bool prev_char = false;      /* There is remembered ch */
  if ( prev_char ) {
    prev_char = false; return the_ch;
  }
  c = getkey();                        /* Get char no echo */
  if ( c & 0x100 ) {                   /* Function / Arrow key */
    prev_char = true; 
    the_ch = (char) ( c & 0xFF );
    return (char) 0;                   /* Marker */
  }
  return (char) (c & 0xFF);            /* Ordinary character */
}

void c_no_echo()
{
}

#endif

#if ENVIRONMENT == UNIX
/*
 * Set the terminal mode to -echo -icanon on first read
 * reset when get ^E
 *
 */

#include <termios.h>
#include <unistd.h>

/* all of these are POSIX and should have been defined by unistd.h: */
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

char c_get_char()
{
  static bool first_time = true;
  static tcflag_t c_lflag;
  static int fd = STDOUT_FILENO;
  static struct termios termios_data;
  char c;

  if ( first_time )
  {
    tcgetattr( fd, &termios_data );
    c_lflag = termios_data.c_lflag;
    termios_data.c_lflag = termios_data.c_lflag & ( ~(ECHO|ICANON) );
    tcsetattr( fd, TCSANOW, &termios_data );
    first_time = false;
  }
  c = getchar();
  if ( c == '\005')
  {
    termios_data.c_lflag = c_lflag;
    tcsetattr( fd, TCSANOW, &termios_data );
  }
  return (char) (c & 0xFF);            /* Ordinary character */
}

#endif

#if ENVIRONMENT == WIN95

/*
 * Uses the C function c_no_echo to turn of echoing of input
 *
 */

#include <termios.h>
#include <unistd.h>

/* all of these are POSIX and should have been defined by unistd.h: */
#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif
#ifndef STDERR_FILENO
#define STDERR_FILENO 2
#endif

void c_no_echo()
{
  static tcflag_t c_lflag;
  static int fd = STDIN_FILENO;
  static struct termios termios_data;
  tcgetattr( fd, &termios_data );
  c_lflag = termios_data.c_lflag;
  termios_data.c_lflag = termios_data.c_lflag & (~ECHO);
  tcsetattr( fd, TCSANOW, &termios_data );
}

#endif

#if ENVIRONMENT == UNIX || ENVIRONMENT == DOS

/*
 * C function to write characters immediately to the terminal
 */

void c_put_char( char ch )
{
  fputc(ch, stdout); fflush( stdout );  /* Output ch */
}

void c_put_str( char *str )
{
  while (*str) fputc(*str++, stdout);  /* Output String */
  fflush( stdout );                    /* Flush buffer */
}

#endif
