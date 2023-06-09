---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:51:03 PM BST  --
---------------------------------------------------------------------
/*///////////////////////////////////////////////////////////////////////////
// I / O     I n t e r f a c e   f o r   A d a 9 5   P r o g r a m s       //
///////////////////////////////////////////////////////////////////////////*/

#define UNIX     1       /* Using GNAT on a UNIX based system all IO in C*/
#define WIN95    3       /* Using GNAT on a WIN 95/NT system */

/* Notes:
 *  ENVIRONMENT = UNIX  - Uses Unix API for all I/O
 *              = WIN95 - Uses Ada95 Input & Output procedures +
 *                             Unix API call to turn of echoing of input
 */

#define ENVIRONMENT	UNIX    /* Environment for program */

/*///////////////////////////////////////////////////////////////////////////
// D o   N o t    C h a n g e   b e y o n d     h e r e                    //
///////////////////////////////////////////////////////////////////////////*/

#define ESC	'\033'

#include <stdio.h>

typedef enum { false, true } bool;

char c_get_char();

#if ENVIRONMENT == UNIX
/*
 * Set the terminal mode to -echo -icanon on first read
 * reset when get ^E
 *
 */

#include <termios.h>
#include <unistd.h>

static tcflag_t c_lflag;
static int fd = 1;                              /* STDOUT_FILENO; */
static struct termios termios_data;

void c_no_echo()
{
  tcgetattr( fd, &termios_data );
  c_lflag = termios_data.c_lflag;
  termios_data.c_lflag = termios_data.c_lflag & ( ~(ECHO|ICANON|ECHOCTL) );
  tcsetattr( fd, TCSANOW, &termios_data );
}

char c_get_char()
{
  char c;

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

void c_no_echo()
{
#ifdef WIN95_HAS_TERMIOS
  static tcflag_t c_lflag;
  static int fd = STDIN_FILENO;
  static struct termios termios_data;
  tcgetattr( fd, &termios_data );
  c_lflag = termios_data.c_lflag;
  termios_data.c_lflag = termios_data.c_lflag & (~ECHO);
  tcsetattr( fd, TCSANOW, &termios_data );
#endif
}

#endif
