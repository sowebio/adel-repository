---------------------------------------------------------------------
-- (C) Michael A Smith 1993-1997                                   --
-- Taken from the book Object-Oriented Software in Ada 95          --
--    Published by International Thomson Publishing                --
--    See http://www.brighton.ac.uk/ada95/home.html                --
-- Version automatically created  Tue Dec 02 13:14:44 GMT 1997     --
---------------------------------------------------------------------

-- The TUI
--   To compile using the GNAT Compiler on Linux, Windows 95/NT
--
--   gnatchop -w x72_tui.ada         -- Split into units
--   gcc -c io.c                     -- Compile C interface
--   gnatmake main.adb -largs io.o   -- Compile program
--
-- Notes:
--   The file io.c is the commented out C code before Raw_io.adb
--   The current I/O system with Windows 95 can not cope with 
--     the fast reception of characters
--   ^C Will Kill the program use ^E to exit
--   ^D Will cause the program to terminate do not use

package Pack_constants is
  VDT_MAX_X    : CONSTANT := 79;      -- Columns on VDT
  VDT_MAX_Y    : CONSTANT := 25;      -- Lines on VDT
  WINDOW_MAX_X : CONSTANT := 79;      -- MAX columns window
  WINDOW_MAX_Y : CONSTANT := 25;      -- MAX lines window

  C_CURSor     : CONSTANT Character := '*';
  C_BLANK      : CONSTANT Character := ' ';
  C_WIN_A      : CONSTANT Character := '#';
  C_WIN_PAS    : CONSTANT Character := '+';
  C_exit       : CONSTANT Character := Character'val(05); --^E
  C_WHERE      : CONSTANT Character := Character'Val(255);
  C_ACTION     : CONSTANT Character := Character'Val(13); --cr
  C_SWITCH     : CONSTANT Character := Character'Val(09); --ht
  C_MENU       : CONSTANT Character := Character'Val(27); --esc
  C_DEL        : CONSTANT Character := Character'Val(08); --^B
  C_NO_CHAR    : CONSTANT Character := Character'Val(00);

  C_LEFT       : CONSTANT Character := Character'Val(12); --^L
  C_RIGHT      : CONSTANT Character := Character'Val(18); --^R
  C_UP         : CONSTANT Character := Character'Val(21); --^U
  C_DOWN       : CONSTANT Character := Character'Val(04); --^D
end Pack_constants;
