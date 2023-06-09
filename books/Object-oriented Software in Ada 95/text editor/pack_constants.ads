pragma Source_Reference (000001, "x80_ed.ada");
---------------------------------------------------------------------
-- (C) Michael A Smith 1993-2000 University of Brighton            --
--     http://www.it.brighton.ac.uk/~mas                           --
---------------------------------------------------------------------
-- Version automatically created  Sat 29 Apr 2000 08:50:57 PM BST  --
---------------------------------------------------------------------
-- The editor
--   To compile using the GNAT Ada 95 Compiler on Windows 95/NT, Linux
-- 
--   gnatchop /w x80_ed.ada          // Split into units
--   gcc /c io.c                     // Compile C interface
--   gnatmake ed.adb /largs io.o     // Compile program
-- 
-- Notes:
--   The file io.c is the commented out C code before Raw_io.adb
--   The current I/O system with Windows 95 can not cope with 
--     the fast reception of characters
--   ^C Will Kill the program use ^E to exit
--   ^D Will cause the program to terminate do not use
-- 
--   In the package Pack_constants
--     ANSI_IN_DEL currently must be set to FALSE for Windows 95/NT
--     as the ANSI escape sequences for Insert and Delete lines are
--     not supported.
--     Using this setting will slow down the refreshing of the screen.


--[pack_constants.ads] Specification
package Pack_Constants is
  Max_Answer     : constant := 80;
  Lines_On_Screen: constant := 25;
  Page_Rack      : constant := Lines_On_Screen/2;

  C_Left         : constant Character := Character'Val(012);
  C_Right        : constant Character := Character'Val(018);
  C_Up           : constant Character := Character'Val(021);
  C_Down         : constant Character := Character'Val(011);

  C_Page_Up      : constant Character := Character'Val(023);
  C_Page_Down    : constant Character := Character'Val(024);

  C_Quit         : constant Character := Character'Val(005);
  C_Debug        : constant Character := Character'Val(020);
  C_Refresh      : constant Character := Character'Val(025);
  C_Del          : constant Character := Character'Val(008);

  C_Open         : constant Character := Character'Val(001);
  C_Close        : constant Character := Character'Val(002);
  C_Set_Fn       : constant Character := Character'Val(006);

  C_Next         : constant Character := Character'Val(007);

  Ansi_Ins_Del   : constant Boolean   := False;
end Pack_Constants;
