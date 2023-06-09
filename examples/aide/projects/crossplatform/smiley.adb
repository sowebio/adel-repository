WITH Ada.Text_Io;
WITH Screen;

PROCEDURE Smiley IS

  ------------------------------------------------------------------
  --|
  --| Draws a "smiley face" in the center of the terminal screen
  --|
  --| Author: Michael B. Feldman, The George Washington University
  --| Last Modified: July 1995
  --|
  ------------------------------------------------------------------

BEGIN -- Smiley

  Screen.Clearscreen;
  Screen.Beep;
  DELAY 0.1;
  Screen.Beep;
  DELAY 0.1;
  Screen.Beep;
  DELAY 0.1;
  Screen.Movecursor (Row => 7, Column => 34);
  Ada.Text_Io.Put (Item =>    "HAVE A NICE DAY!");
  Screen.Movecursor (Row => 9, Column => 39);
  Ada.Text_Io.Put (Item =>     "_____");
  Screen.Movecursor (Row => 10, Column => 37);
  Ada.Text_Io.Put (Item =>   "/       \");
  Screen.Movecursor (Row => 11, Column => 36);
  Ada.Text_Io.Put (Item =>  "/         \");
  Screen.Movecursor (Row => 12, Column => 35);
  Ada.Text_Io.Put (Item => "|           |");
  Screen.Movecursor (Row => 13, Column => 35);
  Ada.Text_Io.Put (Item => "|   O   O   |");
  Screen.Movecursor (Row => 14, Column => 36);
  Ada.Text_Io.Put (Item =>  "\    o    /");
  Screen.Movecursor (Row => 15, Column => 37);
  Ada.Text_Io.Put (Item =>   "\ \___/ /");
  Screen.Movecursor (Row => 16, Column => 38);
  Ada.Text_Io.Put (Item =>    "\     /");
  Screen.Movecursor (Row => 17, Column => 39);
  Ada.Text_Io.Put (Item =>     "-----");
  Screen.Movecursor (Row => 24, Column => 1);

END Smiley;