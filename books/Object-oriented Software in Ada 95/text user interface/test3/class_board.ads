---------------------------------------------------------------------
-- (C) Michael A Smith 1993-1997                                   --
-- Taken from the book Object-Oriented Software in Ada 95          --
--    Published by International Thomson Publishing                --
--    See http://www.brighton.ac.uk/ada95/home.html                --
-- Version automatically created  Tue Dec 02 13:14:43 GMT 1997     --
---------------------------------------------------------------------

with Class_window;
use  Class_window;
package Class_board is
  type Board is private;

  type Game_state is ( WIN, PLAYABLE, DRAW );
  function  valid( the:in Board; pos:in Integer ) return Boolean;
  procedure add(the:in out Board; pos:in Integer;
                piece:in Character);
  function  state( the:in Board ) return Game_state;
  procedure display_board( the:in Board; win:in P_Window );
  procedure update( the:in Board; win:in P_Window );
  procedure reset( the:in out Board );
private
  SIZE_TTT: CONSTANT := 9;                    -- Must be 9
  subtype Board_index is Integer range 1 .. SIZE_TTT;
  subtype Board_range is Board_index;
  type    Board_grid  is array( Board_range ) of Character;
  type Board is record
    sqrs  : Board_grid := ( others => ' ');     -- Initialize
    last  : Board_index := 1;                   -- Last move
    moves : Natural := 0;
  end record;
end Class_board;
