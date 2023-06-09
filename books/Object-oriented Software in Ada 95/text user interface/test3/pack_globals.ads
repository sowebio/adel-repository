

with Class_board, Class_window;
use  Class_board, Class_window;
package Pack_globals is
  game      : Board;       -- The board
  p_win_brd : P_Window;    -- Window to display OXO board in
  p_win_bnr : P_Window;    -- Window to display Banner in
  p_win_r   : P_Window;    -- Window to display commentary in
  player    : Character;   -- Either 'X' or 'O'
end Pack_globals;
