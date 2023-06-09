
with Class_window, Class_board, Pack_globals;
use  Class_window, Class_board, Pack_globals;
procedure re_start( first_player:in Character ) is
begin
  player := first_player;                  -- Start with
  reset( game );                           -- Reset Board
  display_board(game, p_win_brd );         -- Display
  clear( p_win_r.all );                    -- Status info
  put( p_win_r.all, " Player " & player ); -- Player name
end re_start;
