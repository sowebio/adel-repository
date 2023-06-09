

with Simple_io, Class_window, Class_board, Pack_globals;
use  Simple_io, Class_window, Class_board, Pack_globals;
function user_input( cb_mes:in String ) return String is
  move: Integer; last: Positive;
begin
  clear( p_win_r.all );                    -- Clear
  get( cb_mes, move, last );               -- to int
  if valid( game, move ) then              -- Valid
    add( game, move, player );             -- to board
    update( game, p_win_brd );
    case state( game ) is                  -- Game is
      when Win       =>
        put(p_win_r.all, " " & player & " wins");
      when PLAYABLE  =>
        case player is                     -- Next player
          when 'X'    => player := 'O';    --  'X' => 'O'
          when 'O'    => player := 'X';    --  'O' => 'X'
          when others => null;             --
        end case;
        put( p_win_r.all, " Player " & player );
      when DRAW      =>
        put( p_win_r.all, " It's a draw ");
    end case;
  else
    put(p_win_r.all, " " & player & " Square invalid");
  end if;
  return "";
exception
  when others =>
    put(p_win_r.all, " " & player & " re-enter move");
    return "";
end user_input;
