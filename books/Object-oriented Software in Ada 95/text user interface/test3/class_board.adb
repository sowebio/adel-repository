
package body Class_board is

  function  valid(the:in Board; pos:in Integer) return Boolean is
  begin
    return pos in Board_range and then the.sqrs( pos ) = ' ';
  end valid;

  procedure add( the:in out Board; pos:in Integer;
                 piece:in Character ) is
  begin
    the.last := pos;
    the.sqrs( pos ) := piece;
    the.moves := the.moves + 1;
  end add;

  function  state( the:in Board ) return Game_state is
    type Win_line      is array( 1 .. 3 ) of Positive;
    type All_win_lines is range 1 .. 8;
    cells: CONSTANT array ( All_win_lines ) of Win_line :=
       ( (1,2,3), (4,5,6), (7,8,9), (1,4,7),
         (2,5,8), (3,6,9), (1,5,9), (3,5,7) ); -- All win lines
    first : Character;
  begin
    for pwl in All_win_lines loop         -- All Pos Win Lines
      first := the.sqrs( cells(pwl)(1) ); -- First cell in line
      if first /= ' ' then                --  Looks promising
        if first = the.sqrs(cells(pwl)(2)) and then
           first = the.sqrs(cells(pwl)(3)) then return WIN;
        end if;
      end if;
    end loop;
    if the.moves >= 9                     -- Check for draw
      then return DRAW;                   --  Board full
      else return PLAYABLE;               --  Still playable
    end if;
  end state;

  procedure reset( the:in out Board ) is
  begin
    the.sqrs  := ( others => ' ');   -- All spaces
    the.last  := 1;                  -- Last move
    the.moves := 0;                  -- No of moves
  end reset;

  procedure display_board( the:in Board; win:in P_Window ) is
  begin
    position( win.all, 1, 2 );
    put(win.all, " 7 | 8 | 9" ); new_line( win.all );
    put(win.all, " ---------" ); new_line( win.all );
    put(win.all, " 4 | 5 | 6" ); new_line( win.all );
    put(win.all, " ---------" ); new_line( win.all );
    put(win.all, " 1 | 2 | 3" ); new_line( win.all );
  end display_board;

  -- Note mapping for squares

  procedure update( the:in Board; win:in P_Window ) is
    type Co_ordinate is ( X , Y );
    type Cell_pos is array ( Co_ordinate ) of Positive;
    type Board    is array ( 1 .. SIZE_TTT ) of Cell_pos;
    pos: CONSTANT Board :=    ( (2,6), (6,6), (10,6),
                                (2,4), (6,4), (10,4),
                                (2,2), (6,2), (10,2) );
  begin
    position( win.all, pos(the.last)(X), pos(the.last)(Y) );
    put( win.all, the.sqrs( the.last ) );    -- Display counter;
  end update;

end Class_board;
