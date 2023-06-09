
-------------------------------------------------------------------

with Class_screen;
use  Class_screen;
package body Class_window_control is

  procedure add_to_list(p_w:in P_Root_window; ch:in Character) is
  begin
    if the_last_win < MAX_ITEMS then
      the_last_win := the_last_win + 1;
      the_windows( the_last_win ) := ( p_w, ch );
    else
      window_fatal("Cannot register window");
    end if;
  end add_to_list;

  procedure remove_from_list( p_w:in P_Root_window ) is
  begin
    for i in 1 .. the_last_win loop                 -- Look at
      if the_windows( i ).p_w = p_w then            -- Found
        for j in i .. the_last_win-1 loop           -- Delete
          the_windows( j ) := the_windows( j+1 );   --  move up
        end loop;
        the_last_win := the_last_win - 1; exit;     -- Finish
      end if;
    end loop;
  end remove_from_list;

  procedure top( p_w:in P_Root_window ) is
  begin
    for i in 1 .. the_last_win loop               --
      if the_windows( i ).p_w = p_w then          -- Found
        declare
          tmp : Active_window := the_windows( i );
        begin
          for j in i .. the_last_win-1 loop       -- Move down
            the_windows( j ) := the_windows( j+1 );
          end loop;
          the_windows( the_last_win ) := tmp;     -- New top
        end;
        exit;
      end if;
    end loop;
  end top;

  procedure find( p_w:out P_Root_window; ch:in Character ) is
  begin
    p_w := null;
    for i in 1 .. the_last_win loop
      if the_windows( i ).a_ch = ch then
        p_w := the_windows( i ).p_w;
        exit;
      end if;
    end loop;
  end find;

  procedure send_to_top( ch:in Character ) is
  begin
    if the_last_win >= 1 then
      send_to( the_windows(the_last_win).p_w.all, ch );
    end if;
  end send_to_top;

  procedure switch_to_top is
  begin
    if the_last_win >= 1 then
      switch_to( the_windows(the_last_win).p_w.all );
    end if;
  end switch_to_top;

  procedure switch_away_from_top is
  begin
    if the_last_win >= 1 then
      switch_away( the_windows(the_last_win).p_w.all );
    end if;
  end switch_away_from_top;

  -- Of course this allow overlapping wondows

  procedure write_to( p_w:in P_Root_window;
                      x,y:in Positive; mes:in String ) is
    abs_x_crd : Positive := about( p_w.all, abs_X );
    abs_y_crd : Positive := about( p_w.all, abs_Y );
  begin
    position_cursor( abs_x_crd+x-1, abs_y_crd+y-1 );
    Class_screen.put( mes );
  end write_to;

  -- Of course this allow overlapping wondows

  procedure hide_win( p_w:in P_Root_window ) is
    abs_x_crd : Positive := about( p_w.all, abs_X );
    abs_y_crd : Positive := about( p_w.all, abs_Y );
    width     : Positive := about( p_w.all, TOP );
    height    : Positive := about( p_w.all, LEFT );
    spaces    : String( 1 .. width ) := ( others => ' ' );
  begin
    for h in 1 .. height loop
      position_cursor( abs_x_crd, abs_y_crd+h-1 );
      Class_screen.put( spaces );
    end loop;
  end hide_win;

  procedure window_fatal( mes:in String ) is
  begin
    position_cursor( 1, 1 );
    put( "Window fatal error: "& mes );
  end window_fatal;

end Class_window_control;
