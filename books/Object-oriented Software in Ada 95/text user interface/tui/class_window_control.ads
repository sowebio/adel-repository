
with Ada.Finalization, Class_root_window;
use  Ada.Finalization, Class_root_window;
package Class_window_control is

  type Window_control is abstract tagged limited private;
  procedure add_to_list(p_w:in P_Root_window; ch:in Character);
  procedure remove_from_list( p_w:in P_Root_window );
  procedure top( p_w:in P_Root_window );
  procedure find( p_w:out P_Root_window; ch:in Character );

  procedure send_to_top( ch:in Character );
  procedure switch_to_top;
  procedure switch_away_from_top;

  procedure write_to( p_w:in P_Root_window;
                      x,y:in Positive; mes:in String );
  procedure hide_win( p_w:in P_Root_window );
  procedure window_fatal( mes:in String );
private
  type Window_control is
    abstract new Limited_controlled with null record;
  MAX_ITEMS : CONSTANT := 10;
  type Active_window is record           -- Active window
    p_w : P_Root_window;                 -- Window
    a_ch: Character;                     -- Activate character
  end record;

  subtype Window_index is Natural      range 0 .. MAX_ITEMS;
  subtype Window_range is Window_index range 1 .. MAX_ITEMS;
  type    Window_array is array (Window_range) of Active_window;

  the_last_win: Window_index := 0;       -- Last active window
  the_windows : Window_array;            -- All windows
end Class_window_control;
