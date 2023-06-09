

package Class_screen is
  procedure put( ch :in Character );       -- Put char
  procedure put( str:in String );          -- Put string
  procedure clear_screen;                  -- Clear screen
  procedure position_cursor(col:in Positive; row:in Positive);
private
end Class_screen;
