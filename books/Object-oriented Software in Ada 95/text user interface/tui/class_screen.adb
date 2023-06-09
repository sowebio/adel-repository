
with Pack_md_io; use  Pack_md_io;
package body Class_screen is
  PREFIX: CONSTANT String := Character'Val(27) & "[";
  procedure put( n:in Positive );          -- Write decimal number

  procedure put( ch :in Character ) is
  begin
    Pack_md_io.put( ch );
  end put;

  procedure put( str:in String ) is
  begin
    Pack_md_io.put( str );
  end put;

  procedure clear_screen is                 -- Clear screen
  begin
    put( PREFIX & "2J");
  end clear_screen;

  procedure position_cursor(col:in Positive; row:in Positive) is
  begin
    put( PREFIX ); put(row); put(";"); put(col); put("H");
  end position_cursor;

  procedure put( n:in Positive ) is   -- Write decimal number
  begin
    if n >= 10 then put( n / 10 ); end if;
    put( Character'Val(n rem 10 + Character'Pos('0') ) );
  end put;

end Class_screen;
