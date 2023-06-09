
with Class_window, Pack_globals;
use  Class_window, Pack_globals;
function write_name( cb_mes:in String ) return String is
begin
  put( p_w1.all, "Written by Mike Smith" ); new_line( p_w1.all );
  put( p_w1.all, "University of Brighton" ); new_line( p_w1.all );
  new_line( p_w2.all ); put( p_w2.all, "Written by Mike Smith" );
  new_line( p_w2.all ); put( p_w2.all, "University of Brighton" );
  return "";
end write_name;
