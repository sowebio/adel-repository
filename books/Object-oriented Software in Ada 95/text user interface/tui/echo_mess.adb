
with Class_window, Pack_globals;
use  Class_window, Pack_globals;
function echo_mess( cb_mes:in String ) return String is
begin
  clear( p_w1.all );
  put( p_w1.all, cb_mes ); new_line( p_w1.all );
  return "";
end echo_mess;
