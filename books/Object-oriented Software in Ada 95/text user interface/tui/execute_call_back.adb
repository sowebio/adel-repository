
with Class_window, Pack_globals;
use  Class_window, Pack_globals;
function execute_call_back(cb_mes:in String) return String is
  win : Window;
begin
  put( p_w1.all, "Start [" & cb_mes & "]" ); new_line( p_w1.all );
  framework( win, 1, 17, 16, 5 );
  for i in 1 .. 10 loop
    put( win, i ); put( win, " " );
    put( win, cb_mes ); new_line( win );
  end loop;
  put( p_w1.all, "End   [" & cb_mes & "]" ); new_line( p_w1.all );
  return "";
end execute_call_back;
