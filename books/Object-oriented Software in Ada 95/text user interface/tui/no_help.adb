
with Class_window, Pack_globals;
use  Class_window, Pack_globals;
function no_help( cb_mes:in String ) return String is
begin
  put( p_w1.all, " +------------------+" ); new_line( p_w1.all );
  put( p_w1.all, " | There is no Help |" ); new_line( p_w1.all );
  put( p_w1.all, " +------------------+" ); new_line( p_w1.all );
  return "";
end no_help;
