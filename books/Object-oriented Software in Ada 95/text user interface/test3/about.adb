
with Class_window, Pack_globals;
use  Class_window, Pack_globals;
function about( cb_mes:in String ) return String is
begin
  clear( p_win_bnr.all ); position( p_win_bnr.all, 17, 1 );
  put( p_win_bnr.all, "Written in Ada 95");
  return "";
end about;
