--::::::::::
--silenlog.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.

package body silent_log is

   procedure error   (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name) is
   begin
      null;
   end;

   procedure warning (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name) is
   begin
      null;
   end;

   procedure message (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name) is
   begin
       null;
   end;

end silent_log;
