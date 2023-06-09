--::::::::::
--systelog.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with calendar;
package system_log is

   use calendar;

   owner_name: constant string := "sys";
   start_time: constant time   := calendar.clock - 1.0;

   procedure error   (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name);
   procedure warning (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name);
   procedure message (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name);
   procedure assert  (hypothesis: boolean;
		      false_msg: string := "";
		      true_msg: string  := "";
		      moment: time      := start_time;
		      owner: string     := owner_name);

    assertion_failed: exception;

end system_log;
