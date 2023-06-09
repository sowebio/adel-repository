--::::::::::
--progrlog.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with system_log;
with calendar;
generic
   local:  string    := "log";
   global: string    := system_log.owner_name;
   separator: string := "-";
package program_log is

   subtype time is calendar.time;
   owner_name: constant string := global & separator & local;
   start_time: constant time   := system_log.start_time;

   procedure error   (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name)
	     renames system_log.error;
   procedure warning (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name)
             renames system_log.warning;
   procedure message (msg:    string;
		      moment: time   := start_time;
		      owner:  string := owner_name)
             renames system_log.message;
   procedure assert  (hypothesis: boolean;
                          false_msg: string := "";
                           true_msg: string := "";
			     moment: time   := start_time;
			      owner: string := owner_name)
             renames system_log.assert;

    assertion_failed: exception renames system_log.assertion_failed;

end;
