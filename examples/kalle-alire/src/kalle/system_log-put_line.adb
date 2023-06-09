--::::::::::
--sylopuli.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with text_io;
separate (system_log)

procedure put_line (s: string) is
begin
     if s'length > 0 then
        text_io.put_line (s);
     end if;
     null;
end;
