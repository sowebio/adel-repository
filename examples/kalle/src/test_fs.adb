--::::::::::
--test_fs.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with text_io, format_string;
use  text_io;

procedure test_fs is
--  function argument (n: natural) return string renames integer'image;
  function format is new format_string (natural'image);
begin
   put_line (format (" [%40:%50:%60]"));
   put_line (format (" [%40:%50:%%60]"));
   put_line (format ("]%"));
end;
