--::::::::::
--unsigned.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
package unsigned is

-- type byte      is mod 2** 8; for      byte'size use  8;
-- type word      is mod 2**16; for      word'size use 16;
-- type long_word is mod 2**32; for long_word'size use 32;

   type byte      is range 0 .. (2**8  - 1); for      byte'size use  8;
   type word      is range 0 .. (2**16 - 1); for      word'size use 16;
   type long_word is range 0 .. (2**31 - 1); for long_word'size use 32;
-- type long_word is new long_integer;       for long_word'size use 32;

   type    byte_string     is array (positive range <>) of byte;
   subtype word_bytes      is byte_string (1..2);
   subtype long_word_bytes is byte_string (1..4);

   function value (item: byte_string) return standard.string;
   pragma inline (value);

   function value (item: standard.string) return byte_string;
   pragma inline (value);

   function value (item: word_bytes) return word;
   function value (item: word) return word_bytes;

   function value (item: long_word_bytes) return long_word;
   function value (item: long_word) return long_word_bytes;

end unsigned;
