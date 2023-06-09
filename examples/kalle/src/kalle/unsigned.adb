--::::::::::
--unsigned.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with unchecked_conversion;

package body unsigned is

    use_dir_cvt: constant boolean := TRUE;

    function to_long_word (buf: byte_string) return long_word is
       item: long_word := 0;
    begin
       for b in reverse buf'range
       loop
	 item := item * 2 ** byte'size + long_word (buf(b));
       end loop;
       return item;
    end;

    function to_byte_string (item: long_word) return long_word_bytes is
       buf: long_word_bytes := (others => 0);
       tmp: long_word := item;
    begin
       for b in buf'range
       loop
	 buf (b) := byte (tmp mod 2 ** byte'size);
	 tmp     := tmp  /  2 ** byte'size;
	 exit when tmp = 0;
       end loop;
       return buf;
    end;

    function value (item: long_word_bytes) return long_word is
    begin
      return to_long_word (item);
    end;

    function value (item: word_bytes) return word is
    begin
      return word (to_long_word (item));
    end;

    function value (item: word) return word_bytes is
    begin
       return to_byte_string (long_word (item)) (word_bytes'range);
    end;

    function value (item: long_word) return long_word_bytes is
    begin
       return to_byte_string (item);
    end;

    function value (item:  byte_string) return standard.string is

--    subtype bs_type is byte_string (item'range);
--    subtype ss_type is      string (item'range);

--    function to_ss is new unchecked_conversion (bs_type, ss_type);
    begin
--    if use_dir_cvt
--    then
--       return to_ss (item);
--    else
	 declare
	    x: standard.string (item'range);
	    b: constant := character'pos (character'last) + 1;
	 begin
	    if character'pos(character'last) = byte'pos(byte'last)
	    then
	       for i in x'range
	       loop
		    x(i) := standard.character'val (item (i));
	       end loop;
	    else
	       for i in x'range
	       loop
		    x(i) := standard.character'val(item(i) rem b);
	       end loop;
	    end if;
	    return x;
	 end;
--    end if;
    end;

    function  value (item: standard.string) return  byte_string is

--    subtype bs_type is byte_string (item'range);
--    subtype ss_type is      string (item'range);

--    function to_bs is new unchecked_conversion (ss_type, bs_type);
    begin
--    if use_dir_cvt
--    then
--       return to_bs (item);
--    else
	 declare
	    x:  byte_string (item'range);
	 begin
	    for i in x'range
	    loop
		 x(i) := standard.character'pos (item (i));
	    end loop;
	    return x;
	 end;
--    end if;
    end;

end unsigned;
