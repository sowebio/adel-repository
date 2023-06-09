

-- Machine dependent I/O
-- Currently assume input is from a terminal supporting
-- input of ANSI escape sequences

package Pack_md_io is
  procedure put( ch :in Character );           -- Put char
  procedure put( str:in String );              -- Put string
  procedure get_immediate( ch:out Character ); -- no echo
end Pack_md_io;
