pragma Source_Reference (000659, "x80_ed.ada");


-- Machine dependent I/O
-- Currently assume input is from a terminal supporting
-- input of ANSI escape sequences

package Pack_Md_Io is
  procedure Put( Ch :in Character );           -- Put char
  procedure Put( Str:in String );              -- Put string
  procedure Get_Immediate( Ch:out Character ); -- no echo
end Pack_Md_Io;
