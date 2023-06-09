--::::::::::
--arrahand.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with choice, program_log;
package body array_handler is

   function min is new choice (integer, "<=");
   function max is new choice (integer, ">=");

   package log is new program_log ("array_handler", toolname);
   use log;

   assfals: constant string := ": false assertion";
   adachks: constant string := ": system checking";
   on_elem: constant string := " on elem";
   on_arry: constant string := " on array";
   on_fild: constant string := " on field";

   procedure info (vol: string; first, last: integer) is
   begin
	log.message (vol & " (" & integer'image(first) & ", " &
				  integer'image(last ) & ")");
   end;

   -----------------------------------------------------------------------

   function length (item: object) return natural is
   begin
      return item.len;
   end;

   -----------------------------------------------------------------------

   function value  (item: object; position: positive) return elem_type is
   begin
      if extra_check then
          assert (position in 1..item.len, "value" & on_elem & assfals);
      end if;
      return item.data (position);
   end;

   -----------------------------------------------------------------------

   function value (item: object;
      first, last: natural) return elem_array is
   begin
      return item.data(max (first, 1).. min (item.len, last));
   end;

   -----------------------------------------------------------------------

   function value  (item: object) return elem_array is
   begin
      return item.data (1..item.len);
   end;

   -----------------------------------------------------------------------

   function empty  (item: object) return boolean is
   begin
      return item.len = 0;
   end;

   -----------------------------------------------------------------------

   function value (item: elem_array) return object is
   begin
      return (width => item'length,
         len   => item'length,
         data  => item);
   end;

   -----------------------------------------------------------------------

   function value (item: elem_type)  return object is
   begin
      return (width  =>  1,
              len    =>  1,
              data   => (1..1 => item));
   end;

   -----------------------------------------------------------------------

  function "&"  (left: object; right: object) return object is
     tmp: object (left.len + right.len);
  begin
     set (tmp, left);  append (right, tmp);
     return tmp;
  end;

   -----------------------------------------------------------------------

   function "&" (left: object; right: elem_array) return object is
      tmp: object(left.len + right'length);
   begin
      set (tmp, left); append (right, tmp);
      return tmp;
   end;

   -----------------------------------------------------------------------

   function "&" (left: elem_array; right: object) return object is
      tmp: object(right.len + left'length);
   begin
      set (tmp, left); append (right, tmp);
      return tmp;
   end;

   -----------------------------------------------------------------------

   function "&"  (left: object; right: elem_type ) return object is
      tmp: object(left.len + 1);
   begin
      set (tmp, left); append (right, tmp);
      return tmp;
   end;

   -----------------------------------------------------------------------

   function "&"  (left: elem_type; right: object) return object is
      tmp: object(right.len + 1);
   begin
      set (tmp, left); append (right, tmp);
      return tmp;
   end;

   -----------------------------------------------------------------------

   procedure set (item: in out object; value: in object) is
   begin
      if extra_check then
         assert (value.len <= item.width, "set" & on_fild & assfals);
      end if;
      item.len := min(item.width, value.len);
      if item.len > 0
      then
	 item.data(1..item.len) := value.data(1 .. item.len);
      end if;
   end;

   -----------------------------------------------------------------------

   procedure set (item: in out object; value: in elem_array) is
   begin
      if extra_check then
          assert (value'length <= item.width, "set" & on_arry & assfals);
      end if;
      item.len := min(item.width, value'length);
      if item.len > 0
      then
	   item.data(1..item.len) := value(value'first..item.len + value'first - 1);
      end if;
   end;

   -----------------------------------------------------------------------

   procedure set (item: in out object; value: in elem_type) is
   begin
      item.data(1) := value;
      item.len     := 1;
   end;

   -----------------------------------------------------------------------

   procedure set   (item: in out object;
		   value: in elem_type;
		   times: in positive) is
   begin
      set_length (item, times);
      for i in 1..length (item)
      loop
	 item.data(i) := value;
      end loop;
   end;
   -----------------------------------------------------------------------

   procedure clear (item: in out object; from: in positive := 1) is
   begin
      set_length (item, from - 1);
   end;

   -----------------------------------------------------------------------

   procedure set_length (item: in out object; len: in natural) is
   begin
      item.len := min (len, item.width);
   end;

   -----------------------------------------------------------------------

   procedure append (tail: in object; to: in out object) is
      len: natural;
   begin
      len := min(tail.len, to.width - to.len);
      if extra_check then
           assert (tail.len <= len, "append" & on_fild & assfals);
      end if;
      to.data(to.len + 1 .. to.len + len) := tail.data(1..len);
      to.len := to.len + len;
   end;

   -----------------------------------------------------------------------

   procedure append (tail: in elem_array; to: in out object) is
      len: natural;
   begin
      len := min(tail'length, to.width - to.len);
      if extra_check then
            assert (tail'length <= len, "append" & on_arry & assfals);
      end if;
      to.data(to.len + 1 .. to.len + len) :=
      tail(tail'first .. tail'first + len - 1);
      to.len := to.len + len;
   end;

   -----------------------------------------------------------------------

   procedure append (tail: in elem_type ; to: in out object) is
   begin
      if extra_check then
          assert (to.width > to.len, "append" & on_elem & assfals);
      end if;
      if  to.width > to.len then
          to.len := to.len + 1;
          to.data(to.len) := tail;
      end if;
   end;

   -----------------------------------------------------------------------

   procedure append (tail: in object; to: in out object;
                     len:  in natural;
                     pos:  in positive := 1) is
      m: integer;
   begin
      m := min (min (tail.len - pos + 1, to.width - to.len), len);
      if m > 0 then
         to.data(to.len + 1 .. to.len + len) := tail.data(pos..m);
         to.len := to.len + m;
      end if;
   end;

   -----------------------------------------------------------------------

   procedure amend (item: in out object; by: in object; position: in positive) is
      len: natural;
   begin
      len := min(by.len, item.width - position);
      if extra_check then
           assert (by.len <= len, "amend" & on_fild & assfals);
      end if;
      item.data(position .. position + len - 1) :=  by.data(1 .. len);
      item.len := max (item.len, len + position - 1);
   end;

   -----------------------------------------------------------------------

   procedure amend (item: in out object; by: in elem_array; position: in positive) is
      len: natural;
   begin
      len := min(by'length, item.width - position);
      if extra_check then
            assert (by'length <= len, "amend" & on_arry & assfals);
      end if;
      item.data(position .. position + len - 1) :=
      by(by'first .. by'first + len - 1);
      item.len := max (item.len, len + position - 1);
   end;

   -----------------------------------------------------------------------

   procedure amend (item: in out object; by: in elem_type ; position: in positive) is
   begin
      if extra_check then
            assert (1 <= item.len, "amend" & on_arry & assfals);
      end if;
      item.data(position) := by;
   end;

   -----------------------------------------------------------------------

   function locate (frag: elem_array;
		  within: object;
		    from: positive := 1;
		      to: positive := positive'last) return natural is
      last_try: constant integer := (min (within.len, to) - frag'length + 1);
   begin

      if frag'length = 0 or else from > last_try
      then
         return 0;
      end if;

      if frag'length = 1 then
	 return locate (frag(frag'first), within, from, to);
      end if;

      declare
         pos: natural := from;
         equ: natural := 0;
         sub: array (frag'range) of positive;

         procedure subseq_check is
            o: integer;
         begin

            for i in sub'range
            loop
                sub (i) := i;
            end loop;

            for n in frag'first..(frag'last / 2)
            loop
               for m in n..(frag'last - n)
               loop
                  o := m + n;
                  if frag(frag'first..n) = frag(m + 1..o) and then
                     sub (o) > m
                  then
                     sub (o) := m;
                  end if;
               end loop;
            end loop;
         end subseq_check;

      begin

	 pos := locate (frag(frag'first), within, from, last_try);
	 if pos < 1 -- not in from..last_try
         then
             return 0;
         end if;

         subseq_check;

         loop

            exit when pos > last_try;

            for f in frag'range -- frag'first..frag'last
            loop
               exit when within.data (pos + equ) /= frag (f);
               equ := equ + 1;
            end loop;

            if equ > 0 then
               if equ = frag'length then
                  return pos;
               end if;

               pos := pos + sub (sub'first + equ - 1);
               equ := 0;
            else
               pos := pos + 1;
	       pos := locate (frag(frag'first), within, pos, to);
               exit when pos = 0;
            end if;

         end loop;
      end;
      return 0;
   end locate;

   -----------------------------------------------------------------------

   function locate (frag: object;
		  within: object;
		    from: positive := 1;
		      to: positive := positive'last) return natural is
   begin
      return locate (frag.data (1 .. frag.len), within, from, to);
   end;

   -----------------------------------------------------------------------

   function locate (frag: elem_type;
		  within: object;
		    from: positive := 1;
		    to:   positive := positive'last) return natural is
   begin
      for n in from..min (within.len, to)
      loop
         if within.data(n) = frag
         then
            return natural(n);
         end if;
      end loop;
      return 0;
   end;

   -----------------------------------------------------------------------

   procedure delete (item: in out object; from, to: positive) is
   begin
       if item.len > 0 and then
	  from <= to
       then
	  if to > item.len then
	       clear (item, from);
	  else
	       set (item, item.data (1     ..from - 1) &
			  item.data (to + 1..item.len));
	  end if;
       end if;
   exception
       when others => error ("delete" & adachks); raise;
   end;

   -----------------------------------------------------------------------

   procedure expand (item: in out object; from, to: positive) is
   begin
	set (item, item.data (1     ..to      )  &
		   item.data (from  ..item.len));
   exception
       when others => error ("expand" & adachks); raise;
   end;

   -----------------------------------------------------------------------

   procedure exchange (item_a, item_b: in out object) is
   begin
      if extra_check then
          assert (item_a.len <= item_b.width and
                  item_b.len <= item_a.width,
                  "exchange" & on_fild & assfals);
      end if;
      if item_a.len = 0
      then
          if item_b.len = 0
          then
              return;
          end if;
          set (item_a, item_b); item_b.len := 0; return;
      end  if;

      if  item_b.len = 0
      then
          set (item_b, item_a); item_a.len := 0; return;
      end if;

      if  item_a.len > item_b.len
      then
          declare
             elm: elem_type;
          begin
             for n in 1..item_b.len
             loop
                elm := item_a.data(n);
                item_a.data(n) := item_b.data(n);
                item_b.data(n) := elm;
             end loop;
             append (item_a.data(item_b.len + 1..item_a.len), item_b);
	     clear (item_a, item_b.len + 1);
          end;
          return;
      elsif item_b.len > item_a.len
      then
          declare
             elm: elem_type;
          begin
             for n in 1..item_a.len
             loop
                elm := item_b.data(n);
                item_b.data(n) := item_a.data(n);
                item_a.data(n) := elm;
             end loop;
             append (item_b.data(item_a.len + 1..item_b.len), item_b);
	     clear (item_b, item_a.len + 1);
          end;
          return;
      else -- if  item_a.len = item_b.len then
          declare
             elm: elem_type;
          begin
             for n in 1..item_a.len
             loop
                elm := item_a.data(n);
                item_a.data(n) := item_b.data(n);
                item_b.data(n) := elm;
             end loop;
          end;
          return;
      end if;

      raise program_error;

   end;

   -----------------------------------------------------------------------

   procedure insert (item:    in out object;
		     frag:    in     object;
                     from:    in     positive;
                     replace: in     natural := 0) is
   begin
             insert (item     => item,
                     frag     => value(frag),
                     from     => from,
                     replace  => replace);
   end;

   -----------------------------------------------------------------------

   procedure insert (item:    in out object;
		     frag:    in     elem_array;
		     from:    in     positive;
		     replace: in     natural := 0) is

   begin
       if replace < frag'length then
	     expand (item, from + replace, from + frag'length - 1);
       elsif replace > frag'length then
	     delete (item, from + frag'length, from + replace - 1);
       end if;
       amend  (item, frag, from);
   exception
       when others => error ("insert" & on_arry & assfals); raise;
   end;

   -----------------------------------------------------------------------

   procedure insert (item:    in out object;
		     frag:    in     elem_type;
		     from:    in     positive;
		     replace: in     natural := 0) is
   begin
       if replace < 1 then
	     expand (item, from, from);
       elsif replace > 1 then
	     delete (item, from + 1, from + replace - 1);
       end if;

       amend  (item, frag, from);
   exception
       when others => error ("insert" & on_elem & assfals); raise;
   end;

   -----------------------------------------------------------------------

   function suffix (a, b: object) return natural is
	m:  natural := min(a.len, b.len);
   begin
	for i in 0..m - 1
	loop
	     if a.data(a.len - i) /= b.data(b.len - i) then
		return i;
	     end if;
	end loop;
	return m;
   end;

   -----------------------------------------------------------------------

   function prefix (a, b: object) return natural is
	m:  natural := min(a.len, b.len);
   begin
	for i in 1..m
	loop
	     if a.data(i) /= b.data(i) then
		return i - 1;
	     end if;
	end loop;
	return m;
   end;

   -----------------------------------------------------------------------

   function translating_poly_in  (left:  in object;
		      right: in operand;
		      from:  in positive := 1;
		      to:    in positive := positive'last) return result is
   begin
      return op (value (left, from, to), right);
   end;

   -----------------------------------------------------------------------

   function translating_mono_in   (right: in object;
		       from: in positive := 1;
		       to:   in positive := positive'last) return result is
   begin
      return value (value (right, from, to));
   end;

   -----------------------------------------------------------------------

   procedure transforming_poly_in_out (left:  in out object;
			  right: in operand;
			  from:  in positive := 1;
			  to:    in positive := positive'last) is
      tmp: elem_array (max (from, 1).. min (length(left), to));
   begin
      op (tmp, right);
      set (left, tmp);
   end;

   -----------------------------------------------------------------------

   procedure transforming_mono_in_out (right: in out object;
			  from:  in positive := 1;
			  to:    in positive := positive'last) is
      tmp: elem_array (max (from, 1).. min (right.len, to));
   begin
      op (tmp);
      set (right, tmp);
   end;

   -----------------------------------------------------------------------

   procedure transfering_mono_in (item:   in object;
			from: in positive := 1;
			to:   in positive := positive'last) is
   begin
      op (value (item, from, to));
   end;

   -----------------------------------------------------------------------

   procedure transfering_mono_out (item:  in out object;
			from: in positive := 1;
			to:   in positive := positive'last) is
      tmp: elem_array (max (from, 1).. min (length(item), to));
   begin
      op (tmp);
      amend (item, tmp, from);
   end;

   -----------------------------------------------------------------------

   procedure transfering_mono_out_changes (item: in out object) is
      tmp: elem_array (1..item.width);
      len: natural := 0;
   begin
      op (tmp, len);
      set (item, tmp (1..len));
   end;

   -----------------------------------------------------------------------

   procedure transfering_poly_in (file:   in control;
		      item:   in object;
			from: in positive := 1;
			to:   in positive := positive'last) is
   begin
      op (file, item.data(max (from, 1)..min (item.len, to)));
   end;

   -----------------------------------------------------------------------

   procedure transfering_poly_in_out (file: in control;
			  item: in out object;
			  from: in positive := 1;
			    to: in positive := positive'last) is
   begin
      op (file, item.data(max (from, 1)..min (item.len, to)));
   end;

   -----------------------------------------------------------------------

   procedure transfering_poly_out ( file: in control;
			item: in out object;
			from: in positive := 1;
			to:   in positive := positive'last) is
   begin
      op (file, item.data(max (from, 1)..min (item.len, to)));
   end;

   -----------------------------------------------------------------------

   procedure transfering_poly_out_changes (file: in control;
			       item: in out object) is
   begin
      op (file, item.data, item.len);
      item.len := min(item.len, item.width);
   end;

end array_handler;
