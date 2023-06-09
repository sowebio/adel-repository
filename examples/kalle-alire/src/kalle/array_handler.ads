--::::::::::
--arrahand.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
generic

   type elem_type  is private;
   type elem_array is array (positive range <>) of elem_type;
   default_width: positive;
   toolname: string := "array-handler";
   extra_check: boolean := true;

package array_handler is

   type object (width: natural := default_width) is private;

      generic
	  type operand is limited private;
	  type result  is limited private;
	  with function op (left:  in elem_array;
			    right: in operand) return result;
      function translating_poly_in  (left:  in object;
			 right: in operand;
			 from:  in positive := 1;
			 to:    in positive := positive'last) return result;

      generic
	  type result is limited private;
	  with function value (right: elem_array) return result is <>;
      function translating_mono_in   (right: in object;
			  from: in positive := 1;
			  to:   in positive := positive'last) return result;
      generic
	  type operand is limited private;
	  with procedure op (left:  in out elem_array; right: in operand);
      procedure transforming_poly_in_out (left:  in out object;
			     right: in operand;
			     from:  in positive := 1;
			     to:    in positive := positive'last);

      generic
	  with procedure op (right: in out elem_array);
      procedure transforming_mono_in_out (right: in out object;
			     from:  in positive := 1;
			     to:    in positive := positive'last);

      generic
	  with procedure op (data: in elem_array);
      procedure transfering_mono_in (item:   in object;
			   from: in positive := 1;
			   to:   in positive := positive'last);

      generic
	  with procedure op (data: out elem_array);
      procedure transfering_mono_out (item:  in out object;
			   from: in positive := 1;
			   to:   in positive := positive'last);

      generic
	  with procedure op (item: out elem_array; new_length: out natural);
      procedure transfering_mono_out_changes (item: in out object);

      generic
	  type control is limited private;
	  with procedure op (file: in control;
			     data: in elem_array);
      procedure transfering_poly_in (file:   in control;
			 item:   in object;
			   from: in positive := 1;
			   to:   in positive := positive'last);

      generic
	  type control is limited private;
	  with procedure op (file: in control;
			     data: in out elem_array);
      procedure transfering_poly_in_out (file: in control;
			     item: in out object;
			     from: in positive := 1;
			       to: in positive := positive'last);

      generic
	  type control is limited private;
	  with procedure op (file: in control;
			     data: out elem_array);
      procedure transfering_poly_out ( file: in control;
			   item: in out object;
			   from: in positive := 1;
			   to:   in positive := positive'last);

      generic
	  type control is limited private;
	  with procedure op (file: in control;
			     item: out elem_array; new_length: out natural);
      procedure transfering_poly_out_changes (file: in control;
				  item: in out object);

   function length (item: object) return natural;
   pragma inline (length);
   function value  (item: object; position: positive) return elem_type;
   pragma inline (value);
   function value  (item: object; first, last: natural) return elem_array;
   pragma inline (value);
   function value  (item: object) return elem_array;
   pragma inline (value);
   function empty  (item: object) return boolean;
   pragma inline (empty);

   function value (item: elem_array) return object;
   pragma inline (value);
   function value (item: elem_type)  return object;
   pragma inline (value);

   function "&"  (left: object; right: object) return object;
   function "&"  (left: object; right: elem_array) return object;
   function "&"  (left: elem_array; right: object) return object;
   function "&"  (left: object; right: elem_type ) return object;
   function "&"  (left: elem_type ; right: object) return object;

   procedure set   (item: in out object; value: in object);
   procedure set   (item: in out object; value: in elem_array);
   procedure set   (item: in out object; value: in elem_type);
   procedure set   (item: in out object;
		   value: in elem_type;
		   times: in positive);

   procedure clear (item: in out object; from: in positive := 1);
   pragma inline (clear);

   procedure set_length (item: in out object; len: in natural);
   pragma inline (set_length);

   procedure append (tail: in object; to: in out object);
   procedure append (tail: in elem_array; to: in out object);
   procedure append (tail: in elem_type ; to: in out object);
   procedure append (tail: in object; to: in out object;
                     len:  in natural;
                     pos:  in positive := 1);

   procedure amend (item: in out object; by: in object; position: in positive);
   procedure amend (item: in out object; by: in elem_array; position: in positive);
   procedure amend (item: in out object; by: in elem_type ; position: in positive);

   function locate (frag:   object;
		  within:   object;
		    from:   positive := 1;
		    to:     positive := positive'last) return natural;
   function locate (frag:   elem_array;
		  within:   object;
		    from:   positive := 1;
		    to:     positive := positive'last) return natural;
   function locate (frag:   elem_type;
		    within: object;
		    from:   positive := 1;
		    to:     positive := positive'last) return natural;

   procedure delete   (item: in out object; from, to: positive);
   procedure expand   (item: in out object; from, to: positive);
   procedure exchange (item_a, item_b: in out object);

   procedure insert (item:    in out object;
		     frag:    in     elem_type;
                     from:    in     positive;
		     replace: in     natural  := 0);

   procedure insert (item:    in out object;
		     frag:    in     elem_array;
		     from:    in     positive;
		     replace: in     natural := 0);

   procedure insert (item:    in out object;
		     frag:    in     object;
                     from:    in     positive;
                     replace: in     natural := 0);

   function prefix (a, b: object) return natural;
   function suffix (a, b: object) return natural;

   private

      type object (width: natural := default_width)
      is record
         len    : natural := 0;
         data   : elem_array (1..width);
      end record;

end array_handler;
