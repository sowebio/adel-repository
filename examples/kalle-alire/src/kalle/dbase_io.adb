--::::::::::
--dbase_io.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with unsigned, calendar, db_file_io, text_io;
use  unsigned, calendar;

package body dbase_io is

  package dbio renames db_file_io;

  tab_attr: constant dbio.tab_attribute := 3;

  procedure pad (temp: out string; item: in natural) is
     numb: natural := item;
  begin
     for n in reverse temp'range
     loop
	if numb > 0
	then
	   temp (n) := character'val(character'pos('0') + numb mod 10);
	   numb  := numb / 10;
	else
	   temp (n) := '0';
	end if;
     end loop;
  end;
  --------------------------------------------------------------------------

  function convert (attr: col_attribute) return dbio.col_attribute is
  begin
      case attr is
	 when db_character =>  return dbio.col_attribute (67);
	 when db_date      =>  return dbio.col_attribute (68);
	 when db_boolean   =>  return dbio.col_attribute (76);
	 when db_memo      =>  return dbio.col_attribute (77);
	 when db_numeric   =>  return dbio.col_attribute (78);
	 when others       =>  raise  col_data_error;
      end case;
  end;

  --------------------------------------------------------------------------

  function convert (attr: dbio.col_attribute) return col_attribute is
  begin
      case attr is
	 when 67      => return db_character;
	 when 68      => return db_date;
	 when 76      => return db_boolean;
	 when 77      => return db_memo;
	 when 78 | 70 => return db_numeric;
	 when others  => return db_unknown;
      end case;
  end;

  --------------------------------------------------------------------------

  procedure convert (to: in out col_stat; from: in dbio.col_stat) is
  begin
     to.attrib := convert (from.attrib);
     case to.attrib is
	when db_date =>    to.length := 8; to.align  := 0;
	when db_boolean => to.length := 1; to.align  := 0;
	when db_unknown => to.length := 0; to.align  := 0;
	when others =>     to.length := from.length;
			   to.align  := from.align;
     end case;
  end;

  --------------------------------------------------------------------------

  procedure convert (to: in out dbio.col_stat; from: in col_stat) is
  begin
     to.attrib := convert (from.attrib);
     case from.attrib is
	when db_date =>    to.length := 8; to.align  := 0;
	when db_boolean => to.length := 1; to.align  := 0;
	when others =>     to.length := from.length;
			   to.align  := from.align;
     end case;
  end;

  --------------------------------------------------------------------------

  function convert (attr: row_attribute) return dbio.row_attribute is
  begin
      case attr is
	 when true  => return dbio.row_attribute (32);
	 when false => return dbio.row_attribute (42);
	when others => raise row_data_error;
      end case;
  end;

  --------------------------------------------------------------------------

  function convert (attr: dbio.row_attribute) return row_attribute is
  begin
      case attr is
	 when     32 => return true;
	 when     42 => return false;
	 when others => raise row_data_error;
      end case;
  end;

  --------------------------------------------------------------------------

  function convert (bool: byte) return boolean is
  begin
     case bool is
	when 121 | 89 |
	     116 | 84 |
	     49         => return true;
	when 110 | 78 |
	     102 | 70 |
	     48         => return false;
	when others     => raise col_data_error;
     end case;
  end;

  --------------------------------------------------------------------------

  function convert (bool: boolean) return byte is
  begin
     case bool is
	when true  => return 89;
	when false => return 78;
     end case;
  end;


  --------------------------------------------------------------------------

  package body table_io is

    procedure ensure_status (file: in file_type) is
    begin
      if not is_open (file)
      then
	 raise dbio.status_error;
      end if;
    end;

    -----------------------------------------------------------------------

    procedure ensure_type (file: in file_type;
			   name: in identify;
			   attr: in col_attribute) is
    begin
      if col_attrib (file, name) = attr
      then
	 return;
      end if;
      raise col_data_error;
    end;

    -----------------------------------------------------------------------

    function convert (tab: in table) return dbio.table is
       tmp: dbio.table (1..tab'length);
       len: natural := 0;
    begin
       for name in tab'range
       loop
	  if tab (name).attrib /= db_unknown
	  then
	    len := len + 1;
	    convert (tmp (len), tab (name));
	  end if;
       end loop;
       return tmp (1..len);
    exception
       when others => raise tab_data_error;
    end;

    -----------------------------------------------------------------------

    procedure get_table (file : in     file_type;
			 tab  : in out table) is

      tmp: dbio.col_stat;

    begin
      ensure_status (file);
      for name in tab'range
      loop
	 if file.where (name) > 0
	 then
	    tmp.attrib := dbio.col_attrib (file.file, file.where (name));
	    tmp.length := dbio.col_length (file.file, file.where (name));
	    tmp.align  := dbio.col_align  (file.file, file.where (name));
	    convert (tab (name), tmp);
	 else
	    tab (name).attrib := db_unknown;
	    tab (name).length := 0;
	    tab (name).align  := 0;
	 end if;
      end loop;
    end get_table;

    -----------------------------------------------------------------------

    procedure except_mode (file: in file_type; which: in file_mode) is
    begin
      if mode (file) = which
      then
	 raise dbio.mode_error;
      end if;
    end;

    -----------------------------------------------------------------------

    procedure close (file : in out file_type) is
    begin
      dbio.close (file.file);
    end close; 

    -----------------------------------------------------------------------

    procedure reset (file : in out file_type) is
    begin
      dbio.reset (file.file);
    end reset;

    -----------------------------------------------------------------------

    procedure reset (file : in out file_type;
		     mode : in file_mode) is
    begin
      dbio.reset (file.file, dbio.file_mode (mode));
    end reset; 

    -----------------------------------------------------------------------

    function mode (file : in file_type) return file_mode is
    begin
      return file_mode (dbio.mode (file.file));
    end mode; 

    -----------------------------------------------------------------------

    function name (file : in file_type) return string is
    begin
      return dbio.name (file.file);
    end name; 

    -----------------------------------------------------------------------

    function form (file : in file_type) return string is
    begin
      return dbio.form (file.file);
    end form; 

    -----------------------------------------------------------------------

    function is_open (file : in file_type) return boolean is
    begin
      return dbio.is_open (file.file);
    end is_open; 

    -----------------------------------------------------------------------

    function  tab_length  (file: in     file_type) return count is
    begin
      return count (dbio.tab_length (file.file));
    end tab_length;

    -----------------------------------------------------------------------

    function tab_updated (file : in file_type) return time is
    begin
      return dbio.tab_updated (file.file);
    end tab_updated; 

    -----------------------------------------------------------------------

    procedure row_attrib (file     : in out file_type;
			  new_attr : in row_attribute) is
    begin
      ensure_status (file);
      except_mode   (file, in_file);
      dbio.row_attrib (file.file, convert (new_attr));
    end row_attrib;

    -----------------------------------------------------------------------

    function row_attrib (file : in file_type) return row_attribute is
    begin
      return convert (dbio.row_attrib (file.file));
    end row_attrib; 

    -----------------------------------------------------------------------

    procedure row_index (file : in out file_type;
			 to   : in positive_count) is
    begin
      dbio.row_index (file.file, dbio.positive_count(to));
    end row_index; 

    -----------------------------------------------------------------------

    function row_index (file : in file_type) return positive_count is
    begin
      return positive_count (dbio.row_index (file.file));
    end row_index; 

    -----------------------------------------------------------------------

    procedure col_align (file      : in out file_type;
			 name      : in identify;
			 new_align : in natural) is
    begin
      if col_exist (file, name)
      then
	 except_mode   (file, in_file);
	 dbio.col_align (file.file, file.where (name), new_align);
      else
	 raise col_data_error;
      end if;
    end col_align; 

    -----------------------------------------------------------------------

    procedure col_attrib (file     : in out file_type;
			  name     : in identify;
			  new_attr : in col_attribute) is
    begin
      if col_exist (file, name)
      then
	 except_mode   (file, in_file);
	 dbio.col_attrib (file.file, file.where (name), convert(new_attr));
      else
	 raise col_data_error;
      end if;
    end col_attrib;

    -----------------------------------------------------------------------

    function  col_index (file: in file_type;
			 from: in identify) return natural is
    begin
       ensure_status (file);
       return file.where (from);
    end;

    -----------------------------------------------------------------------

    function  col_count  (file: in file_type) return positive is
    begin
       ensure_status (file);
       return dbio.col_count (file.file);
    end;

    -----------------------------------------------------------------------

    function col_align (file : in file_type;
			from : in identify) return natural is
    begin
      if col_exist (file, from)
      then
	 return dbio.col_align (file.file, file.where (from));
      else
	 raise col_data_error;
      end if;
      return 0;
    end col_align;

    -----------------------------------------------------------------------

    function col_attrib (file : in file_type;
			 from : in identify) return col_attribute is
    begin
      if col_exist (file, from)
      then
	 return convert (dbio.col_attrib (file.file, file.where (from)));
      else
	 return db_unknown;
      end if;
    end col_attrib;

    -----------------------------------------------------------------------

    function col_length (file : in file_type;
			 from : in identify) return natural is
    begin
      if col_exist (file, from)
      then
	 return dbio.col_length (file.file, file.where (from));
      else
	 raise col_data_error;
      end if;
    end col_length;

    -----------------------------------------------------------------------

    function  col_exist  (file: in file_type;
			  name:  in identify) return boolean is
    begin
       ensure_status (file);
       return file.where (name) in 1..dbio.col_count (file.file);
    end;

    -----------------------------------------------------------------------

    procedure create(file : in out file_type;
		     cols : in table;
		     name : in string := "";
		     form : in string := "") is
      pos: natural := 0;
    begin
      dbio.create (file.file, convert (cols), tab_attr, name, form);
      for name in identify
      loop
	 if cols (name).attrib /= db_unknown
	 then
	    pos               := pos + 1;
	    file.where (name) := pos;
	    dbio.col_name (file.file, pos, identify'image (name));
	 else
	    file.where (name) := 0;
	 end if;
      end loop;
    exception
      when others => if dbio.is_open (file.file)
		     then
			   dbio.close (file.file);
		     end if;
		     raise;
    end create;

    -----------------------------------------------------------------------

    procedure open (file : in out file_type;
		    mode : in file_mode;
		    name : in string;
		    form : in string := "") is
      use db_file_io;
    begin
      dbio.open (file.file, dbio.file_mode (mode), name, form);
      if dbio.tab_attrib (file.file) = tab_attr
      then
	 for name in identify
	 loop
	    file.where (name) := dbio.col_index (file.file,
					       identify'image (name));
	 end loop;
      else
	 raise data_error;
      end if;
    exception
      when others => if dbio.is_open (file.file)
		     then
			   dbio.close (file.file);
		     end if;
		     raise;
    end open;

    -----------------------------------------------------------------------

    procedure get (file: in out file_type;
		   name: in     identify;
		   item:    out byte_string) is
    begin
      if col_exist (file, name)
      then
	 dbio.get (file.file, file.where (name), item);
      else
	 raise col_data_error;
      end if;
    end get;


    -----------------------------------------------------------------------

    procedure put (file: in out file_type;
		   name: in     identify;
		   item: in     byte_string) is
    begin
      if col_exist (file, name)
      then
	 except_mode   (file, in_file);
	 dbio.put (file.file, file.where (name), item);
      else
	 raise col_data_error;
      end if;
    end put;

    -----------------------------------------------------------------------

    procedure get (file: in out file_type;
		   name: in     identify;
		   item:    out string) is
    begin
      if col_length (file, name) > item'length
      then
	 raise storage_error;
      end if;
      declare
	 temp: byte_string (1..col_length (file, name));
      begin
	  get (file, name, temp);
	  if temp'length = item'length
	  then
	     item := value (temp);
	  else
	    item (1..temp'length) := value (temp);
	    for ws in temp'length + 1..item'last
	    loop
	       item (ws) := ' ';
	    end loop;
	  end if;
      end;
    end;

    -----------------------------------------------------------------------

    procedure put (file: in out file_type;
		   name: in     identify;
		   item: in     string) is
    begin
      except_mode   (file, in_file);
      if col_length (file, name) < item'length
      then
	 raise storage_error;
      end if;

      declare
	 temp: byte_string (1..col_length (file, name));
      begin
	  if temp'length = item'length
	  then
	    temp := value (item);
	  else
	    temp (1..item'length) := value (item);
	    for ws in item'length + 1..temp'last
	    loop
	       temp (ws) := 32;
	    end loop;
	  end if;
	  put (file, name, temp);
      end;
    end;

    -----------------------------------------------------------------------

    procedure get (file: in out file_type;
		   name: in     identify;
		   item:    out boolean) is
    begin
      ensure_type   (file, name, db_boolean);
      declare
	 temp: byte_string (1..col_length (file, name));
      begin
	 get (file, name, temp);
	 item := convert (temp (1));
      end;
    end;

    -----------------------------------------------------------------------

    procedure put (file: in out file_type;
		   name: in     identify;
		   item: in     boolean) is
    begin
      ensure_type   (file, name, db_boolean);
      except_mode   (file, in_file);
      declare
	 temp: byte_string (1..col_length (file, name));
      begin
	 get (file, name, temp);
	 temp (1) := convert (item);
	 put (file, name, temp);
      end;
    end;

    -----------------------------------------------------------------------

    procedure get (file: in out file_type;
		   name: in     identify;
		   item:    out time) is
    begin
      ensure_type   (file, name, db_date);
      declare
	 yr: year_number;
	 mn: month_number;
	 dy: day_number;

	 temp: string (1..8);
      begin
	 get (file, name, temp);

	 begin
	    yr   := year_number'value  (temp (1..4));
	    mn   := month_number'value (temp (5..6));
	    dy   := day_number'value   (temp (7..8));
	    item := time_of (yr, mn, dy);
	 exception
	    when others => raise col_data_error;
	 end;
      end;
    end;

    -----------------------------------------------------------------------

    procedure put (file: in out file_type;
		   name: in     identify;
		   item: in     time) is
    begin
      ensure_type   (file, name, db_date);
      declare
	 yr: year_number;
	 mn: month_number;
	 dy: day_number;
	 sc: day_duration;

	 temp: string (1..8);
      begin
	 split (item, yr, mn, dy, sc);

	 pad (temp (1..4), yr);
	 pad (temp (5..6), mn);
	 pad (temp (7..8), dy);

	 put (file, name, temp);

      end;
    end;

    -----------------------------------------------------------------------

    package body integer_io is

      package num_io is new text_io.integer_io (num);

      procedure get (file : in out file_type;
		     name : in identify;
		     item : out num) is
      begin
	ensure_type   (file, name, db_numeric);

	declare
	   temp: string (1..col_length (file, name));
	   last: positive;
	begin
	   get (file, name, temp);
	   num_io.get (temp, item, last);
	end;
      end get;

      procedure put (file : in out file_type;
		     name : in identify;
		     item : in num) is
      begin
	ensure_type   (file, name, db_numeric);
	except_mode   (file, in_file);

	declare
	   temp: string (1..col_length (file, name));
	begin
	   num_io.put (temp, item, base => 10);
	   put (file, name, temp);
	end;
      end put;

    end integer_io;

    -----------------------------------------------------------------------

    package body fixed_io is

      package num_io is new text_io.fixed_io (num);

      procedure get (file : in out file_type;
		     name : in identify;
		     item : out num) is
      begin
	ensure_type   (file, name, db_numeric);

	declare
	   temp: string (1..col_length (file, name));
	   last: positive;
	begin
	   get (file, name, temp);
	   num_io.get (temp, item, last);
	end;
      end get;

      procedure put (file : in out file_type;
		     name : in identify;
		     item : in num) is
      begin
	ensure_type   (file, name, db_numeric);
	except_mode   (file, in_file);

	declare
	   temp: string (1..col_length (file, name));
	begin
	   num_io.put (temp, item, aft => col_align (file, name));
	   put (file, name, temp);
	end;
      end put;

    end fixed_io;

    -----------------------------------------------------------------------

    package body enumeration_io is

      package enum_io is new text_io.enumeration_io (enum);

      procedure get (file : in out file_type;
		     name : in identify;
		     item : out enum) is
      begin
	ensure_type   (file, name, db_character);
	declare
	   temp: string (1..col_length (file, name));
	   last: positive;
	begin
	   get (file, name, temp);
	   enum_io.get (temp, item, last);
	end;
      end get;

      procedure put (file : in out file_type;
		     name : in identify;
		     item : in enum) is
      begin
	ensure_type   (file, name, db_character);
	except_mode   (file, in_file);

	declare
	   temp: string (1..col_length (file, name));
	begin
	   enum_io.put (temp, item);
	   put (file, name, temp);
	end;
      end put;
    end enumeration_io; 

  end table_io; 

end dbase_io;
