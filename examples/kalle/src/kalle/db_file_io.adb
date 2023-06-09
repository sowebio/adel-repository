--::::::::::
--dbfileio.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.

with calendar, unsigned, unsigned_io, unchecked_conversion,
     unchecked_deallocation, choice;

use  calendar, unsigned;

package body db_file_io is

  tab_desc_length: constant := 64;

  package uio  renames unsigned_io;

  function max is new choice (integer, ">");
  function min is new choice (integer, "<");

  type col_description is
  record
    name   : string (1..32);
    length : natural;         -- name length
    offset : natural;         -- offset in the rec_buffer (from 0)
    params : col_stat;
  end record;

  procedure set_name (desc: in out col_description; name: in  string);

  package db_format_specific is

    subtype col_desc_index  is positive range 1..tab_desc_length;
    type    col_desc_table  is array (col_desc_index range <>) of col_description;

    type file_header is
    record

      tab_attrib    : tab_attribute;
      tab_updated   : time;
      col_begins,
      row_begins    : unsigned_io.count;
      row_posinc    : unsigned_io.count;
      row_length    : natural;
      row_count     : count;
      col_count,
      col_namelen   : natural;

    end record;

    subtype file_type is unsigned_io.file_type;

    procedure read_info (file:   in out file_type;
			 header: in out file_header;
			 tab:  in out col_desc_table);

    procedure write_info (file:   in out file_type;
			  header: in out file_header;
			  tab:      in col_desc_table);

    procedure write_record (file:   in out file_type;
			    header: in     file_header;
			    buf:    in     byte_string;
			    attr:   in     row_attribute;
			    to:     in     positive_count);

    procedure read_record  (file:   in out file_type;
			    header: in     file_header;
			    buf:       out byte_string;
			    attr:      out row_attribute;
			    from:   in     positive_count);

  end db_format_specific;

  use db_format_specific;

  type byte_buffer     is access byte_string;
  type col_desc_buffer is access col_desc_table;

  type file_object is
  record

    uio_file    : uio.file_type;
    headings    : file_header;

    rec_index   : count;               -- current record index
    rec_iopos   : count;               -- record really located
    rec_buffer  : byte_buffer;         -- pointer to the record
    rec_attrib  : row_attribute;       -- abstract record attribute
    rec_changed : boolean;             -- record/attribute data changed

    col_table   : col_desc_buffer;     -- pointer to the column table
    tab_changed : boolean;             -- col_table is changed

  end record;

  package body db_format_specific is separate;

  --------------------------------------------------------------------------

  procedure set_name (desc: in out col_description;
		      name: in string) is
     safe_length: natural := min (name'length, desc.name'length);
  begin
     desc.name (1..safe_length) := name (name'first..name'first - 1 +
						     safe_length);
     desc.length := safe_length;
     for n in 1..safe_length
     loop
	case desc.name (n) is
	   when  'A'..'Z' | '_' | 'a'..'z' | '0'..'9' => null;
	   when others =>
		 desc.length := n - 1;
		 exit;
	end case;
     end loop;
  exception
     when others => raise data_error;
  end;

  procedure release (file: in out file_type) is
    procedure destroy is new unchecked_deallocation (file_object, file_type);
    procedure destroy is new unchecked_deallocation (byte_string, byte_buffer);
    procedure destroy is new unchecked_deallocation (col_desc_table, col_desc_buffer);

  begin
    if file /= null
    then
       if uio.is_open (file.uio_file)
       then
	    uio.close (file.uio_file);
       end if;
       destroy (file.rec_buffer);
       destroy (file.col_table);
       destroy (file);
    end if;
  end;

  --------------------------------------------------------------------------

  procedure ensure_status (file: in file_type) is
  begin
     if not is_open (file) then
	 raise status_error;
     end if;
  end;

  procedure except_mode (file: in file_type; which: in file_mode) is
  begin
     if mode (file) = which then
	 raise mode_error;
     end if;
  end;

  procedure ensure_data (file: in file_type) is

     head: file_header renames file.headings;

--    invalid_data_location        : exception;
--    incomplete_data_record       : exception;
--    invalid_name_passed          : exception;
--    invalid_description_logic    : exception;

  begin

--     if size (file.uio_file) < head.row_begins or else
--        size (file.uio_file) < uio.count (head.row_length)
--                               + head.row_begins
--     then
--              raise data_error;
--     end if;

     if head.row_count > 0 and then
	(size (file.uio_file) - head.row_begins + 1)
	 / uio.count (head.row_count)
	 < uio.count(head.row_length)
     then
	 raise data_error;
     end if;
  exception
     when others => raise data_error;
  end;

  procedure ensure_col (file: in file_type; col: in natural) is
  begin
     if not (col in file.col_table'range)
     then
	 raise use_error;
     end if;
  end;

  procedure update (file: in out file_type) is
  begin
     if mode (file) /= in_file
     then
	if file.tab_changed
	then
	    write_info (file.uio_file, file.headings, file.col_table.all);
	    file.tab_changed := false;
	end if;
	if file.rec_changed
	then
	   write_record (file.uio_file,
			 file.headings,
			 file.rec_buffer.all,
			 file.rec_attrib,
			 file.rec_iopos);
	   file.rec_changed := false;
	end if;
     end if;
  end;

  procedure seek_current (file: in out file_type) is
  begin
    if file.rec_index /= file.rec_iopos
    then
	 read (file, file.rec_buffer.all, file.rec_attrib);
	 file.rec_iopos := file.rec_index;
    end if;
  end;

  --------------------------------------------------------------------------

  function is_open (file : in file_type) return boolean is
  begin
      return file /= null and then uio.is_open (file.uio_file);
  end;

  --------------------------------------------------------------------------

  procedure get_table (file : in file_type;
		       tab  : in out table) is
  begin
       ensure_status (file);

       if file.col_table = null or else
	  file.col_table'length > tab'length
       then
	  raise storage_error;
       end if;

       declare
	  diff: integer := tab'first - file.col_table'first;
       begin
	  for n in file.col_table'range
	  loop
	      tab (n + diff) := file.col_table (n).params;
	  end loop;
       end;
  end;

  --------------------------------------------------------------------------

  procedure delete (file : in out file_type) is
  begin
     ensure_status (file);
     uio.delete (file.uio_file);
     release (file);
  exception
     when others => release (file);
		    raise;
  end;

  --------------------------------------------------------------------------

  function mode (file : in file_type) return file_mode is
  begin
     ensure_status (file);
     return file_mode(uio.mode(file.uio_file));
  end;

  --------------------------------------------------------------------------

  function name (file : in file_type) return string is
  begin
     ensure_status (file);
     return uio.name (file.uio_file);
  end;

  --------------------------------------------------------------------------

  function form (file : in file_type) return string is
  begin
     ensure_status (file);
     return uio.form (file.uio_file);
  end;

  --------------------------------------------------------------------------

  procedure reset (file : in out file_type;
		   mode : in file_mode) is
  begin
     ensure_status (file);
     update (file);
     uio.reset (file.uio_file, uio.file_mode(mode));
  end;

  --------------------------------------------------------------------------

  procedure reset (file : in out file_type) is
  begin
     ensure_status (file);
     update (file);
     uio.reset (file.uio_file);
  end;

  --------------------------------------------------------------------------

  procedure close (file : in out file_type) is
  begin
      ensure_status (file);
      update (file);
      uio.close (file.uio_file);
      release (file);
  end;

  --------------------------------------------------------------------------

  function tab_updated (file : in file_type) return time is
  begin
     ensure_status (file);
     return file.headings.tab_updated;
  end;

  --------------------------------------------------------------------------

  function tab_attrib (file : in file_type) return tab_attribute is
  begin
     ensure_status (file);
     return file.headings.tab_attrib;
  end;

  --------------------------------------------------------------------------

  function tab_length (file : in file_type) return count is
  begin
     ensure_status (file);
     return file.headings.row_count;
  end;

  --------------------------------------------------------------------------

  function row_index (file : in file_type) return positive_count is
  begin
     ensure_status (file);
     return file.rec_index;
  end;


  --------------------------------------------------------------------------

  function row_length (file : in file_type) return natural is
  begin
     ensure_status (file);
     return file.rec_buffer'length;
  end;

  --------------------------------------------------------------------------

  function row_attrib (file : in file_type) return row_attribute is
  begin
     ensure_status (file);
     return file.rec_attrib;
  end;

  --------------------------------------------------------------------------

  function col_count (file : in file_type) return positive is
  begin
     ensure_status (file);
     return file.headings.col_count;
  end;

  --------------------------------------------------------------------------

  function col_length (file : in file_type; from: in positive) return natural is
  begin
     ensure_status (file);
     ensure_col    (file, from);
     return file.col_table (from).params.length;
  end;

  --------------------------------------------------------------------------

  function col_index (file : in file_type;
		      from : in string) return natural is
  begin
     ensure_status (file);
     for i in 1..col_count (file)
     loop
	if col_name (file, i) = from
	then
	    return i;
	end if;
     end loop;
     return 0;
  end;

  --------------------------------------------------------------------------

  function col_align (file : in file_type; from: in positive) return natural is
  begin
     ensure_status (file);
     ensure_col    (file, from);
     return file.col_table (from).params.align;
  end;

  --------------------------------------------------------------------------

  function col_attrib (file : in file_type; from: in positive) return col_attribute is
  begin
     ensure_status (file);
     ensure_col    (file, from);
     return file.col_table (from).params.attrib;
  end;

  --------------------------------------------------------------------------

  function col_name (file : in file_type; from: in positive) return string is
  begin
     ensure_status (file);
     ensure_col    (file, from);
     return file.col_table (from).name (1..file.col_table (from).length);
  end;

  --------------------------------------------------------------------------

  procedure tab_attrib (file: in out file_type; new_attr: in tab_attribute) is
  begin
     ensure_status (file);
     except_mode (file, in_file);
     file.headings.tab_attrib := new_attr;
     file.tab_changed := true;
     file.headings.tab_updated := clock;
  end;

  --------------------------------------------------------------------------

  procedure row_index (file : in out file_type;
		       to   : in positive_count) is
  begin
      ensure_status (file);
      file.rec_index := to;
  end;

  --------------------------------------------------------------------------

  procedure row_attrib  (file: in out file_type; new_attr: in row_attribute) is
  begin
     ensure_status (file);
     except_mode (file, in_file);
     file.rec_attrib  := new_attr;
     file.rec_changed := true;
     file.headings.tab_updated := clock;
  end;

  --------------------------------------------------------------------------

  procedure col_align (file      : in out file_type;
		       col       : in positive;
		       new_align : in natural) is
  begin
     ensure_status (file);
     except_mode (file, in_file);
     ensure_col    (file, col);
     file.col_table (col).params.align := new_align;
     file.tab_changed := true;
     file.headings.tab_updated := clock;
  end;

  --------------------------------------------------------------------------

  procedure col_attrib (file: in out file_type;
			col:  in positive;
			new_attr: in col_attribute) is
  begin
     ensure_status (file);
     except_mode (file, in_file);
     ensure_col    (file, col);
     file.col_table (col).params.attrib := new_attr;
     file.tab_changed := true;
     file.headings.tab_updated := clock;
  end;

  --------------------------------------------------------------------------

  procedure col_name   (file:      in out file_type;
			col:       in positive;
			new_name:  in string) is

  begin
     ensure_status (file);
     except_mode (file, in_file);
     ensure_col    (file, col);
     set_name (file.col_table (col), new_name);
     file.tab_changed := true;
     file.headings.tab_updated := clock;
  end;

  --------------------------------------------------------------------------

  procedure get (file: in out file_type;
		  col: in positive;
		  buf: out byte_string) is
      m, n: natural;
  begin
      ensure_status (file);
      ensure_col    (file, col);
      if buf'length < file.col_table (col).params.length
      then
	 raise storage_error;
      end if;

      if file.rec_index > file.headings.row_count
      then
	 raise use_error; -- true for input
      end if;

      seek_current (file);

      m    := file.col_table (col).offset + file.rec_buffer'first;
      n    := file.col_table (col).params.length - 1;
      buf (buf'first..buf'first + n) := file.rec_buffer (m..m + n);
  end;

  --------------------------------------------------------------------------

  procedure put (file: in out file_type;
		  col: in     positive;
		  buf: in     byte_string) is

      offs, n: natural;

      procedure clear (buf: in out byte_buffer; fill: in byte) is
      begin
	 for n in buf'range
	 loop
	    buf (n) := fill;
	 end loop;
      end;

  begin
      ensure_status (file);
      except_mode (file, in_file);
      ensure_col    (file, col);

      if buf'length < file.col_table (col).params.length
      then
	 raise storage_error;
      end if;

      if file.rec_index > file.headings.row_count
      then

	 update (file);
	 clear  (file.rec_buffer, 32);
	 file.rec_attrib := 32;
	 file.rec_iopos  := 0;

	 write  (file, file.rec_buffer.all, file.rec_attrib);

	 file.rec_index := file.rec_index - 1;
	 file.rec_iopos := file.rec_index;

      end if;

      seek_current (file);

      offs := file.col_table (col).offset + file.rec_buffer'first;
      n    := file.col_table (col).params.length - 1;

      file.rec_buffer (offs..offs + n) := buf (buf'first..buf'first + n);
      file.rec_changed := true;
      file.headings.tab_updated := clock;
  end;

  --------------------------------------------------------------------------

  procedure write (file: in out file_type;
		   buff: in byte_string;
		   attr: in row_attribute) is
  begin
      ensure_status (file);
      except_mode (file, in_file);
      if buff'length < file.rec_buffer'length
      then
	 raise storage_error;
      end if;
      if file.rec_index = file.rec_iopos
      then
	 file.rec_buffer.all       := buff (buff'first..buff'first +
					    file.rec_buffer'length - 1);
	 file.rec_attrib           := attr;
	 file.rec_changed          := true;
	 file.headings.tab_updated := clock;
      else
	 update (file);
	 write_record (file.uio_file,
		       file.headings,
		       buff (buff'first..buff'first +
			     file.rec_buffer'length - 1),
		       attr, file.rec_index);
	 if file.rec_index > file.headings.row_count
	 then
	    file.headings.row_count := file.rec_index;
	 end if;
      end if;
      file.rec_index            := file.rec_index + 1;
      file.tab_changed          := true;
      file.headings.tab_updated := clock;
  end;

  --------------------------------------------------------------------------

  procedure read (file: in out file_type;
		  buff:    out byte_string;
		  attr:    out row_attribute) is
  begin
      ensure_status (file);
      if buff'length < file.rec_buffer'length
      then
	 raise storage_error;
      end if;
      if file.rec_index = file.rec_iopos
      then
	 buff (buff'first..buff'first +
	       file.rec_buffer'length - 1) := file.rec_buffer.all;
	 attr := file.rec_attrib;
      else
	 update (file);
	 read_record (file.uio_file, file.headings,
		      buff (buff'first..buff'first +
			    file.rec_buffer'length - 1),
		      attr, file.rec_index);
	 file.rec_iopos := file.rec_index;
	 file.rec_index := file.rec_index + 1;
      end if;
  end;

  --------------------------------------------------------------------------

  procedure open (file : in out file_type;
		  mode : in file_mode;
		  name : in string;
		  form : in string := "") is
      tab: col_desc_table (1..tab_desc_length);
      use uio;
  begin
      file := new file_object;
      open (file.uio_file,  uio.file_mode (mode),  name,  form);
      read_info (file.uio_file, file.headings, tab);
      ensure_data (file);
      file.col_table   := new col_desc_table'(tab (1..file.headings.col_count));
      file.rec_buffer  := new byte_string (1..file.headings.row_length);
      file.rec_changed := false;
      file.tab_changed := false;
      file.rec_index   := 1;
      file.rec_iopos   := 0;
  exception
      when others => release (file);
		     raise;
  end;

  --------------------------------------------------------------------------

  procedure create (file : in out file_type;
		    cols : in table;
		    attr : in tab_attribute;
		    name : in string := "";
		    form : in string := "") is
      use uio;
      len: long_integer;
  begin
      if cols'length = 0
      then
	 raise use_error;
      end if;
      file := new file_object;
      uio.create (file.uio_file, inout_file, name, form);

      file.col_table := new col_desc_table (1..cols'length);

      len := 0;
      begin

	  for i in file.col_table'range
	  loop
	      file.col_table (i).params := cols (cols'first + i - 1);
	      file.col_table (i).length := 0;
	      file.col_table (i).offset := natural (len);

	      len := len + long_integer (file.col_table (i).params.length);
	      if len > long_integer (natural'last)
	      then
		   raise data_error;
	      end if;
	  end loop;

	  file.headings.row_length  := natural (len);
	  file.headings.tab_attrib  := attr;
	  file.headings.tab_updated := clock;
	  file.headings.row_count   := 0;
	  file.headings.col_count   := cols'length;

      exception
	  when others => raise data_error;
      end;

      write_info (file.uio_file, file.headings, file.col_table.all);

      file.rec_buffer  := new byte_string (1..file.headings.row_length);
      file.rec_changed := false;
      file.tab_changed := false;
      file.rec_index   := 1;
      file.rec_iopos   := 0;

  exception
      when others => release (file);
		     raise;
  end;

end db_file_io;
