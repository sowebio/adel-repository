--::::::::::
--dbfileio.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with unsigned, unsigned_io, calendar, io_exceptions;
use unsigned, calendar, unsigned_io;

package db_file_io is

  type file_type is limited private;
  type file_mode is new unsigned_io.file_mode range in_file..inout_file;

  type    count           is new unsigned_io.count;
  subtype positive_count  is count range 1..count'last;

  type row_attribute   is new byte;
  type col_attribute   is new byte;
  type tab_attribute   is new byte;

  type col_stat is
  record
    attrib   : col_attribute;  -- attribute (tag)
    length   : natural;        -- length of the column
    align    : natural;        -- abstract alignment of column data
  end record;

  type table is array (positive range <>) of col_stat;

  procedure get_table   (file: in file_type; tab: in out table);

  procedure create (file : in out file_type;
		    cols : in table;
		    attr : in tab_attribute;
		    name : in string := "";
		    form : in string := "");

  procedure open (file: in out file_type;
		  mode: in file_mode;
		  name: in string;
		  form: in string := "");

  procedure close  (file: in out file_type);
  procedure reset  (file: in out file_type; mode: in file_mode);
  procedure reset  (file: in out file_type);

  function  mode    (file: in file_type) return file_mode;
  function  name    (file: in file_type) return string;
  function  form    (file: in file_type) return string;
  function  is_open (file: in file_type) return boolean;

  procedure tab_attrib  (file: in out file_type; new_attr: in tab_attribute);
  function  tab_attrib  (file: in file_type) return tab_attribute;
  function  tab_length  (file: in file_type) return count;
  function  tab_updated (file: in file_type) return time;


  procedure row_attrib (file: in out file_type; new_attr: in row_attribute);
  function  row_attrib  (file: in file_type) return row_attribute;
  function  row_length  (file: in file_type) return natural;
  procedure row_index (file: in out file_type; to: in positive_count);
  function  row_index (file: in file_type) return positive_count;

  procedure write (file: in out file_type;
		       buff: in byte_string;
		       attr: in row_attribute);

  procedure read (file: in out file_type;
		      buff:    out byte_string;
		      attr:    out row_attribute);

  procedure col_align  (file:      in out file_type;
			col:       in positive;
			new_align: in natural);

  procedure col_attrib (file:      in out file_type;
			col:       in positive;
			new_attr:  in col_attribute);

  procedure col_name   (file:      in out file_type;
			col:       in positive;
			new_name:  in string);

  function  col_align  (file: in file_type; from: in positive) return natural;
  function  col_attrib (file: in file_type; from: in positive) return col_attribute;
  function  col_name   (file: in file_type; from: in positive) return string;

  function  col_index  (file: in file_type; from: in string) return natural;
  function  col_count  (file: in file_type) return positive;
  function  col_length (file: in file_type; from: in positive) return natural;

  procedure get (file: in out file_type;
		  col: in     positive;
		  buf:    out byte_string);

  procedure put (file: in out file_type;
		  col: in     positive;
		  buf: in     byte_string);

  status_error : exception renames io_exceptions.status_error;
  mode_error   : exception renames io_exceptions.mode_error;
  name_error   : exception renames io_exceptions.name_error;
  use_error    : exception renames io_exceptions.use_error;
  device_error : exception renames io_exceptions.device_error;
  end_error    : exception renames io_exceptions.end_error;
  data_error   : exception renames io_exceptions.data_error;

private

  type file_object;
  type file_type is access file_object;

end db_file_io;
