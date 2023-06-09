--::::::::::
--dbase_io.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with db_file_io, unsigned, calendar;
use  unsigned, calendar;

package dbase_io is

  type col_attribute is (db_character, db_numeric, db_date,
			 db_boolean, db_memo, db_unknown);

  subtype row_attribute is boolean;

  col_data_error: exception;
  row_data_error: exception;
  tab_data_error: exception;

  type col_stat is
  record
       attrib : col_attribute := db_unknown ; --
       length : natural       := 0          ; --
       align  : natural       := 0          ; --
  end record;

  type file_mode      is new db_file_io.file_mode;
  type count          is new db_file_io.count;
  subtype positive_count  is count range 1..count'last;
---  type positive_count is new db_file_io.positive_count;

  generic
       type identify is (<>);
  package table_io is

    type table is array (identify) of col_stat;

    type file_type is limited private;

    procedure create (file: in out file_type;
		      cols: in     table;
		      name: in     string := "";
		      form: in     string := "");

    procedure open   (file: in out file_type;
		      mode: in     file_mode;
		      name: in     string;
		      form: in     string := "");

    procedure reset       (file: in out file_type);
    procedure reset       (file: in out file_type; mode: in file_mode);
    procedure close       (file: in out file_type);
    function  mode        (file: in     file_type) return file_mode;
    function  name        (file: in     file_type) return string;
    function  form        (file: in     file_type) return string;
    function  is_open     (file: in     file_type) return boolean;

    function  tab_updated (file: in     file_type) return time;
    function  tab_length  (file: in     file_type) return count;
    procedure get_table   (file: in     file_type; tab: in out table);

    procedure row_attrib  (file:     in out file_type;
			   new_attr: in row_attribute);
    function  row_attrib  (file: in     file_type) return row_attribute;
    procedure row_index   (file: in out file_type;
			     to: in     positive_count);
    function  row_index   (file: in     file_type) return positive_count;

    procedure col_align  (file:      in out file_type;
			  name:      in identify;
			  new_align: in natural);

    procedure col_attrib (file:      in out file_type;
			  name:      in identify;
			  new_attr:  in col_attribute);

    function  col_align  (file: in file_type;
			  from: in identify) return natural;
    function  col_attrib (file: in file_type;
			  from: in identify) return col_attribute;
    function  col_length (file: in file_type;
			  from: in identify) return natural;
    function  col_index  (file: in file_type;
			  from: in identify) return natural;
    function  col_exist  (file: in file_type;
			  name: in identify) return boolean;
    function  col_count  (file: in file_type) return positive;


    procedure get (file: in out file_type;
		   name: in     identify;
		   item: out byte_string);

    procedure put (file: in out file_type;
		   name: in     identify;
		   item: in  byte_string);

    procedure get (file: in out file_type;
		   name: in     identify;
		   item: out string);

    procedure put (file: in out file_type;
		   name: in     identify;
		   item: in  string);

    procedure get (file: in out file_type;
		   name: in     identify;
		   item: out boolean);

    procedure put (file: in out file_type;
		   name: in     identify;
		   item: in  boolean);

    procedure get (file: in out file_type;
		   name: in     identify;
		   item: out time);

    procedure put (file: in out file_type;
		   name: in     identify;
		   item: in  time);
    generic
	 type num is range <>;
    package integer_io is
	 procedure get (file: in out file_type;
			name: in     identify;
			item:    out num);

	 procedure put (file: in out file_type;
			name: in     identify;
			item: in     num);
    end integer_io;

    generic
	 type num is delta <>;
    package fixed_io is
	 procedure get (file: in out file_type;
			name: in     identify; item:    out num);

	 procedure put (file: in out file_type;
			name: in     identify;
			item: in     num);
    end fixed_io;

    generic
	 type enum is (<>);
    package enumeration_io is
	 procedure get (file: in out file_type;
			name: in     identify;
			item:    out enum);

	 procedure put (file: in out file_type;
			name: in     identify;
			item: in     enum);
    end enumeration_io;

  private
      type col_location is array (identify) of natural;
      type file_type is
      record
	 file:  db_file_io.file_type;
	 count: positive;
	 where: col_location;
      end record;
  end table_io;

end dbase_io;
