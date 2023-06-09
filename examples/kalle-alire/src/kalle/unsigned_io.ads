--::::::::::
--unsignio.ads
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with io_exceptions,
     unsigned;
use  unsigned;
package unsigned_io is

  type    file_type      is limited private;

  type    file_mode      is (in_file, inout_file, out_file);
  subtype count          is long_integer range 0 .. long_integer'last;
  subtype positive_count is count range 1 .. count'last; 

  procedure create(file : in out file_type;
                   mode : in file_mode := inout_file; 
                   name : in string := ""; 
                   form : in string := ""); 

  procedure open(file : in out file_type; 
                 mode : in file_mode; 
                 name : in string; 
                 form : in string := ""); 

  procedure close(file : in out file_type); 
  procedure delete(file : in out file_type); 
  procedure reset(file : in out file_type; 
                  mode : in file_mode); 
  procedure reset(file : in out file_type); 

  function mode(file : in file_type) return file_mode; 
  function name(file : in file_type) return string; 

  function form(file : in file_type) return string; 
  function is_open(file : in file_type) return boolean; 

  procedure read(file : in file_type; item : out byte);
  procedure write(file: in file_type; item: in byte);
  pragma inline (read, write);

  procedure read(file : in file_type; item : out byte_string);
  procedure write(file: in file_type; item: in byte_string);
  pragma inline (read, write);

  generic
      type data_type is private;
  procedure write_bytes (file:   in out file_type;
			 data:   in     data_type);

  generic
      type data_type is private;
  procedure read_bytes (file:   in out file_type;
			data:      out data_type);

  procedure set_index(file: in file_type; to: in positive_count);
  function index(file : in file_type) return positive_count; 
  function size(file : in file_type) return count; 
  function end_of_file(file : in file_type) return boolean; 

  -- exceptions:

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

end unsigned_io;
