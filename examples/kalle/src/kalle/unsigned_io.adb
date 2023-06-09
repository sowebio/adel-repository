--::::::::::
--unsignio.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
with direct_io;
with unchecked_deallocation, unchecked_conversion;
package body unsigned_io is

  package bio is new direct_io(byte);

  type file_object is 
    record
      f : bio.file_type; 
    end record; 

  bio_mode : constant array(file_mode) of bio.file_mode := 
   (in_file => bio.in_file, 
    inout_file => bio.inout_file, 
    out_file => bio.out_file); 

  bin_mode : constant array(bio.file_mode) of file_mode := 
   (bio.in_file => in_file, 
    bio.inout_file => inout_file, 
    bio.out_file => out_file); 

  procedure destroy (file : in out file_type) is
     procedure free is new unchecked_deallocation(file_object, file_type);
  begin
      free (file);
      file := null;
  end;

  procedure create(file : in out file_type;
                   mode : in file_mode := inout_file; 
                   name : in string := ""; 
                   form : in string := "") is 
  begin
    file := new file_object; 
    bio.create(file.f, bio_mode(mode), name, form); 
  exception
    when constraint_error =>  destroy (file);
			      raise status_error;
    when others =>            destroy (file);
			      raise;
  end create; 

  procedure open(file : in out file_type; 
                 mode : in file_mode; 
                 name : in string; 
                 form : in string := "") is 
  begin
    file := new file_object; 
    bio.open(file.f, bio_mode(mode), name, form); 
  exception
    when constraint_error => destroy (file);
			     raise status_error;
    when others =>           destroy (file);
			     raise;
  end open; 

  procedure close(file : in out file_type) is 
  begin
    bio.close(file.f); 
    destroy (file);
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end close; 

  procedure delete(file : in out file_type) is 
  begin
    bio.delete(file.f); 
    destroy (file);
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end delete; 

  procedure reset(file : in out file_type; 
                  mode : in file_mode) is 
  begin
    bio.reset(file.f, bio_mode(mode)); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end reset; 

  procedure reset(file : in out file_type) is 
  begin
    bio.reset(file.f); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end reset; 

  function mode(file : in file_type) return file_mode is 
  begin
    return bin_mode(bio.mode(file.f)); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end mode; 

  function name(file : in file_type) return string is 
  begin
    return bio.name(file.f); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end name; 

  function form(file : in file_type) return string is 
  begin
    return bio.form(file.f); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end form; 

  function is_open(file : in file_type) return boolean is 
  begin
    if file = null then 
      return false; 
    end if; 
    return bio.is_open(file.f); 
  exception
    when others => 
      raise; 
  end is_open; 

  procedure read(file : in file_type; 
                 item : out byte) is 
  begin
    bio.read(file.f, item); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end read; 

  procedure read(file : in file_type; 
                 item : out byte; 
                 from : in positive_count) is 
  begin
    bio.read(file.f, item, bio.count(from)); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end read; 

  procedure read(file : in file_type; 
		 item : out byte_string) is
  begin
    for n in item'range loop
      bio.read(file.f, item(n)); 
    end loop; 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end read; 

  procedure read(file : in file_type; 
		 item : out byte_string;
                 from : in positive_count) is 
  begin
    set_index(file, from); 
    read(file, item); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end read; 

  procedure write(file : in file_type; 
                  item : in byte) is 
  begin
    bio.write(file.f, item); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end write; 

  procedure write(file : in file_type; 
                  item : in byte; 
                  to   : in positive_count) is 
  begin
    bio.write(file.f, item, bio.count(to)); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end write; 

  procedure write(file : in file_type; 
		  item : in byte_string) is
  begin
    for n in item'range loop
      bio.write(file.f, item(n)); 
    end loop; 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end write; 

  procedure write(file : in file_type; 
		  item : in byte_string;
                  to   : in positive_count) is 
  begin
    set_index(file, to); 
    write(file, item); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end write; 

  procedure set_index(file : in file_type; 
                      to   : in positive_count) is 
  begin
    bio.set_index(file.f, bio.count(to)); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end set_index; 

  function index(file : in file_type) return positive_count is 
  begin
    return count(bio.index(file.f)); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end index; 

  function size(file : in file_type) return count is 
  begin
    return count(bio.size(file.f)); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end size; 

  function end_of_file(file : in file_type) return boolean is 
  begin
    return bio.end_of_file(file.f); 
  exception
    when constraint_error => 
      raise status_error; 
    when others => 
      raise; 
  end end_of_file; 

  procedure write_bytes (file:   in out file_type;
			data:   in     data_type) is

      subtype byte_array is byte_string (1..data_type'size/byte'size);
      function convert is new unchecked_conversion (data_type, byte_array);
      buf: byte_array := convert (data);
  begin
      write (file, buf);
  end;

  procedure read_bytes (file:   in out file_type;
		       data:      out data_type) is

      subtype byte_array is byte_string (1..data_type'size/byte'size);
      function convert is new unchecked_conversion (byte_array, data_type);
      buf: byte_array;
  begin
      read (file, buf);
      data := convert (buf);
  end;

end unsigned_io;
