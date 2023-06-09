--::::::::::
--dfidfosp.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.
---------------------------------------------------------------------------

-- Translated from Russian

-- 1) the time of change must be accurate to fractions of a second.
-- (some data can be put right after the field description).
-- 2) field width can be greater than 256 bytes.
-- 3) alphabetic field name can be more than 11 bytes, but not more than ?
-- 4) use_error and end_error on read-write row
-- 5) data_error when reading header and field descriptions

-- Original text

--   1) время изменения обязано иметь точность до долей секунды.
--        (кое-какие данные можно положить сразу после описания полей).
--   2) ширина поля может быть больше 256 байт.
--   3) буквенное имя поля может быть больше 11 байт, но не более ??.
--   4) use_error и end_error при чтении-записи row
--   5) data_error при чтении заголовка и описания полей


separate (db_file_io)
package body db_format_specific is

   use unsigned_io, unsigned;

   dbf_base_year:   constant := 1900;
   dbf_base_month:  constant :=    0;
   dbf_base_day:    constant :=    0;
   dbf_header_pos:  constant :=    1;

   type dbf_header is
      record
         version       : byte;
         year          : byte;
         month         : byte;
         day           : byte;
         num_records   : long_word_bytes;
         header_length : word_bytes;
         record_length : word_bytes;
         reserved      : byte_string (1 .. 20);
      end record;

   type dbf_column is
      record
         name     : byte_string (1 .. 11);
         attr     : byte;
         pointer  : long_word_bytes;
         length   : byte;
         align    : byte;
         reserved : byte_string (1 .. 14);
      end record;

   procedure write_dbf_column is new write_bytes (dbf_column);
   procedure write_dbf_header is new write_bytes (dbf_header);
   procedure  read_dbf_column is new  read_bytes (dbf_column);
   procedure  read_dbf_header is new  read_bytes (dbf_header);

   function convert (name: byte_string) return string is
   begin
      return value (name);
   end;

   function convert (name: string) return byte_string is
   begin
      return value (name);
   end;

   procedure read_header  (file: in out file_type; header: in out file_header) is
      file_header: dbf_header;
      len: word;
   begin
      set_index (file, dbf_header_pos);
      read_dbf_header (file, file_header);
      header.tab_attrib  := tab_attribute (file_header.version);
      len := value (file_header.header_length);
      header.col_begins := uio.count (dbf_header'size/byte'size + 1);
      header.col_count  := natural (len - word (dbf_header'size/byte'size))
        / natural (dbf_column'size/byte'size);
      header.row_begins := uio.count (len + 1);
      header.row_posinc := uio.count (word'(value (file_header.record_length)));
      header.row_length := natural (header.row_posinc - 1);
      header.row_count  := count (long_word'(value (file_header.num_records )));
      header.col_namelen := 11;
      header.tab_updated :=
        time_of (year_number  (integer(file_header.year)  + dbf_base_year),
                 month_number (integer(file_header.month) + dbf_base_month),
                 day_number   (integer(file_header.day)   + dbf_base_day));
   end;

   procedure read_desc    (file: in out file_type;
                           desc: in out col_description;
                           end_of_cols:    out boolean) is
      col: dbf_column;
   begin
      read_dbf_column (file, col);
      if col.name (1) /= 16#0d#
      then
         end_of_cols        := false;
         set_name (desc, convert (col.name));
         desc.params.align  := natural (col.align);
         desc.params.length := natural (col.length);
         desc.params.attrib := col_attribute (col.attr);
      else
         end_of_cols := true;
      end if;
   exception
      when others => raise;
   end;

   procedure write_header (file: in out file_type; header: in file_header) is
      year:        year_number;
      month:       month_number;
      day:         day_number;
      secs:        day_duration;
      file_header: dbf_header;
   begin
      split (header.tab_updated, year, month, day, secs);
      file_header.version := byte (header.tab_attrib);
      file_header.year    := byte (year  - dbf_base_year);
      file_header.month   := byte (month - dbf_base_month);
      file_header.day     := byte (day   - dbf_base_day);

      file_header.header_length := value (word (header.row_begins - 1));
			       -- dbf_header'size / byte'size +
			       -- dbf_column'size / byte'size *
			       -- header.col_count;

      file_header.record_length := value (word (header.row_posinc));
      file_header.num_records   := value (long_word (header.row_count));
      file_header.reserved      := (others => 0);

      set_index (file, dbf_header_pos);
      write_dbf_header (file, file_header);
   end;

   procedure write_desc   (file: in out file_type; desc: in col_description) is
      col: dbf_column;
      procedure put_name (name: in byte_string) is
      begin
         col.name (1..name'length) := name;
         col.name (name'length + 1..col.name'last) := (others => 0);
      end;
   begin
      put_name (convert (desc.name (1..desc.length)));
      col.attr     := byte (desc.params.attrib);
      col.length   := byte (desc.params.length);
      col.align    := byte (desc.params.align);
      col.pointer  := (others => 0);
      col.reserved := (others => 0);
      write_dbf_column (file, col);
   end;

   procedure read_info (file:   in out file_type;
                        header: in out file_header;
                        tab:  in out col_desc_table) is
      eof_cols: boolean := false;
      len_cols: natural := 0;
      col_offs: natural := 0;
   begin
      read_header (file, header);
      if header.col_count > tab'length
      then
         raise storage_error;
      end if;
      set_index (file, header.col_begins);
      for n in tab'range
      loop
         read_desc (file, tab (n), eof_cols);
         tab (n).offset := col_offs;
         exit when eof_cols or else n >= header.col_count;
         col_offs := col_offs + tab(n).params.length;
         len_cols := len_cols + 1;
      end loop;

      if len_cols < 1
      then
         raise data_error;
      end if;

   exception
      when storage_error => raise;
      when others => raise data_error;
   end;

   procedure write_info (file:   in out file_type;
                         header: in out file_header;
                         tab:      in col_desc_table) is
   begin

      header.row_posinc  := unsigned_io.count (header.row_length + 1);
      header.col_namelen := 11;
      header.col_begins  := 33;
      header.row_begins  := 32 * tab'length + 1 + header.col_begins;

      write_header (file, header);
      set_index (file, header.col_begins);
      for n in tab'range
      loop
         write_desc (file, tab (n));
      end loop;
      write (file, 16#0d#);
      set_index (file, size (file) + 1);
      write (file, 16#1a#);
   exception
      when device_error => raise;
      when others => raise data_error;
   end;

   procedure write_record (file:   in out file_type;
                           header: in     file_header;
                           buf:    in     byte_string;
                           attr:   in     row_attribute;
                           to:     in     positive_count) is
   begin
      set_index (file, header.row_posinc * unsigned_io.count(to - 1)
                 + header.row_begins);
      write (file, byte (attr));
      write (file, buf);
   end;

   procedure read_record  (file:   in out file_type;
                           header: in     file_header;
                           buf:       out byte_string;
                           attr:      out row_attribute;
                           from:   in     positive_count) is
      attrib: byte;
   begin
      set_index (file, header.row_posinc * unsigned_io.count(from - 1)
                 + header.row_begins);
      read (file, attrib);
      attr := row_attribute (attrib);
      read (file, buf);
   end;

end;
