--::::::::::
--testdbas.adb
--::::::::::
-- Developed by (C) Wasiliy W. Molostoff 1994, 1995.
--                  Moscow, Russia,
--                  Voice:   7 (095) 398-23-38
--	            e-mail:  edv@edv.msk.ru
-- This is free software; you can  freely  redistribute  it  and/or
-- modify  it  without any restrictions.  Please report any errors.
-- All corrections will be made as soon as possible.

with unsigned, dbase_io, calendar;
use  unsigned, dbase_io;

procedure test_dbase is

  type    field   is (name, phone, memo, male);
  package ph_book is new table_io (field);

  type number is range 0..10**7;

  use ph_book;

  my_phone_book: constant table := (name   => (db_character,  32, 0),
				    phone  => (db_numeric,    number'width, 0),
				    memo   => (db_date,       25, 0),
				    male   => (db_boolean,     0, 0));

  inpf: file_type;
  outf: file_type;
  buff: byte_string (1..256);
  temp: calendar.time;
  numb: number;
  bool: boolean;

  package num_io is new integer_io (number);

  use num_io;

begin
  create (outf, my_phone_book, "ph_book.dbf");
  close (outf);

  open (inpf, inout_file, "ph_book.dbf");

  row_index (inpf, 1);
  put       (inpf, phone, 3981234);
  put       (inpf, name, "William");
  put       (inpf, memo, calendar.clock);
  put       (inpf, male, true);

  get       (inpf, male, bool);
  put       (inpf, male, bool);

  get       (inpf, memo, temp);
  put       (inpf, memo, temp);

  get       (inpf, phone, numb);
  put       (inpf, phone, numb);

  row_index (inpf, 2);
  put       (inpf, phone, 1234567);
  put       (inpf, name, "Qwerty");
  put       (inpf, memo, calendar.clock);
  put       (inpf, male, false);

  get       (inpf, male, bool);
  put       (inpf, male, bool);

  get       (inpf, memo, temp);
  put       (inpf, memo, temp);

  get       (inpf, phone, numb);
  put       (inpf, phone, numb);


  row_index (inpf, 3);
  put       (inpf, phone, 7654321);
  put       (inpf, name, "KALLE");
  put       (inpf, memo, calendar.clock);
  put       (inpf, male, false);

  get       (inpf, male, bool);
  put       (inpf, male, bool);

  get       (inpf, memo, temp);
  put       (inpf, memo, temp);

  get       (inpf, phone, numb);
  put       (inpf, phone, numb);

  row_index (inpf, 4);
  put       (inpf, phone, 1237654);
  put       (inpf, name, "Lib");
  put       (inpf, memo, calendar.clock);
  put       (inpf, male, false);

  get       (inpf, male, bool);
  put       (inpf, male, bool);

  get       (inpf, memo, temp);
  put       (inpf, memo, temp);

  get       (inpf, phone, numb);
  put       (inpf, phone, numb);

  close (inpf);

end test_dbase;
