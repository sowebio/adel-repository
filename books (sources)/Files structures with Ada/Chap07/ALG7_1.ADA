With Text_IO,
     Ch7Struc;
PROCEDURE Alg7_1 Is

    PACKAGE Integer_IO Is New Text_IO.Integer_IO ( Integer );

    carfile         : Text_IO.File_Type;
    car_record      : Ch7Struc.Car_Record_Type;
    error_flag      : Boolean;
    file_size       : Constant Positive := 18;
    lstfile         : Text_IO.File_Type;
    relative_master : Ch7Struc.Rel_Record_Type;
    relativefile    : Ch7Struc.Rel_IO.File_Type;
    relative_index  : Ch7Struc.Rel_IO.Positive_Count;

    PROCEDURE Input_Car Is
        update_code : Character;
    BEGIN
        Text_IO.Get ( carfile, update_code);
        Text_IO.Get ( carfile, car_record.id_number);
        Text_IO.Put ( lstfile, car_record.id_number );
        Text_IO.Put ( lstfile,  ' ' );
        Text_IO.Get ( carfile, car_record.make);
        Text_IO.Put ( lstfile,  "  " );
        Text_IO.Put ( lstfile,  car_record.make );
        Text_IO.Get ( carfile, car_record.style);
        Text_IO.Put ( lstfile,  ' ');
        Text_IO.Put ( lstfile,  car_record.style );
        Text_IO.Get ( carfile, car_record.model);
        Text_IO.Get ( carfile, car_record.mileage);
        Text_IO.Get ( carfile, car_record.color);
        Text_IO.Skip_Line ( carfile );
    END Input_Car;


    PROCEDURE Create_File Is
        PROCEDURE Overflow Is
            hold : Ch7Struc.Rel_Record_Type;
            next : Integer;
        BEGIN
            Text_IO.Put ( lstfile, "stored in overflow " );
            next := Integer ( Ch7Struc.Rel_IO.Size ( relativefile ) ) + 1;
            Integer_IO.Put ( lstfile, next, 0 );
            hold := ( car_record, relative_master.link );
            relative_master.link := next;
            Ch7Struc.Rel_IO.Write ( relativefile, relative_master,
                                    relative_index );
            Ch7Struc.Rel_IO.Write ( relativefile, hold,
                                    Ch7Struc.Rel_IO.Positive_Count ( next ) );
        END Overflow;

    BEGIN  -- Create_File
        Loop
            Exit When Text_IO.End_Of_File ( carfile );
            Input_Car;
            Text_IO.Put ( lstfile, car_record.id_number );
            Text_IO.Put ( lstfile, ' ' );
            relative_index := Ch7Struc.Rel_IO.Positive_Count
                              ( Ch7Struc.Hash ( car_record.id_number ) );
            Text_IO.Put ( lstfile, "hashes to " );
            Integer_IO.Put ( lstfile, Integer ( relative_index ) );
            Text_IO.Put ( lstfile, " ");
            Ch7Struc.Rel_IO.Read ( relativefile, relative_master,
                                   relative_index );
            If relative_master.info.id_number = "   " Then
                relative_master := ( car_record, -1 );
                Ch7Struc.Rel_IO.Write ( relativefile, relative_master,
                                        relative_index );
            Else
                Text_IO.Put ( lstfile, "synonym ");
                Overflow;
            End If;
            Text_IO.New_Line ( lstfile );
        End Loop;
    END Create_File;

    PROCEDURE Dump_File Is
    BEGIN
        Ch7Struc.Rel_IO.Reset ( relativefile );
        Text_IO.New_Page ( lstfile );
        Text_IO.Put_Line ( lstfile, "now list file back" );
        Loop
            Exit When Ch7Struc.Rel_IO.End_Of_File ( relativefile );
            Integer_IO.Put ( lstfile,
                             Integer (Ch7Struc.Rel_IO.Index (relativefile) ),
                             3 );
            Ch7Struc.Rel_IO.Read ( relativefile, relative_master );
            Text_IO.Put ( lstfile, "  " );
            Text_IO.Put ( lstfile, relative_master.info.id_number );
            Text_IO.Put ( lstfile, "  " );
            Text_IO.Put ( lstfile, relative_master.info.make );
            Text_IO.Put ( lstfile, "  " );
            Text_IO.Put ( lstfile, relative_master.info.style );
            Text_IO.Put ( lstfile, "  " );
            Text_IO.Put ( lstfile, relative_master.info.model );
            Text_IO.Put ( lstfile, "  " );
            Text_IO.Put ( lstfile, relative_master.info.mileage );
            Text_IO.Put ( lstfile, "  " );
            Text_IO.Put ( lstfile, relative_master.info.color );
            Text_IO.Put ( lstfile, "  " );
            Integer_IO.Put ( lstfile, relative_master.link, 3 );
            Text_IO.New_Line ( lstfile );
        End Loop;
    END Dump_File;

BEGIN  -- Alg7_1
    Text_IO.Create ( lstfile, Text_IO.OUT_FILE, "alg71out.txt" );
    Ch7Struc.Rel_IO.Create ( relativefile,
                             Ch7Struc.Rel_IO.INOUT_FILE,
                             "relcar.dat");
    For i IN 1 .. file_size Loop
        Ch7Struc.Rel_IO.Write ( relativefile, relative_master );
    End Loop;
    Ch7Struc.Rel_IO.Reset ( relativefile );
    Text_IO.Open ( carfile, Text_IO.IN_FILE, "t1.txt" );
    Create_FIle;
    Text_IO.Close ( carfile );
    Dump_File;
    Ch7Struc.Rel_IO.Close ( relativefile );
    Text_IO.Close ( lstfile );
END Alg7_1;