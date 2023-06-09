With Text_IO,
     Invert;
PROCEDURE Alg10_1 Is
    input_file : Text_IO.File_Type;
    new_record : Invert.Record_Info;
    code       : Character;
BEGIN -- Alg10_1
    Text_IO.Open ( input_file, Text_IO.IN_FILE, "cardata.txt" );
    Invert.Create ( "cardata" );
    Loop
        Exit When Text_IO.End_Of_File ( input_file );
        Text_IO.Get ( input_file, code );
        Text_IO.Get ( input_file, new_record.id_no );
        Text_IO.Put_Line ( new_record.id_no );
        Text_IO.Get ( input_file, new_record.make );
        Text_IO.Get ( input_file, new_record.style );
        Text_IO.Get ( input_file, new_record.model );
        Text_IO.Get ( input_file, new_record.mileage );
        Text_IO.Get ( input_file, new_record.color );
        Text_IO.Skip_Line ( input_file );
        BEGIN
            Invert.Insert ( new_record );
        EXCEPTION
            When Invert.key_not_inserted =>
                Text_IO.Put ( new_record.id_no );
                Text_IO.Put_Line ( "    Duplicate Key " );
        END;
    End Loop;
    Text_IO.Close ( input_file );
    Invert.Close ( "cardata" );
END Alg10_1;