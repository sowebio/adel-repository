With Text_IO,
     MultiLst;
PROCEDURE Alg10_3 Is

    char         : Character;
    input_file   : Text_IO.File_Type;
    new_record   : MultiLst.Record_Info;

BEGIN  -- Alg10_3
    Text_IO.Open ( input_file, Text_IO.IN_FILE, "t1.txt");
    Multilst.Create ( "cardata" );
    Loop
        Exit When Text_IO.End_Of_File ( input_file );
        Text_IO.Get ( input_file, char );
        Text_IO.Get ( input_file, new_record.id_no );
        Text_IO.Put_Line ( new_record.id_no );
        Text_IO.Get ( input_file, new_record.make );
        Text_IO.Get ( input_file, new_record.style );
        Text_IO.Get ( input_file, new_record.model );
        Text_IO.Get ( input_file, new_record.mileage );
        Text_IO.Get ( input_file, new_record.color );
        Text_IO.Skip_Line ( input_file );
        BEGIN
            Multilst.Insert ( new_record );
        EXCEPTION
            When Multilst.key_not_inserted =>
                 Text_IO.Put_Line ( " Duplicate Key " );
        END;
    End Loop;
    Text_IO.Close ( input_file );
    Multilst.Close ( "cardata" );
END Alg10_3;