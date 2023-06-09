With Text_IO,
     Invert;
PROCEDURE Alg10_2 Is

    id_no            : Invert.Primary_Key_Type;
    new_record       : Invert.Record_Info;
    transaction_code : Character;
    transfile        : Text_IO.File_Type;

BEGIN -- Alg10_2
    Text_IO.Open ( transfile, Text_IO.IN_FILE, "cartrans.txt" );
    Invert.Open ( "cardata" );
    Loop
        Exit When Text_IO.End_Of_File ( transfile );
        Text_IO.Get ( transfile, transaction_code );
        Text_IO.Put ( transaction_code );
        Text_IO.Put ( " " );
        Text_IO.Get ( transfile, id_no );
        Text_IO.Put_Line ( id_no );
        BEGIN
            Case transaction_code Is
                When 'A' =>
                    new_record.id_no := id_no;
                    Text_IO.Get ( transfile, new_record.make );
                    Text_IO.Get ( transfile, new_record.style );
                    Text_IO.Get ( transfile, new_record.model );
                    Text_IO.Get ( transfile, new_record.mileage );
                    Text_IO.Get ( transfile, new_record.color );
                    Invert.Insert ( new_record );
                When 'D' =>
                    Invert.Delete ( id_no );
                When Others =>
                    Text_IO.Put_Line ( "Invalid Update Code" );
            End Case;
        EXCEPTION
            When Invert.key_not_inserted => Text_IO.Put_Line("   Duplicate key " );
            When Invert.key_not_deleted => Text_IO.Put_Line("   key not deleted" );
        END;
        Text_IO.Skip_Line ( transfile );
    End Loop;
    Invert.Close ( "cardata" );
    Text_IO.Close ( transfile );
END Alg10_2;