With Text_IO,
     Multilst;
PROCEDURE Alg10_4 Is

    id_no            : Multilst.Primary_Key_Type;
    new_record       : Multilst.Record_Info;
    transaction_code : Character;
    transfile        : Text_IO.File_Type;

BEGIN -- Alg10_4
    Multilst.Open ( "cardata" );
    Text_IO.Open ( transfile, Text_IO.IN_FILE, "cartrans.txt" );
    Loop
        Exit When Text_IO.End_Of_File ( transfile );
        Text_IO.Get ( transfile, transaction_code );
        Text_IO.Put ( transaction_code );
        Text_IO.Put ( " " );
        Text_IO.Get ( transfile, id_no );
        Text_IO.Put_Line ( id_no );
        BEGIN
            Case transaction_code Is
                When 'A' => new_record.id_no := id_no;
                            Text_IO.Get ( transfile, new_record.make );
                            Text_IO.Get ( transfile, new_record.style );
                            Text_IO.Get ( transfile, new_record.model );
                            Text_IO.Get ( transfile, new_record.mileage );
                            Text_IO.Get ( transfile, new_record.color );
                            Multilst.Insert ( new_record );
                            Text_IO.Put_Line ( "    added " );
                When 'D' => Multilst.Delete ( id_no );
                            Text_IO.Put_Line ( "   deleted " );
                When Others => Text_IO.Put_Line ( " invalid update code" );
            End Case;
        EXCEPTION
            When Multilst.key_not_inserted =>
                     Text_IO.Put_Line ( "   duplicate key" );
            When Multilst.key_not_deleted =>
                     Text_IO.Put_Line ( "   key not found on delete " );
        END;
        Text_IO.Skip_Line ( transfile );
    End Loop;
    Text_IO.Close ( transfile );
    Multilst.Close ( "cardata" );
END Alg10_4;