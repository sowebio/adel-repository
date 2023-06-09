With Sequential_IO,
     Text_IO,
     VSAM,
     Ch5Struc;
PROCEDURE VAlg9_1 Is
    FUNCTION Car_Key ( car : Ch5Struc.Car_Master_Record )
             Return Ch5Struc.Id_String Is
    BEGIN
        Return car.id_number;
    END Car_Key;
    PROCEDURE Print_Key ( key : Ch5Struc.Id_String) Is
    BEGIN
        Text_IO.Put_Line ( key & '*' );
    END Print_Key;
    PACKAGE Car_BPlus Is New VSAM ( 3, 3,
                                          Ch5Struc.Id_String,
                                          Ch5Struc.Car_Master_Record,
                                          Car_Key, ">" , Print_Key);

    PROCEDURE Get_next_trans (transaction : In Out Ch5Struc.Car_Master_Record)  Is
        update_code :  Character;
    BEGIN  -- Get_next_trans
        Text_IO.Get ( Ch5Struc.transfile, update_code);
        Text_IO.Put ( ' ' );
        Text_IO.Put ( update_code );
        Text_IO.Put ( ' ' );
        Text_IO.Get ( Ch5Struc.transfile, transaction.id_number);
        Text_IO.Put ( transaction.id_number );
        Text_IO.Put ( ' ' );
        Case update_code Is
            When 'A' =>
                Text_IO.Get ( Ch5Struc.transfile, transaction.make);
                Text_IO.Put ( ' ' );
                Text_IO.Put ( transaction.make );
                Text_IO.Get ( Ch5Struc.transfile, transaction.style);
                Text_IO.Put ( ' ');
                Text_IO.Put ( transaction.style );
                Text_IO.Get ( Ch5Struc.transfile, transaction.model);
                Text_IO.Put ( ' ' );
                Text_IO.Put ( transaction.model );
                Text_IO.Get ( Ch5Struc.transfile, transaction.mileage);
                Text_IO.Put ( ' ');
                Text_IO.Put ( transaction.mileage );
                Text_IO.Get ( Ch5Struc.transfile, transaction.color);
                Text_IO.Put ( ' ' );
                Text_IO.Put ( transaction.color );
                Text_IO.Put ( ' ' );
            When Others =>
                Text_IO.Put ( "                             " );
                Text_IO.Put ( "invalid update code" );
        End Case;
        Text_IO.Skip_Line ( Ch5Struc.transfile );
    END Get_next_trans;

BEGIN  --Alg9_1
    Text_IO.Open ( Ch5Struc.transfile, Text_IO.IN_FILE, "cartrans.txt" );
    Car_BPlus.Create_BPlus_Tree ( "cardata.bpt", "cardata.blk" );
    Loop
        Exit When Text_IO.End_Of_File ( Ch5Struc.transfile );
        Get_Next_Trans (Ch5Struc.master_record);
        BEGIN
            Car_BPlus.Insert ( Ch5Struc.master_record );
            Text_IO.Put ( Ch5Struc.master_record.id_number );
            Text_IO.Put_Line ( "inserted" );
        EXCEPTION
            When Car_BPlus.key_not_inserted =>
                Text_IO.Put ( Ch5Struc.master_record.id_number );
                Text_IO.Put_Line ( " is a duplicate id number." );
        END;
    End Loop;
    Car_BPlus.Close_BPlus_Tree ( "cardata.bpt", "cardata.blk" );
END VAlg9_1;
