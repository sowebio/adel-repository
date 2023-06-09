With Sequential_IO,
     Text_IO,
     BPLus_Tree,
     Ch5Struc;
PROCEDURE Alg9_5 Is
    FUNCTION Car_Key ( car : Ch5Struc.Car_Master_Record )
             Return Ch5Struc.Id_String Is
    BEGIN
        Return car.id_number;
    END Car_Key;
    PACKAGE Car_BPlus Is New BPlus_Tree ( 3, 3,
                                          Ch5Struc.Id_String,
                                          Ch5Struc.Car_Master_Record,
                                          Car_Key, ">" );
PROCEDURE Get_next_trans (transaction : In Out Ch5Struc.Car_Transaction_Record) Is
BEGIN  -- Get_next_trans
    Text_IO.Get ( Ch5Struc.transfile, transaction.update_code);
    Text_IO.Put ( ' ' );
    Text_IO.Put ( transaction.update_code );
    Text_IO.Put ( ' ' );
    Text_IO.Get ( Ch5Struc.transfile, transaction.id_number);
    Text_IO.Put ( transaction.id_number );
    Text_IO.Put ( ' ' );
    Case transaction.update_code Is
        When 'A' =>
            Text_IO.Get ( Ch5Struc.transfile, transaction.make);
            Text_IO.Put ( "  " );
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
        When 'C' =>
            Text_IO.Get ( Ch5Struc.transfile, transaction.mileage);
            Text_IO.Put ( "  " );
            Text_IO.Put ( transaction.mileage );
            Text_IO.Put ( "           " );
        When 'D' =>
            Text_IO.Put ("                                       " );
        When Others =>
            Text_IO.Put ( "                             " );
            Text_IO.Put ( "invalid update code" );
    End Case;
    Text_IO.Skip_Line ( Ch5Struc.transfile );
END Get_next_trans;

BEGIN  -- Alg9_5
    Car_BPlus.Open_BPlus_Tree ( "cardata.bpt", "cardata.blk" );
    Text_IO.Open ( Ch5Struc.transfile, Text_IO.IN_FILE, "cartrans.txt" );
    Loop
        Exit When Text_IO.End_Of_File ( Ch5Struc.transfile );
        Get_Next_Trans (Ch5Struc.transaction);
        Case Ch5Struc.transaction.update_code Is
            When 'A' => DECLARE
                            trans : Ch5Struc.Car_Transaction_Record
                                        Renames Ch5Struc.transaction;
                        BEGIN
                            Ch5Struc.master_record :=
                                ( trans.id_number,
                                  trans.make,
                                  trans.style,
                                  trans.model,
                                  trans.mileage,
                                  trans.color );
                            Car_BPlus.Insert ( Ch5Struc.master_record );
                            Text_IO.Put_Line ( " add");
                        EXCEPTION
                            When Car_BPlus.key_not_inserted =>
                                Text_IO.Put_Line ("  duplicate add");
                        END;
            When 'C' => BEGIN
                            Car_BPlus.Retrieve ( Ch5Struc.transaction.id_number,
                                                 Ch5Struc.master_record );
                            Ch5Struc.master_record.mileage :=
                                Ch5Struc.transaction.mileage;
                            Car_BPLus.Replace ( Ch5Struc.master_record );
                            Text_IO.Put_Line ( "  changed");
                        EXCEPTION
                             When Car_BPlus.key_not_found =>
                                 Text_IO.Put_Line ( " no matching master for change");
                        END;
            When 'D' => BEGIN
                            Car_BPlus.Delete (Ch5Struc.transaction.id_number);
                            Text_IO.Put_Line ( " deleted");
                        EXCEPTION
                            When Car_BPlus.key_not_deleted =>
                                Text_IO.Put_Line ("  no matching master for delete");
                        END;
            When Others => Text_IO.Put_Line ( " invalid update code");
        End Case;
    End Loop;
    Car_BPlus.Close_BPlus_Tree ( "cardata.bpt", "cardata.blk" );
    Text_IO.Close ( Ch5Struc.transfile );
END Alg9_5;