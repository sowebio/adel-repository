With ISAM,
     Text_IO,
     Ch5STruc;
Procedure Alg9_10 Is
    Function Car_Key ( car_data : Ch5Struc.Car_Master_Record )
             Return Ch5Struc.Id_String Is
    Begin
        Return car_data.id_number;
    End Car_Key;
    Package Car_ISAM Is New ISAM ( 30,
                                     3,
                                     Ch5Struc.Id_String,
                                     Ch5Struc.Car_Master_Record,
                                     Car_Key,
                                     ">",
                                     "<" );
    Package Flt_IO Is New Text_IO.Float_IO(Float);

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


Begin
    Car_ISAM.Open_ISAM ( "carisam.inx",
                          "carisam.blk",
                          "carisam.ovr" );
    Text_IO.Open ( Ch5Struc.transfile, Text_IO.IN_FILE, "cartrans.txt" );
    Loop
        Exit When Text_IO.End_Of_File ( Ch5Struc.transfile );
        Get_Next_Trans (Ch5Struc.transaction);
        Case Ch5Struc.transaction.update_code Is
            When 'A' => Declare
                            trans : Ch5Struc.Car_Transaction_Record
                                        renames Ch5Struc.transaction;
                        Begin
                            Ch5Struc.master_record :=
                            ( trans.id_number,
                              trans.make,
                              trans.style,
                              trans.model,
                              trans.mileage,
                              trans.color );
                            Car_ISAM.Insert ( Ch5Struc.master_record );
                            Text_IO.Put_Line ( " add");
                        Exception
                            When Car_ISAM.key_not_inserted =>
                                Text_IO.Put_Line ("  duplicate add");
                        End;
            When 'C' => Begin
                            Car_ISAM.Retrieve ( Ch5Struc.transaction.id_number,
                                                 Ch5Struc.master_record );
                            Ch5Struc.master_record.mileage :=
                                Ch5Struc.transaction.mileage;
                            Car_ISAM.Replace ( Ch5Struc.master_record );
                            Text_IO.Put_Line ( "  changed");
                        Exception
                             When Car_ISAM.key_not_found =>
                                 Text_IO.Put_Line ( " no matching master for change");
                        End;
            When 'D' => Begin
                            Car_ISAM.Delete (Ch5Struc.transaction.id_number);
                            Text_IO.Put_Line ( " deleted");
                        Exception
                            When Car_ISAM.key_not_deleted =>
                                Text_IO.Put_Line ("  no matching master for delete");
                        End;
            When Others => Text_IO.Put_Line ( " invalid update code");
        End Case;
    End Loop;
    Car_ISAM.Close_ISAM;
    Text_IO.Close ( Ch5Struc.transfile );
End Alg9_10;