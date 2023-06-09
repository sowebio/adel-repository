With Text_IO,
     B_Tree,
     Ch5Struc,
     Direct_IO;
PROCEDURE Alg8_3 Is

    Package Rel_Car_IO Is New Direct_IO ( Ch5Struc.Car_Master_Record );

    record_number     : Positive;
    relative_file     : Rel_Car_IO.File_Type;

    Package Car_B_Tree Is New B_Tree ( 5, Ch5Struc.Id_String, ">" );

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


BEGIN  -- Alg8_3
    Rel_Car_IO.Open ( relative_file, Rel_Car_IO.INOUT_FILE, "relcar.dat  ");
    Text_IO.Open ( Ch5Struc.transfile, Text_IO.IN_FILE, "cartrans.txt");
    Car_B_Tree.Open_B_Tree ( "carbtree.dat  " );
    Loop
        Exit When Text_IO.End_of_File ( Ch5Struc.transfile );
        Get_Next_Trans ( Ch5Struc.transaction );
        Text_IO.Put ( Ch5Struc.transaction.id_number );
        Case Ch5Struc.transaction.update_code Is
            When 'A' =>
                 Begin
                     Ch5Struc.master_record :=
                            ( Ch5Struc.transaction.id_number,
                              Ch5Struc.transaction.make,
                              Ch5Struc.transaction.style,
                              Ch5Struc.transaction.model,
                              Ch5Struc.transaction.mileage,
                              Ch5Struc.transaction.color );
                     record_number := Positive (Rel_Car_IO.Size ( relative_file )) + 1;
                     Car_B_Tree.Insert ( Ch5Struc.master_record.id_number,
                                         record_number );
                     Rel_Car_IO.Write ( relative_file, Ch5Struc.master_record,
                                        Rel_Car_IO.Positive_Count ( record_number ) );
                     Text_IO.Put_Line ( " add   ");
                 Exception
                     When Car_B_Tree.key_not_inserted =>
                         Text_IO.Put_Line ("  duplicate add");
                 End;
            When 'C' =>
                 Begin
                     Car_B_Tree.Search ( Ch5Struc.transaction.id_number,
                                         record_number );
                     Rel_Car_IO.Read ( relative_file, Ch5Struc.master_record,
                                       Rel_Car_IO.Positive_Count ( record_number ) );
                     Ch5Struc.master_record.mileage := Ch5Struc.transaction.mileage;
                     Rel_Car_IO.Write ( relative_file, Ch5Struc.master_record,
                                       Rel_Car_IO.Positive_Count ( record_number ) );
                     Text_IO.Put_Line ( "  changed" );
                 Exception
                     When Car_B_Tree.key_not_found =>
                         Text_IO.Put_Line ( " no matching master for change");
                 End;
            When 'D' =>
                 Begin
                     Car_B_Tree.Delete (Ch5Struc.transaction.id_number);
                     Text_IO.Put_Line ( " deleted");
                 Exception
                     When Car_B_Tree.key_not_deleted =>
                        Text_IO.Put_Line ("  no matching master for delete");
                 End;
            When Others => Text_IO.Put_Line ( " invalid update code");
        End Case;
        Text_IO.New_Line;
    End Loop;
    Rel_Car_IO.Close ( relative_file );
    Text_IO.Close ( Ch5Struc.transfile );
    Car_B_Tree.Close_B_Tree;
END Alg8_3;
