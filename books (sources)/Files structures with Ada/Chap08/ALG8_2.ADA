With Text_IO,
     B_Tree,
     Ch5Struc,
     Direct_IO;
PROCEDURE Alg8_2 Is

    Package Rel_Car_IO Is New Direct_IO ( Ch5Struc.Car_Master_Record );

    relative_file     : Rel_Car_IO.File_Type;
    transaction_file  : Text_IO.File_Type;
    transaction_record: Ch5Struc.Car_Master_Record;

    Package Car_B_Tree Is New B_Tree ( 5, Ch5Struc.Id_String, ">" );

    PROCEDURE Get_next_trans (transaction : In Out Ch5Struc.Car_Master_Record)  Is
        update_code :  Character;
    BEGIN  -- Get_next_trans
        Text_IO.Get ( transaction_file, update_code);
        Text_IO.Put ( ' ' );
        Text_IO.Put ( update_code );
        Text_IO.Put ( ' ' );
        Text_IO.Get ( transaction_file, transaction.id_number);
        Text_IO.Put ( transaction.id_number );
        Text_IO.Put ( ' ' );
        Case update_code Is
            When 'A' =>
                Text_IO.Get ( transaction_file, transaction.make);
                Text_IO.Put ( ' ' );
                Text_IO.Put ( transaction.make );
                Text_IO.Get ( transaction_file, transaction.style);
                Text_IO.Put ( ' ');
                Text_IO.Put ( transaction.style );
                Text_IO.Get ( transaction_file, transaction.model);
                Text_IO.Put ( ' ' );
                Text_IO.Put ( transaction.model );
                Text_IO.Get ( transaction_file, transaction.mileage);
                Text_IO.Put ( ' ');
                Text_IO.Put ( transaction.mileage );
                Text_IO.Get ( transaction_file, transaction.color);
                Text_IO.Put ( ' ' );
                Text_IO.Put ( transaction.color );
                Text_IO.Put ( ' ' );
            When Others =>
                Text_IO.Put ( "                             " );
                Text_IO.Put ( "invalid update code" );
        End Case;
        Text_IO.Skip_Line ( transaction_file );
    END Get_next_trans;

BEGIN  -- Alg8_2
    Rel_Car_IO.Create ( relative_file, Rel_Car_IO.INOUT_FILE, "relcar.dat  ");
    Text_IO.Open ( transaction_file, Text_IO.IN_FILE, "t1.txt");
    Car_B_Tree.Create_B_Tree ( "carbtree.dat  " );
    Loop
        Exit When Text_IO.End_of_File ( transaction_file );
        Get_Next_Trans ( transaction_record );
        Rel_Car_IO.Write ( relative_file, transaction_record );
        Text_IO.Put ( transaction_record.id_number );
        Car_B_Tree.Insert
               ( transaction_record.id_number,
                 Positive ( Rel_Car_IO.Index ( relative_file ) ) - 1 );
        Text_IO.New_Line;
    End Loop;
    Rel_Car_IO.Close ( relative_file );
    Text_IO.Close ( transaction_file );
    Car_B_Tree.Close_B_Tree;
END Alg8_2;