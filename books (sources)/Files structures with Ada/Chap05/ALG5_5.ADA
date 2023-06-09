With Text_IO;
With Sequential_IO;
With Ch5Struc;
Use Ch5Struc;
PROCEDURE Alg5_5 Is
    PACKAGE Float_IO Is New Text_IO.Float_IO ( Float );
    PACKAGE Integer_IO Is New Text_IO.Integer_IO ( Integer );
    BLANKS           : Constant String (1..6) := "      ";
    current_key      : Id_String;
    hold_master      : Car_Master_Record;
    master_allocated : Boolean;

    PROCEDURE Get_Next_Trans Is
    BEGIN
        If Text_IO.End_Of_File ( transfile ) Then
            transaction.update_code := ' ';
            transaction.id_number := SENTINEL;
        Else
            Text_IO.Get  ( transfile, transaction.update_code );
            Text_IO.Get ( transfile, transaction.id_number );
        End If;
        Text_IO.Put ( transaction.update_code );
        Text_IO.Put ( " " );
        Text_IO.Put ( transaction.id_number );
    END Get_Next_Trans;

    PROCEDURE Get_Next_Master Is
    BEGIN  -- Get_Next_Master
        If Car_IO.End_Of_File (masterfile) Then
            master_record.id_number := sentinel;
        Else
            Car_IO.Read (masterfile, master_record);
        End If;
    EXCEPTION   -- master file does not exist
        When Car_IO.Status_Error => Null;
    END Get_Next_Master;

    PROCEDURE Choose_Smaller_Key Is
    BEGIN
        If transaction.id_number < master_record.id_number Then
            current_key := transaction.id_number;
        Else
            current_key := master_record.id_number;
        End If;
    END Choose_Smaller_Key;

    PROCEDURE Check_Initial_Status_Of_Master Is
    BEGIN
        If master_record.id_number = current_key Then
            hold_master := master_record;
            master_allocated := TRUE;
            Get_Next_Master;
        Else
            master_allocated := FALSE;
        End If;
    END Check_Initial_Status_Of_Master;

    PROCEDURE Check_Final_Status_Of_Master Is
    BEGIN
        If master_allocated Then
            Car_IO.Write (newfile, hold_master);
        End If;
    END Check_Final_Status_Of_Master;

    PROCEDURE Change Is
    BEGIN
        -- Change master record
        Text_IO.Get ( transfile, hold_master.mileage );
    END Change;

    PROCEDURE Process_One_Transaction Is
    BEGIN
        If master_allocated Then
            Case transaction.update_code Is
                When 'A' =>
                    Text_IO.Put_Line ( "duplicate add" );
                When 'C' =>
                    Change;
                    Text_IO.Put_Line ( " change" );
                When 'D' =>
                    master_allocated := FALSE;
                    Text_IO.Put_Line ( " deleted" );
                When Others =>
                    Text_IO.Put ( transaction.update_code );
                    Text_IO.Put_Line ( " Illegal update code " );
            End Case;
            Text_IO.Skip_Line ( transfile );
        Else
            Case transaction.update_code Is
                When 'A' =>
                     Text_IO.Put ( " addition " );
                     Text_IO.Put_Line ( transaction.id_number );
                     hold_master.id_number := transaction.id_number;
                     Text_IO.Get ( transfile, hold_master.make );
                     Text_IO.Get ( transfile, hold_master.style );
                     Text_IO.Get ( transfile, hold_master.model );
                     Text_IO.Get ( transfile, hold_master.mileage );
                     Text_IO.Get ( transfile, hold_master.color );
                     master_allocated := TRUE;
                When 'C' | 'D' =>
                     Text_IO.Put_Line (" no matching master record ");
                When Others =>
                    Text_IO.Put ( transaction.update_code );
                    Text_IO.Put_Line ( " Illegal update code " );
            End Case;
            Text_IO.Skip_Line ( transfile );
        End If;
        Get_Next_Trans;
    END Process_One_Transaction;

  PROCEDURE List_master Is
    masters_read : Integer := 0;
    new_record   : Car_Master_Record;
  BEGIN  -- List_master
      Car_IO.Open ( newfile, Car_IO.IN_FILE, "newmstr.dat" );
      Text_IO.New_Page;
      Text_IO.Put ( BLANKS );
      Text_IO.Put_Line ( "***** dump of new master file *****" );
      Loop
          Exit When Car_IO.End_Of_File (newfile);
          masters_read := masters_read + 1;
          Car_IO.Read ( newfile, new_record );
          Text_IO.Put ( BLANKS );
          Text_IO.Put ( new_record.id_number );
          Text_IO.Put ( "---" );
          Text_IO.Put ( new_record.make );
          Text_IO.Put ( "---" );
          Text_IO.Put ( new_record.style );
          Text_IO.Put ( "---" );
          Text_IO.Put ( new_record.model );
          Text_IO.Put ( "---" );
          Text_IO.Put ( new_record.mileage );
          Text_IO.Put ( "---" );
          Text_IO.Put ( new_record.color );
          Text_IO.New_Line;
      End Loop;
      Text_IO.Put ( BLANKS);
      Integer_IO.Put ( masters_read );
      Text_IO.Put_Line ( " new master records read ");
      Car_IO.Close ( newfile );
  END List_master;

BEGIN  -- Alg5_5
    BEGIN
        Car_IO.Open (masterfile, Car_IO.IN_FILE, "carmstr.dat");
    EXCEPTION       --master file does not exist
        When Car_IO.Name_Error =>  master_record.id_number := SENTINEL;
    END;
    Car_IO.Create (newfile, Car_IO.OUT_FILE, "newmstr.dat");
    BEGIN
        Text_IO.Open (transfile, Text_IO.IN_FILE, "cartrans.txt");
    EXCEPTION
        When Text_IO.Name_Error => Null;
    END;
    Get_Next_Trans;
    Get_Next_Master;
    Choose_Smaller_Key;
    Loop
        Exit When current_key = SENTINEL;
        Check_Initial_Status_Of_Master;
        Loop
            Exit When transaction.id_number /= current_key;
            Process_One_Transaction;
        End Loop;
        Check_Final_Status_Of_Master;
        Choose_Smaller_Key;
    End Loop;
    Car_IO.Close ( newfile );
    Text_IO.Close ( transfile );
    List_Master;
    Text_IO.New_Page;
    Car_IO.Close ( masterfile );
EXCEPTION
    When Car_IO.Status_Error => Null;
END Alg5_5;