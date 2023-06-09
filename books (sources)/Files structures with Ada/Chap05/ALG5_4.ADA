With Text_IO;
With Sequential_IO;
With Ch5Struc;
Use Ch5Struc;
PROCEDURE Alg5_4 Is
    PACKAGE Integer_IO Is New Text_IO.Integer_IO ( Integer );
    adds_processed      : Integer := 0;
    BLANKS              : Constant String ( 1 .. 6 ) := "      ";
    changes_processed   : Integer := 0;
    deletefound         : Boolean;
    deletes_processed   : Integer := 0;
    masters_read,
    new_masters_written : Integer := 0;
    new_record          : Car_Master_Record;
    trans_error         : Integer := 0;
    trans_read          : Integer := 0;

PROCEDURE Get_next_trans Is
BEGIN  -- Get_next_trans
    If Text_IO.End_Of_File ( transfile) Then
        transaction.id_number := SENTINEL;
    Else
        trans_read := trans_read + 1;
        Text_IO.Get ( transfile, transaction.update_code);
        Text_IO.Put ( BLANKS );
        Text_IO.Put ( transaction.update_code );
        Text_IO.Put ( ' ' );
        Text_IO.Get ( transfile, transaction.id_number);
        Text_IO.Put ( transaction.id_number );
        Text_IO.Put ( ' ' );
        Case transaction.update_code Is
            When 'A' =>
                Text_IO.Get ( transfile, transaction.make);
                Text_IO.Put ( "  " );
                Text_IO.Put ( transaction.make );
                Text_IO.Get ( transfile, transaction.style);
                Text_IO.Put ( ' ');
                Text_IO.Put ( transaction.style );
                Text_IO.Get ( transfile, transaction.model);
                Text_IO.Put ( ' ' );
                Text_IO.Put ( transaction.model );
                Text_IO.Get ( transfile, transaction.mileage);
                Text_IO.Put ( ' ');
                Text_IO.Put ( transaction.mileage );
                Text_IO.Get ( transfile, transaction.color);
                Text_IO.Put ( ' ' );
                Text_IO.Put ( transaction.color );
                Text_IO.Put ( ' ' );
            When 'C' =>
                Text_IO.Get ( transfile, transaction.mileage);
                Text_IO.Put ( "  " );
                Text_IO.Put ( transaction.mileage );
                Text_IO.Put ( "           " );
            When 'D' =>
                Text_IO.Put ("                                       " );
            When Others =>
                Text_IO.Put ( "                             " );
                Text_IO.Put ( "invalid update code" );
        End Case;
        Text_IO.Skip_Line ( transfile );
    End If;
END Get_next_trans;

PROCEDURE Get_next_master Is
BEGIN  -- Get_next_master
    If  Car_IO.End_Of_File ( masterfile ) Then
        master_record.id_number := SENTINEL;
    Else
        masters_read := masters_read + 1;
        Car_IO.Read ( masterfile, master_record );
    End If;
EXCEPTION
    When Car_IO.Status_Error => Null;  --master file does not exist
END Get_next_master;

PROCEDURE Change_master ( hold_record : In Out Car_master_record ) Is
BEGIN  -- Change_master
    Text_IO.Put_Line ( "Change_master " );
    hold_record.mileage := transaction.mileage;
    changes_processed := changes_processed + 1;
END Change_master;

PROCEDURE Build_new_record Is
                 -- Build_new_record moves all the information in the
                 -- transaction into new_record
BEGIN  -- Build_new_record
    Text_IO.Put_Line ( "Build_new_record" );
    new_record :=  (transaction.id_number, transaction.make, transaction.style,
                    transaction.model, transaction.mileage, transaction.color);
END Build_new_record;

PROCEDURE Match Is
BEGIN  -- Match
    Case transaction.update_code Is
        When 'A' =>
            Text_IO.Put_Line ( "invalid add; on master file" );
            trans_error := trans_error + 1;
        When 'C' =>
            Change_master ( master_record );
        When 'D' =>
            deletes_processed := deletes_processed + 1;
            Text_IO.Put_Line ( " deleted" );
            Get_next_master;
        When Others =>
            trans_error := trans_error + 1;
            Text_IO.Put_Line ( ";on master file" );
    End case;
    Get_next_trans;
END Match;

PROCEDURE Nomatch Is
    PROCEDURE MatchesToAdds Is
    BEGIN  -- MatchesToAdds
        Case transaction.update_code Is
           When 'A'=>
               Text_IO.Put_Line ( "duplicate add" );
               trans_error := trans_error + 1;
           When 'C' =>
               Change_master ( new_record );
           When 'D' =>
               deletefound := TRUE ;
               Text_IO.Put_line ("Deleted" );
               deletes_processed := deletes_processed + 1;
           When Others =>
               trans_error := trans_error + 1;
               Text_IO.Put_Line ( "& an add in run" );
        End case;
    END MatchesToAdds;

    PROCEDURE NoMatchError Is
    BEGIN  -- NoMatchError
        Text_IO.Put_Line ( ";not on master file " );
        trans_error := trans_error + 1;
    END NoMatchError;

BEGIN  -- Nomatch
    Case transaction.update_code Is
        When 'A' =>
             Build_new_record;
             adds_processed := adds_processed + 1;
             deletefound := FALSE ;
             Get_next_trans;
             Loop
                 Exit When ( transaction.id_number = SENTINEL )
                      Or   ( transaction.id_number /= new_record.id_number )
                      Or   ( deletefound );
                 MatchesToAdds;
                 Get_next_trans;
             End Loop;
             If Not deletefound Then
                 Car_IO.Write (newfile, new_record);
                 new_masters_written := new_masters_written + 1;
             End If;
        When 'C' =>
            NoMatchError;
            Get_next_trans;
        When 'D' =>
            NoMatchError;
            Get_next_trans;
        When Others =>
            NoMatchError;
            Get_next_trans;
    End case;
END Nomatch;


   PROCEDURE Print_counts Is

   BEGIN  -- Print_counts
       Text_IO.New_Page;
       Text_IO.Put ( BLANKS );
       Integer_IO.Put ( masters_read );
       Text_IO.Put_Line ( " master records read" );
       Text_IO.Put ( BLANKS );
       Integer_IO.Put ( trans_read );
       Text_IO.Put_Line ( " transactions read" );
       Text_IO.Put ( BLANKS );
       Text_IO.Put ( BLANKS );
       Integer_IO.Put ( trans_error);
       Text_IO.Put_Line ( " transactions in error" );
       Text_IO.Put ( BLANKS );
       Text_IO.Put ( BLANKS );
       Integer_IO.Put ( adds_processed );
       Text_IO.Put_Line ( " add transactions processed" );
       Text_IO.Put ( BLANKS );
       Text_IO.Put ( BLANKS );
       Integer_IO.Put ( changes_processed );
       Text_IO.Put_Line ( " changes processed" );
       Text_IO.Put ( BLANKS );
       Text_IO.Put ( BLANKS );
       Integer_IO.Put ( deletes_processed );
       Text_IO.Put_Line ( " deletes processed" );
       Text_IO.Put ( BLANKS );
       Integer_IO.Put ( new_masters_written );
       Text_IO.Put_Line ( " new master records written" );
   END Print_counts;

  PROCEDURE List_master Is

  BEGIN  -- List_master
      Car_IO.Open ( newfile, Car_IO.IN_FILE, "NEWMSTR.DAT" );
      masters_read := 0;
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


BEGIN  -- Alg5_4
    BEGIN
        Car_IO.Open ( masterfile, Car_IO.IN_FILE,  "CARMSTR.DAT" );
    EXCEPTION   --master file does not exist
        When Car_IO.Name_Error => master_record.id_number := SENTINEL;
    END;
    Text_IO.Open ( transfile, Text_IO.IN_FILE, "cartrans.TXT" );
    Car_IO.Create ( newfile, Car_IO.OUT_FILE, "NEWMSTR.DAT" );
    Get_next_trans;
    Get_next_master;
    Loop
        Exit When ( master_record.id_number = SENTINEL )
              And ( transaction.id_number = SENTINEL );
        If ( master_record.id_number < transaction.id_number ) Then
            Car_IO.Write (newfile, master_record);
            new_masters_written := new_masters_written + 1;
            Get_next_master;
        Else          -- master key >= trans key
            If  master_record.id_number = transaction.id_number Then
                Match;
            Else       -- master key > trans key
                Nomatch;
            End If;
        End If;
    End Loop;
    Text_IO.Close ( transfile );
    Car_IO.Close ( newfile );
    Print_counts;
    List_master;
    Car_IO.Close ( masterfile );
EXCEPTION
    When Car_IO.Status_Error => Null;  --master file did not exist
END Alg5_4;