With Text_IO,
     Ch7Struc;
PROCEDURE Alg7_4 Is

    PACKAGE  Int_IO Is New Text_IO.Integer_IO ( Integer );
    blank_id_number   : Constant Ch7Struc.Key_Type := "   ";
    error             : Boolean;
    location,
    previous          : Ch7Struc.Rel_IO.Positive_Count;
    relativefile      : Ch7Struc.Rel_IO.File_Type;
    relative_index    : Ch7Struc.Rel_IO.Positive_Count;
    relative_master   : Ch7Struc.Rel_Record_Type;
    trans             : Text_IO.File_Type;
    trans_record      : Ch7Struc.Rel_Record_Type;
    trans_update_code : Character;

PROCEDURE Initialize Is

BEGIN  -- Initialize
    Text_IO.Put_Line ( "         ***** AUDIT/ERROR LIST *****" );
    Text_IO.New_Line;
    Text_IO.Put_Line
    ( "ID NUMBER MAKE   STYLE   MODEL  MILEAGE   COLOR " );
    Text_IO.New_Line;
    Text_IO.Put_Line
    ( "**********************************************************************" );
END Initialize;

PROCEDURE Input_Trans_Record ( trans : IN OUT Text_IO.File_Type;
                               trans_record : IN OUT Ch7Struc.Rel_Record_Type ) Is
    trans_record_info : Ch7Struc.Car_Record_Type Renames trans_record.info ;
BEGIN  -- Input_Trans_Record
    Text_IO.Get ( trans, trans_update_code );
    Text_IO.Get ( trans, trans_record_info.id_number );
    Text_IO.Put ( trans_update_code );
    Text_IO.Put ( ' ' );
    Text_IO.Put ( trans_record_info.id_number );
    Text_IO.Put ( "  " );
    Case trans_update_code Is
        When 'A' =>
            Text_IO.Get ( trans, trans_record_info.make );
            Text_IO.Get ( trans, trans_record_info.style );
            Text_IO.Get ( trans, trans_record_info.model );
            Text_IO.Get ( trans, trans_record_info.mileage );
            Text_IO.Get ( trans, trans_record_info.color );
            Text_IO.Put ( trans_record_info.make );
            Text_IO.Put ( trans_record_info.style );
            Text_IO.Put ( trans_record_info.model );
            Text_IO.Put ( trans_record_info.mileage );
            Text_IO.Put ( trans_record_info.color );
        When 'C' =>
            Text_IO.Get ( trans, trans_record_info.mileage );
            Text_IO.Put ( trans_record_info.mileage );
            Text_IO.Put ( "         " );
        When 'D' =>
            Text_IO.Put ( "                                   " );
        When Others =>
            Text_IO.Put ( "                                   " );
            Text_IO.Put_Line ( "  INVALID UPDATE CODE" );
    End Case;
    Text_IO.Skip_Line ( trans );
END Input_Trans_Record;

PROCEDURE Find_Record ( relative_master : In Out Ch7Struc.Rel_Record_Type;
                        relative_index,
                        previous : In Out Ch7Struc.Rel_IO.Positive_Count;
                        error : In Out Boolean;
                        key : In Ch7Struc.Key_Type ) Is

    trans_record_info : Ch7Struc.Car_Record_Type Renames trans_record.info;
BEGIN  -- Find_Record
    Ch7Struc.Rel_IO.Read ( relativefile, relative_master, relative_index );
    If relative_master.info.id_number = key Then
        error := FALSE;
    ElsIf relative_master.link = -1 Then
        error := TRUE;
    Else
        previous := relative_index;
        relative_index := Ch7Struc.Rel_IO.Positive_Count ( relative_master.link );
        Find_Record ( relative_master, relative_index, previous, error, key );
    End If;
END Find_Record;

PROCEDURE Add Is
    overflow : Integer;
BEGIN
    Text_IO.Put_Line ( "  VALID ADD" );
    If relative_master.info.id_number = blank_id_number Then
        trans_record.link := -1;
        Ch7Struc.Rel_IO.Write ( relativefile, trans_record, relative_index );
    Else
        Ch7Struc.Rel_IO.Read ( relativefile, relative_master, location );
        trans_record.link := relative_master.link;
        overflow := Integer ( Ch7Struc.Rel_IO.Size ( relativefile ) ) + 1;
        relative_master.link := overflow;
        Ch7Struc.Rel_IO.Write ( relativefile, relative_master, location );
        Ch7Struc.Rel_IO.Write ( relativefile, trans_record,
                                Ch7Struc.Rel_IO.Positive_Count ( overflow ) );
    End If;
END Add;

PROCEDURE Change Is

BEGIN
    Text_IO.Put_Line ( "  VALID CHANGE" );
    relative_master.info.mileage := trans_record.info.mileage;
    Ch7Struc.Rel_IO.Write ( relativefile, relative_master, relative_index );
END Change;

PROCEDURE Delete Is
    null_record : Ch7Struc.Rel_Record_Type;
    synonym   : Ch7Struc.Rel_IO.Positive_Count;
BEGIN
    Text_IO.Put_Line ( "  VALID DELETE" );
    If Positive ( relative_index ) = Positive ( location ) Then
        If relative_master.link = -1 Then
            Ch7Struc.Rel_IO.Write ( relativefile, null_record, location );
        Else
            synonym := Ch7Struc.Rel_IO.Positive_Count ( relative_master.link );
            Ch7Struc.Rel_IO.Read ( relativefile, relative_master, synonym );
            Ch7Struc.Rel_IO.Write ( relativefile, relative_master, relative_index );
            Ch7Struc.Rel_IO.Write ( relativefile, null_record, synonym );
        End If;
    Else
        synonym := Ch7Struc.Rel_IO.Positive_Count ( relative_master.link );
        Ch7Struc.Rel_IO.Read ( relativefile, relative_master, previous );
        relative_master.link := Integer ( synonym );
        Ch7Struc.Rel_IO.Write ( relativefile, relative_master, previous );
        Ch7Struc.Rel_IO.Write ( relativefile, null_record, relative_index );
    End If;
END Delete;


PROCEDURE List_Master Is

    i : Integer;

    PROCEDURE Print_Heading Is

    BEGIN
        Text_IO.New_Line;
        Text_IO.Put_Line
      ( "                               ***** UPDATED RELATIVE MASTER *****" );

        Text_IO.New_Line;
        Text_IO.Put_Line
( "     LOCATION   ID  NUMBER  MAKE  STYLE    POINTER" );
        Text_IO.New_Line;
    END Print_Heading;

BEGIN  -- List_Master
    Print_Heading;
    i := 1;
    Loop
        Exit When Ch7Struc.Rel_IO.End_of_File ( relativefile );
        Ch7Struc.Rel_IO.Read ( relativefile, relative_master );
        Declare
            relative_master_info : Ch7Struc.Car_Record_Type Renames relative_master.info;
        Begin
            If relative_master_info.id_number /= blank_id_number
            Or i = 1   Then
                Int_IO.Put ( i, 10 );
                Text_IO.Put ( "      " );
                Text_IO.Put ( relative_master_info.id_number );
                Text_IO.Put ( "    " );
                Text_IO.Put ( relative_master_info.make );
                Text_IO.Put ( relative_master_info.style );
                Int_IO .Put ( relative_master.link, 8 );
                Text_IO.New_Line;
            End If;
            i := i + 1;
        End;  -- Declare
    End Loop;
END List_Master;


BEGIN  -- Alg7_4
    Ch7Struc.Rel_IO.Open ( relativefile, Ch7Struc.Rel_IO.INOUT_FILE, "relcar.dat" );
    Text_IO.Open ( trans, Text_IO.IN_FILE, "cartrans.txt" );
    Initialize;
    Loop
        Input_Trans_Record ( trans, trans_record );
        location := Ch7Struc.Rel_IO.Positive_Count
                    ( Ch7Struc.Hash ( trans_record.info.id_number ) );
        relative_index := location;
        Find_Record ( relative_master, relative_index, previous, error,
                      trans_record.info.id_number );
        Case trans_update_code Is
            When 'A'=>
                If error Then
                    Add;
                Else
                    Text_IO.Put_Line ( "  INVALID ADD; ALREADY ON MASTER" );
                End If;
            When 'C'=>
                If error Then
                    Text_IO.Put_Line ( "  INVALID CHANGE; NOT ON MASTER" );
                Else
                    Change;
                End If;
            When 'D' =>
                If error Then
                    Text_IO.Put_Line ( "  INVALID DELETE; NOT ON MASTER" );
                Else
                    Delete;
                End If;
            When Others =>
                Text_IO.Put_Line ( "  INVALID TRANSACTION CODE" );
        End Case;
        Exit When Text_IO.End_of_File ( trans );
    End Loop;
    Ch7Struc.Rel_IO.Reset ( relativefile );
    List_Master;
    Ch7Struc.Rel_IO.Close ( relativefile );
    Text_IO.New_Page;
END Alg7_4;