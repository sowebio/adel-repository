PACKAGE Invert Is
    SubType Primary_Key_Type Is String (1..3);
    SubType Secondary_Key_Type Is String (1..10);
    Type Record_Info Is Record
                            id_no   : Primary_Key_Type;
                            make    : Secondary_Key_Type;
                            style   : Secondary_Key_Type;
                            model   : Secondary_Key_Type;
                            mileage : Secondary_Key_Type;
                            color   : Secondary_Key_Type;
                        End Record;
    Type Secondary_Key_Name_Type Is ( MAKE, STYLE );

    PROCEDURE Create ( external_filename_prefix : String );
    PROCEDURE Open   ( external_filename_prefix : String );
    PROCEDURE Close  ( external_filename_prefix : String );
    PROCEDURE Insert ( new_record : Record_Info );
    PROCEDURE Delete ( id_no : In Primary_Key_Type );
    key_not_inserted,
    key_not_deleted : Exception;
END Invert;

With B_Tree;
With Direct_IO;
With Sequential_IO;
PACKAGE Body Invert Is
    Type Primary_Key_Record;
    Type Primary_Key_Access Is Access Primary_Key_Record;
    Type Primary_Key_Record Is Record
                                  primary_key : Primary_Key_Type;
                                  next_key    : Primary_Key_Access;
                               End Record;
    SubType Key_Value_Type Is String ( 1..10 );
    Type Key_Value_Record;
    Type Key_Value_Access Is Access Key_Value_Record;
    Type Key_Value_Record Is Record
                                 key_value  : Key_Value_Type;
                                 next_value : Key_Value_Access;
                                 first_key  : Primary_Key_Access;
                             End Record;

    Type Status_Type Is ( ATTRIBUTE, PRIME_KEY );
--    Type Index_Record_Type ( status : Status_Type := ATTRIBUTE ) Is
--         Record
--             Case status Is
--                 When ATTRIBUTE => key_value     : Key_Value_Type;
--                                   first_address : Integer;
--                 When PRIME_KEY => primary_key   : Primary_Key_Type;
--                                   next_address  : Integer;
--             End Case;
--         End Record;

    Type Index_Record_Type Is Record
                                  key_value     : Key_Value_Type;
                                  first_address : Integer;
                                  primary_key   : Primary_Key_Type;
                                  next_address  : Integer;
                              End Record;
    PACKAGE Data_File_IO Is New Direct_IO ( Record_Info );
    PACKAGE B_Tree_IO Is New B_Tree ( 3, Primary_Key_Type, ">" );
    PACKAGE Index_File_IO Is New Sequential_IO ( Index_Record_Type );
    index_file    : Index_File_IO.File_Type;
    data_file     : Data_File_IO.File_Type;
    key_not_found : Exception;
    header        : Array ( Secondary_Key_Name_Type ) Of Key_Value_Access
                                                         := ( Others => NULL );
    record_number : Natural := 1;

    PROCEDURE Create ( external_filename_prefix : String ) Is
    BEGIN  -- Create
        Data_File_IO.Create ( data_file,
                              Data_File_IO.INOUT_FILE,
                              external_filename_prefix & ".dat" );
        B_Tree_IO.Create_B_Tree ( external_filename_prefix & ".btr" );
    END Create;

    PROCEDURE Search_List ( header : In Key_Value_Access;
                            secondary_key : In Secondary_Key_Type;
                            where_found, previous : In Out Key_Value_Access
                           ) Is
        --
        -- returns (in where_found) NULL if
        --     secondary_key is not found in index
        -- or returns (in where_found) a pointer to the node
        --     in the linked structure if found
        -- previous contains the pointer to the node previous to
        --     the node containing the secondary_key in case a
        --     deletion is to be made
    BEGIN  -- Search_List
        previous := NULL;
        where_found := header;
        Loop
            Exit When where_found = NULL;
            If where_found.key_value = secondary_key Then
                Return;
            End If;
            previous := where_found;
            where_found := where_found.next_value;
        End Loop;
        Raise key_not_found;
    END Search_List;

    PROCEDURE Insert ( new_record : Record_Info ) Is
        next_record_location : Natural;
        PROCEDURE Insert_Index ( secondary_key_name : In Secondary_Key_Name_Type;
                                 secondary_key : In Secondary_Key_Type;
                                 id_no : In Primary_Key_Type ) Is
            --
            -- secondary_key_name indicates which index is to be built:
            --     MAKE or STYLE
            -- secondary_key is either the make or style that was read
            --     into new_record
            previous,
            where_found   : Key_Value_Access;

        BEGIN -- Insert_Index
            Search_List ( header ( secondary_key_name ), secondary_key,
                          where_found, previous );
                -- Add_To_Key_List
                -- add the record location of new record to the front of
                -- the list of addresses for the key_value pointed to
                -- by where_found
            where_found.first_key := New Primary_Key_Record'
                                     ( id_no, where_found.first_key );

        EXCEPTION
            When key_not_found =>
                    -- Add_To_Directory
                    -- add the secondary_key as a first Key_Value_Record in the
                    -- linked structure with the address of the primary key
                    -- of new record as the first_key
                header ( secondary_key_name ) :=
                    New Key_Value_Record' ( secondary_key,
                                            header ( secondary_key_name ),
                                            New Primary_Key_Record'( id_no, NULL ) );

        END Insert_Index;

    BEGIN -- Insert
        next_record_location := Natural ( Data_File_IO.Index ( data_file ) ) + 1;
        B_Tree_IO.Insert ( new_record.id_no, Positive ( next_record_location ) );
        Insert_Index ( MAKE, new_record.make, new_record.id_no );
        Insert_Index ( STYLE, new_record.style, new_record.id_no );
        Data_File_IO.Write ( data_file,
                             new_record,
                             Data_File_IO.Positive_Count ( next_record_location ) );
    EXCEPTION
        When B_Tree_IO.key_not_inserted => Raise key_not_inserted;
    END Insert;

    PROCEDURE Delete ( id_no : In Primary_Key_Type ) Is
        location   : Positive;
        new_record : Record_Info;
        PROCEDURE Delete_Index ( secondary_key_name : In Secondary_Key_Name_Type;
                                 secondary_key : In Secondary_Key_Type;
                                 id_no : In Primary_Key_Type ) Is
            --
            -- secondary_key_name indicates which index is to have a deletion:
            --     MAKE or STYLE index
            -- secondary_key is either the make or style that is
            --     to be deleted from the directory
            --
            previous_pointer,
            primary_pointer : Primary_Key_Access;
            previous,
            where_found     : Key_Value_Access;
        BEGIN -- Delete_Index
            Search_List ( header( secondary_key_name ), secondary_key, where_found, previous );
            If where_found.first_key.primary_key = id_no Then
                --
                -- delete first key
                --
                where_found.first_key := where_found.first_key.next_key;
                If where_found.first_key = NULL Then
                    --
                    -- delete entire entry since the only id_no has been deleted
                    --
                    If header( secondary_key_name ) = where_found Then
                        -- first key value in linked structure
                         header( secondary_key_name ) := where_found.next_value;
                    Else
                        previous.next_value := where_found.next_value;
                    End If;
                End If;
            Else    -- search for id_no in list of addresses
                primary_pointer := where_found.first_key.next_key;
                previous_pointer := where_found.first_key;
                Loop
                    Exit When primary_pointer = NULL;
                    If primary_pointer.primary_key = id_no Then
                        previous_pointer.next_key :=
                             primary_pointer.next_key;
                        Return;
                    Else
                        previous_pointer := primary_pointer;
                        primary_pointer := primary_pointer.next_key;
                    End If;
                End Loop;
                Raise key_not_deleted;
            End If;
        EXCEPTION
            When key_not_found => Raise key_not_deleted;
        END Delete_Index;

    BEGIN -- Delete
        B_Tree_IO.Search ( id_no, location );
        Data_File_IO.Read ( data_file,
                            new_record,
                            Data_File_IO.Positive_Count ( location ) );
        Delete_Index ( MAKE, new_record.make, new_record.id_no );
        Delete_Index ( STYLE, new_record.style, new_record.id_no );
        new_record.id_no := "NIL";
        Data_File_IO.Write ( data_file,
                             new_record,
                             Data_File_IO.Positive_Count ( location ) );
        B_Tree_IO.Delete ( id_no );
    EXCEPTION
        When B_Tree_IO.key_not_found
           | B_Tree_IO.key_not_deleted => Raise key_not_deleted;
    END Delete;

    PROCEDURE Open   ( external_filename_prefix : String ) Is
        PROCEDURE Input_Indexes ( external_filename : String;
                                  header : In Out Key_Value_Access ) Is
            key_index_record     : Index_Record_Type; -- (ATTRIBUTE);
            key_pointer          : Key_Value_Access;
            next_key_pointer     : Key_Value_Access;
            next_primary_pointer : Primary_Key_Access;
            primary_index_record : Index_Record_Type; -- (PRIME_KEY);
            primary_pointer      : Primary_Key_Access;
        BEGIN  -- Input_Indexes
            Index_File_IO.Open ( index_file,
                                 Index_File_IO.IN_FILE,
                                 external_filename );
            Index_File_IO.Read ( index_file, key_index_record );
            header := New Key_Value_Record'
                          ( key_index_record.key_value, NULL, NULL );
            key_pointer := header;
            Loop
                Index_File_IO.Read ( index_file, primary_index_record );
                primary_pointer := New Primary_Key_Record'
                                       ( primary_index_record.primary_key, NULL );
                key_pointer.first_key := primary_pointer;
                Loop
                    Exit When primary_index_record.next_address = -1;
                    Index_File_IO.Read ( index_file, primary_index_record );
                    next_primary_pointer := New Primary_Key_Record'
                                        ( primary_index_record.primary_key, NULL );
                    primary_pointer.next_key := next_primary_pointer;
                    primary_pointer := next_primary_pointer;
                End Loop;
                Exit When Index_File_IO.End_Of_File ( index_file );
                Index_File_IO.Read ( index_file, key_index_record );
                next_key_pointer := New Key_Value_Record'
                                    ( key_index_record.key_value, NULL, NULL );
                key_pointer.next_value := next_key_pointer;
                key_pointer := next_key_pointer;
            End Loop;
            Index_File_IO.Close ( index_file );
        END Input_Indexes;

    BEGIN  -- Open
        Data_File_IO.Open ( data_file,
                            Data_File_IO.INOUT_FILE,
                            external_filename_prefix & ".dat" );
        B_Tree_IO.Open_B_Tree ( external_filename_prefix & ".btr" );
        Input_Indexes ( external_filename_prefix & ".mak", header ( MAKE ) );
        Input_Indexes ( external_filename_prefix & ".sty", header ( STYLE ) );
    END Open;

    PROCEDURE Close  ( external_filename_prefix : String ) Is
        PROCEDURE Output_Indexes ( external_filename : String;
                                   header : In Out Key_Value_Access ) Is
            --
            -- output either the MAKE or the STYLE indexes
            -- (indicated by secondary_key_name) sequentially  to an external
            -- file for  storage
            --
            -- all  pointers  must  be  converted  to  record
            -- addresses and all "NULL" pointer values must be
            -- converted to -1
            key_pointer      : Key_Value_Access;
            lcv              : Positive;
            primary_pointer  : Primary_Key_Access;
            one_index_record : Index_Record_Type;  -- (ATTRIBUTE);
            two_index_record : Index_Record_Type;  -- (PRIME_KEY);
            record_number    : Positive := 1;

        BEGIN  -- Output_Indexes
            Index_File_IO.Create ( index_file,
                                   Index_File_IO.OUT_FILE,
                                   external_filename);
            key_pointer := header;
            Loop
                Exit When key_pointer = NULL;
                one_index_record.key_value := key_pointer.key_value;
                one_index_record.first_address := record_number + 1;
                    --
                    -- output index_record to location record_number
                    -- of index_file
                    --
                Index_File_IO.Write ( index_file, one_index_record );
                record_number := record_number + 1;
                primary_pointer := key_pointer.first_key;

                Loop
                    Exit When primary_pointer = NULL;
                    two_index_record.primary_key := primary_pointer.primary_key;
                    If primary_pointer.next_key /= NULL Then
                        two_index_record.next_address := record_number + 1;
                    Else
                        two_index_record.next_address := -1;
                    End If;
                    Index_File_IO.Write ( index_file, two_index_record );
                    record_number := record_number + 1;
                    primary_pointer := primary_pointer.next_key;
                End Loop;
                key_pointer := key_pointer.next_value;
            End Loop;
            Index_File_IO.Close ( index_file );
        END Output_Indexes;

    BEGIN  -- Close
        Data_File_IO.Close ( data_file );
        B_Tree_IO.Close_B_Tree;
        Output_Indexes ( external_filename_prefix & ".mak", header ( MAKE ) );
        Output_Indexes ( external_filename_prefix & ".sty", header ( STYLE ) );
    END Close;
END Invert;