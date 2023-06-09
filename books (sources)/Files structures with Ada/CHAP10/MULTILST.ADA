PACKAGE MultiLst Is
    SubType Primary_Key_Type Is String (1..3);
    SubType Secondary_Key_Type Is String (1..10);
    Type Record_Info Is Record
                            id_no      : Primary_Key_Type;
                            make       : Secondary_Key_Type;
                            make_link  : Primary_Key_Type;
                            style      : Secondary_Key_Type;
                            style_link : Primary_Key_Type;
                            model      : Secondary_Key_Type;
                            mileage    : Secondary_Key_Type;
                            color      : Secondary_Key_Type;
                        End Record;

    PROCEDURE Create ( external_filename_prefix : String );
    PROCEDURE Open   ( external_filename_prefix : String );
    PROCEDURE Close  ( external_filename_prefix : String );
    PROCEDURE Insert ( new_record : In Out Record_Info );
    PROCEDURE Delete ( id_no : In Primary_Key_Type );
    key_not_inserted,
    key_not_deleted : Exception;
END Multilst;

With Sequential_IO,
     Direct_IO,
     B_Tree;
PACKAGE Body Multilst Is

    Type Key_Value_Record;
    Type Key_Value_Access Is Access Key_Value_Record;
    Type Key_Value_Record Is Record
                                 key_value  : Secondary_Key_Type;
                                 next_value : Key_Value_Access;
                                 first_key  : Primary_Key_Type;
                                 length     : Positive;
                             End Record;
    Type Index_Type Is ( MAKE, STYLE );

    Type Index_Record_Type Is Record
                                  key_value : Secondary_Key_Type;
                                  first_key : Primary_Key_Type;
                                  length    : Positive;
                              End Record;
    PACKAGE Data_File_IO Is New Direct_IO ( Record_Info );
    PACKAGE B_Tree_IO Is New B_Tree ( 3, Primary_Key_Type, ">" );
    PACKAGE Index_IO Is New Sequential_IO ( Index_Record_Type );
    data_file     : Data_File_IO.File_Type;
    index_file    : Index_IO.File_Type;
    header        : Array ( Index_Type ) Of Key_Value_Access := ( Others => NULL );
    key_not_found : Exception;

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
        -- where_found returns NIL if secondary_key is not found in
        --     index and returns a pointer to the node in the
        --     linked structure if found
        --
    BEGIN -- Search_List
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

    PROCEDURE Insert ( new_record : In Out Record_Info ) Is
        next_record_number : Natural;
        PROCEDURE Insert_Index ( index_name : In Index_Type;
                                 secondary_key : In Secondary_Key_Type;
                                 link_field : In Out Primary_Key_Type;
                                 id_no : In Primary_Key_Type ) Is
            -- index_name indicates which index is to be built:
            --     MAKE or STYLE
            -- secondary_key is either the make or style that was read
            --     into new_record
            -- link_field is either the make_link or style_link of
            --     new_record
            --
            previous,
            where_found : Key_Value_Access;

        BEGIN -- Insert_Index
            Search_List ( header ( index_name ), secondary_key, where_found,
                          previous );
                -- Add_To_Key_List
                --
                -- add the primary key of new_record to the list of primary
                --     keys for the key_value pointed to by where_found
                --
                where_found.length := where_found.length + 1;
                link_field := where_found.first_key;
                where_found.first_key := id_no;
        EXCEPTION
            When key_not_found =>
                -- Add_To_Index
                --
                -- add the secondary_key as the new value in the linked
                --    structure with the primary key of new_record as the
                --    first_key
                --
                header ( index_name ) := New Key_Value_Record'
                            ( secondary_key, header ( index_name ), id_no, 1 );
                link_field := "NIL";
        END Insert_Index;

    BEGIN -- Insert
        next_record_number := Natural ( Data_File_IO.Size ( data_file ) ) + 1;
        B_Tree_IO.Insert ( new_record.id_no, Positive ( next_record_number ) );
        Insert_Index ( MAKE, new_record.make, new_record.make_link,
                       new_record.id_no );
        Insert_Index ( STYLE, new_record.style, new_record.style_link,
                       new_record.id_no );
        Data_File_IO.Write ( data_file,
                             new_record,
                             Data_File_IO.Positive_Count ( next_record_number ) );
    EXCEPTION
        When B_Tree_IO.key_not_inserted => Raise key_not_inserted;
    END Insert;

    PROCEDURE Delete ( id_no : In Primary_Key_Type ) Is
        location   : Positive;
        new_record : Record_Info;
        PROCEDURE Delete_Index ( index : In Index_Type;
                                 secondary_key : In Secondary_Key_Type;
                                 id_no : In Primary_Key_Type;
                                 hold_record : Record_Info ) Is

            link          : Primary_Key_Type;
            previous      : Key_Value_Access;
            prev_key      : Primary_Key_Type;
            prev_record   : Record_Info;
            record_number : Positive;
            where_found   : Key_Value_Access;
        BEGIN --Delete_Index
            Search_List ( header ( index ),
                          secondary_key,
                          where_found,
                          previous );
            If where_found.first_key = id_no Then
                  --
                  -- delete first key
                  --
                If where_found.length = 1 Then  -- delete entire entry
                    If previous = NULL Then  -- first entry
                        header ( index ) := where_found.next_value;
                    Else
                        previous.next_value := where_found.next_value;
                    End If;
                Else -- find next primary key to become first_key
                    Case index Is
                        When MAKE  =>
                            where_found.first_key := hold_record.make_link;
                        When STYLE =>
                            where_found.first_key := hold_record.style_link;
                    End Case;
                    where_found.length := where_found.length - 1;
                End If;
            Else -- id_no not = first_key
                prev_key := where_found.first_key;
                B_Tree_IO.Search ( prev_key, record_number );
                Data_File_IO.Read ( data_file,
                                    prev_record,
                                    Data_File_IO.Positive_Count ( record_number ) );
                Case index Is
                    When MAKE  => link := prev_record.make_link;
                    When STYLE => link := prev_record.style_link;
                End Case;
                Loop
                    If link = "NIL" Then
                        Raise key_not_deleted;
                    End If;
                        -- search for id_no in linked list of records
                    If link = id_no Then
                        Case index Is
                            When MAKE => prev_record.make_link :=
                                             hold_record.make_link;
                            When STYLE => prev_record.style_link :=
                                              hold_record.style_link;
                       End Case;
                       Data_File_IO.Write ( data_file, prev_record,
                           Data_File_IO.Positive_Count ( record_number ) );
                       where_found.length := where_found.length - 1;
                       Exit;
                   Else
                       prev_key := link;
                       B_Tree_IO.Search ( prev_key, record_number );
                       Data_File_IO.Read ( data_file, prev_record,
                           Data_File_IO.Positive_Count ( record_number ) );
                       Case index Is
                           When MAKE  => link := prev_record.make_link;
                           When STYLE => link := prev_record.style_link;
                       End Case;
                    End If;
                End Loop;
            End If;
        EXCEPTION
            When B_Tree_IO.key_not_found => Raise key_not_deleted;
        END Delete_Index;

    BEGIN  -- Delete
        B_Tree_IO.Search ( id_no, location );
        Data_File_IO.Read ( data_file,
                            new_record,
                            Data_File_IO.Positive_Count ( location ) );
        Delete_Index ( MAKE, new_record.make, new_record.id_no, new_record );
        Delete_Index ( STYLE, new_record.style, new_record.id_no, new_record );
        new_record.id_no := "NIL";
        Data_File_IO.Write ( data_file, new_record,
                            Data_File_IO.Positive_Count ( location ) );
        B_Tree_IO.Delete ( id_no );
    EXCEPTION
        When B_Tree_IO.key_not_found
           | B_Tree_IO.key_not_deleted => Raise key_not_deleted;
    END Delete;

    PROCEDURE Open ( external_filename_prefix : String ) Is
        PROCEDURE Input_Indexes ( external_filename : In String;
                                  header : Out Key_Value_Access ) Is
            --
            -- input either the MAKE_INDEX or the STYLE_INDEX (index)
            --     sequentially from an external file
            --
            first,
            lcv,
            next         : Key_Value_Access;
            index_record : Index_Record_Type;
        BEGIN -- Input_Indexes
            Index_IO.Open ( index_file, Index_IO.IN_FILE, external_filename );
            lcv := New Key_Value_Record;
            first := lcv;
            Loop
                Exit When Index_IO.End_Of_File ( index_file );
                Index_IO.Read ( index_file, index_record );
                lcv.next_value := New Key_Value_Record'
                                    ( index_record.key_value, null,
                                      index_record.first_key,
                                      index_record.length );
                lcv := lcv.next_value;
            End Loop;
            lcv.next_value := Null;
            header := first.next_value;
            Index_IO.Close ( index_file );
        END Input_Indexes;
    BEGIN  -- Open
        Data_File_IO.Open ( data_file,
                            Data_File_IO.INOUT_FILE,
                            external_filename_prefix & ".dat" );
        B_Tree_IO.Open_B_Tree ( external_filename_prefix & ".btr" );
        Input_Indexes ( external_filename_prefix & ".mak", header ( Make ) );
        Input_Indexes ( external_filename_prefix & ".sty", header ( STYLE ) );
    END Open;

    PROCEDURE Close ( external_filename_prefix : String ) Is
        PROCEDURE Output_Indexes ( external_filename : In String;
                                   header : In Key_Value_Access ) Is
            --
            -- output either the MAKE or the STYLE index
            --     sequentially to an external file for storage
            --
            index_record : Index_Record_Type;
            lcv : Key_Value_Access;
        BEGIN  -- Output_Indexes
            Index_IO.Create ( index_file, Index_IO.OUT_FILE, external_filename );
            lcv := header;
            Loop
                Exit When lcv = NULL;
                index_record := ( lcv.key_value, lcv.first_key, lcv.length );
                Index_IO.Write ( index_file, index_record );
                lcv := lcv.next_value;
            End Loop;
           Index_IO.Close ( index_file );
        END Output_Indexes;

    BEGIN  -- Close
        Data_File_IO.Close ( data_file );
        B_Tree_IO.Close_B_Tree;
        Output_Indexes ( external_filename_prefix & ".mak", header ( MAKE ) );
        Output_Indexes ( external_filename_prefix & ".sty", header ( STYLE ) );
    END Close;
END Multilst;