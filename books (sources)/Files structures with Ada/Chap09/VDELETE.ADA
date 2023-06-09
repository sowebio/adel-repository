Separate (VSAM)
    PROCEDURE Delete ( key : Key_Type ) Is
        Type Operation_Type Is (REPLACE, DELETE, NIL );
        action            : Operation_Type;
        data_block        : Data_Block_Type;
        data_block_number : Natural;
        first_key         : Key_Type;
        hold_tuple        : Index_Entry;
        last_key          : Key_Type;                                      --++
        location          : Natural;
        successful        : Boolean;

        PROCEDURE Delete_Index_Entry ( action : In Out Operation_Type;
                                       key : Key_Type;
                                       hold_tuple : In Out Index_Entry ) Is
            index,
            index_block_address : Positive;
            index_block         : Index_node;
            predecessor_found   : Boolean := FALSE;

            PROCEDURE Locate_Predecessor (previous : Positive ) Is
                data_block         : Data_Block_Type;
                hold_address       : Natural;
                index_block        : Index_Node;
                next_block         : Data_Block_Type;
                next_block_address : Natural;
            BEGIN --Locate_Predecessor
                hold_address := index_block.tuple(previous).address;
                Index_Block_IO.Read ( index_blocks_file, index_block,
                    Index_Block_IO.Positive_Count ( hold_address ) );
                Loop
                    Exit When index_block.level = DATA;
                    hold_address := index_block.tuple(index_block.entries).address;
                    Index_Block_IO.Read ( index_blocks_file, index_block,
                         Index_Block_IO.Positive_Count ( hold_address ) );
                End Loop;
                hold_address := index_block.tuple(index_block.entries).address;
                Data_Block_IO.Read ( data_blocks_file, data_block,
                     Data_Block_IO.Positive_Count ( hold_address ) );
                next_block_address := data_block.link;
                Data_Block_IO.Read ( data_blocks_file, next_block,
                     Data_Block_IO.Positive_Count ( next_block_address ) );
                data_block.link := next_block.link;
                Data_Block_IO.Write ( data_blocks_file, data_block,
                     Data_Block_IO.Positive_Count ( hold_address ) );
            END Locate_Predecessor;

            PROCEDURE Replace_Entry ( key : Key_Type;
                                      hold_tuple : In Out Index_Entry;
                                      index_block : In Out Index_Node;
                                      index_block_address : Positive;
                                      index : Positive;
                                      action : In Out Operation_Type ) Is
            BEGIN  -- Replace_Entry
                If index_block.tuple(index).key = key Then
                    index_block.tuple(index) := hold_tuple;
                    Index_Block_IO.Write ( index_blocks_file, index_block,
                          Index_Block_IO.Positive_Count ( index_block_address ) );
                End If;
--                If index = 1 Then
                If index = index_block.entries Then                        --++
--                    hold_tuple := ( index_block.tuple(1).key,
                    hold_tuple := ( index_block.tuple(index_block.entries).key,--++
                                    index_block_address );
                    -- action remains REPLACE
                Else
                    action := NIL;
                End If;
            END Replace_Entry;

            PROCEDURE Delete_Entry ( index_block : In Out Index_Node;
                                     index_block_address : Positive;
                                     index : Positive;
                                     action : In Out Operation_Type;
                                     predecessor_found : In Out Boolean ) Is
                key      : Key_Type;
                last_index,                                                --++
                previous : Natural;
            BEGIN  -- Delete_Entry
--                key := index_block.tuple(1).key;
                key := index_block.tuple(index_block.entries).key;         --++
                If index_block.level = DATA Then
                    If index = 1 Then
                        predecessor_found := FALSE;
                    Else
                      --   link around deleted data block
                        previous := index_block.tuple(index - 1).address;
                        Data_Block_IO.Read ( data_blocks_file, data_block,
                           Data_Block_IO.Positive_Count ( previous ) );
                        data_block.link := index_block.tuple(index + 1).address;
                        Data_Block_IO.Write ( data_blocks_file, data_block,
                             Data_Block_IO.Positive_Count (previous ) );
                        predecessor_found := TRUE;
                    End If;
                Else -- index_block.level = INDEX
                    If Not predecessor_found And index > 1 Then
                        Locate_Predecessor ( index - 1 );
                        predecessor_found := TRUE;
                    End If;
                End If;
                If index_block.entries = 1 Then  -- delete only entry
                   Return;
                Else
                    last_index := index_block.entries;                      --++
                    index_block.entries := index_block.entries - 1;
                    For count In index .. index_block.entries Loop
                         index_block.tuple(count) := index_block.tuple(count + 1);
                    End Loop;
                    Index_Block_IO.Write ( index_blocks_file, index_block,
                        Index_Block_IO.Positive_Count ( index_block_address ) );
--                    If index = 1 Then
                    If index = last_index Then
--                        hold_tuple := ( index_block.tuple ( 1 ).key,
                        hold_tuple := ( index_block.tuple ( last_index ).key,--++
                                        index_block_address );
                        -- replace key with hold_tuple entry in parent
                        action := REPLACE;
                    Else
                        action := NIL;
                    End If;
                End If;
            END Delete_Entry;

        BEGIN --Delete_Index_Entry
            Loop
                Exit When Stack_Package.Status ( stack ) = EMPTY;
                Stack_Package.Pop ( stack, index_block_address );
                Stack_Package.Pop ( stack, index );
                Index_Block_IO.Read ( index_blocks_file, index_block,
                      Index_Block_IO.Positive_Count ( index_block_address ) );
                Case action Is
                    When REPLACE =>
                         Replace_Entry ( key, hold_tuple, index_block,
                                         index_block_address, index, action);

                    When DELETE  =>
                         Delete_Entry ( index_block, index_block_address,
                                        index, action, predecessor_found );
                    When NIL => Null;
                End Case;
            End Loop;
            If action = DELETE Then   -- last data block has been deleted
                index_block.entries := 0;
                Index_Block_IO.Write ( index_blocks_file, index_block, 1 );
            End If;
        END Delete_Index_Entry;

    BEGIN  -- Delete
        Find_Node ( key, data_block_number, location, data_block, successful );
        If successful Then
--            first_key := Key_Of ( data_block.tuple( 1 ) );
            last_key := Key_Of ( data_block.tuple( data_block.entries ) ); --++
            If data_block.entries = 1  Then  -- delete only entry
                action := DELETE;
                Delete_Index_Entry ( action, key, hold_tuple );
            Else
                For lcv In location + 1 .. data_block.entries Loop
                    data_block.tuple (lcv - 1) := data_block.tuple (lcv);
                End Loop;
                data_block.entries := data_block.entries - 1;
                Data_Block_IO.Write ( data_blocks_file, data_block,
                     Data_Block_IO.Positive_Count ( data_block_number ) );

--                If location = 1 Then --  delete first entry
                If key = last_key Then -- delete last entry                --++
--                    hold_tuple := (Key_Of ( data_block.tuple(1)),
                    hold_tuple := (Key_Of ( data_block.tuple(data_block.entries)),--++
                                            data_block_number);
                    -- replace key with hold_tuple entry in parent
                    action := REPLACE;
                    Delete_Index_Entry ( action, key, hold_tuple );
                End If;
            End If;
            Stack_Package.Create ( stack ); --clear the stack
        Else
            Stack_Package.Create ( stack ); --clear the stack
            Raise key_not_deleted;
        End If;
    END Delete;
