Separate (VSAM)
    PROCEDURE Insert ( data_record : Record_Info ) Is
        Type Operation_Type Is ( INSERT_KEY, REPLACE_KEY, NIL );
        data_block        : Data_Block_Type;
        data_block_number : Natural;
        hold_tuple        : Index_Entry;
        key               : Key_Type;
        location          : Natural;
        new_block         : Data_Block_Type;
        successful        : Boolean;
        operation           : Operation_Type := INSERT_KEY;
        PROCEDURE New_Root ( hold_tuple : Index_Entry ) Is
            first_node          : Index_Node;
            next_index_location : Positive;
            new_node            : Index_Node;
            root_address        : Natural;
            root_node           : Index_Node;
        BEGIN --New_Root
            Index_Block_IO.Read ( index_blocks_file, first_node, 1 );
            root_address := first_node.entries;
            If root_address = 0 Then  -- no root exists yet
                new_node.level := DATA;
                new_node.tuple (1) := hold_tuple;
                new_node.entries := 1;
            Else
                Index_Block_IO.Read ( index_blocks_file, root_node,
                      Index_Block_IO.Positive_Count ( root_address ) );
                new_node.level := INDEX;
--                new_node.tuple(1) := ( root_node.tuple(1).key, root_address);
                new_node.tuple(1) := ( root_node.tuple(root_node.entries).key,--++
                                       root_address);                      --++
                new_node.tuple(2) := hold_tuple;
                new_node.entries := 2;
            End If;
            next_index_location :=
                Positive ( Index_Block_IO.Size ( index_blocks_file ) ) + 1;
            Index_Block_IO.Write ( index_blocks_file, new_node,
                 Index_Block_IO.Positive_Count ( next_index_location ) );
            first_node.entries := next_index_location;
            Index_Block_IO.Write (index_blocks_file, first_node, 1 );
        END New_Root;


           PROCEDURE Insert_Into_Index_Blocks ( key : Key_Type;
                                                address : Positive ) Is
               hold_tuple          : Index_Entry;
               index,
               index_block_address : Positive;
               node                : Index_Node;
               PROCEDURE Insert_Tuple ( hold_tuple : In Out Index_Entry;
                                        node : In Out Index_Node;
                                        index_block_address : In Positive;
                                        index : In Positive;
                                        operation : In Out Operation_Type ) Is
                   PROCEDURE Split Is
                       j                   : Natural := 1;
                       new_node            : Index_Node;
                       next_index_location : Positive;
                   BEGIN
                       new_node.tuple(1) := hold_tuple;
                       For count In index + 1 .. node.entries Loop
                           j := j + 1;
                           new_node.tuple(j) := node.tuple(count);
                       End Loop;
                       new_node.entries := j;
                       new_node.level := node.level;
                       node.entries := index;
                       next_index_location :=
                           Positive ( Index_Block_IO.Size ( index_blocks_file ) )
                                                     + 1;
--                       hold_tuple := ( new_node.tuple(1).key,
                       hold_tuple := ( new_node.tuple(new_node.entries).key,--++
                                       next_index_location );
                       Index_Block_IO.Write ( index_blocks_file, new_node,
                           Index_Block_IO.Positive_Count ( next_index_location ) );
                       Index_Block_IO.Write ( index_blocks_file, node,
                           Index_Block_IO.Positive_Count ( index_block_address ) );
                   END Split;
               BEGIN --Insert_Tuple
                    If node.entries < INDEX_BLOCK_SIZE Then
                            -- resulting node is large enough for addition
                        operation := NIL;
                        For count In Reverse index + 1 .. node.entries Loop
                            node.tuple (count + 1) := node.tuple (count);
                        End Loop;
                        node.entries := node.entries + 1;
                        node.tuple (index + 1) := hold_tuple;
                        Index_Block_IO.Write ( index_blocks_file, node,
                            Index_Block_IO.Positive_Count ( index_block_address ) );
                        If index + 1 = node.entries Then -- added new last key--++
                            operation := REPLACE_KEY;                          --++
                            hold_tuple := (node.tuple(node.entries).key,      --++
                                           index_block_address);              --++
                        Else                                                  --++
                            operation := NIL;
                        End If;                                               --++
                   Else   --  node is full so addition will cause a split
                       Split;
                       -- operation retains INSERT_KEY upon return
                   End If;
               END Insert_Tuple;

               PROCEDURE Replace_Tuple ( hold_tuple :  In Out Index_Entry;
                                         node : In Out Index_Node;
                                         index_block_address : In Positive;
                                         index : In Positive;
                                         operation : In Out Operation_Type) Is
               BEGIN -- Replace_Tuple
                   node.tuple(index) := hold_tuple;
                   Index_Block_IO.Write ( index_blocks_file, node,
                         Index_Block_IO.Positive_Count ( index_block_address));
                   If index = node.entries Then
                       hold_tuple := ( node.tuple(node.entries).key,
                                       index_block_address);
                       -- operation remains REPLACE_KEY
                    Else
                        operation := NIL;
                    End If;
                END Replace_Tuple;
           BEGIN --Insert_Into_Index_Blocks
               hold_tuple := ( key, address );
               Loop
                   Exit When Stack_Package.Status ( stack ) = EMPTY;
                    -- pop off ancestors
                   Stack_Package.Pop ( stack, index_block_address );
                   Stack_Package.Pop ( stack, index );
                   Case operation Is
                      When INSERT_KEY => 
                     --
                     -- index_block_address is the address of the index_block
                     -- at the next higher level; insert into position index + 1
                     --
                       Index_Block_IO.Read ( index_blocks_file, node,
                           Index_Block_IO.Positive_Count (index_block_address ));
                       Insert_Tuple ( hold_tuple, node, index_block_address,
                                      index, operation );
                      When REPLACE_KEY =>      -- this whole section is added 
                       Index_Block_IO.Read ( index_blocks_file, node,
                           Index_Block_IO.Positive_Count (index_block_address));
                       Replace_Tuple ( hold_tuple, node, index_block_address,
                                      index, operation);
                      When NIL => Null;
                   End Case;
               End Loop;
               If operation = INSERT_KEY Then
                 --
                 -- new root contains hold_tuple with old root as
                 -- tuple(1); tuple = (key, addr(key))
                 --
                   New_Root ( hold_tuple );
               End If;
           END Insert_Into_Index_Blocks;

        PROCEDURE Split_Data_Block ( data_block : In Out Data_Block_Type;
                                     location : IN Positive;
                                     new_block : In Out Data_Block_Type ) Is
             -- Splits records from positions location..entries of
             -- data block into a new data block and adjusts indexes
           entry_position : Positive := 1;
           index : Positive;                                               --++
           index_block_address : Positive;                                 --++
           next_data_location : Natural;
           node : Index_Node;                                              --++
        BEGIN --Split_Data_Block
            For lcv In location .. data_block.entries Loop
                entry_position := entry_position + 1;
                new_block.tuple (entry_position) := data_block.tuple (lcv);
            End Loop;

            new_block.entries := entry_position;
            data_block.entries := location - 1;
            Stack_Package.Pop ( stack, index_block_address );              --++
            Stack_Package.Pop ( stack, index );                            --++
            Index_Block_IO.Read ( index_blocks_file, node,                 --++
                  Index_Block_IO.Positive_Count ( index_block_address ));  --++
            node.tuple(index).key :=                                       --++
                Key_Of (data_block.tuple(data_block.entries));             --++
            Index_Block_IO.Write ( index_blocks_file, node,                --++
                Index_Block_IO.Positive_Count ( index_block_address ));    --++
            -- Push a copy of current index node info on stack so can replace
            -- key of current block and also insert new block into same 
            -- index block
            Stack_Package.Push ( stack, index );                           --++
            Stack_Package.Push ( stack, index_block_address );             --++
            new_block.link := data_block.link;
            next_data_location :=
                Natural ( Data_Block_IO.Size ( data_blocks_file ) ) + 1;
            data_block.link := next_data_location;

            Data_Block_IO.Write ( data_blocks_file, data_block,            --++
                Data_Block_IO.Positive_Count ( location ) );               --++
            Data_Block_IO.Write ( data_blocks_file, new_block,
                Data_Block_IO.Positive_Count ( next_data_location ) );
--            Insert_Into_Index_Blocks ( Key_Of (new_block.tuple(1)),
            operation := INSERT_KEY;                                       --++
            Insert_Into_Index_Blocks                                       --++
                ( Key_Of (new_block.tuple(new_block.entries)),             --++
                  next_data_location );
        END Split_Data_Block;

    BEGIN  --Insert
       -- Build Indexed Sequential File
       --
        key := Key_Of ( data_record );
        Find_Node ( key, data_block_number, location, data_block, successful );
        If successful Then
            Raise key_not_inserted;
        End If;
        If data_block_number = 0 Then
            data_block_number := 1;
            hold_tuple := ( key, 1 );
            New_Root ( hold_tuple );
        End If;
        If data_block.entries = DATA_BLOCK_SIZE Then
            new_block.tuple(1) := data_record;
            Split_Data_Block ( data_block, location, new_block );
        Else
            data_block.entries := data_block.entries + 1;
            For lcv In Reverse  location + 1 .. data_block.entries Loop
                data_block.tuple (lcv) := data_block.tuple (lcv - 1);
            End Loop;
            data_block.tuple ( location ) := data_record;
            If location = data_block.entries Then --change last key in index
                operation := REPLACE_KEY;
                Insert_Into_Index_Blocks 
                  ( Key_Of ( data_block.tuple(data_block.entries)),
                    data_block_number);
            End If;
        End If;
        Data_Block_IO.Write ( data_blocks_file, data_block,
            Data_Block_IO.Positive_Count ( data_block_number ) );
    END Insert;
