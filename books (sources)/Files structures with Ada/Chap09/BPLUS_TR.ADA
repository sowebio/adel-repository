With Text_IO;
GENERIC
    INDEX_BLOCK_SIZE : Positive;
    DATA_BLOCK_SIZE  : Positive;
    Type key_Type Is Private;
    Type Record_Info Is Private;
    With FUNCTION Key_Of ( a_record : Record_Info ) Return Key_Type;
    With FUNCTION ">" ( left_operand, right_operand : Key_Type )
                        Return Boolean;
PACKAGE BPlus_Tree Is
    PROCEDURE Create_BPlus_Tree ( external_bplus_tree_filename,
                                  external_data_block_file : String );
    PROCEDURE Open_BPlus_Tree ( external_bplus_tree_filename,
                                external_data_block_file : String );
    PROCEDURE Insert ( data_record : Record_Info );
    PROCEDURE Delete ( key : Key_Type );
    PROCEDURE Retrieve ( key : Key_Type; data_record : Out Record_Info);
    PROCEDURE Replace ( data_record : Record_Info );
    PROCEDURE Close_BPlus_Tree ( external_bplus_tree_filename,
                                 external_data_block_file : String );
    key_not_found    : Exception;
    key_not_inserted : Exception;
    key_not_deleted  : Exception;
END BPlus_Tree;

With IO_Exceptions;
With Direct_IO;
With StackPkg;
PACKAGE Body BPlus_Tree Is
    PACKAGE Stack_Package Is New StackPkg ( Positive );
    Use Stack_Package;
    stack : Stack_Package.Stack_Type;
    Type Index_Entry Is Record
                            key     : Key_Type;
                            address : Positive;
                        End Record;
    Type Index_Tuple Is Array ( 1 .. INDEX_BLOCK_SIZE ) Of Index_Entry;
    Type Level_Indicator Is ( DATA, INDEX );
    Type Index_Node Is Record
                           entries : Natural;
                           level : Level_Indicator;
                           tuple   : Index_Tuple;
                       End Record;
    PACKAGE Index_Block_IO Is New Direct_IO ( Index_Node );
    index_blocks_file : Index_Block_IO.File_Type;

    Type Data_Tuple Is Array ( 1 .. DATA_BLOCK_SIZE ) Of Record_Info;
    Type Data_Block_Type Is Record
                                entries : Natural;
                                tuple   : Data_Tuple;
                                link    : Natural;
                            End Record;
    PACKAGE Data_Block_IO Is New Direct_IO ( Data_Block_Type );
    data_blocks_file : Data_Block_IO.File_Type;

    PROCEDURE Create_BPlus_Tree ( external_bplus_tree_filename,
                                  external_data_block_file : String ) Is
        first_node : Index_Node;
    BEGIN -- Create_BPlus_Tree
        Text_IO.Put_Line ("Create_BPlus_Tree");
        Index_Block_IO.Create ( index_blocks_file,
                                Index_Block_IO.INOUT_File,
                                external_bplus_tree_filename );
        first_node.entries := 0;
        Index_Block_IO.Write ( index_blocks_file, first_node, 1 );
        Data_Block_IO.Create ( data_blocks_file,
                               Data_Block_IO.INOUT_FILE,
                               external_data_block_file );
    END Create_BPlus_Tree;

    PROCEDURE Open_BPlus_Tree ( external_bplus_tree_filename,
                                external_data_block_file : String ) Is
    BEGIN  --Open_BPlus_Tree

        Index_Block_IO.Open  ( index_blocks_file,
                               Index_Block_IO.INOUT_File,
                               external_bplus_tree_filename );
        Data_Block_IO.Open ( data_blocks_file,
                             Data_Block_IO.INOUT_FILE,
                             external_data_block_file );
    END Open_BPlus_Tree;

    PROCEDURE Find_Node (key : Key_Type;
                         data_block_number : In Out Natural;
                         location : Out Natural;
                         data_block : In Out Data_Block_Type;
                         successful : In Out Boolean ) Is
        first_node   : Index_Node;
        root_address : Natural;
        PROCEDURE Search_Indexes ( key : Key_Type;
                                   index_block_address : Natural;
                                   successful : Out Boolean;
                                   data_block_number : Out Natural ) Is
            found       : Boolean := FALSE;
            index_block : Index_Node;
            lcv         : Natural := 1;
        BEGIN --Search_Indexes
            Text_IO.Put_Line ("Search_Indexes");
            If index_block_address = 0 Then
                successful := FALSE;
                data_block_number := 0;
            Else
                Index_Block_IO.Read ( index_blocks_file, index_block,
                      Index_Block_IO.Positive_Count ( index_block_address ) );
                Loop
                    Exit When lcv > index_block.entries;
                    If key > index_block.tuple( lcv ).key Then
                        lcv := lcv + 1;
                    Elsif key = index_block.tuple(lcv).key Then
                        found := TRUE;
                        Exit;
                    Else -- key < index_block.tuple( lcv ).key
                        found := TRUE;
                        lcv := lcv - 1;
                        Exit;
                    End If;
                End Loop;
                If lcv > index_block.entries Then
                  --
                  -- key was > last key in index_block so look in
                  -- last address
                  --
                    found := TRUE;
                    lcv := lcv - 1;
                End If;
                Stack_Package.Push ( stack, lcv );
                Stack_Package.Push ( stack, index_block_address );
                successful := found;
                If found Then
                    If lcv = 0 Then
                      --
                      -- key was < first key in index_block so is not
                      -- in the file
                      --
                        data_block_number := 0;
                        found := FALSE;
                        successful := found;
                    Else
                      --
                      -- search next level down
                      --
                        If index_block.level = DATA Then
                            data_block_number := index_block.tuple(lcv).address;
                        Else  -- index_block.level = INDEX
                            Search_Indexes ( key, index_block.tuple(lcv).address,
                                             successful, data_block_number );
                        End If;
                    End If;
                End If;
            End If;
        END Search_Indexes;

        PROCEDURE Search_Data_Block ( key : Key_Type;
                                      data_block : Data_Block_Type;
                                      location : Out Natural;
                                      successful : Out Boolean ) Is
            lcv : Natural := 1;
        BEGIN --Search_Data_Block
            Text_IO.Put_Line("Search_Data_Block");
            successful := FALSE;
            Loop
                Exit When lcv > data_block.entries;
                If key > Key_Of ( data_block.tuple( lcv ) ) Then
                    lcv := lcv + 1;
                Elsif key = Key_Of ( data_block.tuple( lcv ) ) Then
                    successful := TRUE;
                    Exit;
                Else  -- key < Key_Of ( data_block.tuple( lcv ) )
                    Exit;
                  -- key not found in data block so return lcv as the
                  -- location of the first entry > key.
                  -- key belongs immediately prior to location lcv
                  --
                End If;
            End Loop;
            location := lcv;
        END Search_Data_Block;

    BEGIN --Find_Node
          Text_IO.Put_Line ("Find_Node");
          Index_Block_IO.Read ( index_blocks_file, first_node, 1 );
          root_address := first_node.entries;
          Search_Indexes ( key, root_address, successful, data_block_number );
          If successful Then
              Data_Block_IO.Read ( data_blocks_file, data_block,
                   Data_Block_IO.Positive_Count ( data_block_number ) );
              Search_Data_Block ( key, data_block, location, successful );
          Else
              data_block.entries := 0;
              data_block.link := 0;
              location := 1;
          End If;
      END Find_Node;

    PROCEDURE Insert ( data_record : Record_Info ) Is
        data_block        : Data_Block_Type;
        data_block_number : Natural;
        hold_tuple        : Index_Entry;
        key               : Key_Type;
        location          : Natural;
        new_block         : Data_Block_Type;
        successful        : Boolean;

        PROCEDURE New_Root ( hold_tuple : Index_Entry ) Is
            first_node          : Index_Node;
            next_index_location : Positive;
            new_node            : Index_Node;
            root_address        : Natural;
            root_node           : Index_Node;
        BEGIN --New_Root
            Text_IO.Put_Line ("New_Root");
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
                new_node.tuple(1) := ( root_node.tuple(1).key, root_address);
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

        PROCEDURE Split_Data_Block ( data_block : In Out Data_Block_Type;
                                     location : IN Positive;
                                     new_block : In Out Data_Block_Type ) Is
             -- Splits records from positions location..entries of
             -- data block into a new data block and adjusts indexes
           entry_position : Positive := 1;
           next_data_location : Natural;

           PROCEDURE Insert_Into_Index_Blocks ( key : Key_Type;
                                                address : Positive ) Is
               Type Operation_Type Is ( INSERT_KEY, NIL );
               hold_tuple          : Index_Entry;
               index,
               index_block_address : Positive;
               node                : Index_Node;
               operation           : Operation_Type := INSERT_KEY;
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
                       hold_tuple := ( new_node.tuple(1).key,
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
                   Else   --  node is full so addition will cause a split
                       Split;
                       -- operation retains INSERT_KEY upon return
                   End If;
               END Insert_Tuple;

           BEGIN --Insert_Into_Index_Blocks
               hold_tuple := ( key, address );
               Loop
                   Exit When Stack_Package.Status ( stack ) = EMPTY;
                    -- pop off ancestors
                   Stack_Package.Pop ( stack, index_block_address );
                   Stack_Package.Pop ( stack, index );
                     --
                     -- index_block_address is the address of the index_block
                     -- at the next higher level; insert into position index + 1
                     --
                   If operation = INSERT_KEY Then
                       Index_Block_IO.Read ( index_blocks_file, node,
                           Index_Block_IO.Positive_Count (index_block_address ));
                       Insert_Tuple ( hold_tuple, node, index_block_address,
                                      index, operation );
                   End If;
               End Loop;
               If operation = INSERT_KEY Then
                 --
                 -- new root contains hold_tuple with old root as
                 -- tuple(1); tuple = (key, addr(key))
                 --
                   New_Root ( hold_tuple );
               End If;
           END Insert_Into_Index_Blocks;

        BEGIN --Split_Data_Block
            For lcv In location .. data_block.entries Loop
                entry_position := entry_position + 1;
                new_block.tuple (entry_position) := data_block.tuple (lcv);
            End Loop;

            new_block.entries := entry_position;
            data_block.entries := location - 1;

            new_block.link := data_block.link;
            next_data_location :=
                Natural ( Data_Block_IO.Size ( data_blocks_file ) ) + 1;
            data_block.link := next_data_location;
            Data_Block_IO.Write ( data_blocks_file, new_block,
                Data_Block_IO.Positive_Count ( next_data_location ) );
            Insert_Into_Index_Blocks ( Key_Of (new_block.tuple(1)),
                                       next_data_location );
        END Split_Data_Block;

    BEGIN  --Insert
       -- Create Indexed Sequential File
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
            Stack_Package.Create ( stack ); --clear the stack
        End If;
        Data_Block_IO.Write ( data_blocks_file, data_block,
            Data_Block_IO.Positive_Count ( data_block_number ) );
    END Insert;


    PROCEDURE Delete ( key : Key_Type ) Is
        Type Operation_Type Is (REPLACE, DELETE, NIL );
        action            : Operation_Type;
        data_block        : Data_Block_Type;
        data_block_number : Natural;
        first_key         : Key_Type;
        hold_tuple        : Index_Entry;
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
                If index = 1 Then
                    hold_tuple := ( index_block.tuple(1).key,
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
                previous : Natural;
            BEGIN  -- Delete_Entry
                key := index_block.tuple(1).key;
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
                    index_block.entries := index_block.entries - 1;
                    For count In index .. index_block.entries Loop
                         index_block.tuple(count) := index_block.tuple(count + 1);
                    End Loop;
                    Index_Block_IO.Write ( index_blocks_file, index_block,
                        Index_Block_IO.Positive_Count ( index_block_address ) );
                    If index = 1 Then
                        hold_tuple := ( index_block.tuple ( 1 ).key,
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
            first_key := Key_Of ( data_block.tuple( 1 ) );
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

                If location = 1 Then --  delete first entry
                    hold_tuple := (Key_Of ( data_block.tuple(1)),data_block_number);
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

    PROCEDURE Retrieve ( key : Key_Type; data_record : Out Record_Info ) Is
        data_block        : Data_Block_Type;
        data_block_number : Natural;
        location          : Natural;
        successful        : Boolean;
    BEGIN
        Find_Node (key, data_block_number, location, data_block, successful );
        Stack_Package.Create ( stack ); --clear the stack
        If successful Then
            data_record := data_block.tuple (location);
        Else
            Raise key_not_found;
        End If;
    END Retrieve;

    PROCEDURE Replace ( data_record : Record_Info ) Is
        data_block : Data_Block_Type;
        data_block_number,
        location   : Natural;
        key        : Key_Type;
        successful : Boolean;
    BEGIN --Replace
        key := Key_Of ( data_record );
        Find_Node (key, data_block_number, location, data_block, successful );
        Stack_Package.Create ( stack ); --clear the stack
        If successful Then
            data_block.tuple (location) := data_record;
            Data_Block_IO.Write ( data_blocks_file, data_block,
                 Data_Block_IO.Positive_Count ( data_block_number ) );
        Else
            Raise key_not_found;
        End If;
    END Replace;

    PROCEDURE Close_BPlus_Tree ( external_bplus_tree_filename,
                                 external_data_block_file : String ) Is
    BEGIN --Close_BPlus_Tree
        Index_Block_IO.Close  ( index_blocks_file );
        Data_Block_IO.Close ( data_blocks_file );
    END Close_BPlus_Tree;

BEGIN -- BPlus_Tree;
    Stack_Package.Create ( stack );
END BPlus_Tree;
