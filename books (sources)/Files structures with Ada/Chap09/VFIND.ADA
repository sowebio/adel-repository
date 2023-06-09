Separate (VSAM)
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
--            found       : Boolean := FALSE;
            index_block : Index_Node;
            lcv         : Natural := 1;
        BEGIN --Search_Indexes
            If index_block_address = 0 Then
                successful := FALSE;
                data_block_number := 0;
            Else
                Index_Block_IO.Read ( index_blocks_file, index_block,
                      Index_Block_IO.Positive_Count ( index_block_address ) );
                Loop
--                    Exit When lcv > index_block.entries;
                    If lcv > index_block.entries Then                      --++
                        -- key was > last key in index_block so            --++
                        -- is not in file; return last block               --++
                        lcv := index_block.entries;                        --++
                        Exit;                                              --++
                    End If;                                                --++
                    If key > index_block.tuple( lcv ).key Then
                        lcv := lcv + 1;
--                      Elsif key = index_block.tuple(lcv).key Then
--                          found := TRUE;
--                          Exit;
--                    Else -- key < index_block.tuple( lcv ).key
--                        found := TRUE;
--                        lcv := lcv - 1;
--                        Exit;
                    Else -- key <= index_block.tuple(lcv).key             --++
                        Exit;                                             --++
                    End If;
                End Loop;
--                If lcv > index_block.entries Then
--                --
--                -- key was > last key in index_block so look in
--                -- last address
--                --
--                    found := TRUE;
--                    lcv := lcv - 1;
--                    End If;
                Stack_Package.Push ( stack, lcv );
                Stack_Package.Push ( stack, index_block_address );
--                successful := found;
                successful := TRUE;                                       --++
--                If found Then
--                    If lcv = 0 Then
--                      --
--                      -- key was < first key in index_block so is not
--                      -- in the file
--                      --
--                        data_block_number := 0;
--                        found := FALSE;
--                        successful := found;
--                    Else
                      --
                      -- search next level down
                      --
                        If index_block.level = DATA Then
                            data_block_number := index_block.tuple(lcv).address;
                        Else  -- index_block.level = INDEX
                            Search_Indexes ( key, index_block.tuple(lcv).address,
                                             successful, data_block_number );
                        End If;
--                    End If;
--                End If;
            End If;
        END Search_Indexes;

        PROCEDURE Search_Data_Block ( key : Key_Type;
                                      data_block : Data_Block_Type;
                                      location : Out Natural;
                                      successful : Out Boolean ) Is
            lcv : Natural := 1;
        BEGIN --Search_Data_Block
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
