With Direct_IO;
GENERIC
    degree : Positive;
    Type Key_Type Is Private;
    With FUNCTION ">" ( left_operand, right_operand : Key_Type )
                        Return Boolean;
PACKAGE B_Tree Is
    PROCEDURE Create_B_Tree ( external_b_tree_filename : String );
    PROCEDURE Open_B_Tree ( external_b_tree_filename : String );
    PROCEDURE Insert ( key : Key_Type; key_position : Positive );
    PROCEDURE Delete ( key : Key_Type );
    PROCEDURE Search ( key : Key_Type; key_position : Out Positive );
    PROCEDURE Close_B_Tree ;
    key_not_inserted : Exception;
    key_not_deleted  : Exception;
    key_not_found    : Exception;
END B_Tree;

PACKAGE Body B_Tree Is

    Type A_Tuple Is Record
                        key     : Key_Type;
                        address : Positive;
                        subtree : Natural;
                    End Record;
    Type Tuple_Array Is Array ( Integer Range <> ) Of A_Tuple;
    Type Node_Type Is Record
                         number_of_tuples : Natural;
                         first_subtree    : Natural;
                         tuple            : Tuple_Array ( 1 .. degree - 1 );
                     End Record;
    Type BigNode_Type Is Record
                            number_of_tuples : Natural;
                            first_subtree    : Natural;
                            tuple            : Tuple_Array ( 1 .. degree );
                        End Record;
    Type Operation_Type Is ( INSERT_KEY, DELETE_KEY, NIL, SEARCH_KEY );

    Package B_Tree_IO Is New Direct_IO ( Node_Type );

    b_tree_file : B_Tree_IO.File_Type;
    hold_tuple  : A_Tuple;
    key_found   : Exception;
    next        : Positive;

    PROCEDURE Create_B_Tree ( external_b_tree_filename : String ) Is
        node : Node_Type;
    BEGIN
        B_Tree_IO.Create ( b_tree_file, B_Tree_IO.INOUT_FILE,
                             external_b_tree_filename );
        node.number_of_tuples := 0;
        B_Tree_IO.Write ( b_tree_file, node, 1 );
        next := 1;
    END Create_B_Tree;

    PROCEDURE Open_B_Tree ( external_b_tree_filename : String ) Is
    BEGIN
        B_Tree_IO.Open ( b_tree_file, B_Tree_IO.INOUT_FILE,
                           external_b_tree_filename );
        next := Positive ( B_Tree_IO.Size ( b_tree_file ) );
    END Open_B_Tree;

    PROCEDURE Find_node ( hold_tuple : IN OUT A_Tuple;
                          p : In Natural;
                          operation : In Out Operation_Type ) Is
        ith_position : Integer;
        node         : Node_Type;

        PROCEDURE Insert_Tuple ( hold_tuple : IN OUT A_Tuple;
                                 ith_position  : Positive;
                                 node : In Out Node_Type;
                                 operation : In Out Operation_Type ) Is

            PROCEDURE Split  ( hold_tuple : IN OUT A_Tuple;
                               ith_position  : Positive;
                               node : In Out Node_Type ) Is
                bignode    : BigNode_Type;
                index      : Integer;
                middle     : Positive;
                newnode    : Node_Type;
                temp_tuple : A_Tuple;

            BEGIN  -- Split
                bignode.number_of_tuples := node.number_of_tuples;
                bignode.first_subtree := node.first_subtree;
                For lcv In 1 .. ith_position  - 1 Loop
                    bignode.tuple ( lcv ) := node.tuple ( lcv );
                End Loop;
                bignode.tuple ( ith_position  ) := hold_tuple;
                For lcv In ith_position  .. node.number_of_tuples Loop
                    bignode.tuple ( lcv  + 1 ) := node.tuple ( lcv );
                End Loop;
                bignode.number_of_tuples := bignode.number_of_tuples + 1;

                middle := ( degree - 1 ) / 2 + 1;  -- withdraw middle tuple
                hold_tuple := bignode.tuple ( middle );
                next := next + 1;
                hold_tuple.subtree := next;

                node.first_subtree := bignode.first_subtree;
                For lcv In 1 .. middle - 1 Loop
                      temp_tuple := bignode.tuple ( lcv );
--                        node.tuple ( lcv ) := bignode.tuple ( lcv );
                      node.tuple ( lcv ) := temp_tuple;
                End Loop;
                node.number_of_tuples := middle - 1;

                B_Tree_IO.Write ( b_tree_file, node,
                                    B_Tree_IO.Positive_Count ( p ) );

                newnode.first_subtree := bignode.tuple ( middle ).subtree;
                index := 0;
                For lcv In middle + 1 .. bignode.number_of_tuples Loop
                    index := index + 1;
                    temp_tuple := bignode.tuple( lcv );
--                      newnode.tuple ( index ) := bignode.tuple( lcv );
                    newnode.tuple ( index ) := temp_tuple;
                End Loop;
                newnode.number_of_tuples := index;

                B_Tree_IO.Write ( b_tree_file, newnode,
                                    B_Tree_IO.Positive_Count ( next ) );

           END Split;

        BEGIN  --  Insert_Tuple
                   -- insert hold_tuple into position ith_position of node (address p)
            If node.number_of_tuples < degree - 1 Then
                   -- resulting node is not too big
                operation := NIL;
                For lcv In Reverse ith_position  .. node.number_of_tuples Loop
                    node.tuple ( lcv + 1 ) := node.tuple ( lcv );
                End Loop;
                node.number_of_tuples := node.number_of_tuples + 1;
                node.tuple ( ith_position  ) := hold_tuple;
                B_Tree_IO.Write ( b_tree_file, node,
                                    B_Tree_IO.Positive_Count ( p ) );
            Else
                Split ( hold_tuple, ith_position , node );
            End If;
        END Insert_Tuple;

    BEGIN  -- Find_Node
        If p = 0 Then
            Return;
        Else
            B_Tree_IO.Read ( b_tree_file, node,
                               B_Tree_IO.Positive_Count ( p ) );
            If hold_tuple.key >
               node.tuple ( node.number_of_tuples ).key Then
                ith_position := node.number_of_tuples  + 1;
                Find_Node ( hold_tuple, node.tuple ( node.number_of_tuples ).subtree,
                           operation);
                If operation = INSERT_KEY Then
                    Insert_Tuple ( hold_tuple, ith_position, node, operation );
                End If;
            Else  -- hold_tuple .key <= node.tuple ( ith_position )
                ith_position := 1;
                Loop
                    If hold_tuple.key > node.tuple ( ith_position ).key Then
                        ith_position := ith_position + 1;
                    ElsIf hold_tuple.key = node.tuple ( ith_position ).key Then
                        hold_tuple.address := node.tuple ( ith_position ).address;
                        Raise key_found;
                    Else
                    --  hold_tuple.key < node.tuple ( ith_position ).key
                        If ith_position = 1 Then
                            Find_Node ( hold_tuple,
                                        node.first_subtree,
                                        operation );
                        Else
                            Find_Node ( hold_tuple,
                                        node.tuple ( ith_position - 1 ).subtree,
                                        operation );
                        End If;
                        If operation = INSERT_KEY Then
                            Insert_Tuple ( hold_tuple, ith_position, node, operation );
                        End If;
                        Exit;
                    End If;
                End Loop;
            End If;
        End If;
    END Find_Node;

    PROCEDURE Insert ( key : Key_Type; key_position : Positive ) Is
        new_node  : Node_Type;
        new_tuple : A_Tuple;
        root_node : Node_Type;
        operation : Operation_Type := INSERT_KEY;
    BEGIN  -- Insert
        B_Tree_IO.Read ( b_tree_file, root_node, 1 );
        new_tuple := ( key, key_position, 0 );
        Find_Node ( new_tuple, root_node.number_of_tuples, operation );
        If operation = INSERT_KEY Then
            new_node.number_of_tuples := 1;
            B_Tree_IO.Read ( b_tree_file, root_node, 1 );
            new_node.first_subtree := root_node.number_of_tuples;
            new_node.tuple ( 1 ) := ( new_tuple.key,
                                        new_tuple.address,
                                        new_tuple.subtree );
            next := next + 1;
            B_Tree_IO.Write ( b_tree_file, new_node,
                                B_Tree_IO.Positive_Count ( next ) );
            root_node.number_of_tuples := next;
            B_Tree_IO.Write ( b_tree_file, root_node, 1 );
        End If;
    EXCEPTION
        When key_found => Raise key_not_inserted;
    END Insert;

    PROCEDURE Delete ( key : Key_Type ) Is
        KEY_NOT_FOUND   : Exception;
        MINIMUM         :  Positive := degree / 2;
        node_undersized : Boolean := FALSE;
        operation       : Operation_Type := DELETE_KEY;
        root_address    : Positive;
        root_node       : Node_Type;

        PROCEDURE Delete_Tuple ( delete_value : Key_Type;
                                 p : In Natural;
                                 node_undersized : In Out Boolean ) Is
                 -- search node with address p for key
            child_address : Natural;
            ith_position  : Natural;
            node          : Node_Type;

            PROCEDURE Under_Flow ( parent_address,
                                   child_address : In Positive;
                                   tuple_index : In Out Natural;
                                   node_undersized : Out Boolean ) Is

            child_node    : Node_Type;
            left          : Positive;
            left_extras   : Integer;
            left_node     : Node_Type;
            parent_node   : Node_Type;
            right_address : Positive;
            right_extras  : Integer;
            right_node    : Node_Type;

            PROCEDURE Balance_From_Right Is
            BEGIN
                For j In 1 .. right_extras - 1 Loop
                    child_node.tuple ( child_node.number_of_tuples + j )
                       := right_node.tuple( j );
                End Loop;
                child_node.number_of_tuples := MINIMUM - 1 + right_extras;
                B_Tree_IO.Write ( b_tree_file, child_node,
                                  B_Tree_IO.Positive_Count ( child_address ) );
                parent_node.tuple( tuple_index ) := right_node.tuple( right_extras );
                parent_node.tuple ( tuple_index ).subtree := right_address;
                B_Tree_IO.Write ( b_tree_file, parent_node,
                                  B_Tree_IO.Positive_Count ( parent_address ) );
                right_node.first_subtree
                    := right_node.tuple( right_extras ).subtree;
                right_node.number_of_tuples
                    := right_node.number_of_tuples - right_extras;
                For j In 1 .. right_node.number_of_tuples Loop
                    right_node.tuple ( j )
                        := right_node.tuple ( right_extras + j );
                End Loop;
                B_Tree_IO.Write ( b_tree_file, right_node,
                                  B_Tree_IO.Positive_Count ( right_address ) );
                node_undersized := FALSE;
            END Balance_From_Right;

            PROCEDURE Balance_From_Left Is
            BEGIN
                For j In Reverse 1 .. MINIMUM Loop
                         -- move tuples over 'left_extras' places to make
                         -- room for tuples at lower end
                    child_node.tuple ( j + left_extras ) :=
                        child_node.tuple ( j );
                End Loop;
                child_node.tuple ( left_extras ) := parent_node.tuple ( tuple_index );
                child_node.tuple ( left_extras ).subtree
                    := child_node.first_subtree;
                left_node.number_of_tuples := MINIMUM + 1;
                For j In Reverse 1 .. left_extras - 1 Loop
                    child_node.tuple( j )
                        := left_node.tuple( left_node.number_of_tuples + j );
                End Loop;
                child_node.first_subtree
                    := left_node.tuple( left_node.number_of_tuples ).subtree;
                child_node.number_of_tuples := MINIMUM - 1 + left_extras;
                parent_node.tuple( tuple_index )
                    := left_node.tuple ( left_node.number_of_tuples );
                parent_node.tuple ( tuple_index ).subtree:= child_address;
                left_node.number_of_tuples := left_node.number_of_tuples - 1;
                B_Tree_IO.Write ( b_tree_file, parent_node,
                                  B_Tree_IO.Positive_Count ( parent_address ) );
                B_Tree_IO.Write ( b_tree_file, left_node,
                                  B_Tree_IO.Positive_Count ( left ) );
                B_Tree_IO.Write ( b_tree_file, child_node,
                                  B_Tree_IO.Positive_Count ( child_address ) );
                node_undersized := FALSE;
            END Balance_From_Left;

            PROCEDURE Merge_With_Right Is
            BEGIN
                For j IN 1 .. MINIMUM Loop
                    child_node.tuple( MINIMUM + j ) := right_node.tuple( j );
                End Loop;
                For j In tuple_index .. parent_node.number_of_tuples - 1 Loop
                    parent_node.tuple( j ) := parent_node.tuple( j + 1 );
                End Loop;
                child_node.number_of_tuples := degree - 1;
                right_node.number_of_tuples := 0;
                B_Tree_IO.Write ( b_tree_file, right_node,
                                  B_Tree_IO.Positive_Count ( right_address ) );
                parent_node.number_of_tuples :=
                    parent_node.number_of_tuples - 1;
                node_undersized := parent_node.number_of_tuples < MINIMUM;
                B_Tree_IO.Write ( b_tree_file, child_node,
                                  B_Tree_IO.Positive_Count ( child_address ) );
                B_Tree_IO.Write ( b_tree_file, parent_node,
                                  B_Tree_IO.Positive_Count ( parent_address ) );
            END Merge_With_Right;

            PROCEDURE Merge_With_Left Is
            BEGIN
                left_node.number_of_tuples := left_node.number_of_tuples + 1;
                left_node.tuple( left_node.number_of_tuples )
                    := parent_node.tuple ( tuple_index );
                left_node.tuple( left_node.number_of_tuples ).subtree
                    := child_node.first_subtree;
                For j In 1 .. child_node.number_of_tuples Loop
                    left_node.tuple ( left_node.number_of_tuples + j )
                        := child_node.tuple ( j );
                End Loop;
                left_node.number_of_tuples := degree - 1;
                parent_node.number_of_tuples := parent_node.number_of_tuples - 1;
                node_undersized := parent_node.number_of_tuples < MINIMUM;
                B_Tree_IO.Write ( b_tree_file, left_node,
                                  B_Tree_IO.Positive_Count ( left ) );
                B_Tree_IO.Write ( b_tree_file, parent_node,
                                  B_Tree_IO.Positive_Count ( parent_address ) );
            END Merge_With_Left;

            PROCEDURE Choose_Left_Sibling Is
            BEGIN
                If tuple_index = 1 Then
                    left := parent_node.first_subtree;
                Else
                    left := parent_node.tuple ( tuple_index - 1 ).subtree;
                End If;
                B_Tree_IO.Read ( b_tree_file, left_node,
                                 B_Tree_IO.Positive_Count ( left ) );
                left_extras := (left_node.number_of_tuples - MINIMUM + 1) / 2;
            END Choose_Left_Sibling;

        BEGIN  -- Under_Flow
            B_Tree_IO.Read ( b_tree_file, child_node,
                             B_Tree_IO.Positive_Count ( child_address ) );
            B_Tree_IO.Read ( b_tree_file, parent_node,
                             B_Tree_IO.Positive_Count ( parent_address ) );
            If tuple_index < parent_node.number_of_tuples Then
                    -- a right sibling exists
                tuple_index := tuple_index + 1;
                right_address := parent_node.tuple ( tuple_index ).subtree;
                B_Tree_IO.Read ( b_tree_file, right_node,
                                 B_Tree_IO.Positive_Count ( right_address ) );
                right_extras := (right_node.number_of_tuples
                                  - MINIMUM + 1) / 2;
                    -- right_extras = number of tuples over the MINIMUM
                    -- that can be balanced between child_node
                    -- and right_node
                child_node.number_of_tuples := child_node.number_of_tuples + 1;
                child_node.tuple ( child_node.number_of_tuples )
                    := parent_node.tuple ( tuple_index );
                child_node.tuple ( child_node.number_of_tuples ).subtree
                    := right_node.first_subtree;
                    --  replace predecessor of node to original location
                If right_extras > 0 Then
                    -- move right_extras number of tuples from right_node
                    -- to child_node
                    Balance_From_Right;
                ElsIf tuple_index /= 0 Then
                      -- check to see if left sibling exists and has extras
                      -- left sibling exists

                        Choose_Left_Sibling;
                        If left_extras > 0 Then
                            Balance_From_Left;
                        Else
                            -- left sibling has no extras so merge child_node
                            -- and right_node into child_node
                            Merge_With_Right;
                        End If;
                Else
                    -- left sibling does not exist so merge child_node
                    -- and right_node into child_node
                    Merge_With_Right;
                End If;
            Else
                -- no right sibling exists; choose left sibling
                Choose_Left_Sibling;
                If left_extras > 0 Then
                    Balance_From_Left;
                Else
                    Merge_With_Left;
                End If;
            End If;

        END Under_Flow;

            PROCEDURE Found_Delete_Value Is

                PROCEDURE Find_Predecessor ( decendant_address : In Positive;
                                             node_undersized : In Out Boolean ) Is
                    decendant_node : Node_Type;
                    n, q : Natural;
                BEGIN  -- Find_Predecessor
                    B_Tree_IO.Read ( b_tree_file, decendant_node,
                             B_Tree_IO.Positive_Count ( decendant_address ) );
                    n := decendant_node.number_of_tuples;
                    q := decendant_node.tuple( n ).subtree;
                    If q = 0 Then
                        decendant_node.tuple( n ).subtree :=
                            node.tuple( ith_position ).subtree;
                        node.tuple( ith_position ) := decendant_node.tuple( n );
                        decendant_node.number_of_tuples
                            := decendant_node.number_of_tuples - 1;
                        node_undersized := decendant_node.number_of_tuples
                                           < MINIMUM;
                        B_Tree_IO.Write ( b_tree_file, node,
                                          B_Tree_IO.Positive_Count ( p ) );
                        B_Tree_IO.Write ( b_tree_file, decendant_node,
                                          B_Tree_IO.Positive_Count
                                              ( decendant_address ) );
                    Else  -- look deeper in the tree for predecessor
                        Find_Predecessor ( q, node_undersized );
                        If node_undersized Then
                            Under_Flow ( decendant_address, q,
                                         decendant_node.number_of_tuples,
                                         node_undersized );
                        End If;
                    End If;
                End Find_Predecessor;

            BEGIN  --Found_Delete_Value
                If ith_position = 1 Then
                    child_address := node.first_subtree;
                Else
                    child_address := node.tuple( ith_position - 1 ).subtree;
                End If;
                If child_address = 0 Then
                       -- delete key(ith_position ) from leaf node
                    node.number_of_tuples := node.number_of_tuples - 1;
                    node_undersized := node.number_of_tuples < MINIMUM;
                    For j In ith_position .. node.number_of_tuples Loop
                        node.tuple( j ) := node.tuple( j + 1 );
                    End Loop;
                    B_Tree_IO.Write ( b_tree_file, node,
                                      B_Tree_IO.Positive_Count ( p ) );
                Else  --    delete key(ith_position ) from nonterminal node;
                      --    find predecessor to replace key(ith_position )
                    Find_Predecessor ( child_address, node_undersized );
                    If node_undersized Then
                        ith_position := ith_position - 1;
                        Under_Flow ( p, child_address, ith_position,
                                     node_undersized );
                    End If;
                End If;
            END Found_Delete_Value;

        BEGIN  -- Delete_Tuple
            If p = 0 Then        --  not found
                Raise key_not_deleted;
            Else
                B_Tree_IO.Read ( b_tree_file, node,
                                 B_Tree_IO.Positive_Count ( p ) );
                If delete_value >
                   node.tuple ( node.number_of_tuples ).key Then
                    ith_position := node.number_of_tuples;
                    child_address := node.tuple( ith_position ).subtree;
                    Delete_Tuple ( delete_value, child_address, node_undersized );
                    If node_undersized Then
                        Under_Flow ( p, child_address, ith_position, node_undersized );
                    End If;
                Else
                    ith_position := 1;
                    Loop
                        If delete_value > node.tuple( ith_position ).key Then
                            ith_position := ith_position + 1;
                        ElsIf delete_value = node.tuple( ith_position ).key Then
                            Found_Delete_Value;
                            Exit;
                        Else
                        -- delete_value < node.tuple( ith_position ).key Then

                            If ith_position > 1 Then
                                child_address := node.tuple( ith_position - 1 ).subtree;
                            Else
                                child_address := node.first_subtree;
                            End If;
                            Delete_Tuple ( delete_value, child_address,
                                           node_undersized );
                            If node_undersized Then
                                ith_position := ith_position - 1;
                                Under_Flow ( p, child_address, ith_position,
                                             node_undersized );
                            End If;
                            Exit;
                        End If;
                    End Loop;
                End If;
            End If;
        END Delete_Tuple;

    BEGIN  -- Delete
        If degree / 2 * 2 = degree Then  -- test for even numbered degree
            MINIMUM := MINIMUM - 1;
        End If;
        B_Tree_IO.Read ( b_tree_file, root_node, 1 );
        root_address := root_node.number_of_tuples;
        Delete_Tuple ( key, root_address, node_undersized );
        If node_undersized
        And Then root_node.number_of_tuples = 0 Then
                 -- reducing the height of the B_tree
            root_node.number_of_tuples := root_node.first_subtree;
            B_Tree_IO.Write ( b_tree_file, root_node, 1 );
        End If;
    END Delete;

    PROCEDURE Search ( key : Key_Type; key_position : Out Positive ) Is
        new_tuple : A_Tuple;
        root_node : Node_Type;
        operation : Operation_Type := SEARCH_KEY;
    BEGIN
        B_Tree_IO.Read ( b_tree_file, root_node, 1 );
        new_tuple := ( key, 1, 0 );
        Find_Node ( new_tuple, root_node.number_of_tuples, operation );
        key_position := 1;
        Raise key_not_found;
    EXCEPTION
        When key_found =>
            key_position := new_tuple.address;
    END Search;


    PROCEDURE Close_B_Tree Is
    BEGIN
            B_Tree_IO.Close ( b_tree_file );
    END Close_B_Tree;

END B_Tree;