With Text_IO;
GENERIC
    INDEX_BLOCK_SIZE : Positive;
    DATA_BLOCK_SIZE  : Positive;
    Type key_Type Is Private;
    Type Record_Info Is Private;
    With FUNCTION Key_Of ( a_record : Record_Info ) Return Key_Type;
    With FUNCTION ">" ( left_operand, right_operand : Key_Type )
                        Return Boolean;
    With PROCEDURE Print_Key ( key : Key_Type );
PACKAGE VSAM Is
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
END VSAM;

With IO_Exceptions;
With Direct_IO;
With StackPkg;
PACKAGE Body VSAM Is
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
                         successful : In Out Boolean ) Is Separate;

    PROCEDURE Insert ( data_record : Record_Info ) Is Separate;

    PROCEDURE Delete ( key : Key_Type ) Is Separate;

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

BEGIN -- VSAM;
    Stack_Package.Create ( stack );
END VSAM;
