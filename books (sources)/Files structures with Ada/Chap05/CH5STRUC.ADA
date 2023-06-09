With Sequential_IO;
With Text_IO;
PACKAGE Ch5Struc Is
    SubType Id_String Is String (1..3);
    SubType String10 Is String (1..10);
    Type Car_Master_Record Is
             Record
                 id_number : Id_String;
                 make      : String10;
                 style     : String10;
                 model     : String10;
                 mileage   : String10;
                 color     : String10;
             End Record;
    Type Car_Transaction_Record  Is
             Record
                 update_code : Character;
                 id_number   : Id_String;
                 make        : String10;
                 style       : String10;
                 model       : String10;
                 mileage     : String10;
                 color       : String10;
             End Record;
    PACKAGE Car_IO Is New Sequential_IO ( Car_master_record );
    master_record : Car_master_record ;
    masterfile    : Car_IO.File_Type;
    new_record    : Car_Master_Record;
    newfile       : Car_IO.File_TYpe;
    SENTINEL      : Id_String := "ZZZ";
    transaction   : Car_Transaction_Record;
    transfile     : Text_IO.File_Type;
END Ch5Struc;