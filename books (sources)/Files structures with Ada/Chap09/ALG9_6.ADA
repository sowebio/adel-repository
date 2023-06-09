With ISAM,
     Ch5Struc;
Procedure Alg9_6 Is
    Function Car_Key ( car_data : Ch5Struc.Car_Master_Record )
             Return Ch5Struc.Id_String Is
    Begin
        Return car_data.id_number;
    End Car_Key;
    Package Car_ISAM Is New ISAM ( 30,
                                     3,
                                     Ch5Struc.Id_String,
                                     Ch5Struc.Car_Master_Record,
                                     Car_Key,
                                     ">",
                                     "<" );
Begin
    Car_ISAM.Create_ISAM ( "carmstr.dat",
                            "carisam.inx",
                            "carisam.blk",
                            "carisam.ovr" );
    Car_ISAM.Close_ISAM;
End Alg9_6;