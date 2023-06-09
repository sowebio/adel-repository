--
-- @filename module-html.ads
-- @author Julien Burdy & Vincent Decorges
-- @date 17.6.02
-- @brief AdaDoc HTML writer module
--
-- History  :
-- Date     Modification               Author
--
package Module.HTML is

   type Object is new Module.Object with private;


   --
   -- Purpose : Receive notification of the beginning of a document.
   --           This callback will be called only once.
   --
   -- Self         : Object itself.
   -- Package_Name : The name of the package
   -- Atts         : The attributs dictionary of the XML current element.
   -- Tags         : The attributs user defined tags.
   --
   procedure Begin_Document (Self : Object;
                             Package_Name : String;
                             Atts, Tags : Dict_Type);


   --
   -- Purpose : Receive notification of the end of a document.
   --           This callback will be called only once.
   --
   -- Self : Object itself.
   --
   procedure End_Document (Self : Object);


   --
   -- Purpose : Receive notification of the beginning of a group of elements.
   --
   -- Self  : Object itself.
   -- Group : the beginning group.
   --
   procedure Start_Group (Self : Object;
                          Group : Element_Type);

   --
   -- Purpose : Receive notification of the end of each elements (Element_Type).
   --
   -- Self    : Object itself.
   -- Element : the ending element
   --
   procedure End_Element (Self : Object;
                          Element : Element_Type);

   --
   -- Purpose : Receive notification of each variant part choice.
   --
   -- Self      : Object itself.
   -- Atts      : The attributs dictionary of the XML current element.
   -- Its_Value : value of the choice
   --
   procedure A_Choice (Self : Object;
                       Atts : Dict_Type;
                       Its_Value : String);

   --
   -- Purpose : Receive notification of each clause.
   --
   -- Self      : Object itself.
   -- Atts      : The attributs dictionary of the XML current element.
   -- Its_Value : value of the clause
   --
   procedure A_Clause (Self : Object;
                       Atts : Dict_Type;
                       Its_Value : String);

   --
   -- Purpose : Receive notification of each exception.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the exception name
   --
   procedure An_Exception (Self : Object;
                           Atts : Dict_Type;
                           Its_Name : String);

   --
   -- Purpose : Receive notification of each pragma.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the pragma name
   --
   procedure A_Pragma (Self : Object;
                       Atts : Dict_Type;
                       Its_Name : String);

   --
   -- Purpose : Receive notification of each function.
   --
   -- Self            : Object itself.
   -- Atts            : The attributs dictionary of the XML current element.
   -- Its_Name        : the function name
   -- Its_Return_Type : the return type of the function
   -- In_Prot_Obj     : true if the function is in a protected object
   --
   procedure A_Function (Self : Object;
                         Atts : Dict_Type;
                         Its_Name,
                         Its_Return_Type : String;
                         In_Prot_Obj : Boolean);

   --
   -- Purpose : Receive notification of each generic instance.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the generic instance name
   --
   procedure A_Generic_Instance (Self : Object;
                                 Atts : Dict_Type;
                                 Its_Name : String);

   --
   -- Purpose : Receive notification of each generic package or subprogram.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the package or subprogram name
   --
   procedure A_Package_Subprogram_Gen (Self : Object;
                                       Atts : Dict_Type;
                                       Its_Name : String);

   --
   -- Purpose : Receive notification of each parameter (procedure, function, entry).
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the parameter name
   -- Its_Type : the parameter type
   -- Its_Mode : the parameter mode (in, out, in out, access)
   --
   procedure A_Parameter (Self : Object;
                          Atts : Dict_Type;
                          Its_Name,
                          Its_Type,
                          Its_Mode : String);

   --
   -- Purpose : Receive notification of each procedure.
   --
   -- Self        : Object itself.
   -- Atts        : The attributs dictionary of the XML current element.
   -- Its_Name    : the procedure name
   -- In_Prot_Obj : true if the function is in a protected object
   --
   procedure A_Procedure (Self : Object;
                          Atts : Dict_Type;
                          Its_Name : String;
                          In_Prot_Obj : Boolean);

   --
   -- Purpose : Receive notification of each protected type.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the protected object name
   --
   procedure A_Protected_Object (Self : Object;
                                 Atts : Dict_Type;
                                 Its_Name : String);

   --
   -- Purpose : Receive notification of each record.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the record name
   --
   procedure A_Record (Self : Object;
                       Atts : Dict_Type;
                       Its_Name : String);

   --
   -- Purpose : Receive notification of each rename.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the rename name
   --
   procedure A_Rename (Self : Object;
                       Atts : Dict_Type;
                       Its_Name : String);

   --
   -- Purpose : Receive notification of each task.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the task name
   --
   procedure A_Task (Self : Object;
                     Atts : Dict_Type;
                     Its_Name : String);

   --
   -- Purpose : Receive notification of each type definition.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the type name
   -- Its_Def  : the type definition
   --
   procedure A_Type_Definition (Self : Object;
                                Atts : Dict_Type;
                                Its_Name,
                                Its_Def : String);

   --
   -- Purpose : Receive notification of each variant part.
   --
   -- Self             : Object itself.
   -- Atts             : The attributs dictionary of the XML current element.
   -- Its_Discriminant : the discriminant of this choice
   --
   procedure A_Variant_Part (Self : Object;
                             Atts : Dict_Type;
                             Its_Discriminant : String);

   --
   -- Purpose : Receive notification of each entry.
   --
   -- Self     : Object itself.
   -- Atts     : The attributs dictionary of the XML current element.
   -- Its_Name : the entry name
   --
   procedure An_Entry (Self : Object;
                       Atts : Dict_Type;
                       Its_Name : String);

   --
   -- Purpose : Receive notification of each object.
   --
   -- Self         : Object itself.
   -- Atts         : The attributs dictionary of the XML current element.
   -- Its_Name     : the object name
   -- Its_Type     : the object type
   -- Is_Component : true if this object is in a component
   --
   procedure An_Object (Self : Object;
                        Atts : Dict_Type;
                        Its_Name,
                        Its_Type : String;
                        Is_Component : Boolean);

private

   type Object is new Module.Object with null record;

end Module.HTML;
