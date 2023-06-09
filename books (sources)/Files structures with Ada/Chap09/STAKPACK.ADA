GENERIC
    Type Stack_Entry Is Private;
    size : Integer := 50;
PACKAGE StackPkg is
    Type Status_Type is (EMPTY, OK, FULL);
    BOTTOM : Constant := 1;
    Type Space_Type is Array (BOTTOM..SIZE) Of Stack_Entry;
    Type Stack_Type is Record
                           top : Integer;
                           space : Space_Type;
                       End Record;

    PROCEDURE Create (stack : in out Stack_Type);
    PROCEDURE Push (stack : in out Stack_Type; info : in Stack_Entry);
    PROCEDURE Pop  (stack : in out Stack_Type; info : out Stack_Entry);
    FUNCTION Status (stack : Stack_Type) Return Status_Type;
    FUNCTION Tos (stack : Stack_Type) Return Stack_Entry;
END StackPkg;

PACKAGE Body StackPkg is

    PROCEDURE Create (stack : in out Stack_Type) is
    BEGIN   --Create
        stack.top := 0;
    END Create;

    PROCEDURE Push (stack : in out Stack_Type;
                    info : in Stack_Entry) is
    BEGIN   --Push
            stack.top := stack.top + 1;
            stack.space (stack.top) := info;
    END Push;

    PROCEDURE Pop  (stack : in out Stack_Type;
                     info : out Stack_Entry) is
    BEGIN   --Pop
            info := stack.space (stack.top);
            stack.top := stack.top - 1;
    END Pop;

    FUNCTION Status (stack : Stack_Type) Return Status_Type is
    BEGIN   --Status
        If stack.top < BOTTOM Then
            Return EMPTY;
        Elsif stack.top = SIZE Then
            Return FULL;
        Else
            Return OK;
        End If;
    END Status;

    FUNCTION Tos (stack : Stack_Type) Return Stack_Entry is
    BEGIN   --Tos
        Return stack.space (stack.top);
    END Tos;

END StackPkg;
