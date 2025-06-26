CLASS zcl_wx_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA config TYPE ztwx_config .

    CLASS-METHODS class_constructor .
    CLASS-METHODS load .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_CONFIG IMPLEMENTATION.


  METHOD class_constructor.
    load( ).

  ENDMETHOD.


  METHOD load.

    SELECT SINGLE * FROM ztwx_config WHERE sysid = @sy-sysid INTO @config.

  ENDMETHOD.
ENDCLASS.
