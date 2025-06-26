CLASS zcl_wechat_config DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA: BEGIN OF config,
                  corp_id     TYPE string,
                  corp_secret TYPE string,
                END OF config.

    CLASS-METHODS class_constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WECHAT_CONFIG IMPLEMENTATION.


  METHOD class_constructor.

    config-corp_id     = `xxxxx`.
    config-corp_secret = `xxxxxxxxxxx`.

  ENDMETHOD.
ENDCLASS.
