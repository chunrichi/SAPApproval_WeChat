INTERFACE zif_wx_oa_ft
  PUBLIC .


  DATA ft TYPE REF TO zcl_wx_oa_ft .
  DATA approval TYPE REF TO zcl_wx_approval .

  METHODS send
    EXPORTING
      !errmsg      TYPE string
      !approval    TYPE ztwx_approval
    RETURNING
      VALUE(sp_no) TYPE string .
ENDINTERFACE.
