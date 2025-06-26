INTERFACE zif_wx_oa_ft
  PUBLIC .


  INTERFACES zif_wx_ap_result .

  ALIASES granted
    FOR zif_wx_ap_result~granted .
  ALIASES rejected
    FOR zif_wx_ap_result~rejected .

  DATA ft TYPE REF TO zcl_wx_oa_ft .
  DATA approval TYPE REF TO zcl_wx_approval .

  METHODS send
    EXPORTING
      !errmsg      TYPE string
    RETURNING
      VALUE(sp_no) TYPE string .
ENDINTERFACE.
