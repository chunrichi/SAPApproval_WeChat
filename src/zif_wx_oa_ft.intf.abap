INTERFACE zif_wx_oa_ft
  PUBLIC .


  DATA ft TYPE REF TO zcl_wx_oa_ft .
  DATA approval TYPE REF TO zcl_wx_approval .

  METHODS send
    IMPORTING
      !uname       TYPE sy-uname DEFAULT sy-uname
    EXPORTING
      !errmsg      TYPE string
      !approval    TYPE ztwx_approval
    RETURNING
      VALUE(sp_no) TYPE string .
  METHODS resend
    IMPORTING
      !ap_no       TYPE ztwx_approval-ap_no
      !uname       TYPE sy-uname OPTIONAL
    EXPORTING
      !errmsg      TYPE string
      !approval    TYPE ztwx_approval
    RETURNING
      VALUE(sp_no) TYPE string .
ENDINTERFACE.
