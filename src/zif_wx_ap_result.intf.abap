INTERFACE zif_wx_ap_result
  PUBLIC .


  METHODS granted IMPORTING sp_no TYPE ztwx_approval-sp_no.
  METHODS rejected IMPORTING sp_no TYPE ztwx_approval-sp_no.
  METHODS process_result IMPORTING sp_no TYPE ztwx_approval-sp_no
                                   apsta TYPE ztwx_approval-apsta.
ENDINTERFACE.
