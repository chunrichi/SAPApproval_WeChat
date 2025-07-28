CLASS zcl_wx_oa_ft_send DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_ft .

    ALIASES approval
      FOR zif_wx_oa_ft~approval .
    ALIASES ft
      FOR zif_wx_oa_ft~ft .
    ALIASES send
      FOR zif_wx_oa_ft~send .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FT_SEND IMPLEMENTATION.


  METHOD zif_wx_oa_ft~send.

    " ------------------- 发起审批 ----------------------
    DATA(ls_result) = me->approval->send( me->ft ).

    IF ls_result-errcode = 0.
      sp_no = ls_result-sp_no.
      approval = me->approval->approval.
    ELSE.
      CLEAR sp_no.
      MESSAGE ls_result-errmsg TYPE 'S' DISPLAY LIKE 'E'.
      errmsg = ls_result-errmsg.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
