REPORT zjob_wx_approval_result.

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------
TABLES: ztwx_approval.

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------

CLASS lcl_progress_bar DEFINITION DEFERRED.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  SELECT-OPTIONS: s_sp_no FOR ztwx_approval-sp_no.
  SELECT-OPTIONS: s_apsta FOR ztwx_approval-apsta.
SELECTION-SCREEN END OF BLOCK blck1.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

  IF s_apsta[] IS INITIAL.
    s_apsta[] = VALUE #( sign = 'I' option = 'LE' ( low = 1 ) ).
  ENDIF.

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  " 程序加锁
  PERFORM frm_lock_prog.

  PERFORM frm_get_data.

  PERFORM frm_dequeue_prog.

*----------------------------------------------------------------------*
*       CLASS lcl_progress_bar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_progress_bar DEFINITION.
  PUBLIC SECTION.

    DATA: count     TYPE i,
          base_desc TYPE string,
          curr      TYPE i.

    METHODS constructor IMPORTING i_count     TYPE i OPTIONAL
                                  i_base_desc TYPE text50 OPTIONAL.

    METHODS add IMPORTING i_add  TYPE i DEFAULT 1
                          i_desc TYPE data OPTIONAL.
  PRIVATE SECTION.
    DATA: percent     TYPE p DECIMALS 0 LENGTH 5,
          percent_old TYPE p DECIMALS 0 LENGTH 5.
    METHODS display IMPORTING desc TYPE data.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_progress_bar IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_progress_bar IMPLEMENTATION.

  METHOD constructor.
    me->count = i_count.
    me->base_desc = i_base_desc.
  ENDMETHOD.

  METHOD add.

    me->curr = me->curr + i_add.

    me->percent = me->curr / me->count * 100.

    "CHECK me->percent_old <> me->percent.

    me->percent_old = me->percent.

    me->display( i_desc ).

  ENDMETHOD.

  METHOD display.
    DATA: lv_text TYPE string.

    lv_text = |[{ me->curr }/{ me->count }] | && me->base_desc.

    REPLACE FIRST OCCURRENCE OF '&' IN lv_text WITH desc.

    IF sy-batch = 'X'.
      MESSAGE lv_text TYPE 'S'.
    ELSE.

      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = me->percent
          text       = lv_text.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Form frm_lock_prog
*&---------------------------------------------------------------------*
*& 程序加锁 避免增量处理时同时运行
*&---------------------------------------------------------------------*
FORM frm_lock_prog .
  CALL FUNCTION 'ENQUEUE_ES_PROG'
    EXPORTING
      mode_trdir     = 'E'
      name           = sy-cprog
      x_name         = ' '
      _scope         = '2'
      _wait          = 'X'
      _collect       = ' '
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.
  IF sy-subrc <> 0.
    IF sy-msgid = 'MC' AND sy-msgty = 'E' AND sy-msgno = '601'.
      MESSAGE ID 'ZWX01' TYPE sy-msgty NUMBER 011
        WITH sy-msgv1 sy-cprog.
    ELSE.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_dequeue_prog
*&---------------------------------------------------------------------*
*&  解锁
*&---------------------------------------------------------------------*
FORM frm_dequeue_prog .
  CALL FUNCTION 'DEQUEUE_ES_PROG'
    EXPORTING
      mode_trdir = 'E'
      name       = sy-cprog
      x_name     = ' '
      _scope     = '3'
      _synchron  = ' '
      _collect   = ' '.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_get_data
*&---------------------------------------------------------------------*
*&  获取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .
  DATA: BEGIN OF result,
          errcode TYPE string,
          errmsg  TYPE string,
          info    TYPE REF TO zcl_wx_oa_ap_info,
        END OF result.

  SELECT DISTINCT
    sp_no
    FROM ztwx_approval
    WHERE sp_no IN @s_sp_no
      AND statu = 'S'
      AND apsta IN @s_apsta
    INTO TABLE @DATA(lt_sp_no).

  IF lt_sp_no IS INITIAL.
    " 未找到满足条件的数据
    MESSAGE s003(zwechat) DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  DATA(lr_pb) = NEW lcl_progress_bar( i_count     = lines( lt_sp_no )
                                      i_base_desc = '正在处理审批单据 &'(t01) ).

  DATA(l_result) = NEW zcl_wx_approval_result( ).

  LOOP AT lt_sp_no INTO DATA(ls_sp_no).
    lr_pb->add( i_desc = ls_sp_no-sp_no ).

    l_result->load_approval_info( sp_no = ls_sp_no-sp_no ).

    l_result->upd_approval( sp_no = ls_sp_no-sp_no ).

    l_result->cust_approval_result( ).
  ENDLOOP.

ENDFORM.
