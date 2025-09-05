REPORT zjob_wx_approval_cleaner.

" 看公司要求 默认是不做处理的

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

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  " 清理数据
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_data AS CHECKBOX USER-COMMAND dat MODIF ID dat.
    SELECTION-SCREEN COMMENT 10(10) TEXT-dat MODIF ID gp1 FOR FIELD p_data.

    SELECTION-SCREEN POSITION 25.
    " 过期时间
    PARAMETERS: p_ed01 TYPE int3 MODIF ID dat.
    SELECTION-SCREEN COMMENT 30(10) TEXT-t01 MODIF ID gp1 FOR FIELD p_ed01.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck1.

SELECTION-SCREEN BEGIN OF BLOCK blck2 WITH FRAME.
  " 清理事件
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_evet AS CHECKBOX USER-COMMAND evt MODIF ID evt.
    SELECTION-SCREEN COMMENT 10(10) TEXT-evt MODIF ID gp1 FOR FIELD p_evet.

    SELECTION-SCREEN POSITION 25.
    " 过期时间
    PARAMETERS: p_ed02 TYPE int3 MODIF ID evt.
    SELECTION-SCREEN COMMENT 30(10) TEXT-t01 MODIF ID gp1 FOR FIELD p_ed02.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck2.

SELECTION-SCREEN BEGIN OF BLOCK blck3 WITH FRAME.
  " 清理审批
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_appr AS CHECKBOX USER-COMMAND apr MODIF ID apr.
    SELECTION-SCREEN COMMENT 10(10) TEXT-apr MODIF ID gp1 FOR FIELD p_appr.

    SELECTION-SCREEN POSITION 25.
    " 过期时间
    PARAMETERS: p_ed03 TYPE int3 MODIF ID apr.
    SELECTION-SCREEN COMMENT 30(10) TEXT-t01 MODIF ID gp1 FOR FIELD p_ed03.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blck3.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

  p_ed01 = p_ed02 = p_ed03 = 60.

*&----------------------------------------------------------------------
*                     At Selection-Screen Output
*&----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF ( screen-group1 = 'EVT' OR screen-group1 = 'DAT' ) AND p_appr = 'X'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

  CASE sy-ucomm.
    WHEN 'DAT'.

    WHEN 'EVT'.
      p_appr = p_evet.
      p_data = p_evet.
    WHEN 'APR'.
      p_evet = p_appr.
      p_data = p_appr.
  ENDCASE.

AT SELECTION-SCREEN ON p_ed03.
  IF p_ed03 <> 0 AND p_appr = 'X'.
    p_ed01 = p_ed02 = p_ed03.
  ENDIF.

*&----------------------------------------------------------------------
*                     Start-Of-Selection
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_clean_data.

*&---------------------------------------------------------------------*
*& Form frm_clean_data
*&---------------------------------------------------------------------*
*&  清除数据
*&---------------------------------------------------------------------*
FORM frm_clean_data .
  DATA: lv_timestamp TYPE timestamp.
  DATA: lv_datum TYPE datum,
        lv_uzeit TYPE uzeit.

  CHECK p_data = 'X' OR p_evet = 'X' OR p_appr = 'X'.

  IF p_data = 'X'.
    lv_datum = sy-datum - p_ed01.
    CONVERT DATE lv_datum TIME lv_uzeit INTO TIME STAMP lv_timestamp TIME ZONE 'UTC+8'.

    DELETE FROM ztwx_log_data WHERE ap_no IN ( SELECT ap_no FROM ztwx_approval WHERE changed_at <= @lv_timestamp ).
  ENDIF.

  IF p_evet = 'X'.
    lv_datum = sy-datum - p_ed02.
    CONVERT DATE lv_datum TIME lv_uzeit INTO TIME STAMP lv_timestamp TIME ZONE 'UTC+8'.

    DELETE FROM ztwx_log_event WHERE ap_no IN ( SELECT ap_no FROM ztwx_approval WHERE changed_at <= @lv_timestamp ).
  ENDIF.

  IF p_appr = 'X'.
    lv_datum = sy-datum - p_ed03.
    CONVERT DATE lv_datum TIME lv_uzeit INTO TIME STAMP lv_timestamp TIME ZONE 'UTC+8'.

    DELETE FROM ztwx_approval WHERE changed_at <= @lv_timestamp.
  ENDIF.

  COMMIT WORK.

ENDFORM.
