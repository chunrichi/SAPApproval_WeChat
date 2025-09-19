REPORT zwxr_user_info.

*&----------------------------------------------------------------------
*                     Type-Pools
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Tables
*&----------------------------------------------------------------------

*&----------------------------------------------------------------------
*                     Types
*&----------------------------------------------------------------------
TYPES: BEGIN OF ty_display,
         bname      TYPE user_addrp-bname,          " 用户名
         name_text  TYPE user_addrp-name_text,      " 姓名
         tel_number TYPE adcp-tel_number,           " 维护手机号
         phone      TYPE ztwx_user_info-phone,      " 关联手机号
         smtp_addr  TYPE adr6-smtp_addr,            " 维护邮箱
         " email      TYPE string,                    " 关联邮箱
         userid     TYPE ztwx_user_info-userid,     " 企微账户id
         created_at TYPE ztwx_user_info-created_at, " 首次拉取时间
         changed_at TYPE ztwx_user_info-changed_at, " 上次拉取时间

         box        TYPE char1,        " -
         icon       TYPE char4,        " 结果
         message    TYPE bapi_msg,     " 消息
       END OF ty_display.

*&----------------------------------------------------------------------
*                     Variables
*&----------------------------------------------------------------------
DATA: gs_layout   TYPE lvc_s_layo,
      gt_fieldcat TYPE lvc_t_fcat.

DATA: gt_display TYPE TABLE OF ty_display.

*&----------------------------------------------------------------------
*                     Select Screen
*&----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME.
  PARAMETERS: p_sall AS CHECKBOX DEFAULT '' USER-COMMAND sall.
SELECTION-SCREEN END OF BLOCK blck1.

*&----------------------------------------------------------------------
*                     Initialization
*&----------------------------------------------------------------------
INITIALIZATION.

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

  PERFORM frm_get_data.
  PERFORM frm_set_fieldcat.
  PERFORM frm_set_layout.
  PERFORM frm_alv_display.

*&---------------------------------------------------------------------*
*& Form frm_get_data
*&---------------------------------------------------------------------*
*&  获取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .

  IF p_sall = 'X'.
    SELECT
      us~bname,
      us~name_text,
      zt~phone,
      zt~userid,
      zt~created_at,
      zt~changed_at
      FROM user_addrp AS us
      LEFT JOIN ztwx_user_info AS zt ON zt~uname = us~bname
      INTO CORRESPONDING FIELDS OF TABLE @gt_display.
  ELSE.

    SELECT
      us~bname,
      us~name_text,
      zt~phone,
      zt~userid,
      zt~created_at,
      zt~changed_at
      FROM user_addrp AS us
      INNER JOIN ztwx_user_info AS zt ON zt~uname = us~bname
      INTO CORRESPONDING FIELDS OF TABLE @gt_display.
  ENDIF.

  SORT gt_display BY bname.

  IF gt_display IS NOT INITIAL.
    SELECT
      us~bname AS uname,
      cp~tel_number AS phone
      FROM usr21 AS us
      LEFT JOIN adcp AS cp ON cp~addrnumber = us~addrnumber
                          AND cp~persnumber = us~persnumber
      FOR ALL ENTRIES IN @gt_display
      WHERE us~bname = @gt_display-bname
      INTO TABLE @DATA(lt_sap_tel).
    SORT lt_sap_tel BY uname.

    SELECT
      us~bname AS uname,
      cp~smtp_addr AS email
      FROM usr21 AS us
      LEFT JOIN adr6 AS cp ON cp~addrnumber = us~addrnumber
                          AND cp~persnumber = us~persnumber
      FOR ALL ENTRIES IN @gt_display
      WHERE us~bname = @gt_display-bname
      INTO TABLE @DATA(lt_sap_mal).
    SORT lt_sap_mal BY uname.
  ENDIF.

  LOOP AT gt_display REFERENCE INTO DATA(l_display).

    READ TABLE lt_sap_tel INTO DATA(ls_sap_tel) WITH KEY uname = l_display->bname BINARY SEARCH.
    IF sy-subrc = 0.
      l_display->tel_number = ls_sap_tel-phone.
    ENDIF.

    READ TABLE lt_sap_mal INTO DATA(ls_sap_mal) WITH KEY uname = l_display->bname BINARY SEARCH.
    IF sy-subrc = 0.
      l_display->smtp_addr = ls_sap_mal-email.
    ENDIF.

    IF l_display->tel_number <> l_display->phone.
      l_display->icon = icon_led_yellow.
      l_display->message = '电话号码发生更改'(t01).
    ENDIF.

    IF l_display->tel_number IS INITIAL.
      l_display->icon = icon_led_red.
      l_display->message = '请在 su01 中维护手机号信息'(t02).
      CONTINUE.
    ENDIF.

    IF l_display->icon IS INITIAL.
      l_display->icon = icon_led_green.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_alv_display
*&---------------------------------------------------------------------*
*&  ALV 展示
*&---------------------------------------------------------------------*
FORM frm_alv_display .
  DATA: lt_events TYPE slis_t_event WITH HEADER LINE.
  DATA: ls_variant TYPE disvariant.
  DATA: lv_lines TYPE i.

  DATA: lv_message TYPE string.

  lv_lines = lines( gt_display ).
  IF lv_lines <> 0.
    lv_message = '查到 & 条数据'(m03).
    REPLACE '&' IN lv_message WITH |{ lv_lines }|.
    MESSAGE lv_message TYPE 'S'. " 查到 & 条数据
  ENDIF.

  ls_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_save                   = 'X'
      i_default                = 'X'
      is_variant               = ls_variant
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'FRM_PF_STATUS'
      i_callback_user_command  = 'FRM_USER_COMMAND'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat
      it_events                = lt_events[]
      i_grid_settings          = VALUE lvc_s_glay( edt_cll_cb = abap_true )
    TABLES
      t_outtab                 = gt_display
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_fieldcat
*&---------------------------------------------------------------------*
*&  设置 FIELDCAT
*&---------------------------------------------------------------------*
FORM frm_set_fieldcat .
  REFRESH gt_fieldcat.

  PERFORM frm_set_fcat USING 'ICON'       ''               ''           TEXT-001. " 结 果
  PERFORM frm_set_fcat USING 'MESSAGE'    ''               ''           TEXT-002. " 消 息
  PERFORM frm_set_fcat USING 'BNAME'      'USER_ADDRP'     'BNAME'      TEXT-003. " 用户名
  PERFORM frm_set_fcat USING 'NAME_TEXT'  'USER_ADDRP'     'NAME_TEXT'  TEXT-004. " 姓 名
  PERFORM frm_set_fcat USING 'TEL_NUMBER' 'ADCP'           'TEL_NUMBER' TEXT-005. " 维护手机号
  PERFORM frm_set_fcat USING 'PHONE'      'ZTWX_USER_INFO' 'PHONE'      TEXT-006. " 关联手机号
  PERFORM frm_set_fcat USING 'SMTP_ADDR'  'ADR6'           'SMTP_ADDR'  TEXT-007. " 维护邮箱
  "PERFORM frm_set_fcat USING 'EMAIL'      'ZTWX_USER_INFO' 'EMAIL'     TEXT-008. " 关联邮箱
  PERFORM frm_set_fcat USING 'USERID'     'ZTWX_USER_INFO' 'USERID'     TEXT-009. " 企微账户id
  PERFORM frm_set_fcat USING 'CREATED_AT' 'ZTWX_USER_INFO' 'CREATED_AT' TEXT-010. " 首次拉取时间
  PERFORM frm_set_fcat USING 'CHANGED_AT' 'ZTWX_USER_INFO' 'CHANGED_AT' TEXT-011. " 上次拉取时间
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_FCAT
*&---------------------------------------------------------------------*
*&  设置fcat
*&---------------------------------------------------------------------*
FORM frm_set_fcat USING   p_fieldname TYPE lvc_s_fcat-fieldname
                          p_ref_table TYPE lvc_s_fcat-ref_table
                          p_ref_field TYPE lvc_s_fcat-ref_field
                          p_coltext   TYPE lvc_s_fcat-coltext.
  DATA: lv_decimals_o TYPE lvc_s_fcat-decimals_o.


  APPEND VALUE lvc_s_fcat( fieldname = p_fieldname
                           coltext   = p_coltext
                           ref_table = p_ref_table
                           ref_field = p_ref_field
                           scrtext_l = p_coltext
                           scrtext_m = p_coltext
                           scrtext_s = p_coltext ) TO gt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).

  IF p_fieldname = 'ICON'.
    <ls_fieldcat>-icon = 'X'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_SET_LAYOUT
*&---------------------------------------------------------------------*
*&  设置 LAYOUT
*&---------------------------------------------------------------------*
FORM frm_set_layout .

  gs_layout = VALUE #(
                       zebra      = 'X'
                       cwidth_opt = 'X'
                       box_fname  = 'BOX'
                     ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_PF_STATUS
*&---------------------------------------------------------------------*
*       toolbar 设置
*----------------------------------------------------------------------*
FORM frm_pf_status USING p_extab TYPE slis_t_extab.
  DATA: lt_extab TYPE slis_t_extab.
  MOVE-CORRESPONDING p_extab TO lt_extab.

  " PERFORM adapt_excluding_tab(saplslvc_fullscreen) CHANGING lt_extab[].

  " SET PF-STATUS 'STANDARD' EXCLUDING lt_extab OF PROGRAM 'SAPLKKBL'.
  SET PF-STATUS 'STANDARD' EXCLUDING lt_extab.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FRM_USER_COMMAND
*&---------------------------------------------------------------------*
*       点击事件处理
*----------------------------------------------------------------------*
FORM frm_user_command USING p_ucomm LIKE sy-ucomm
                            ps_selfield TYPE slis_selfield.
  DATA: lr_grid TYPE REF TO cl_gui_alv_grid.
  DATA: lt_rows TYPE lvc_t_row,
        ls_row  TYPE lvc_s_row.
  DATA: lv_subrc TYPE i.

  CASE p_ucomm.
    WHEN 'LOAD'.
      PERFORM frm_load_userid.

      p_ucomm = '&NTE'.
    WHEN '&IC1'.
      READ TABLE gt_display INTO DATA(ls_display) INDEX ps_selfield-tabindex.
      IF sy-subrc = 0.
        SET PARAMETER ID 'XUS' FIELD ls_display-bname.
        CALL TRANSACTION 'SU01'.
      ENDIF.
    WHEN '&NTE'.
      PERFORM frm_get_data.
      ps_selfield-refresh = 'X'.
      RETURN.
    WHEN OTHERS.
  ENDCASE.

  "ps_selfield-refresh = 'X'.
  ps_selfield-col_stable = 'X' .
  ps_selfield-row_stable = 'X' .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_load_userid
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
FORM frm_load_userid .

  IF NOT line_exists( gt_display[ box = 'X' ] ).
    MESSAGE e103(zwx01).
  ENDIF.

  DATA(lo_wx) = NEW zcl_wx_approval( ).

  LOOP AT gt_display REFERENCE INTO DATA(l_display) WHERE box = 'X'.

    l_display->userid = lo_wx->userid( l_display->bname ).
  ENDLOOP.

ENDFORM.
