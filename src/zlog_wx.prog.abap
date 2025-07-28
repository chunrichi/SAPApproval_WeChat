REPORT zlog_wx.
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
TYPES: BEGIN OF ty_display,
         aptyp      TYPE ztwx_approval-aptyp,       " 申请类型
         aptnm      TYPE ztwx_ft_conf-aptnm,
         ap_no      TYPE ztwx_approval-ap_no,       " 申请编号
         sp_no      TYPE ztwx_approval-sp_no,       " 表单编号
         docum      TYPE ztwx_approval-docum,       " 单据号
         stamp      TYPE ztwx_approval-stamp,       " 发起时间
         statu      TYPE ztwx_approval-statu,       " 发起状态
         etamp      TYPE ztwx_approval-etamp,       " 返回时间
         etatu      TYPE ztwx_approval-etatu,       " 返回状态
         apsta      TYPE ztwx_approval-apsta,       " 审批状态
         apamp      TYPE ztwx_approval-apamp,       " 审批时间
         apusr      TYPE ztwx_approval-apusr,       " 发起人
         mnote      TYPE ztwx_approval-mnote,       " 备注
         tp_id      TYPE ztwx_approval-tp_id,       " 模板 id
         tcode      TYPE ztwx_approval-tcode,       " 事务代码
         batch      TYPE ztwx_approval-batch,       " 后台处理处于活动状态
         changer    TYPE ztwx_approval-changer,     " 修改人
         changed_at TYPE ztwx_approval-changed_at,  " 最后修改时间

         log_i      TYPE char4,
         log_o      TYPE char4,
         apstt      TYPE text10,

         style      TYPE lvc_t_styl,
         scolo      TYPE lvc_t_scol,
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
  SELECT-OPTIONS: s_aptyp FOR ztwx_approval-aptyp,
                  s_ap_no FOR ztwx_approval-ap_no,
                  s_sp_no FOR ztwx_approval-docum,
                  s_docum FOR ztwx_approval-docum,
                  s_stamp FOR ztwx_approval-stamp,
                  s_apusr FOR ztwx_approval-apusr,
                  s_apsta FOR ztwx_approval-apsta.
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
*                     Start-Of-Selection
*&----------------------------------------------------------------------
AT SELECTION-SCREEN.

*&----------------------------------------------------------------------
*                     At Selection-Screen
*&----------------------------------------------------------------------
START-OF-SELECTION.

  PERFORM frm_get_data.
  PERFORM frm_fix_data.

  PERFORM frm_set_fieldcat.
  PERFORM frm_set_layout.
  PERFORM frm_alv_display.

*&---------------------------------------------------------------------*
*& Form frm_get_data
*&---------------------------------------------------------------------*
*&  获取数据
*&---------------------------------------------------------------------*
FORM frm_get_data .

  " 取数
  SELECT
    *
    FROM ztwx_approval
    INTO CORRESPONDING FIELDS OF TABLE @gt_display
    WHERE aptyp IN @s_aptyp
      AND ap_no IN @s_ap_no
      AND docum IN @s_sp_no
      AND docum IN @s_docum
      AND stamp IN @s_stamp
      AND apusr IN @s_apusr
      AND apsta IN @s_apsta.

  IF gt_display IS INITIAL.
    " 未找到满足条件的数据
    MESSAGE e003(zwechat).
  ENDIF.

  SELECT
    aptyp,
    aptnm
    FROM ztwx_ft_conf
    INTO TABLE @DATA(lt_aptyp).
  SORT lt_aptyp BY aptyp.


  DATA(l_desc) = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_name( 'ZTWX_APPROVAL-APSTA' ) ).
  DATA(l_fixvals) = l_desc->get_ddic_fixed_values( p_langu = sy-langu ).
  SORT l_fixvals BY low.

  LOOP AT gt_display ASSIGNING FIELD-SYMBOL(<ls_display>).

    READ TABLE lt_aptyp INTO DATA(ls_aptyp) WITH KEY aptyp = <ls_display>-aptyp BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_display>-aptnm = ls_aptyp-aptnm.
    ENDIF.

    IF <ls_display>-statu = 'S'.
      <ls_display>-log_o = icon_led_green.
    ELSE.
      <ls_display>-log_o = icon_led_red.
    ENDIF.

    IF <ls_display>-etatu = 'S'.
      <ls_display>-log_i = icon_led_green.
    ELSEIF <ls_display>-etatu = 'E'.
      <ls_display>-log_i = icon_led_red.
    ENDIF.


    READ TABLE l_fixvals INTO DATA(l_fixval) WITH KEY low = <ls_display>-apsta BINARY SEARCH.
    IF sy-subrc = 0.
      <ls_display>-apstt = l_fixval-ddtext.
    ENDIF.

    IF <ls_display>-apsta <= 01.
      <ls_display>-scolo = VALUE #( ( fname = 'APSTT' color-col = 2 ) ).
    ELSEIF <ls_display>-apsta = 02.
      <ls_display>-scolo = VALUE #( ( fname = 'APSTT' color-col = 5 ) ).
    ELSE.
      <ls_display>-scolo = VALUE #( ( fname = 'APSTT' color-col = 6 ) ).
    ENDIF.
  ENDLOOP.

  SORT gt_display BY stamp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_fix_data
*&---------------------------------------------------------------------*
*&  补充数据
*&---------------------------------------------------------------------*
FORM frm_fix_data .
  " 补充数据内容

  CHECK gt_display IS NOT INITIAL.

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

  PERFORM frm_set_fcat USING 'AP_NO'      'ZTWX_APPROVAL' 'AP_NO'       TEXT-001. " 申请编号
  PERFORM frm_set_fcat USING 'APTNM'      'ZTWX_FT_CONF'  'APTNM'       TEXT-002. " 申请类型
  PERFORM frm_set_fcat USING 'SP_NO'      'ZTWX_APPROVAL' 'SP_NO'       TEXT-003. " 表单编号
  PERFORM frm_set_fcat USING 'DOCUM'      'ZTWX_APPROVAL' 'DOCUM'       TEXT-004. " 单据号
  PERFORM frm_set_fcat USING 'STAMP'      'ZTWX_APPROVAL' 'STAMP'       TEXT-005. " 发起时间
  PERFORM frm_set_fcat USING 'LOG_O'      ''               ''           TEXT-006. " 发起状态
  PERFORM frm_set_fcat USING 'ETAMP'      'ZTWX_APPROVAL' 'ETAMP'       TEXT-007. " 返回时间
  PERFORM frm_set_fcat USING 'LOG_I'      ''               ''           TEXT-008. " 返回状态
* PERFORM frm_set_fcat USING 'APSTA'      'ZTWX_APPROVAL' 'APSTA'       TEXT-009. " 审批状态
  PERFORM frm_set_fcat USING 'APSTT'      ''               ''           TEXT-009. " 审批状态
  PERFORM frm_set_fcat USING 'APAMP'      'ZTWX_APPROVAL' 'APAMP'       TEXT-010. " 审批时间
  PERFORM frm_set_fcat USING 'APUSR'      'ZTWX_APPROVAL' 'APUSR'       TEXT-011. " 发起人
  PERFORM frm_set_fcat USING 'MNOTE'      'ZTWX_APPROVAL' 'MNOTE'       TEXT-012. " 备注
* PERFORM frm_set_fcat USING 'TP_ID'      'ZTWX_APPROVAL' 'TP_ID'       TEXT-013. " 模板 id
  PERFORM frm_set_fcat USING 'TCODE'      'ZTWX_APPROVAL' 'TCODE'       TEXT-014. " 事务代码
  PERFORM frm_set_fcat USING 'BATCH'      'ZTWX_APPROVAL' 'BATCH'       TEXT-015. " 后台处理
* PERFORM frm_set_fcat USING 'CHANGER'    'ZTWX_APPROVAL' 'CHANGER'     TEXT-016. " 修改人
  PERFORM frm_set_fcat USING 'CHANGED_AT' 'ZTWX_APPROVAL' 'CHANGED_AT'  TEXT-017. " 最后修改时间

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
  IF p_fieldname = 'AP_NO'.
    <ls_fieldcat>-key = 'X'.
  ENDIF.

  IF p_fieldname = 'BATCH'.
    <ls_fieldcat>-checkbox = 'X'.
  ENDIF.

  IF p_fieldname = 'LOG_I' OR p_fieldname = 'LOG_O'.
    <ls_fieldcat>-icon = 'X'.
  ENDIF.

  IF p_fieldname = 'APSTT'.
    <ls_fieldcat>-just = 'C'.
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
                       stylefname = 'STYLE'
                       ctab_fname = 'SCOLO'
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

  " SET PF-STATUS 'STANDARD' EXCLUDING lt_extab OF PROGRAM 'SAPLKKBL'.
  SET PF-STATUS 'STANDARD'.

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

    WHEN '&NTE'.
      " 刷新
      REFRESH gt_display.
      PERFORM frm_get_data.
      PERFORM frm_fix_data.

      ps_selfield-refresh = 'X'.
    WHEN 'EVENLOG'.

      PERFORM frm_display_events.
    WHEN 'QUERY'.

      PERFORM frm_load_new_state.
      p_ucomm = '&NTE'.
    WHEN '&IC1'.
      READ TABLE gt_display INTO DATA(ls_display) INDEX ps_selfield-tabindex.
      IF sy-subrc = 0.


      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  " ps_selfield-refresh = 'X'.
  ps_selfield-col_stable = 'X' .
  ps_selfield-row_stable = 'X' .
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_display_events
*&---------------------------------------------------------------------*
*& 展示操作详情
*&---------------------------------------------------------------------*
FORM frm_display_events .
  DATA: lv_lines TYPE i,
        lt_event TYPE TABLE OF zswx_log_event.

  LOOP AT gt_display TRANSPORTING NO FIELDS WHERE box = 'X'.
    ADD 1 TO lv_lines.
    CHECK lv_lines < 2.
  ENDLOOP.
  IF sy-subrc <> 0 OR lv_lines > 1.
    " 请选择一行数据
    MESSAGE e101(zwx01).
  ENDIF.

  DATA(ls_display) = gt_display[ box = 'X' ].

  SELECT
    evnnm,
    evnid,
    parms,
    batch,
    changer AS evnum,
    changed_at AS evntm
    FROM ztwx_log_event
    WHERE ap_no = @ls_display-ap_no
    INTO TABLE @DATA(lt_events).
  IF sy-subrc <> 0.
    " 暂无操作
    MESSAGE e102(zwx01).
  ENDIF.

  LOOP AT lt_events REFERENCE INTO DATA(l_event).
    APPEND CORRESPONDING #( l_event->* ) TO lt_event ASSIGNING FIELD-SYMBOL(<ls_event>).

    IF l_event->evnid(1) = 'S'.
      <ls_event>-icons = icon_led_green.
    ELSEIF l_event->evnid(1) = 'E'.
      <ls_event>-icons = icon_led_red.
    ENDIF.

    MESSAGE ID 'ZWX01' TYPE l_event->evnid(1) NUMBER l_event->evnid+1 WITH l_event->parms INTO <ls_event>-message.
  ENDLOOP.


  TRY.
      cl_salv_table=>factory(
        EXPORTING
          list_display = ''
        IMPORTING
          r_salv_table = DATA(l_popup_alv)
        CHANGING
          t_table      = lt_event[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.


  l_popup_alv->get_columns( )->set_optimize( abap_true ).

  l_popup_alv->set_screen_popup(
    start_line   = 10
    start_column = 30
    end_line     = 20
    end_column   = 130 ).

  DATA: lo_column TYPE REF TO cl_salv_column_list.
  TRY.
      lo_column ?= l_popup_alv->get_columns( )->get_column( 'BATCH' ).
      lo_column->set_cell_type( if_salv_c_cell_type=>checkbox ).
    CATCH cx_root.
  ENDTRY.
  TRY.
      lo_column ?= l_popup_alv->get_columns( )->get_column( 'ICONS' ).
      lo_column->set_icon( 'X' ).
    CATCH cx_root.
  ENDTRY.
  TRY.
      l_popup_alv->get_columns( )->get_column( 'EVNNM' )->set_alignment( if_salv_c_alignment=>centered ).
    CATCH cx_root.
  ENDTRY.

  l_popup_alv->display( ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_load_new_state
*&---------------------------------------------------------------------*
*& 刷新状态
*&---------------------------------------------------------------------*
FORM frm_load_new_state .

  IF NOT line_exists( gt_display[ box = 'X' ] ).
    " 请至少选择一行数据
    MESSAGE e103(zwx01).
  ENDIF.

  DATA: lt_rspar_tab TYPE TABLE OF rsparams.

  lt_rspar_tab = VALUE #( FOR _ IN gt_display WHERE    ( box  = 'X' )
                          selname = 'S_SP_NO'
                          kind    = 'S'                ( sign = 'I' option = 'EQ' low = _-sp_no ) ).
  APPEND VALUE #( selname = 'APSTA'
                  kind    = 'S' sign = 'E' option = 'EQ' ) TO lt_rspar_tab.

  SUBMIT zjob_wx_approval_result
    WITH SELECTION-TABLE lt_rspar_tab
    AND RETURN.

ENDFORM.