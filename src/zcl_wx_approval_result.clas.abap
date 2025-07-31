CLASS zcl_wx_approval_result DEFINITION
  PUBLIC
  INHERITING FROM zcl_wechat_approval
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      BEGIN OF result,
        errcode TYPE string,
        errmsg  TYPE string,
        info    TYPE REF TO zcl_wx_oa_ap_info,
      END OF result .

    METHODS constructor .
    METHODS load_approval_info
      IMPORTING
        !sp_no TYPE ztwx_approval-sp_no .
    METHODS cust_approval_result.
    METHODS unix2timestamp
      IMPORTING
        !unix       TYPE int4
      EXPORTING
        !timestamp  TYPE timestamp
        !timestampl TYPE timestampl .
  PROTECTED SECTION.

    DATA hook_cache TYPE REF TO zcl_wx_cache .

    METHODS last_approval_time
      RETURNING
        VALUE(unix) TYPE int4 .
  PRIVATE SECTION.
    DATA: approvals TYPE TABLE OF ztwx_approval.

    METHODS upd_approval
      IMPORTING
        !sp_no TYPE ztwx_approval-sp_no .
ENDCLASS.



CLASS ZCL_WX_APPROVAL_RESULT IMPLEMENTATION.


  METHOD constructor.

    IF zcl_wx_config=>config IS INITIAL.
      " 002 未维护企业微信应用信息
      RAISE EXCEPTION TYPE zcx_wx_error MESSAGE e002(zwechat).
    ENDIF.

    super->constructor(
      corpid     = zcl_wx_config=>config-corp_id
      corpsecret = zcl_wx_config=>config-corp_secret
    ).

    me->hook_cache = NEW #( ).

    SET HANDLER me->hook_cache->hook_cache_token_get FOR me->http.
    SET HANDLER me->hook_cache->hook_cache_token_set FOR me->http.
  ENDMETHOD.


  METHOD cust_approval_result.
    DATA: lr_if TYPE REF TO zif_wx_ap_result.

    CHECK me->result-info IS BOUND.
    CHECK me->approvals IS NOT INITIAL.

    SELECT
      clsnm
      FROM ztwx_ft_conf
      FOR ALL ENTRIES IN @me->approvals
      WHERE aptyp = @me->approvals-aptyp
        AND clsnm IS NOT INITIAL
      INTO TABLE @DATA(lt_handle).

    CHECK lt_handle IS NOT INITIAL.

    LOOP AT lt_handle INTO DATA(ls_handle).

      TRY.
          CREATE OBJECT lr_if TYPE (ls_handle-clsnm).
        CATCH cx_sy_create_object_error INTO DATA(l_cx).
          " IGNORE
          CONTINUE.
      ENDTRY.

      TRY.

          lr_if->process_result( sp_no = me->approvals[ 1 ]-sp_no
                                 apsta = me->approvals[ 1 ]-apsta ).

        CATCH cx_root.
      ENDTRY.

    ENDLOOP.

  ENDMETHOD.


  METHOD load_approval_info.
    CLEAR me->result.
    CLEAR me->approvals.

    TRY.
        me->read( EXPORTING sp_no  = |{ sp_no }|
                  IMPORTING result = me->result ).
      CATCH cx_root INTO DATA(l_cx).
        me->result-errcode = 4.
        me->result-errmsg = me->http->g_error_message.
    ENDTRY.

    IF me->result-info IS BOUND.
      me->result-info->sp_no = sp_no.
    ENDIF.

    upd_approval( sp_no ).

  ENDMETHOD.


  METHOD unix2timestamp.

    DATA: l_unix TYPE string.

    CHECK unix IS NOT INITIAL.

    l_unix = unix && '000'.

    cl_pco_utility=>convert_java_timestamp_to_abap(
      EXPORTING
        iv_timestamp = l_unix
      IMPORTING
        ev_date      = DATA(l_date)
        ev_time      = DATA(l_time)
    ).

    timestampl = timestamp = l_date && l_time.

  ENDMETHOD.


  METHOD upd_approval.
    TYPES: ty_approval_x TYPE ztwx_approval WITH INDICATORS upd TYPE abap_bool.
    DATA: lt_approval TYPE TABLE OF ty_approval_x,
          ls_approval TYPE ty_approval_x.
    DATA: l_set TYPE string.

    SELECT
      *
      FROM ztwx_approval
      WHERE sp_no = @sp_no
      INTO TABLE @me->approvals.

    CHECK me->approvals IS NOT INITIAL.

    LOOP AT me->approvals ASSIGNING FIELD-SYMBOL(<l_approval>).
      MOVE-CORRESPONDING <l_approval> TO ls_approval.

      GET TIME STAMP FIELD ls_approval-etamp.
      ls_approval-etatu = COND #( WHEN me->result-errcode = 0 THEN 'S' ELSE 'E' ).
      ls_approval-upd-etamp = abap_true.
      ls_approval-upd-etatu = abap_true.

      IF ls_approval-apamp IS INITIAL.
        unix2timestamp( EXPORTING unix       = last_approval_time( )
                        IMPORTING timestampl = ls_approval-apamp ).
        ls_approval-upd-apamp = abap_true.
      ENDIF.

      IF me->result-info IS BOUND.
        ls_approval-apsta = me->result-info->sp_status.
        ls_approval-upd-apsta = abap_true.
      ENDIF.

      ls_approval-changer = sy-uname.
      GET TIME STAMP FIELD ls_approval-changed_at.
      ls_approval-upd-changer = abap_true.
      ls_approval-upd-changed_at = abap_true.

      MOVE-CORRESPONDING ls_approval TO <l_approval>.

      APPEND ls_approval TO lt_approval.
      CLEAR ls_approval.
    ENDLOOP.

    UPDATE ztwx_approval FROM TABLE @lt_approval INDICATORS SET STRUCTURE upd.

  ENDMETHOD.


  METHOD last_approval_time.

    CHECK me->result IS NOT INITIAL.

    CHECK me->result-info->sp_status = 2
       OR me->result-info->sp_status = 3.

    LOOP AT me->result-info->process_list-node_list REFERENCE INTO DATA(l_node)
      WHERE node_type = 1
        AND ( sp_status = 2 OR sp_status = 3 ).

      LOOP AT l_node->sub_node_list REFERENCE INTO DATA(l_sub)
        WHERE ( sp_yj = 2 OR sp_yj = 3 ).

        IF unix < l_sub->sptime.
          unix = l_sub->sptime.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
