CLASS zcl_wx_approval DEFINITION
  PUBLIC
  INHERITING FROM zcl_wechat_approval
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA approval TYPE ztwx_approval .
    DATA log_data TYPE REF TO zcl_wx_log_data.

    METHODS constructor .
    METHODS set_aptyp
      IMPORTING
        !aptyp TYPE ztwx_approval-aptyp .
    METHODS set_apusr
      IMPORTING
        !apusr TYPE ztwx_approval-apusr .
    METHODS set_docum
      IMPORTING
        !docum TYPE data .
    METHODS set_mnote
      IMPORTING
        !mnote TYPE ztwx_approval-mnote .

    METHODS send
        REDEFINITION .
    METHODS userid
        REDEFINITION .

  PROTECTED SECTION.
    DATA: hook_cache TYPE REF TO zcl_wx_cache.
    DATA: log_event TYPE REF TO zcl_wx_log_event.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_WX_APPROVAL IMPLEMENTATION.


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

    me->log_event = NEW #( ).
    me->log_data = NEW #( ).
  ENDMETHOD.


  METHOD userid.

    " 当前用户电话号码取值
    SELECT SINGLE
      us~bname AS uname,
      cp~tel_number AS phone
      FROM usr21 AS us
      LEFT JOIN adcp AS cp ON cp~addrnumber = us~addrnumber
                          AND cp~persnumber = us~persnumber
      WHERE us~bname = @uname
      INTO @DATA(ls_sap_info).

    " 电话号码和缓存校验 => 一致则直接返回 userid
    SELECT SINGLE
      phone,
      userid
      FROM ztwx_user_info
      WHERE uname = @uname
      INTO @DATA(ls_info_cache).

    IF ls_sap_info-phone = ls_info_cache-phone
      AND ls_sap_info-phone IS NOT INITIAL
      AND ls_info_cache-userid IS NOT INITIAL.
      userid = ls_info_cache-userid.
      RETURN.
    ENDIF.

    IF ls_sap_info-phone IS INITIAL.
      " 未找到 & 的手机号
      RAISE EXCEPTION TYPE zcx_wx_error MESSAGE e001(zwechat) WITH uname.
      " 报错在 send 中处理并记录，不再中断
    ENDIF.

    " 取新值
    DATA(l_ecode) = me->http->userid(
      EXPORTING
        phone  = |{ ls_sap_info-phone }|
      IMPORTING
        userid = userid
    ).

    DATA: ls_user_info TYPE ztwx_user_info.
    IF l_ecode = 200.
      ls_user_info-uname = uname.
      ls_user_info-phone = ls_user_info-phone.
      ls_user_info-userid = userid.

      IF ls_info_cache-userid IS NOT INITIAL.
        GET TIME STAMP FIELD ls_user_info-created_on.
      ENDIF.
      GET TIME STAMP FIELD ls_user_info-changed_on.

      MODIFY ztwx_user_info FROM ls_user_info.
    ENDIF.


  ENDMETHOD.


  METHOD send.

    me->approval-ap_no = lcl_snro=>next( ).
    me->log_event->ap_no = me->approval-ap_no.

    me->log_data->ap_no = me->approval-ap_no.
    me->log_data->log( ).

    GET TIME STAMP FIELD me->approval-stamp.

    me->approval-apsta = 'I'.
    me->approval-apusr = sy-uname.

    me->approval-tp_id = data->template_id.
    me->approval-tcode = sy-tcode.
    me->approval-batch = sy-batch.

    IF data->creator_userid IS INITIAL.
      " 未获取到发起人（未维护手机号）
      result-errcode = 4.

      IF 1 = 2. MESSAGE e001(zwx01) WITH sy-uname. ENDIF.
      me->log_event->log( evnid = 'e001' parms = sy-uname ).
    ELSE.
      result = super->send( data ).
    ENDIF.

    IF result-errcode = 0.
      me->approval-sp_no = result-sp_no.
      me->approval-statu = 'S'.

      IF 1 = 2. MESSAGE s002(zwx01). ENDIF.
      me->log_event->log( evnid = 's002' ).
    ELSE.
      me->approval-statu = 'E'.

      IF 1 = 2. MESSAGE e003(zwx01) WITH result-errmsg. ENDIF.
      me->log_event->log( evnid = 'e003' parms = result-errmsg ).
    ENDIF.

    MODIFY ztwx_approval CONNECTION r/3*wechat FROM me->approval.

    COMMIT CONNECTION r/3*wechat.
  ENDMETHOD.


  METHOD set_docum.

    me->approval-docum = docum.
  ENDMETHOD.


  METHOD set_apusr.

    me->approval-apusr = apusr.
  ENDMETHOD.


  METHOD set_mnote.

    me->approval-mnote = mnote.
  ENDMETHOD.


  METHOD set_aptyp.

    me->approval-aptyp = aptyp.
  ENDMETHOD.
ENDCLASS.
