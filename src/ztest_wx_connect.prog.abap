REPORT ztest_wx_connect.


CLASS lcl_http DEFINITION INHERITING FROM zcl_wechat_http.

  PUBLIC SECTION.
    METHODS check RETURNING VALUE(ok) TYPE abap_bool.
ENDCLASS.

CLASS lcl_http IMPLEMENTATION.

  METHOD check.

    me->access_token( ).

    IF me->token_cache-token IS INITIAL.
      ok = abap_false.
    ELSE.
      ok = abap_true.
    ENDIF.

  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

  DATA(lo_http) = NEW lcl_http(
    corpid     = zcl_wx_config=>config-corp_id
    corpsecret = zcl_wx_config=>config-corp_secret
  ).

  IF lo_http->check( ) = abap_true.
    MESSAGE 'load access token sucess' TYPE 'I' DISPLAY LIKE 'S'.
  ELSE.
    MESSAGE 'load access token failed' TYPE 'I' DISPLAY LIKE 'E'.
  ENDIF.
