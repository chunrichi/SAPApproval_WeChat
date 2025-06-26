CLASS zcl_wx_cache DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS hook_cache_token_get
      FOR EVENT hook_cache_token_get OF zcl_wechat_http
      IMPORTING
        !sender .
    METHODS hook_cache_token_set
      FOR EVENT hook_cache_token_set OF zcl_wechat_http
      IMPORTING
        !sender .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS lc_token_cname TYPE ztwx_cache-cname VALUE 'TOKEN' ##NO_TEXT.

ENDCLASS.



CLASS ZCL_WX_CACHE IMPLEMENTATION.


  METHOD hook_cache_token_get.
    DATA: lv_timestamp TYPE timestamp.

    SELECT SINGLE cache FROM ztwx_cache WHERE cname = @lc_token_cname
      INTO @DATA(l_cache).
    CHECK sy-subrc = 0.

    /ui2/cl_json=>deserialize( EXPORTING json        = l_cache
                                         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                               CHANGING  data        = sender->token_cache ).

    GET TIME STAMP FIELD lv_timestamp.
    lv_timestamp = cl_abap_tstmp=>add_to_short( tstmp = lv_timestamp secs = 10 ).

    " 内部取值
    IF sender->token_cache IS NOT INITIAL
      AND sender->token_cache-timestamp > lv_timestamp.
      RETURN.
    ELSE.
      CLEAR: sender->token_cache.
    ENDIF.
  ENDMETHOD.


  METHOD hook_cache_token_set.

    DATA(ls_cache) = VALUE ztwx_cache( cname = lc_token_cname
                                       cache = /ui2/cl_json=>serialize(
                                       data        = sender->token_cache
                                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case ) ).

    MODIFY ztwx_cache FROM ls_cache.

  ENDMETHOD.
ENDCLASS.
