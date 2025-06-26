CLASS zcl_wechat_approval DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_filter,
        key   TYPE string,
        value TYPE string,
      END OF ty_filter .
    TYPES:
      tt_filters    TYPE STANDARD TABLE OF ty_filter WITH DEFAULT KEY .
    TYPES:
      tt_sp_no_list TYPE STANDARD TABLE OF string WITH DEFAULT KEY .
    TYPES: BEGIN OF ty_instance_result,
             errcode TYPE i,
             errmsg  TYPE string,
             sp_no   TYPE string,
           END OF ty_instance_result.

    DATA g_error_message TYPE string .

    METHODS constructor
      IMPORTING
        !corpid     TYPE any
        !corpsecret TYPE any .
    METHODS send
      IMPORTING
                !data         TYPE REF TO zcl_wx_oa_ft
      RETURNING VALUE(result) TYPE ty_instance_result.
    METHODS read
      IMPORTING
        !sp_no  TYPE string
      EXPORTING
        !result TYPE data .
    METHODS list
      IMPORTING
        !starttime        TYPE timestamp
        !endtime          TYPE timestamp
        !new_cursor       TYPE i
        !size             TYPE i DEFAULT 100
        !filters          TYPE tt_filters
      EXPORTING
        !subrc            TYPE sysubrc
        !next_cursor      TYPE i
      RETURNING
        VALUE(sp_no_list) TYPE tt_sp_no_list .
    METHODS userid
      IMPORTING
        !uname        TYPE syuname DEFAULT sy-uname
      RETURNING
        VALUE(userid) TYPE string .
  PROTECTED SECTION.
    DATA http TYPE REF TO zcl_wechat_http.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WECHAT_APPROVAL IMPLEMENTATION.


  METHOD constructor.

    me->http = NEW #( corpid = corpid corpsecret = corpsecret ).

  ENDMETHOD.


  METHOD list.
    DATA: BEGIN OF ls_req,
            starttime  TYPE string,
            endtime    TYPE string,
            new_cursor TYPE string,
            size       TYPE i,
            filters    TYPE tt_filters,
          END OF ls_req,
          BEGIN OF ls_res,
            errcode     TYPE i,
            errmsg      TYPE string,
            sp_no_list  TYPE tt_sp_no_list,
            next_cursor TYPE i,
          END OF ls_res.

    CONVERT TIME STAMP starttime TIME ZONE 'UTC' INTO DATE DATA(l_date) TIME DATA(l_time).
    cl_pco_utility=>convert_abap_timestamp_to_java( EXPORTING iv_date      = l_date
                                                              iv_time      = l_time
                                                              iv_msec      = 0
                                                    IMPORTING ev_timestamp = ls_req-starttime ).

    CONVERT TIME STAMP endtime TIME ZONE 'UTC' INTO DATE l_date TIME l_time.
    cl_pco_utility=>convert_abap_timestamp_to_java( EXPORTING iv_date      = l_date
                                                              iv_time      = l_time
                                                              iv_msec      = 0
                                                    IMPORTING ev_timestamp = ls_req-endtime ).

    ls_req-new_cursor = new_cursor.
    ls_req-size       = size.
    ls_req-filters    = filters.

    me->http->g_pretty_name = /ui2/cl_json=>pretty_mode-low_case.

    DATA(l_ecode) = me->http->post(
      EXPORTING
        url    = `https://qyapi.weixin.qq.com/cgi-bin/oa/getapprovalinfo`
        data   = ls_req
      IMPORTING
        result = ls_res
    ).

    CLEAR me->g_error_message.
    IF l_ecode = 200 AND ls_res-errcode = 0.
      sp_no_list = ls_res-sp_no_list.
      next_cursor = ls_res-next_cursor.
    ELSE.
      subrc = 4.
      IF ls_res-errmsg IS INITIAL.
        me->g_error_message = l_ecode && '->' && me->http->g_error_message.
      ELSE.
        me->g_error_message = ls_res-errmsg.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD read.
    DATA: BEGIN OF ls_req,
            sp_no TYPE string,
          END OF ls_req.

    " 参考 https://developer.work.weixin.qq.com/document/path/91983
    " 此处处理做简化

    me->http->g_pretty_name = /ui2/cl_json=>pretty_mode-low_case.

    ls_req-sp_no = sp_no.

    DATA(l_ecode) = me->http->post(
      EXPORTING
        url    = `https://qyapi.weixin.qq.com/cgi-bin/oa/getapprovaldetail`
        data   = ls_req
      IMPORTING
        result = result
    ).

    IF l_ecode <> 200.
      " http 请求异常
      RAISE EXCEPTION TYPE zcx_wx_error MESSAGE e004(zwechat) WITH me->http->g_error_message.
    ENDIF.

  ENDMETHOD.


  METHOD send.

    me->http->g_pretty_name = /ui2/cl_json=>pretty_mode-low_case.

    DATA(l_ecode) = me->http->post(
      EXPORTING
        url    = `https://qyapi.weixin.qq.com/cgi-bin/oa/applyevent`
        data   = data
      IMPORTING
        result = result
    ).

    IF l_ecode <> 200 AND result-errmsg = 0.
      result-errcode = 4.
      result-errmsg = me->http->g_error_message.
    ENDIF.

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

    " 取新值
    DATA(l_ecode) = me->http->userid(
      EXPORTING
        phone  = |{ ls_sap_info-phone }|
      IMPORTING
        userid = userid
    ).

  ENDMETHOD.
ENDCLASS.
