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

    " 申请人userid
    me->ft->creator_userid = me->approval->userid( uname ).

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


  METHOD zif_wx_oa_ft~resend.
    DATA: BEGIN OF ls_key,
            ap_no TYPE ztwx_log_data-ap_no,
          END OF ls_key.
    DATA: BEGIN OF ls_map_data,
            name TYPE string,
            data TYPE /ui2/cl_json=>json,
          END OF ls_map_data,
          lt_map_data LIKE SORTED TABLE OF ls_map_data WITH UNIQUE KEY name.
    DATA: lv_json TYPE string.
    DATA: lt_ptab TYPE abap_parmbind_tab,
          lt_etab TYPE abap_excpbind_tab.
    DATA: ls_param_def TYPE vseoparam.

    " 拉取数据
    ls_key-ap_no = ap_no.

    IMPORT json = lv_json FROM DATABASE ztwx_log_data(zz) ID ls_key.

    " 重构参数
    /ui2/cl_json=>deserialize( EXPORTING json             = lv_json
                                         assoc_arrays     = 'X'
                                         assoc_arrays_opt = 'X'
                               CHANGING  data             = lt_map_data ).

    DATA(lo_class) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( me ) ).
    DATA(lv_class_name) = substring_after( val = cl_abap_classdescr=>get_class_name( me ) sub = `\CLASS=` ).

    DATA(ls_method) = lo_class->methods[ name = 'MAP' ].
    SORT ls_method-parameters BY name.

    LOOP AT ls_method-parameters REFERENCE INTO DATA(l_method_param)
        WHERE parm_kind = 'I'.

      READ TABLE lt_map_data INTO DATA(l_map) WITH KEY name = l_method_param->name.
      IF sy-subrc = 0.

        CALL FUNCTION 'SEO_PARAMETER_GET'
          EXPORTING
            parkey       = VALUE seoscokey( clsname = lv_class_name
                                            cmpname = 'MAP'
                                            sconame = l_method_param->name )
          IMPORTING
            parameter    = ls_param_def
          EXCEPTIONS
            not_existing = 1
            deleted      = 2
            is_exception = 3
            OTHERS       = 4.

        INSERT VALUE #( name = l_method_param->name
                        kind = cl_abap_objectdescr=>exporting ) INTO TABLE lt_ptab ASSIGNING FIELD-SYMBOL(<ls_ptab>).

        CALL FUNCTION 'SEO_TYPE_GET'
          EXPORTING
            typkey       = VALUE seocmpkey( clsname = lv_class_name
                                            cmpname = ls_param_def-type )
          EXCEPTIONS
            not_existing = 1
            deleted      = 2
            is_attribute = 3
            is_method    = 4
            is_event     = 5.

        IF sy-subrc = 1.
          CREATE DATA <ls_ptab>-value TYPE (ls_param_def-type).
        ELSE.
          DATA(l_type) = |{ lv_class_name }=>{ ls_param_def-type }|.
          CREATE DATA <ls_ptab>-value TYPE (l_type).
        ENDIF.

        /ui2/cl_json=>deserialize( EXPORTING json = l_map-data
                                   CHANGING  data = <ls_ptab>-value ).
      ENDIF.
    ENDLOOP.

    " lt_etab = VALUE abap_excpbind_tab( ( name = 'E' value = 4 ) ).

    " 字段映射
    CALL METHOD ('MAP')
      PARAMETER-TABLE lt_ptab
      EXCEPTION-TABLE lt_etab.

    " 推送
    me->approval->is_resend( ap_no ).

    sp_no = me->send( EXPORTING uname    = uname
                      IMPORTING errmsg   = errmsg
                                approval = approval ).

  ENDMETHOD.
ENDCLASS.
