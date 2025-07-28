CLASS zcl_wx_log_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA ap_no TYPE ztwx_approval-ap_no .

    METHODS constructor IMPORTING !ap_no TYPE ztwx_approval-ap_no OPTIONAL.

    METHODS log RETURNING VALUE(instance) TYPE REF TO zcl_wx_log_data.

    METHODS field IMPORTING name TYPE fieldname
                            data TYPE REF TO data.
    METHODS method_info IMPORTING object_ref TYPE REF TO object RETURNING VALUE(method) TYPE abap_methdescr.
  PROTECTED SECTION.
    TYPES: BEGIN OF ty_map_data,
             name TYPE string,
             data TYPE /ui2/cl_json=>json, "REF TO data,
           END OF ty_map_data,
           tt_map_data TYPE SORTED TABLE OF ty_map_data WITH UNIQUE KEY name.
  PRIVATE SECTION.
    DATA: map_data TYPE tt_map_data.

ENDCLASS.



CLASS ZCL_WX_LOG_DATA IMPLEMENTATION.


  METHOD constructor.

    me->ap_no = ap_no.
  ENDMETHOD.


  METHOD log.
    DATA: ls_log_data TYPE ztwx_log_data.
    DATA: lv_json TYPE string.

    instance = me.

    CHECK me->map_data IS NOT INITIAL.

    lv_json = /ui2/cl_json=>serialize( data         = me->map_data
                                       pretty_name  = 'L'
                                       assoc_arrays = 'X' ).

    ls_log_data-relid = 'ZZ'.
    ls_log_data-ap_no = me->ap_no.

    EXPORT json = lv_json TO DATA BUFFER ls_log_data-clustd.

    MODIFY ztwx_log_data CONNECTION r/3*wechat FROM @ls_log_data.

    FREE me->map_data.
  ENDMETHOD.


  METHOD field.
    APPEND VALUE #( name = name
                    data = /ui2/cl_json=>serialize( data        = data
                                                    pretty_name = 'L' ) ) TO me->map_data.
  ENDMETHOD.


  METHOD method_info.
    DATA: lt_sys_callst TYPE sys_callst.

    CALL FUNCTION 'SYSTEM_CALLSTACK'
      EXPORTING
        max_level    = 2
      IMPORTING
        et_callstack = lt_sys_callst.


    DATA(lo_class) = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref( object_ref ) ).

    method = lo_class->methods[ name = lt_sys_callst[ 2 ]-eventname ].
    SORT method-parameters BY name.

  ENDMETHOD.
ENDCLASS.
