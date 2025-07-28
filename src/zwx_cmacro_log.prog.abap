 DATA(/zwx/ls_method) = me->approval->log_data->method_info( me ).
 LOOP AT /zwx/ls_method-parameters REFERENCE INTO DATA(/zwx/l_method_param)
     WHERE parm_kind = 'I'.
   ASSIGN (/zwx/l_method_param->name) TO FIELD-SYMBOL(<zwx_lv_field>).
   IF sy-subrc = 0.
     me->approval->log_data->field( name = /zwx/l_method_param->name
                                    data = REF #( <zwx_lv_field> ) ).
   ENDIF.
 ENDLOOP.
