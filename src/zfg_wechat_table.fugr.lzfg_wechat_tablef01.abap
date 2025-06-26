*----------------------------------------------------------------------*
***INCLUDE LZFG_WECHAT_TABLEF01.
*----------------------------------------------------------------------*


FORM frm_change_at_stamp.
  ASSIGN (vim_view_name) TO FIELD-SYMBOL(<ls_table>).
  IF sy-subrc = 0.
    ASSIGN COMPONENT 'CHANGER' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<l_changer>).
    IF sy-subrc = 0.
      <l_changer> = sy-uname.
    ENDIF.

    ASSIGN COMPONENT 'CHANGED_AT' OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<l_changat>).
    IF sy-subrc = 0.
      GET TIME STAMP FIELD <l_changat>.
    ENDIF.
  ENDIF.
ENDFORM.
