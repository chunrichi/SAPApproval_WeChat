CLASS zcl_wx_log_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA ap_no TYPE ztwx_approval-ap_no .

    METHODS constructor IMPORTING !ap_no TYPE ztwx_approval-ap_no OPTIONAL.
    METHODS log IMPORTING !evnid          TYPE ztwx_log_event-evnid
                          !parms          TYPE data OPTIONAL
                          !local          TYPE abap_bool DEFAULT abap_false
                RETURNING VALUE(instance) TYPE REF TO zcl_wx_log_event.
    METHODS commit.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: evnnm TYPE ztwx_log_event-evnnm.
    DATA: elog TYPE ztwx_log_event.
ENDCLASS.



CLASS zcl_wx_log_event IMPLEMENTATION.


  METHOD constructor.

    me->ap_no = ap_no.
  ENDMETHOD.


  METHOD log.

    SELECT SINGLE
      MAX( evnnm )
      INTO @DATA(lv_evnnm)
      FROM ztwx_log_event
      WHERE ap_no = @ap_no.
    IF ap_no IS INITIAL.
      lv_evnnm = 0.
    ENDIF.

    IF me->evnnm <= lv_evnnm.
      me->evnnm = lv_evnnm.
    ENDIF.
    me->evnnm = me->evnnm + 1.

    me->elog-ap_no = me->ap_no.
    me->elog-evnnm = me->evnnm MOD 1000.
    me->elog-evnid = to_upper( evnid ).
    me->elog-parms = parms.

    me->elog-batch = sy-batch.
    me->elog-changer = sy-uname.
    GET TIME STAMP FIELD me->elog-changed_at.

    " 由于更新的主键不同 不会由于 session 的不同 导致 hana 更新的时候互锁卡死
    IF local = abap_true.
      MODIFY ztwx_log_event FROM @me->elog.
    ELSE.
      MODIFY ztwx_log_event CONNECTION r/3*wechat FROM @me->elog.
    ENDIF.

    instance = me.

  ENDMETHOD.


  METHOD commit.
    CLEAR me->evnnm.
    COMMIT CONNECTION r/3*wechat.
  ENDMETHOD.
ENDCLASS.
