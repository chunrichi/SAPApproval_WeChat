CLASS zcl_wx_log_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA ap_no TYPE ztwx_approval-ap_no .

    METHODS constructor IMPORTING !ap_no TYPE ztwx_approval-ap_no OPTIONAL.
    METHODS log IMPORTING !evnid          TYPE ztwx_log_event-evnid
                          !parms          TYPE data OPTIONAL
                RETURNING VALUE(instance) TYPE REF TO zcl_wx_log_event.
    METHODS commit.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: evnnm TYPE ztwx_log_event-evnnm.
    DATA: elog TYPE ztwx_log_event.
ENDCLASS.



CLASS ZCL_WX_LOG_EVENT IMPLEMENTATION.


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
    me->elog-evnnm = me->evnnm.
    me->elog-evnid = to_upper( evnid ).
    me->elog-parms = parms.

    me->elog-batch = sy-batch.
    me->elog-changer = sy-uname.
    GET TIME STAMP FIELD me->elog-changed_at.

    MODIFY ztwx_log_event CONNECTION r/3*wechat FROM @me->elog.

    instance = me.

  ENDMETHOD.


  METHOD commit.
    CLEAR me->evnnm.
    COMMIT CONNECTION r/3*wechat.
  ENDMETHOD.
ENDCLASS.
