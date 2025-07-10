CLASS zcl_wx_oa_fc_date DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_fc .

    TYPES:
      BEGIN OF ENUM t_type,
        day, hour,
      END OF ENUM t_type .

    DATA control TYPE string VALUE `Date` ##NO_TEXT.
    DATA id TYPE string .
    DATA:
      BEGIN OF value,
        BEGIN OF date,
          type        TYPE string,
          s_timestamp TYPE string,
        END OF date,
      END OF value .

    METHODS constructor
      IMPORTING
        !id   TYPE string
        !type TYPE t_type DEFAULT day.

    METHODS set
      IMPORTING
                !data           TYPE data
      RETURNING VALUE(instance) TYPE REF TO zif_wx_oa_fc.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FC_DATE IMPLEMENTATION.


  METHOD constructor.

    me->id = id.

    CASE type.
      WHEN day.
        me->value-date-type = 'day'.
      WHEN hour.
        me->value-date-type = 'hour'.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD set.
    DATA: lv_timestamp  TYPE timestamp,
          lv_timestamps TYPE string,
          lv_datum      TYPE datum,
          lv_uzeit      TYPE uzeit,
          lv_unix       TYPE string.

    DESCRIBE FIELD data TYPE DATA(l_type).

    IF l_type = 'D'.
      cl_pco_utility=>convert_abap_timestamp_to_java(
        EXPORTING
          iv_date      = data
          iv_time      = '000000'
        IMPORTING
          ev_timestamp = lv_unix
      ).
    ELSEIF l_type = 'T'.
      cl_pco_utility=>convert_abap_timestamp_to_java(
        EXPORTING
          iv_date      = sy-datum
          iv_time      = data
        IMPORTING
          ev_timestamp = lv_unix
      ).
    ELSEIF l_type = 'P'.
      lv_timestamps = |{ data }|.

      IF strlen( lv_timestamps ) = 14.
        lv_datum = lv_timestamps(8).
        lv_uzeit = lv_timestamps+8.

        " 日期 + 时间
        cl_pco_utility=>convert_abap_timestamp_to_java(
          EXPORTING
            iv_date      = lv_datum
            iv_time      = lv_uzeit
          IMPORTING
            ev_timestamp = lv_unix
        ).
      ENDIF.
    ENDIF.

    DATA(len) = strlen( lv_unix ) - 3.

    IF len >= 0.
      me->value-date-s_timestamp = lv_unix(len).
    ENDIF.

    instance = me.
  ENDMETHOD.
ENDCLASS.
