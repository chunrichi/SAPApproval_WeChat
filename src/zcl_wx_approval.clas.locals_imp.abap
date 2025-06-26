*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

CLASS lcl_snro DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS next RETURNING VALUE(snro) TYPE zewxap_no.

  PROTECTED SECTION.
    CLASS-METHODS init.

ENDCLASS.


CLASS lcl_snro IMPLEMENTATION.

  METHOD init.
    DATA:
      ls_nrint         TYPE inriv,
      ls_error         TYPE inrer,
      lv_error_occured TYPE c,
      lt_nrint_error   TYPE STANDARD TABLE OF inriv,
      lt_nrinterval    TYPE STANDARD TABLE OF inriv.

    " Check whether the number range interval already exists.
    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_LIST'
      EXPORTING
        nr_range_nr1               = '01'
        object                     = 'ZWX_AP_NO'
      TABLES
        interval                   = lt_nrinterval
      EXCEPTIONS
        nr_range_nr1_not_found     = 1
        nr_range_nr1_not_intern    = 2
        nr_range_nr2_must_be_space = 3
        nr_range_nr2_not_extern    = 4
        nr_range_nr2_not_found     = 5
        object_not_found           = 6
        subobject_must_be_space    = 7
        subobject_not_found        = 8
        error_message              = 9
        OTHERS                     = 10.
    CASE sy-subrc.
      WHEN 0.
*     number range interval is already defined ==> nothing to do
        IF NOT lt_nrinterval[] IS INITIAL.
          EXIT.
        ENDIF.
      WHEN 1.
      WHEN OTHERS.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.

    " Enqueue Number range object
    CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
      EXPORTING
        object           = 'ZWX_AP_NO'
      EXCEPTIONS
        foreign_lock     = 1
        object_not_found = 2
        system_failure   = 3
        error_message    = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    lt_nrinterval = VALUE #( ( nrrangenr = '01' fromnumber = '00000001'
                               tonumber  = '99999999'
                               procind   = 'I' ) ).

    CALL FUNCTION 'NUMBER_RANGE_INTERVAL_UPDATE'
      EXPORTING
        object           = 'ZWX_AP_NO'
      IMPORTING
        error            = ls_error
        error_occured    = lv_error_occured
*       WARNING_OCCURED  =
      TABLES
        error_iv         = lt_nrint_error
        interval         = lt_nrinterval
      EXCEPTIONS
        object_not_found = 1
        error_message    = 2
        OTHERS           = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    IF NOT lv_error_occured IS INITIAL.
      " PASS
    ENDIF.

    CALL FUNCTION 'NUMBER_RANGE_UPDATE_CLOSE'
      EXPORTING
        object                 = 'ZWX_AP_NO'
      EXCEPTIONS
        no_changes_made        = 1
        object_not_initialized = 2
        error_message          = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD next.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'ZWX_AP_NO'
        ignore_buffer           = ' '
      IMPORTING
        number                  = snro
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.
    CASE sy-subrc.
      WHEN 0.
      WHEN 1.
        init( ).

        " determine number from number range
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = '01'
            object                  = 'ZWX_AP_NO'
            ignore_buffer           = ' '
          IMPORTING
            number                  = snro
          EXCEPTIONS
            interval_not_found      = 1
            number_range_not_intern = 2
            object_not_found        = 3
            quantity_is_0           = 4
            quantity_is_not_1       = 5
            interval_overflow       = 6
            buffer_overflow         = 7
            error_message           = 8
            OTHERS                  = 9.
        IF sy-subrc <> 0.
          " MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          "         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

      WHEN OTHERS.
        " MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        "         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.

  ENDMETHOD.

ENDCLASS.
