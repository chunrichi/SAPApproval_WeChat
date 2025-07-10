CLASS zcl_wx_oa_fc_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_wx_oa_fc .

    TYPES:
      BEGIN OF ty_children,
        list TYPE STANDARD TABLE OF REF TO zif_wx_oa_fc WITH EMPTY KEY,
      END OF ty_children,
      tt_children TYPE STANDARD TABLE OF ty_children WITH EMPTY KEY.

    DATA control TYPE string VALUE `Table` ##NO_TEXT.
    DATA id TYPE string .
    DATA:
      BEGIN OF value,
        children TYPE tt_children,
      END OF value .


    METHODS constructor
      IMPORTING
        !id TYPE string .

    METHODS add
      RETURNING VALUE(instance) TYPE REF TO zcl_wx_oa_fc_table.

    METHODS set
      IMPORTING !node           TYPE REF TO zif_wx_oa_fc
      RETURNING VALUE(instance) TYPE REF TO zcl_wx_oa_fc_table.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: children TYPE REF TO ty_children.
ENDCLASS.



CLASS ZCL_WX_OA_FC_TABLE IMPLEMENTATION.


  METHOD constructor.
    me->id = id.
  ENDMETHOD.


  METHOD add.

    APPEND INITIAL LINE TO me->value-children REFERENCE INTO me->children.

    instance = me.
  ENDMETHOD.


  METHOD set.

    APPEND node TO me->children->list.

    instance = me.
  ENDMETHOD.
ENDCLASS.
