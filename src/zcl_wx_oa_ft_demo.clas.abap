CLASS zcl_wx_oa_ft_demo DEFINITION
  PUBLIC
  INHERITING FROM zcl_wx_oa_ft_send
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS map
      IMPORTING
        !text01         TYPE string
        !selector02     TYPE string
        !number03       TYPE string
        !textarea04     TYPE string
        !textarea05     TYPE string OPTIONAL
        !file06         TYPE string OPTIONAL
      RETURNING
        VALUE(instance) TYPE REF TO zcl_wx_oa_ft_demo .
    METHODS constructor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FT_DEMO IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    me->approval = NEW zcl_wx_approval( ).

    me->approval->set_aptyp( 'DEMO' ). " DEMO

    me->ft = NEW zcl_wx_oa_ft(
      template_id = '3WN63LowpfnXkcDgnz8kfZMZ7Uq5w78fswFS8tvb'
    ).

  ENDMETHOD.


  METHOD map.
    instance = me.

    me->approval->set_docum( text01 ).
    " me->approval->set_mnote(  ).

    " 摘要信息
    me->ft->set_summary( VALUE #( ( text = '摘要1' lang = 'zh_CN' ) ) ).

    " 控件: Text 发文标题
    " 赋值:必输
    DATA(l_01_text) = NEW zcl_wx_oa_fc_text( `Text-1572857932948`
      )->set( text01 ).
    APPEND CAST zif_wx_oa_fc( l_01_text ) TO me->ft->apply_data-contents.


    " 控件: Selector 发文类型
    " 赋值:必输
    DATA(l_02_selector) = NEW zcl_wx_oa_fc_selector( id = `Selector-1573203804088`
      type = zcl_wx_oa_fc_selector=>single
      )->set( VALUE #(
        ( key = 'option-1573203804088' )   " 类型一
      " ( key = 'option-1573203804089' )   " 类型二
    ) ).
    APPEND CAST zif_wx_oa_fc( l_02_selector ) TO me->ft->apply_data-contents.

    " waiting todo

  ENDMETHOD.
ENDCLASS.
