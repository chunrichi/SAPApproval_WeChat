CLASS zcl_wx_oa_ft DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_node,
        type    TYPE i,
        apv_rel TYPE i,
        userid  TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
      END OF ty_node .
    TYPES:
      tt_node_list TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_summary_list,
        summary_info TYPE zif_wx_oa_fc=>tt_text,
      END OF ty_summary_list .
    TYPES:
      tt_summary_list TYPE STANDARD TABLE OF ty_summary_list WITH DEFAULT KEY .

    DATA creator_userid TYPE string .
    DATA template_id TYPE string .
    DATA use_template_approver TYPE i .
    DATA choose_department TYPE i .
    DATA:
      BEGIN OF process,
        node_list TYPE tt_node_list,
      END OF process .
    DATA:
      BEGIN OF apply_data,
        contents TYPE TABLE OF REF TO zif_wx_oa_fc,
      END OF apply_data .
    DATA summary_list TYPE tt_summary_list .

    METHODS constructor
      IMPORTING
        !template_id           TYPE data
        !use_template_approver TYPE i DEFAULT 1 .
    METHODS set_summary
      IMPORTING
        !summary TYPE zif_wx_oa_fc=>tt_text .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_FT IMPLEMENTATION.


  METHOD constructor.
    " 模板id
    me->template_id = template_id.

    " 审批人模式：0 指定、1 使用模板
    me->use_template_approver = use_template_approver.


    " 提单者提单部门id，不填默认为主部门
    " l_fc->choose_department   = 2.

    " ------------------- 流程列表 ----------------------
    " use_template_approver = 0 时必填
    " 暂不支持

  ENDMETHOD.


  METHOD set_summary.

    " 摘要信息，用于显示在审批通知卡片、审批列表的摘要信息，最多3行

    APPEND VALUE #( summary_info = summary ) TO me->summary_list.
  ENDMETHOD.
ENDCLASS.
