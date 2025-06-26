CLASS zcl_wx_oa_ap_info DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_details,
             BEGIN OF approver,
               userid TYPE string,
             END OF approver,
             speech    TYPE string,
             sp_status TYPE i,
             sptime    TYPE int4,
             media_id  TYPE TABLE OF string WITH DEFAULT KEY,
           END OF ty_details,
           tt_details TYPE TABLE OF ty_details WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_sp_record,
             sp_status    TYPE i,
             approverattr TYPE i,
             details      TYPE tt_details,
           END OF ty_sp_record,
           tt_sp_record TYPE TABLE OF ty_sp_record WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_content,
             control TYPE REF TO zif_wx_oa_fc,
             id      TYPE string,
             title   TYPE string,
             value   TYPE string,
             hidden  TYPE i,
           END OF ty_content,
           tt_contents TYPE TABLE OF ty_content WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_comment,
             BEGIN OF commentuserinfo,
               userid TYPE string,
             END OF commentuserinfo,
             commenttime    TYPE int4,
             commentcontent TYPE string,
             commentid      TYPE string,
             media_id       TYPE TABLE OF string WITH DEFAULT KEY,
           END OF ty_comment,
           tt_comments TYPE TABLE OF ty_comment WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_sub_node_list,
             userid    TYPE string,
             speech    TYPE string,
             sp_yj     TYPE i,
             sptime    TYPE int4,
             media_ids TYPE TABLE OF string WITH DEFAULT KEY,
           END OF ty_sub_node_list,
           tt_sub_node_list TYPE TABLE OF ty_sub_node_list WITH DEFAULT KEY.

    TYPES: BEGIN OF ty_node_list,
             node_type     TYPE i,
             sp_status     TYPE i,
             apv_rel       TYPE i,
             sub_node_list TYPE tt_sub_node_list,
           END OF ty_node_list,
           tt_node_list TYPE TABLE OF ty_node_list WITH DEFAULT KEY.

    DATA sp_no TYPE string .
    DATA sp_name TYPE string .
    DATA sp_status TYPE i .
    DATA template_id TYPE string .
    DATA apply_time TYPE int4 .
    DATA: BEGIN OF applyer,
            userid  TYPE string,
            partyid TYPE string,
          END OF applyer.
    DATA: BEGIN OF batch_applyer,
            " 和applyer字段互斥
            userid TYPE string,
          END OF batch_applyer.
    DATA: sp_record TYPE tt_sp_record.
    DATA: BEGIN OF notifyer,
            userid TYPE string,
          END OF notifyer.
    DATA: BEGIN OF apply_data,
            contents TYPE tt_contents,
          END OF apply_data.
    DATA: comments TYPE tt_comments.
    DATA: BEGIN OF process_list,
            node_list TYPE tt_node_list,
          END OF process_list.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WX_OA_AP_INFO IMPLEMENTATION.
ENDCLASS.
