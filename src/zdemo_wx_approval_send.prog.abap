REPORT zdemo_wx_approval_send.


DATA(l_sp_no) = NEW zcl_wx_oa_ft_demo( )->map(
  text01     = ''
  selector02 = ''
  number03   = ''
  textarea04 = ''
  textarea05 = ''
  file06     = '' )->send( ).
