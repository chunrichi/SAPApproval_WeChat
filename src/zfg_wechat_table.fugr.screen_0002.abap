PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_ztwx_ft_conf CURSOR nextline.
    MODULE liste_show_liste.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD ztwx_ft_conf-aptyp .
      FIELD ztwx_ft_conf-aptnm .
      FIELD ztwx_ft_conf-clsnm .
      FIELD ztwx_ft_conf-changer .
      FIELD ztwx_ft_conf-changed_at .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD ztwx_ft_conf-aptyp .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
