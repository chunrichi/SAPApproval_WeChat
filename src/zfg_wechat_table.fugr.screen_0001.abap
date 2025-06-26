PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_ztwx_config CURSOR nextline.
    MODULE liste_show_liste.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD ztwx_config-sysid .
      FIELD ztwx_config-corp_id .
      FIELD ztwx_config-corp_secret .
      FIELD ztwx_config-api_url .
      FIELD ztwx_config-changer .
      FIELD ztwx_config-changed_at .
      MODULE set_update_flag ON CHAIN-REQUEST.
    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD ztwx_config-sysid .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
