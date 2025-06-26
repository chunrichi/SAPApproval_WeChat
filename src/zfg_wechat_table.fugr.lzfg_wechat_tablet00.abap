*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTWX_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZTWX_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTWX_CONFIG                   .
CONTROLS: TCTRL_ZTWX_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZTWX_FT_CONF....................................*
DATA:  BEGIN OF STATUS_ZTWX_FT_CONF                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTWX_FT_CONF                  .
CONTROLS: TCTRL_ZTWX_FT_CONF
            TYPE TABLEVIEW USING SCREEN '0002'.
*.........table declarations:.................................*
TABLES: *ZTWX_CONFIG                   .
TABLES: *ZTWX_FT_CONF                  .
TABLES: ZTWX_CONFIG                    .
TABLES: ZTWX_FT_CONF                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
