*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZTWX_CONFIG.....................................*
DATA:  BEGIN OF STATUS_ZTWX_CONFIG                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTWX_CONFIG                   .
CONTROLS: TCTRL_ZTWX_CONFIG
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTWX_CONFIG                   .
TABLES: ZTWX_CONFIG                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
