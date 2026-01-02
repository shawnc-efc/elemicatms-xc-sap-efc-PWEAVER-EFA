*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /PWEAVER/EFA_TRC................................*
DATA:  BEGIN OF STATUS_/PWEAVER/EFA_TRC              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/PWEAVER/EFA_TRC              .
CONTROLS: TCTRL_/PWEAVER/EFA_TRC
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: */PWEAVER/EFA_TRC              .
TABLES: /PWEAVER/EFA_TRC               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
