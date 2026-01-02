*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /PWEAVER/EFA_FED................................*
DATA:  BEGIN OF STATUS_/PWEAVER/EFA_FED              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/PWEAVER/EFA_FED              .
CONTROLS: TCTRL_/PWEAVER/EFA_FED
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: */PWEAVER/EFA_FED              .
TABLES: /PWEAVER/EFA_FED               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
