*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /PWEAVER/EFA_UPS................................*
DATA:  BEGIN OF STATUS_/PWEAVER/EFA_UPS              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/PWEAVER/EFA_UPS              .
CONTROLS: TCTRL_/PWEAVER/EFA_UPS
            TYPE TABLEVIEW USING SCREEN '9000'.
*.........table declarations:.................................*
TABLES: */PWEAVER/EFA_UPS              .
TABLES: /PWEAVER/EFA_UPS               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
