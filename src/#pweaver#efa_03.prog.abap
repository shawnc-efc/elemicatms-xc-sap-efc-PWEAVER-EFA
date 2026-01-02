*&---------------------------------------------------------------------*
*& Report  /PWEAVER/EFA_03
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /PWEAVER/EFA_03 MESSAGE-ID TREE_MODEL_MSG.
*REPORT SAPSIMPLE_TREE_MODEL_DEMO MESSAGE-ID TREE_MODEL_MSG.

INCLUDE /PWEAVER/FREIGHT_AUDIT_TOP .

INCLUDE /PWEAVER/FREIGHT_AUDIT_CL1.

INCLUDE /PWEAVER/FREIGHT_AUDIT_O01 .

INCLUDE /PWEAVER/FREIGHT_AUDIT_I01 .
 .
INCLUDE /PWEAVER/FREIGHT_AUDIT_F01 .
initialization.




  start-of-selection.
* create the application object
* this object is needed to handle the ABAP Objects Events of
* Controls
  create object g_application.

  set screen 100.
