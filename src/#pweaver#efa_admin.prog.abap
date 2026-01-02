*&---------------------------------------------------------------------*
*& Report  /PWEAVER/EFA_ADMIN
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
Report  /PWEAVER/EFA_ADMIN.

INCLUDE /PWEAVER/EFA_ADMIN_TOP                  .    " global Data
INCLUDE /PWEAVER/EFA_ADMIN_DISPLAY_F01.

INITIALIZATION.
PERFORM display_plant_carrier_list.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR  p_filnam.
PERFORM choose_file.

START-OF-SELECTION.
PERFORM upload_invoice.
