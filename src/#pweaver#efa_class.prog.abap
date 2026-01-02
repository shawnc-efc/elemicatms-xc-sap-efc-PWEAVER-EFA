*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/EFA_CLASS
*&---------------------------------------------------------------------*

CLASS lcl_application DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
        FOR EVENT node_double_click
        OF cl_simple_tree_model
        IMPORTING node_key,
*
*      handle_toolbar
*        FOR EVENT toolbar OF cl_gui_alv_grid
*        IMPORTING e_object e_interactive,
*
*      handle_user_command
*         FOR EVENT user_command OF cl_gui_alv_grid
*         IMPORTING e_ucomm,
*
      handle_hotspot_click
         FOR EVENT hotspot_click OF cl_gui_alv_grid
         IMPORTING e_row_id.
ENDCLASS.                    "lcl_application DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_application IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.

  METHOD handle_node_double_click.
  ENDMETHOD.                    "handle_node_double_click
  METHOD handle_hotspot_click.
  ENDMETHOD.                    "handle_hotspot_click

ENDCLASS.                    "lcl_application IMPLEMENTATION
