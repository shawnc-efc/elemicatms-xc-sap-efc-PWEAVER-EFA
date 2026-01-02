*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/FREIGHT_AUDIT_O01
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  STATUS_9000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9000 output.

    set pf-status '/PWEAVER/TRACK'.

 if init is initial.

    init = 'X'.



    create object:

    container exporting container_name = 'WEB_CONTAINER'.



    call method c_oi_container_control_creator=>get_document_viewer
      importing
        viewer = document_viewer.



*    call method document_viewer->init_viewer
*      exporting
*        parent = container.




  endif.
  if v_viewer is initial.
    v_viewer = 'X'.
    call method document_viewer->init_viewer
      exporting
        parent = container.
  endif.

  call method document_viewer->view_document_from_url
    exporting
      document_url = pw_url
      show_inplace = 'X'.

endmodule.                 " STATUS_9000  OUTPUT
       " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
MODULE PBO_100 OUTPUT.
SET PF-STATUS '/PWEAVER/TRACK'.
SEt TITLEBAR '/PWEAVER/EFA'.

  if first is initial.   ""  To Create the docking container for the first time
    repid = sy-repid.
    dynnr = sy-dynnr.

* create the docking container
    create object g_docking_container
                  exporting repid     = repid
                            dynnr     = dynnr
                            side      = g_docking_container->dock_at_left
                            extension = 1500.

    first = 'X'.

        create object g_splitter_container
                 exporting parent = g_docking_container
                           rows    = 1
                           columns = 2.

        call method g_splitter_container->get_container
          exporting
            row       = 1
            column    = 1
          receiving
            container = g_container_1.

        CALL METHOD G_SPLITTER_CONTAINER->SET_COLUMN_WIDTH
          EXPORTING
            ID                = 1
            WIDTH             = 25
*          IMPORTING
*            RESULT            =
          EXCEPTIONS
            CNTL_ERROR        = 1
            CNTL_SYSTEM_ERROR = 2
            others            = 3
                .
        IF SY-SUBRC <> 0.
         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.





        call method g_splitter_container->get_container
          exporting
            row       = 1
            column    = 2
          receiving
            container = g_container_2.

*        CALL METHOD G_CONTAINER_1->SET_WIDTH
*          EXPORTING
*            WIDTH      = 100
*          EXCEPTIONS
*            CNTL_ERROR = 1
*            others     = 2
*                .
*        IF SY-SUBRC <> 0.
*         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                    WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*        ENDIF.
  endif.
  IF g_tree IS INITIAL.
    " The Tree Model has not been created yet.
    " Create a Tree Model and insert nodes into it.
    PERFORM create_and_init_tree.
  ENDIF.


ENDMODULE.                 " PBO_100  OUTPUT
