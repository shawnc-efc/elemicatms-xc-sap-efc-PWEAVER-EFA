*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/EFA_ADMIN_FORM
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT


form get_file_name .
  clear p_filnam.
  call function 'KD_GET_FILENAME_ON_F4'
       exporting
*            MASK      = '*.xls'
            static    = 'X'
       changing
            file_name = p_filnam.

endform.                    " GET_FILE_NAME
*&---------------------------------------------------------------------*
*&      Form  GET_DATA_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form get_data_internal_table .

* If Input file name is not initial.
**Get the text for selected carrier in the list box
  read table it_vrm_carrier into wa_vrm_carrier with key key = ps_carr.
  if sy-subrc = 0.
    ps_carr = wa_vrm_carrier-text.
  endif.
**Get the text for selected plant in the list box

  read table it_vrm_plant into wa_vrm_plant with key key = ps_plant.
  if sy-subrc = 0.
    ps_plant = wa_vrm_plant-text.
  endif.
  clear : wa_vrm_carrier,wa_vrm_plant.

  if not p_filnam is initial.

*PERFORM excel_upload.
*it_data[] = t_data[].
** Upload EXCEL data into internal table
*    call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
*      EXPORTING
*        filename                = p_filnam
*        i_begin_col             = 1
*        i_begin_row             = 1
*        i_end_col               = 256
*        i_end_row               = 65356
**i_end_row                       =  1999999997
*      TABLES
*        intern                  = it_data
*      EXCEPTIONS
*        inconsistent_parameters = 1
*        upload_ole              = 2
*        others                  = 3.
    CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
      EXPORTING
        FILENAME                      = p_filnam
        I_BEGIN_COL                   = 1
        I_BEGIN_ROW                   = 1
        I_END_COL                     = 256
        I_END_ROW                     = 65356
      TABLES
        INTERN                        = it_data
     EXCEPTIONS
       INCONSISTENT_PARAMETERS       = 1
       UPLOAD_OLE                    = 2
       OTHERS                        = 3
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.


    if sy-subrc  <> 0.
      message I398(00) with 'Error in Uploading the Excel File'.
    elseif it_data[] is initial .
      message I398(00) with 'Error in Uploading the Excel File'.
    endif.
    if ps_carr = 'FEDEX' AND it_data[] IS NOT INITIAL.


*
*                     call function 'ZBAPI_FEDEX_EXCEL_UPLOAD'
*                       exporting
*                         ps_plant       = ps_plant
*                         p_date         = p_date
*                         ps_carr        = ps_carr
**                      IMPORTING
**                        SUBRC          =
*                       tables
*                         it_data        = it_data

      .
      CALL FUNCTION '/PWEAVER/FEDEX_INVOICE_UPLOAD'
        EXPORTING
*                        PS_PLANT       = PS_PLANT
          p_date         = P_DATE
          ps_carr        = PS_CARR
*                      IMPORTING
*                        SUBRC          =
       TABLES
         IT_DATA        = IT_DATA
                .



      if sy-subrc = 0.
        message I398(00) with 'Fedex Data Uploaded Successfully'.

      endif.

    elseif ps_carr = 'UPS' AND it_data[] IS NOT INITIAL.
*
*                     call function 'ZBAPI_UPS_EXCEL_UPLOAD'
*                       exporting
*                         ps_plant       = ps_plant
*                         p_date         = p_date
*                         ps_carr        = ps_carr
**                      IMPORTING
**                        SUBRC          =
*                       tables
*                         it_data        = it_data
*                               .
      CALL FUNCTION '/PWEAVER/UPS_INVOICE_UPLOAD'
        EXPORTING
*         PS_PLANT       = PS_PLANT
          p_date         = P_DATE
          ps_carr        = PS_CARR
*       IMPORTING
*         SUBRC          =
       TABLES
         IT_DATA        = IT_DATA
                .

      if sy-subrc = 0.
        message I398(00) with 'UPS Data Uploaded Successfully'.
      endif.
    ELSEIF PS_CARR = 'FEDEX FRT' AND it_data[] IS NOT INITIAL.
      CALL FUNCTION '/PWEAVER/FEDEXFREIGHT_UPLOAD'
        EXPORTING
          ps_plant       = PS_PLANT
          p_date         = P_DATE
          ps_carr        = PS_CARR
* IMPORTING
*   SUBRC          =
        tables
          it_data        = IT_DATA
                .
      if sy-subrc = 0.
        message I398(00) with 'FEDEX FREIGHT Data Uploaded Successfully'.
      endif.


    endif.
  endif.


endform.                    " GET_DATA_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*&      Form  excel_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM excel_upload .
*
*DATA: o_error       TYPE REF TO i_oi_error,
*      o_control     TYPE REF TO i_oi_container_control,
*      o_document    TYPE REF TO i_oi_document_proxy,
*      o_spreadsheet TYPE REF TO i_oi_spreadsheet.
*
*DATA: obj_container TYPE REF TO cl_gui_custom_container.
*
*DATA: t_files       TYPE filetable,
*      s_files       TYPE file_table,
*      v_doc_name    TYPE char256,
*      v_changed     TYPE int4,
*      v_rcode       TYPE int4,
*      t_ranges      TYPE soi_range_list,
*      lt_ranges TYPE SOI_FULL_RANGE_TABLE,
*      ls_ranges LIKE LINE OF lt_ranges,
*      s_ranges      TYPE soi_range_item,
*      s_data        TYPE soi_generic_item,
*      v_action      TYPE int4.
*
*
*  CALL METHOD c_oi_container_control_creator=>get_container_control
*       IMPORTING control = o_control
*                 error   = o_error.
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
*
*  CREATE OBJECT obj_container
*    EXPORTING
*     container_name               = 'CONTAINER'
*    EXCEPTIONS
*     cntl_error                  = 1
*     cntl_system_error           = 2
*     create_error                = 3
*     lifetime_error              = 4
*     lifetime_dynpro_dynpro_link = 5
*     others                      = 6.
*
*  IF sy-subrc <> 0.
*    MESSAGE e208(00) WITH 'Error creating container'.
*  ENDIF.
*
** Establish connection to GUI Control
*  CALL METHOD o_control->init_control
*      EXPORTING r3_application_name = 'Excel Document Container'
*                 parent              = obj_container
*       IMPORTING error               = o_error.
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
*
** Create Document Proxy
*  CALL METHOD o_control->get_document_proxy
*       EXPORTING document_type   = soi_doctype_excel_sheet
*       IMPORTING document_proxy  = o_document
*                 error           = o_error.
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
*CONCATENATE 'FILE://' p_filnam INTO v_doc_name.
** Open Spreadsheet in SAPWORKDIR
*  CALL METHOD o_document->open_document
*       EXPORTING document_title   = 'Excel'
*                 document_url     = v_doc_name
*                 no_flush         = ''
*       IMPORTING error            = o_error.
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
*
** Open Spreadsheet interface
*  CALL METHOD o_document->get_spreadsheet_interface
*       EXPORTING no_flush        = ''
*       IMPORTING sheet_interface = o_spreadsheet
*                 error           = o_error.
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
*
** Set selection for 1000 rows
*  CALL METHOD o_spreadsheet->set_selection
*               EXPORTING top   = 1
*                         left  = 1
*                         rows  = '65536'
*                         columns = '256'.
*
** Define Range in spreadsheet
*  CALL METHOD o_spreadsheet->insert_range
*         EXPORTING name      = 'Test'
*                   rows      = '65536'
*                   columns   = '256'
*                   no_flush  = ''
*         IMPORTING error     = o_error.
*
**ls_ranges-name  = 'Test1'.
**ls_ranges-top   = 1.
**ls_ranges-left  = 1.
**ls_ranges-rows  = '999'.
**ls_ranges-columns = '256'.
**
**APPEND ls_ranges to lt_ranges.
**
**ls_ranges-name  = 'Test2'.
**ls_ranges-top   = 1000.
**ls_ranges-left  = 1.
**ls_ranges-rows  = '999'.
**ls_ranges-columns = '256'.
**
**APPEND ls_ranges to lt_ranges.
*
**CALL METHOD o_spreadsheet->insert_ranges
**       EXPORTING ranges    = lt_ranges
**       IMPORTING error     = o_error.
*
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
*
**  s_ranges-name    = 'Test'.
**  s_ranges-rows    = '65356'.
**  s_ranges-columns = '256'.
**  APPEND s_ranges TO t_ranges.
*
** Get data
*REFRESH t_data.
*  CALL METHOD o_spreadsheet->get_ranges_data
*         EXPORTING all       = 'X'
*                   no_flush  = ''
*         IMPORTING contents  = t_data
*                   error     = o_error
*         CHANGING  ranges    = t_ranges.
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
** Close document
*
** Close the document
*  CALL METHOD o_document->close_document
*         EXPORTING do_save     = ''
*                   no_flush    = ''
*         IMPORTING has_changed = v_changed
*                   error       = o_error.
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
*
** Clear Document Resources
*  CALL METHOD o_document->release_document
*         EXPORTING no_flush = ''
*         IMPORTING error    = o_error.
*
*  IF o_error->has_failed = 'X'.
*    CALL METHOD o_error->raise_message
*         EXPORTING type = 'E'.
*  ENDIF.
*
** Clear table of file names
*  FREE: t_files,
*        o_control.

ENDFORM.                    " excel_upload
