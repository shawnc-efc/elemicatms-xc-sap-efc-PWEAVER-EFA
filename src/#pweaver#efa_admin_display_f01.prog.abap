*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/EFA_ADMIN_DISPLAY_F01
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT

FORM display_plant_carrier_list .
  CLEAR: it_plant, it_carrier, wa_plant, wa_carrier.
  CLEAR: it_vrm_plant, it_vrm_carrier, wa_vrm_plant, wa_vrm_carrier.
  SELECT DISTINCT plant FROM /pweaver/cconfig
                        INTO TABLE it_plant.
  IF sy-subrc EQ 0.
    LOOP AT it_plant INTO wa_plant.
      wa_vrm_plant-key  = wa_plant-werks.
      wa_vrm_plant-text = wa_plant-werks.
      APPEND wa_vrm_plant TO it_vrm_plant.
      CLEAR  wa_vrm_plant.
    ENDLOOP.
  ENDIF.

  SELECT DISTINCT carriertype FROM /pweaver/cconfig
                              INTO TABLE it_carrier.
  IF sy-subrc EQ 0.
    LOOP AT it_carrier INTO wa_carrier.
      wa_vrm_carrier-key  = wa_carrier-carriertype.
      wa_vrm_carrier-text = wa_carrier-carriertype.
      APPEND wa_vrm_carrier TO it_vrm_carrier.
      CLEAR  wa_vrm_carrier.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'PS_PLANT'
      values = it_vrm_plant.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'PS_CARR'
      values = it_vrm_carrier.
ENDFORM.                    " DISPLAY_PLANT_CARRIER_LIST

*}   INSERT
*&---------------------------------------------------------------------*
*&      Form  CHOOSE_FILE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM choose_file .
  CLEAR: p_filnam.

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
* EXPORTING
*   PROGRAM_NAME        = SYST-REPID
*   DYNPRO_NUMBER       = SYST-DYNNR
*   FIELD_NAME          = ' '
*   STATIC              = ' '
*   MASK                = ' '
*   FILEOPERATION       = 'R'
    CHANGING
      file_name           = p_filnam
*   LOCATION_FLAG       = 'P'
   EXCEPTIONS
     mask_too_long       = 1
     OTHERS              = 2.

  BREAK-POINT.
ENDFORM.                    " CHOOSE_FILE
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_INVOICE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_invoice .
  IF p_filnam IS NOT INITIAL.
    CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
        filename                = p_filnam
        i_begin_col             = 1
        i_begin_row             = 1
        i_end_col               = 256
        i_end_row               = 65356
      TABLES
        intern                  = it_data
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
    ELSE.
      IF ps_carr EQ 'FEDEX'.
      ELSEIF ps_carr EQ 'UPS'.
      ELSEIF ps_carr  EQ 'LTL'.
      ENDIF.
    ENDIF.

  ENDIF.
ENDFORM.                    " UPLOAD_INVOICE
