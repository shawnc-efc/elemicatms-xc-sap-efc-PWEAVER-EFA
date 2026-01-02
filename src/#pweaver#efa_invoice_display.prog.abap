*&---------------------------------------------------------------------*
*& Report  /PWEAVER/EFA_INVOICE_DISPLAY
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /PWEAVER/EFA_INVOICE_DISPLAY no standard page heading.


include /pweaver/efa_invoice_hist_top.

*---------------------------------------------------------------------*
*INITIALISATION.
initialization.

delete  from memory id 'ZPWEFA_DIS'.
delete from memory id 'ZPWEFA_CONS'.

****Get all the careers
  refresh : it_carrier.
  clear   : it_carrier.
*  select distinct carriertype from zpwcarrierconfig into table it_carrier.

*  SET PF-STATUS 'ZEFA_ADMIN'.




*---------------------------------------------------------------------*
* EVENT : AT SELECTION-SCREEN                                         *
*---------------------------------------------------------------------*
*AT SELECTION-SCREEN .


*---------------------------------------------------------------------*
* EVENT : AT SELECTION-SCREEN                                         *
*---------------------------------------------------------------------*
at selection-screen output.

clear : wa_carrier,wa_vrm_carrier.

*  loop at it_carrier into wa_carrier.
*    clear : v_tabix,v_number.
*    v_tabix = sy-tabix.
*    concatenate '0' v_tabix into v_number.
*    wa_vrm_carrier-key = v_number.
*    wa_vrm_carrier-text = wa_carrier-carriertype.
*    append  wa_vrm_carrier to it_vrm_carrier.
*    clear wa_vrm_carrier.
*  endloop.

    wa_vrm_carrier-key = 'UPS'.
    wa_vrm_carrier-text = 'UPS'.
    append  wa_vrm_carrier to it_vrm_carrier.
    clear wa_vrm_carrier.

    wa_vrm_carrier-key = 'FEDEX'.
    wa_vrm_carrier-text = 'FEDEX'.
    append  wa_vrm_carrier to it_vrm_carrier.
    clear wa_vrm_carrier.
**     wa_vrm_carrier-key = 'FEDEX FRT'.
**    wa_vrm_carrier-text = 'FEDEX FRT'.
**    append  wa_vrm_carrier to it_vrm_carrier.
**    clear wa_vrm_carrier.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'PS_CARR'
      values = it_vrm_carrier[].


  read table it_vrm_carrier into wa_vrm_carrier with key key = ps_carr.
  if sy-subrc = 0.
    ps_carr = wa_vrm_carrier-text.
  endif.


*******get the invoice numbers






*---------------------------------------------------------------------*
* EVENT : AT SELECTION-SCREEN                                         *
*---------------------------------------------------------------------*
at selection-screen.



*----------------------------------------------------------------------*
* START OF SELECTION                                                  *
*----------------------------------------------------------------------*
start-of-selection.
  set pf-status '/PWEAVER/DISPLAY'.
** Get the data from excel into internal table
  perform get_data_internal_table.
*


*****************************************************************
* Top Of Page Event to print Headers at start of new page        *
*****************************************************************
top-of-page.
** Page Header
  perform page_header.
*
end-of-selection.
*
**Listing the results .
*
  perform write_report.

  lines = sy-linno - 1.



*---------------------------------------------------------------------*
* EVENT : USER-COMMAND                                                *
*---------------------------------------------------------------------*
at user-command.

  if sy-ucomm = 'DELETE'.
    perform delete_rows.
   elseif sy-ucomm = 'DISPLAY'.
    perform display_efa.
    ELSEIF SY-UCOMM = 'SELECTALL'.

      PERFORM SELECTALL.
ELSEIF SY-UCOMM = 'DESELECT'.
  PERFORM DESELECT.
  ELSEIF SY-UCOMM = 'EXIT'.
    LEAVE TO SCREEN 0.
  ENDIF.
  IF SY-UCOMM = 'BACK'.
    LEAVE TO CURRENT TRANSACTION.
  endif.


INCLUDE /pweaver/EFA_INVOICE_HISt_FORM.
