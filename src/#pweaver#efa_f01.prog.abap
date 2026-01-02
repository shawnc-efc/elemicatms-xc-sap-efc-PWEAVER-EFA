*----------------------------------------------------------------------*
***INCLUDE /PWEAVER/EFA_F01 .
*----------------------------------------------------------------------*

*{   INSERT         ESRK911241                                        1
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_INVOICE_LIST
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DISPLAY_INVOICE_LIST .

  DATA : s_date TYPE RANGE OF sy-datum,
         ls_date LIKE LINE OF s_date.

  IF scr_carrier IS INITIAL.
    MESSAGE 'Please select Carrier.' TYPE 'S'.
    EXIT.
  ENDIF.

  IF scr_inv_ldate IS NOT INITIAL.
    ls_date-sign   = 'I'.
    ls_date-option = 'BT'.
    ls_date-low    = scr_inv_ldate.
    ls_date-high   = scr_inv_hdate.
    APPEND ls_date TO s_date.
  ENDIF.

  CLEAR it_invoice.
  IF scr_carrier = 'UPS'.
    SELECT DISTINCT invoice_date
                    invoice_number
                    invoice_amount
               FROM /pweaver/efa_ups
         INTO TABLE it_invoice
              WHERE invoice_date IN s_date.
  ENDIF.


*  IF SCR_CONS IS NOT INITIAL.
  CLEAR wa_invoice.
  LOOP AT it_invoice INTO wa_invoice.
    CLEAR wa_invoice-discount.
    SELECT SUM( incen_amt ) FROM  /pweaver/efa_ups
                            INTO  wa_invoice-discount
                            WHERE invoice_number = wa_invoice-number.

    scr_cons_amount   =  scr_cons_amount + wa_invoice-amount.
    scr_cons_discount = scr_cons_discount + wa_invoice-discount.
    MODIFY it_invoice FROM wa_invoice TRANSPORTING discount.
    CLEAR wa_invoice.
  ENDLOOP.
ENDFORM.                    " DISPLAY_INVOICE_LIST

*}   INSERT
