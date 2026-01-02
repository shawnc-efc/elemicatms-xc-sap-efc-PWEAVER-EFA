*----------------------------------------------------------------------*
***INCLUDE /PWEAVER/EFA_O01 .
*----------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Module  STATUS_0900  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0900 OUTPUT.


  SET PF-STATUS '/PWEAVER/EFA_MAIN'.
  SET TITLEBAR '/PWEAVER/EFA'.

  IF it_invoice IS INITIAL.
    tc_invoices-invisible = 'X'.
  ELSE.
    CLEAR tc_invoices-invisible.
  ENDIF.

  LOOP AT SCREEN.
    IF screen-name = 'PB_EFA' OR screen-name = 'TF_LIST'.
      IF it_invoice IS INITIAL.
        screen-invisible = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF g_application IS INITIAL.
    CREATE OBJECT g_application.
  ENDIF.

  IF it_vrm_carrier IS INITIAL.
    wa_vrm_carrier-key = 'UPS'.
    wa_vrm_carrier-text = 'UPS'.
    APPEND  wa_vrm_carrier TO it_vrm_carrier.
    CLEAR wa_vrm_carrier.

    wa_vrm_carrier-key = 'FEDEX'.
    wa_vrm_carrier-text = 'FEDEX'.
    APPEND  wa_vrm_carrier TO it_vrm_carrier.
    CLEAR wa_vrm_carrier.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'SCR_CARRIER'
      values = it_vrm_carrier.

ENDMODULE.                 " STATUS_0900  OUTPUT

*}   INSERT
