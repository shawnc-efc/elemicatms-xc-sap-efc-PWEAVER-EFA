*----------------------------------------------------------------------*
***INCLUDE /PWEAVER/EFA_I01 .
*----------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0900 INPUT.
CASE OKCODE_900.
    WHEN 'EXEC' .
      CLEAR OKCODE_900.
      PERFORM DISPLAY_INVOICE_LIST.
    WHEN 'EFA'.
      CLEAR OKCODE_900.
*      PERFORM DISPLAY_EFA.

    WHEN 'BACK'.
      CLEAR OKCODE_900.
      LEAVE PROGRAM.
    WHEN 'REFRESH'.
      CLEAR OKCODE_900.
      LEAVE TO CURRENT TRANSACTION.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0900  INPUT

*}   INSERT
*&---------------------------------------------------------------------*
*&      Module  update_selected  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_selected INPUT.
MODIFY IT_INVOICE FROM WA_INVOICE INDEX TC_INVOICES-CURRENT_LINE.
ENDMODULE.                 " update_selected  INPUT
