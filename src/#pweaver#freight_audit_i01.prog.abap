*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/FREIGHT_AUDIT_I01
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT

*&---------------------------------------------------------------------*
*&      Module  PAI_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module  pai_100 input.
  data: return_code type i.


* CL_GUI_CFW=>DISPATCH must be called if events are registered
* that trigger PAI
* this method calls the event handler method of an event
  call method cl_gui_cfw=>dispatch
    importing return_code = return_code.

  if return_code <> cl_gui_cfw=>rc_noevent.
    " a control event occured => exit PAI
    clear g_ok_code.
    exit.
  endif.
*********************

*CALL METHOD GO_GRID->REFRESH_TABLE_DISPLAY.

  case g_ok_code.
    when 'BACK'.                       " Finish program

      delete  from memory id 'ZPWEFA_DIS'.
      free memory id 'ZPWEFA_DIS'.
      if not g_custom_container is initial.
         "destroy tree container (detroys contained tree control, too)
        call method g_custom_container->free
          exceptions
            cntl_system_error = 1
            cntl_error        = 2.
        if sy-subrc <> 0.
*          message a000.
        endif.
        clear g_custom_container.
        clear g_tree.
      endif.
      leave program.
  endcase.

* CAUTION: clear ok code!
  clear g_ok_code.

endmodule.                             " PAI_0100  INPUT



                  "values_form
*&---------------------------------------------------------------------*
*&      Module  CANCEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cancel input.
delete  from memory id 'ZPWEFA_DIS'.
        call method document_viewer->destroy_viewer.
        if sy-subrc eq 0.
         clear v_viewer.
        endif.
  leave to screen 0.

endmodule.                 " CANCEL  INPUT
