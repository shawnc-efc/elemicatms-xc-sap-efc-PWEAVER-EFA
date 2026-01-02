*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/FREIGHT_AUDIT_CL1
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT

class lcl_application definition.

  public section.
    methods:
      handle_node_double_click
        for event node_double_click
        of cl_simple_tree_model
        importing node_key,

          handle_toolbar for event toolbar of cl_gui_alv_grid
          importing
            e_object e_interactive,

          handle_user_command for event user_command of cl_gui_alv_grid
          importing e_ucomm,
********************************
handle_hotspot_click
for event hotspot_click of cl_gui_alv_grid
importing e_row_id
          e_column_id.
**********************************


endclass.                    "lcl_application DEFINITION





*---------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
class lcl_application implementation.

  method handle_node_double_click.
    " this method handles the node double click event of the tree
    " model instance

    data: long_key type tm_nodekey.


    g_event = 'NODE_DOUBLE_CLICK'.
    g_node_key = node_key.

    data : wa_node(30) type c,
          v_node(20) type c.

    export wa_node from g_node_key to memory id 'NODE_MEMID'.


    " show the key of the double clicked node in a dynpro field
*    g_event = 'ProcessWeaver CLICK'.

    split node_key at '#' into: week_dt service.

    v_node = node_key.

    if v_node <> 'EFA'.
      if  v_node <> 'FEDEX'.
        if v_node <> 'FEDEX'.
          if v_node <> 'UPS'.
            if v_node <> space .

              clear : v_invoice_number.
*    v_invoice_number = node_key+0(9).
              split node_key at '#' into: v_invoice_number temp_service.
            endif.
          endif.
        endif.
      endif.
    endif.

    if go_custom_container is initial.

      create object go_custom_container
         exporting
*    PARENT                      =
           container_name              = 'CUSTOM2'
*    STYLE                       =
*    LIFETIME                    = lifetime_default
*    REPID                       =
*    DYNNR                       =
*    NO_AUTODEF_PROGID_DYNNR     =
         exceptions
           cntl_error                  = 1
           cntl_system_error           = 2
           create_error                = 3
           lifetime_error              = 4
           lifetime_dynpro_dynpro_link = 5
           others                      = 6
           .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.


      create object go_grid
        exporting
*    I_SHELLSTYLE      = 0
*    I_LIFETIME        =
    i_parent          = g_container_2   "go_custom_container
    i_appl_events     = 'X'
*    I_PARENTDBG       =
*    I_APPLOGPARENT    =
*    I_GRAPHICSPARENT  =
*    I_NAME            =
*    I_FCAT_COMPLETE   = SPACE
        exceptions
          error_cntl_create = 1
          error_cntl_init   = 2
          error_cntl_link   = 3
          error_dp_create   = 4
          others            = 5
          .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.


    endif.
*    PERFORM zone.

*    SET HANDLER g_application->handle_user_command FOR go_grid.
*    SET HANDLER g_application->handle_toolbar FOR go_grid.

*    perform carrier_remittance_report.


  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
   IF wa_export_output-carrier = 'UPS'.
*   CALL METHOD go_grid->set_toolbar_interactive.
    if temp_invoice NE v_invoice_number.
*  refresh it_ups_data.
*    refresh it_ups_summ.
      refresh it_ups.
    endif.
    if it_ups is INITIAL.
      IF it_cons_export IS INITIAL.
        CLEAR wa_export_output.
        read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
        temp_invoice = v_invoice_number.
        IF wa_export_output-carrier = 'UPS'.
*   perform fetch_data.
*    perform fetch_data_ups.
          PERFORM get_ups_data.
        ENDIF.
      ELSE.
        CLEAR wa_cons_export.
        read table it_cons_export into wa_cons_export with key account_no = v_invoice_number.
        temp_invoice = v_invoice_number.
        IF wa_cons_export-carrier = 'UPS'.
*   perform fetch_data.
*    perform fetch_data_ups.
          PERFORM get_ups_data.
        ENDIF.
      ENDIF.

    endif.



    case service.
      when 'SERVICE_TYPE'.
*       perform servicetype.
*        PERFORM service_type.
        PERFORM service_type_temp.
      when 'ZONE'.
*        perform zone.
*        perform zone_type.
        perform zone_type_temp.
      when 'PACKAGE_TYPE'.
*        perform package_type.
        perform package_type_temp.
      when 'PAYMENT_METHOD'.
*        perform paymentmethod.
        perform payment_type.
      when 'SHIP_DAY'.
*        perform shipday.
        perform ship_day.
      when 'INCADD'.
*        perform incorretaddr.
*        PERFORM addr_corr_ups.
        PERFORM addr_correction.
*      when 'CSR'.
*        perform csr.
      when 'ALLDET'.
*       perform alldetails.
        perform get_all_details.
*      when 'COMPARE'.
*        perform compare_fedex_sap.
      when 'GSR'.
*        perform gsr.
        PERFORM gsr_ups.


   when 'MANIFEST'.

         PERFORM MANIFEST_FREIGHT.

*         PERFORM MANIFEST_DEMO.
   when 'AUDIT'.
     PERFORM AUDIT.
     when 'INBOUND'.


*perform inbound.
*PERFORM FINAL.
   when 'SUMMARY'.
     PERFORM SUMMARY.


    endcase.

endif.


  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.

         IF wa_export_output-carrier = 'FEDEX'.

             refresh t_efa_fed.


            select * from /pweaver/efa_fed into table t_efa_f where INVOICE_NUMBER = v_invoice_number.

             case service.

                when 'SERVICE_TYPE'.
                     perform servicetype.
*
                when 'ZONE'.
                     perform zone.
*
*               when 'PACKAGE_TYPE'.
**                   perform package_type.
*                    perform package_type_temp.
                 when 'PAYMENT_METHOD'.
                     perform paymentmethod.

                 when 'SHIP_DAY'.
                     perform shipday.

                 when 'INCADD'.
                    perform incorretaddr.
*
*                when 'CSR'.
*                      perform csr.
                 when 'ALLDET'.
                    perform alldetails.

                  when 'MANIFEST'.

                    PERFORM Manifest_EFA_Fedex.

*               when 'COMPARE'.
*                    perform compare_fedex_sap.
                when 'GSR'.
                    perform gsr.
             endcase.

          ENDIF.





  endmethod.                    "handle_node_double_click


  method handle_toolbar.

    if approve = 'X'.
      constants:

        c_button_normal           type i value 0,
        c_menu_and_default_button type i value 1,
        c_menu                    type i value 2,
        c_separator               type i value 3,
        c_radio_button            type i value 4,
        c_checkbox                type i value 5,
        c_menu_entry              type i value 6.

      data:
          ls_toolbar  type stb_button.

      clear ls_toolbar.
      move c_separator to ls_toolbar-butn_type..
      append ls_toolbar to e_object->mt_toolbar.

      clear ls_toolbar.
      move 'APRV'        to ls_toolbar-function.
      move  icon_change    to ls_toolbar-icon.
      move 'Approve' to ls_toolbar-quickinfo.
      move 'Approve'        to ls_toolbar-text.
      move ' '             to ls_toolbar-disabled.
      append ls_toolbar    to e_object->mt_toolbar.

    endif.
  endmethod.                    "handle_toolbar




  method handle_user_command.
    case e_ucomm.
      when 'APRV'.
        data : doc_number(10) type c value '400000011',
              comp_code(4) type c value '0005',
              year(4) type c value '2006'.

        set parameter id: 'BLN' field doc_number,
                          'BUK' field comp_code,
                           'GJR' field year.


        call transaction 'FB03' and skip first screen.

    endcase.
  endmethod.                    "handle_user_command







*******************FOR HOTSPOT

  method handle_hotspot_click.

    data n type i.

    if  service eq  'GSR'.
      read table it_output_gsr_type into wa_output_gsr_type index e_row_id-index."w_index_rows-index.
*    n = STRLEN( wa_output7-trackingnumber ).
*
*    wa_output7-trackingnumber = wa_output7-trackingnumber+1(n).

      if sy-subrc = 0.

        if wa_export_output-carrier = 'FEDEX'.

          concatenate 'http://www.fedex.com/Tracking?ascend_header=1&clienttype=dotcom&cntry_code=us&language=english&tracknumbers=' wa_output_gsr_type-exp_grd_trck_id
           into pw_url.
        else.
          concatenate 'http://wwwapps.ups.com/WebTracking/processInputRequest?tracknum=' wa_output_gsr_type-exp_grd_trck_id into pw_url.

        endif.

      endif.


    elseif service eq 'INCADD'.


      read table it_output_addcorr_type into wa_output_addcorr_type index e_row_id-index."w_index_rows-index.
**    n = STRLEN( wa_zpwefainvoice-trackingnumber ).
**
**    wa_zpwefainvoice-trackingnumber = wa_zpwefainvoice-trackingnumber+1(n).

      if sy-subrc = 0.

        if wa_export_output-carrier = 'FEDEX'.

          if e_column_id = 'EXP_GRD_TRCK_ID'.
            concatenate 'http://www.fedex.com/Tracking?ascend_header=1&clienttype=dotcom&cntry_code=us&language=english&tracknumbers=' wa_output_addcorr_type-exp_grd_trck_id
             into pw_url.
          elseif e_column_id = 'POD'.
            concatenate  'https://www.fedex.com/Spod?ascend_header=1&clienttype=dotcom&show_custom_form=&cntry_code=us&cacheKey=&tracknumber_list=' wa_output_addcorr_type-exp_grd_trck_id
           '&language=english&action=showLetter&spodLetterCacheKey=pKp8LsQdmbfM9k4kzGyyxSZQxTvwbjpR1XvkR1HJgVPTjqTBYjVG!-669679058!1269616765933|' wa_output_addcorr_type-exp_grd_trck_id
                      into pw_url.
          endif.


        endif.
**         READ TABLE IT_OUPUT_ADDCORR_TYPE INTO WA_OUPUT_ADDCORR_TYPE INDEX e_row_id-index.
*        IF SY-SUBRC = 0.
*          IF WA_INCT_ADDR-CARRIER = 'UPS'.
*
*            CONCATENATE 'http://wwwapps.ups.com/WebTracking/processInputRequest?tracknum=' WA_INCT_ADDR-TRACKINGNUMBER INTO PW_URL.
*
*          ENDIF.
*        ENDIF.
      endif.

    elseif service eq 'ALLDET'.



*

*      if sy-subrc = 0.

        if wa_export_output-carrier = 'FEDEX' .
          read table it_output_alldet_type into wa_output_alldet_type index e_row_id-index.
          if sy-subrc = 0.
          if e_column_id = 'EXP_GRD_TRCK_ID'.
            concatenate 'http://www.fedex.com/Tracking?ascend_header=1&clienttype=dotcom&cntry_code=us&language=english&tracknumbers=' wa_output_alldet_type-exp_grd_trck_id
             into pw_url.
          elseif e_column_id = 'POD'.
            concatenate  'https://www.fedex.com/Spod?ascend_header=1&clienttype=dotcom&show_custom_form=&cntry_code=us&cacheKey=&tracknumber_list=' wa_output_alldet_type-exp_grd_trck_id
           '&language=english&action=showLetter&spodLetterCacheKey=pKp8LsQdmbfM9k4kzGyyxSZQxTvwbjpR1XvkR1HJgVPTjqTBYjVG!-669679058!1269616765933|' wa_output_alldet_type-exp_grd_trck_id
                      into pw_url.




          endif.
          endif.
        ELSEIF wa_export_output-carrier = 'UPS' .
            READ TABLE IT_ALL_DETAILS into wa_all_details index e_row_id-index.
            if sy-subrc = 0.
          concatenate 'http://wwwapps.ups.com/WebTracking/processInputRequest?tracknum=' wa_all_details-tracking_number into pw_url.
        endif.
        endif.

*      endif.

*      READ TABLE IT_FINAL_ALL_DETAILS_UPS INTO WA_FINAL_ALL_DETAILS_UPS INDEX E_ROW_ID-INDEX.
*
*      IF SY-SUBRC = 0.
*        IF WA_FINAL_ALL_DETAILS_UPS-CARRIER = 'UPS'.
*
*          CONCATENATE 'http://wwwapps.ups.com/WebTracking/processInputRequest?tracknum=' WA_FINAL_ALL_DETAILS_UPS-TRACKINGNUMBER INTO PW_URL.
*
*        ENDIF.
*      ENDIF.


    elseif service eq 'COMPARE'.

      read table it_output_comp_type into wa_output_comp_type index e_row_id-index.

      if sy-subrc = 0.

        if wa_export_output-carrier = 'FEDEX' .
          if e_column_id = 'EXP_GRD_TRCK_ID'.
            concatenate 'http://www.fedex.com/Tracking?ascend_header=1&clienttype=dotcom&cntry_code=us&language=english&tracknumbers=' wa_output_comp_type-exp_grd_trck_id
             into pw_url.
          elseif e_column_id = 'POD'.

            concatenate  'https://www.fedex.com/Spod?ascend_header=1&clienttype=dotcom&show_custom_form=&cntry_code=us&cacheKey=&tracknumber_list=' wa_output_comp_type-exp_grd_trck_id
           '&language=english&action=showLetter&spodLetterCacheKey=pKp8LsQdmbfM9k4kzGyyxSZQxTvwbjpR1XvkR1HJgVPTjqTBYjVG!-669679058!1269616765933|' wa_output_comp_type-exp_grd_trck_id
                      into pw_url.
          elseif e_column_id = 'TRACKING_NUMBER'.
            concatenate 'http://www.fedex.com/Tracking?ascend_header=1&clienttype=dotcom&cntry_code=us&language=english&tracknumbers=' wa_output_comp_type-tracking_number
             into pw_url.
          endif.
        endif.

      endif.

*      READ TABLE IT_FINAL_ALL_DETAILS_UPS INTO WA_FINAL_ALL_DETAILS_UPS INDEX E_ROW_ID-INDEX.
*
*      IF SY-SUBRC = 0.
*        IF WA_FINAL_ALL_DETAILS_UPS-CARRIER = 'UPS'.
*
*          CONCATENATE 'http://wwwapps.ups.com/WebTracking/processInputRequest?tracknum=' WA_FINAL_ALL_DETAILS_UPS-TRACKINGNUMBER INTO PW_URL.
*
*        ENDIF.
*      ENDIF.







    endif.

    call screen 9000.

  endmethod.                    "handle_hotspot_click
endclass.                    "lcl_application IMPLEMENTATION
