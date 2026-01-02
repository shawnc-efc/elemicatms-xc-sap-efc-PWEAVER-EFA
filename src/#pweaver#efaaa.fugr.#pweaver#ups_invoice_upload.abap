FUNCTION /PWEAVER/UPS_INVOICE_UPLOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PS_PLANT) OPTIONAL
*"     VALUE(P_DATE) TYPE  SY-DATUM OPTIONAL
*"     VALUE(PS_CARR) TYPE  /PWEAVER/CCONFIG-CARRIERTYPE OPTIONAL
*"  EXPORTING
*"     REFERENCE(SUBRC) TYPE  SY-SUBRC
*"  TABLES
*"      IT_DATA STRUCTURE  KCDE_CELLS
*"----------------------------------------------------------------------


  DATA : is_data TYPE KCDE_CELLS.

  DATA : v_let(52) TYPE c VALUE  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.

  DATA : it_zpwefa_ups TYPE TABLE OF /pweaver/efa_ups INITIAL SIZE 0,
         is_zpwefa_ups TYPE /pweaver/efa_ups,
         wa_zpwefa_ups TYPE /pweaver/efa_ups,
         cm_zpwefa_ups TYPE /pweaver/efa_ups.
  DATA : IT_ZEFA_UPS TYPE TABLE OF /pweaver/efa_ups INITIAL SIZE 0,
       WA_ZEFA_UPS TYPE /pweaver/efa_ups.

*  DATA : V_COUNTER(5) TYPE C,
*         COUNTER(5) TYPE C .
  DATA : v_counter TYPE i,
         counter TYPE i ,
         temp TYPE d.

  DATA : date(2),
         date_year(7),
         month(2),
         year(4).
  data : v_poddate type /PWEAVER/MANFEST-pod_date,
         v_expdate type /PWEAVER/MANFEST-pod_date,
         v_diff_days type VTBBEWE-ATAGE,
         v_podsig type /PWEAVER/MANFEST-pod_signature.


  TABLES : /pweaver/efa_ups.
*           /PWEAVER/MANFEST.
  DATA : v_count(5) TYPE c.
  SELECT MAX( counter ) FROM /pweaver/efa_ups INTO v_counter.
  IF NOT v_counter IS INITIAL.
    counter = v_counter.
  ENDIF.
  CLEAR : IT_ZEFA_UPS,
          WA_ZEFA_UPS.
*  delete from /pweaver/efa_ups.
*  delete from /pweaver/efa_trc.
  LOOP AT it_data INTO is_data.

*if is_data-row = '0001'.
*  continue.
*endif.

    AT NEW row.
      CLEAR is_zpwefa_ups.
    ENDAT.

    IF is_data-col = '0001'.
*      IF IS_DATA-VALUE CA V_LET. " commented as per LTB
*        CONTINUE. "commented as per LTB
*      ELSE.              "as per LTB
      MOVE is_data-value TO is_zpwefa_ups-version.
*      ENDIF.              "as per LTB
    ENDIF.
    IF is_data-col = '0002'.
      MOVE is_data-value TO is_zpwefa_ups-recipient_number.
    ENDIF.
    IF is_data-col = '0003'.
      SHIFT is_data-value LEFT DELETING LEADING '0'.
      MOVE is_data-value TO is_zpwefa_ups-account_number.
    ENDIF.
    IF is_data-col = '0004'.
      MOVE is_data-value TO is_zpwefa_ups-account_country.
    ENDIF.
    IF is_data-col = '0005'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-invoice_date.
    ENDIF.
    IF is_data-col = '0006'.
      MOVE is_data-value TO is_zpwefa_ups-invoice_number.
    ENDIF.
    IF is_data-col = '0007'.
      MOVE is_data-value TO is_zpwefa_ups-invc_type_code.
    ENDIF.
    IF is_data-col = '0008'.
      MOVE is_data-value TO is_zpwefa_ups-invc_typ_det_cd.
    ENDIF.
    IF is_data-col = '0009'.
      MOVE is_data-value TO is_zpwefa_ups-acct_tax_id.
    ENDIF.
    IF is_data-col = '0010'.
      MOVE is_data-value TO is_zpwefa_ups-invc_curr_code.
    ENDIF.
    IF is_data-col = '0011'.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-invoice_amount.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-invoice_amount.
      ENDIF.

    ENDIF.
    IF is_data-col = '0012'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-transaction_date.
    ENDIF.
    IF is_data-col = '0013'.
      MOVE is_data-value TO is_zpwefa_ups-pickup_rec_num.
    ENDIF.
    IF is_data-col = '0014'.
      MOVE is_data-value TO is_zpwefa_ups-lead_ship_number.
    ENDIF.
    IF is_data-col = '0015'.
      MOVE is_data-value TO is_zpwefa_ups-world_erase_num.
    ENDIF.
    IF is_data-col = '0016'.
      MOVE is_data-value TO is_zpwefa_ups-shp_ref_num_1.
**************************** added on 7/16/2013 : for leading Zeroes issue ******************************
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = is_zpwefa_ups-shp_ref_num_1
        IMPORTING
          OUTPUT = is_zpwefa_ups-shp_ref_num_1.
********************************** end of change : 7/16/2013 ********************************************
    ENDIF.
    IF is_data-col = '0017'.
      MOVE is_data-value TO is_zpwefa_ups-shp_ref_num_2.
**************************** added on 7/16/2013 : for leading Zeroes issue ******************************
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = is_zpwefa_ups-shp_ref_num_2
        IMPORTING
          OUTPUT = is_zpwefa_ups-shp_ref_num_2.
********************************** end of change : 7/16/2013 ********************************************
    ENDIF.
    IF is_data-col = '0018'.
      MOVE is_data-value TO is_zpwefa_ups-bill_opt_code.
    ENDIF.
    IF is_data-col = '0019'.
      MOVE is_data-value TO is_zpwefa_ups-pckg_qty.
    ENDIF.
    IF is_data-col = '0020'.
      MOVE is_data-value TO is_zpwefa_ups-ovr_qty.
    ENDIF.
    IF is_data-col = '0021'.
      MOVE is_data-value TO is_zpwefa_ups-trck_num.
    ENDIF.
    IF is_data-col = '0022'.
      MOVE is_data-value TO is_zpwefa_ups-pckg_ref_num_1.
**************************** added on 7/16/2013 : for leading Zeroes issue ******************************
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = is_zpwefa_ups-pckg_ref_num_1
        IMPORTING
          OUTPUT = is_zpwefa_ups-pckg_ref_num_1.
********************************** end of change : 7/16/2013 ********************************************
    ENDIF.
    IF is_data-col = '0023'.
      MOVE is_data-value TO is_zpwefa_ups-pckg_ref_num_2.
**************************** added on 7/16/2013 : for leading Zeroes issue ******************************
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = is_zpwefa_ups-pckg_ref_num_2
        IMPORTING
          OUTPUT = is_zpwefa_ups-pckg_ref_num_2.
********************************** end of change : 7/16/2013 ********************************************
    ENDIF.
    IF is_data-col = '0024'.
      MOVE is_data-value TO is_zpwefa_ups-pckg_ref_num_3.
    ENDIF.
    IF is_data-col = '0025'.
      MOVE is_data-value TO is_zpwefa_ups-pckg_ref_num_4.
    ENDIF.
    IF is_data-col = '0026'.
      MOVE is_data-value TO is_zpwefa_ups-pckg_ref_num_5.
    ENDIF.
    IF is_data-col = '0027'.
*      MOVE is_data-value TO is_zpwefa_ups-entered_weight.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-entered_weight.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-entered_weight.
      ENDIF.
    ENDIF.
    IF is_data-col = '0028'.
      MOVE is_data-value TO is_zpwefa_ups-ent_wgt_uom.
    ENDIF.
    IF is_data-col = '0029'.
*      MOVE is_data-value TO is_zpwefa_ups-billed_weight.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-billed_weight.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-billed_weight.
      ENDIF.
    ENDIF.
    IF is_data-col = '0030'.
      MOVE is_data-value TO is_zpwefa_ups-billed_wgt_uom.
    ENDIF.
    IF is_data-col = '0031'.
      MOVE is_data-value TO is_zpwefa_ups-container_type.
    ENDIF.
    IF is_data-col = '0032'.
      MOVE is_data-value TO is_zpwefa_ups-billed_wgh_type.
    ENDIF.
    IF is_data-col = '0033'.
      MOVE is_data-value TO is_zpwefa_ups-package_dimens.
    ENDIF.
    IF is_data-col = '0034'.
      MOVE is_data-value TO is_zpwefa_ups-z_one.
    ENDIF.
    IF is_data-col = '0035'.
      MOVE is_data-value TO is_zpwefa_ups-chrg_catg_code.
    ENDIF.
    IF is_data-col = '0036'.
      MOVE is_data-value TO is_zpwefa_ups-chrg_catg_det_cd.
    ENDIF.
    IF is_data-col = '0037'.
      MOVE is_data-value TO is_zpwefa_ups-chrg_src.
    ENDIF.

    IF is_data-col = '0038'.
      MOVE is_data-value TO is_zpwefa_ups-type_code_1.
    ENDIF.
    IF is_data-col = '0039'.
      MOVE is_data-value TO is_zpwefa_ups-type_det_code_1.
    ENDIF.
    IF is_data-col = '0040'.
      MOVE is_data-value TO is_zpwefa_ups-type_det_val_1.
    ENDIF.

    IF is_data-col = '0041'.
      MOVE is_data-value TO is_zpwefa_ups-type_code_2.
    ENDIF.

    IF is_data-col = '0042'.
      MOVE is_data-value TO is_zpwefa_ups-type_det_code_2.
    ENDIF.

    IF is_data-col = '0043'.
      MOVE is_data-value TO is_zpwefa_ups-type_det_val_2.
    ENDIF.

    IF is_data-col = '0044'.
      MOVE is_data-value TO is_zpwefa_ups-chrg_class_code.
    ENDIF.

    IF is_data-col = '0045'.
      MOVE is_data-value TO is_zpwefa_ups-chrg_desc_code.
    ENDIF.

    IF is_data-col = '0046'.
      MOVE is_data-value TO is_zpwefa_ups-chrg_desc.
    ENDIF.


    IF is_data-col = '0047'.
      MOVE is_data-value TO is_zpwefa_ups-chrg_unit_qty.
    ENDIF.
    IF is_data-col = '0048'.
      MOVE is_data-value TO is_zpwefa_ups-bas_curr_code.
    ENDIF.
    IF is_data-col = '0049'.
*      MOVE is_data-value TO is_zpwefa_ups-basis_value.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-basis_value.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-basis_value.
      ENDIF.
    ENDIF.
    IF is_data-col = '0050'.
      MOVE is_data-value TO is_zpwefa_ups-tax_indicator.
    ENDIF.
    IF is_data-col = '0051'.
      MOVE is_data-value TO is_zpwefa_ups-trans_curr_code.
    ENDIF.
    IF is_data-col = '0052'.
*      MOVE is_data-value TO is_zpwefa_ups-incen_amt.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-incen_amt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-incen_amt.
      ENDIF.
    ENDIF.
    IF is_data-col = '0053'.
*      MOVE is_data-value TO is_zpwefa_ups-net_amnt.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-net_amnt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-net_amnt.
      ENDIF.
    ENDIF.
    IF is_data-col = '0054'.
      MOVE is_data-value TO is_zpwefa_ups-misc_curr_code.
    ENDIF.
    IF is_data-col = '0055'.
*      MOVE is_data-value TO is_zpwefa_ups-misc_incv_amnt.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-misc_incv_amnt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-misc_incv_amnt.
      ENDIF.
    ENDIF.
    IF is_data-col = '0056'.
*      MOVE is_data-value TO is_zpwefa_ups-misc_net_amnt.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-misc_net_amnt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-misc_net_amnt.
      ENDIF.
    ENDIF.
    IF is_data-col = '0057'.
      MOVE is_data-value TO is_zpwefa_ups-altinvc_curr_cod.
    ENDIF.
    IF is_data-col = '0058'.
*      MOVE is_data-value TO is_zpwefa_ups-alt_invc_amt.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-alt_invc_amt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-alt_invc_amt  .
      ENDIF.
    ENDIF.
    IF is_data-col = '0059'.
      MOVE is_data-value TO is_zpwefa_ups-invc_exch_rate.
    ENDIF.
    IF is_data-col = '0060'.
      MOVE is_data-value TO is_zpwefa_ups-tax_var_amnt.
    ENDIF.
    IF is_data-col = '0061'.
*      MOVE is_data-value TO is_zpwefa_ups-curr_var_amnt.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-curr_var_amnt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-curr_var_amnt  .
      ENDIF.
    ENDIF.
    IF is_data-col = '0062'.
      MOVE is_data-value TO is_zpwefa_ups-invc_lvl_chrg.
    ENDIF.
    IF is_data-col = '0063'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
*      CONCATENATE is_data-value+6(4) is_data-value+0(2) is_data-value+3(2) into is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-invc_due_date.
    ENDIF.

    IF is_data-col = '0064'.
      MOVE is_data-value TO is_zpwefa_ups-alt_invc_num.
    ENDIF.
    IF is_data-col = '0065'.
      MOVE is_data-value TO is_zpwefa_ups-store_number.
    ENDIF.
    IF is_data-col = '0066'.
      MOVE is_data-value TO is_zpwefa_ups-cust_ref_num.
    ENDIF.
    IF is_data-col = '0067'.
      MOVE is_data-value TO is_zpwefa_ups-sender_name.
    ENDIF.
    IF is_data-col = '0068'.
      MOVE is_data-value TO is_zpwefa_ups-sender_cmp_name.
    ENDIF.
    IF is_data-col = '0069'.
      MOVE is_data-value TO is_zpwefa_ups-sender_addr_lin1.
    ENDIF.
    IF is_data-col = '0070'.
      MOVE is_data-value TO is_zpwefa_ups-sender_addr_lin2.
    ENDIF.
    IF is_data-col = '0071'.
      MOVE is_data-value TO is_zpwefa_ups-sender_city.
    ENDIF.
    IF is_data-col = '0072'.
      MOVE is_data-value TO is_zpwefa_ups-sender_state.
    ENDIF.
    IF is_data-col = '0073'.
      MOVE is_data-value TO is_zpwefa_ups-sender_postal.
    ENDIF.
    IF is_data-col = '0074'.
      MOVE is_data-value TO is_zpwefa_ups-sender_country.
    ENDIF.
    IF is_data-col = '0075'.
      MOVE is_data-value TO is_zpwefa_ups-receiver_name.
    ENDIF.
    IF is_data-col = '0076'.
      MOVE is_data-value TO is_zpwefa_ups-receiver_comp_nm.
    ENDIF.
    IF is_data-col = '0077'.
      MOVE is_data-value TO is_zpwefa_ups-rec_addr_line_1.
    ENDIF.
    IF is_data-col = '0078'.
      MOVE is_data-value TO is_zpwefa_ups-rec_addr_line_2.
    ENDIF.

    IF is_data-col = '0079'.
      MOVE is_data-value TO is_zpwefa_ups-receiver_city.
    ENDIF.
    IF is_data-col = '0080'.
      MOVE is_data-value TO is_zpwefa_ups-receiver_state.
    ENDIF.
    IF is_data-col = '0081'.
      MOVE is_data-value TO is_zpwefa_ups-receiver_postal.
    ENDIF.
    IF is_data-col = '0082'.
      MOVE is_data-value TO is_zpwefa_ups-receiver_country.
    ENDIF.
    IF is_data-col = '0083'.
      MOVE is_data-value TO is_zpwefa_ups-thrd_party_name.
    ENDIF.
    IF is_data-col = '0084'.
      MOVE is_data-value TO is_zpwefa_ups-thrd_prty_com_nm.
    ENDIF.
    IF is_data-col = '0085'.
      MOVE is_data-value TO is_zpwefa_ups-thrd_pty_add_ln1.
    ENDIF.
    IF is_data-col = '0086'.
      MOVE is_data-value TO is_zpwefa_ups-thrd_pty_add_ln2.
    ENDIF.
    IF is_data-col = '0087'.
      MOVE is_data-value TO is_zpwefa_ups-thrd_pty_city.
    ENDIF.
    IF is_data-col = '0088'.
      MOVE is_data-value TO is_zpwefa_ups-thrd_pty_state.
    ENDIF.
    IF is_data-col = '0089'.
      MOVE is_data-value TO is_zpwefa_ups-thrd_pty_pstl.
    ENDIF.
    IF is_data-col = '0090'.
      MOVE is_data-value TO is_zpwefa_ups-thrd_pty_cnty.
    ENDIF.
    IF is_data-col = '0091'.
      MOVE is_data-value TO is_zpwefa_ups-sold_to_name.
    ENDIF.
    IF is_data-col = '0092'.
      MOVE is_data-value TO is_zpwefa_ups-sold_to_comp_nam.
    ENDIF.
    IF is_data-col = '0093'.
      MOVE is_data-value TO is_zpwefa_ups-sold_to_add_ln1.
    ENDIF.
    IF is_data-col = '0094'.
      MOVE is_data-value TO is_zpwefa_ups-sold_to_add_ln2.
    ENDIF.
    IF is_data-col = '0095'.
      MOVE is_data-value TO is_zpwefa_ups-sold_to_city.
    ENDIF.
    IF is_data-col = '0096'.
      MOVE is_data-value TO is_zpwefa_ups-sold_to_state.
    ENDIF.
    IF is_data-col = '0097'.
      MOVE is_data-value TO is_zpwefa_ups-sold_to_postl.
    ENDIF.
    IF is_data-col = '0098'.
      MOVE is_data-value TO is_zpwefa_ups-sold_to_cnty.
    ENDIF.
    IF is_data-col = '0099'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr_qual_1.
    ENDIF.
    IF is_data-col = '0100'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr_1_name.
    ENDIF.
    IF is_data-col = '0101'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr1_com_nm.
    ENDIF.
    IF is_data-col = '0102'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr1add_ln1.
    ENDIF.
    IF is_data-col = '0103'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr1add_ln2.
    ENDIF.
    IF is_data-col = '0104'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr1_cty.
    ENDIF.
    IF is_data-col = '0105'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr1_state.
    ENDIF.
    IF is_data-col = '0106'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr1_postal.
    ENDIF.
    IF is_data-col = '0107'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr1_cnty.
    ENDIF.
    IF is_data-col = '0108'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr_qual_2.
    ENDIF.
    IF is_data-col = '0109'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr_2_name.
    ENDIF.
    IF is_data-col = '0110'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr2_com_nm.
    ENDIF.
    IF is_data-col = '0111'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr2add_ln1.
    ENDIF.
    IF is_data-col = '0112'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr2add_ln2.
    ENDIF.
    IF is_data-col = '0113'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr2_cty.
    ENDIF.
    IF is_data-col = '0114'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr2_state.
    ENDIF.
    IF is_data-col = '0115'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr2_postal.
    ENDIF.
    IF is_data-col = '0116'.
      MOVE is_data-value TO is_zpwefa_ups-msc_addr2_cnty.
    ENDIF.
    IF is_data-col = '0117'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
*      CONCATENATE is_data-value+6(4) is_data-value+0(2) is_data-value+3(2) into is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-ship_date.
    ENDIF.
    IF is_data-col = '0118'.
      MOVE is_data-value TO is_zpwefa_ups-ship_exp_date.
    ENDIF.
    IF is_data-col = '0119'.
      MOVE is_data-value TO is_zpwefa_ups-ship_imp_date.
    ENDIF.
    IF is_data-col = '0120'.
      MOVE is_data-value TO is_zpwefa_ups-entry_date.
    ENDIF.
    IF is_data-col = '0121'.
      MOVE is_data-value TO is_zpwefa_ups-direct_ship_date.
    ENDIF.
    IF is_data-col = '0122'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
*      CONCATENATE is_data-value+6(4) is_data-value+0(2) is_data-value+3(2) into is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-shp_del_date.
    ENDIF.
    IF is_data-col = '0123'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
*      CONCATENATE is_data-value+6(4) is_data-value+0(2) is_data-value+3(2) into is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-ship_rel_date.
    ENDIF.
    IF is_data-col = '0124'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
*      CONCATENATE is_data-value+6(4) is_data-value+0(2) is_data-value+3(2) into is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-cycle_date.
    ENDIF.
    IF is_data-col = '0125'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
*      CONCATENATE is_data-value+6(4) is_data-value+0(2) is_data-value+3(2) into is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-eft_date.
    ENDIF.
    IF is_data-col = '0126'.
      SPLIT is_data-value AT '/' INTO month date year.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = month
        IMPORTING
          output = month.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = date
        IMPORTING
          output = date.
      CONCATENATE year month date INTO is_data-value.
*      CONCATENATE is_data-value+6(4) is_data-value+0(2) is_data-value+3(2) into is_data-value.
      MOVE is_data-value TO is_zpwefa_ups-val_date.
    ENDIF.
    IF is_data-col = '0127'.
      MOVE is_data-value TO is_zpwefa_ups-entry_port.
    ENDIF.
    IF is_data-col = '0128'.
      MOVE is_data-value TO is_zpwefa_ups-entry_number.
    ENDIF.
    IF is_data-col = '0129'.
      MOVE is_data-value TO is_zpwefa_ups-export_place.
    ENDIF.
    IF is_data-col = '0130'.
      MOVE is_data-value TO is_zpwefa_ups-shp_val_amnt.
    ENDIF.
    IF is_data-col = '0131'.
      MOVE is_data-value TO is_zpwefa_ups-shp_desc.
    ENDIF.
    IF is_data-col = '0132'.
      MOVE is_data-value TO is_zpwefa_ups-ent_curr_code.
    ENDIF.
    IF is_data-col = '0133'.
      MOVE is_data-value TO is_zpwefa_ups-cust_num.
    ENDIF.
    IF is_data-col = '0134'.
      MOVE is_data-value TO is_zpwefa_ups-exch_rate.
    ENDIF.
    IF is_data-col = '0135'.
      MOVE is_data-value TO is_zpwefa_ups-mst_waybil_num.
    ENDIF.
    IF is_data-col = '0136'.
      MOVE is_data-value TO is_zpwefa_ups-epu.
    ENDIF.
    IF is_data-col = '0137'.
      MOVE is_data-value TO is_zpwefa_ups-entry_type.
    ENDIF.
    IF is_data-col = '0138'.
      MOVE is_data-value TO is_zpwefa_ups-cpc_code.
    ENDIF.
    IF is_data-col = '0139'.
      MOVE is_data-value TO is_zpwefa_ups-ln_item_num.
    ENDIF.
    IF is_data-col = '0140'.
      MOVE is_data-value TO is_zpwefa_ups-goods_desc.
    ENDIF.
    IF is_data-col = '0141'.
      MOVE is_data-value TO is_zpwefa_ups-entered_value.
    ENDIF.
    IF is_data-col = '0142'.
*      MOVE is_data-value TO is_zpwefa_ups-duty_amount.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-duty_amount.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-duty_amount.
      ENDIF.
    ENDIF.
    IF is_data-col = '0143'.
      MOVE is_data-value TO is_zpwefa_ups-weight.
    ENDIF.
    IF is_data-col = '0144'.
      MOVE is_data-value TO is_zpwefa_ups-unit_of_measure.
    ENDIF.
    IF is_data-col = '0145'.
      MOVE is_data-value TO is_zpwefa_ups-item_qty.
    ENDIF.
    IF is_data-col = '0146'.
      MOVE is_data-value TO is_zpwefa_ups-item_uom.
    ENDIF.
    IF is_data-col = '0147'.
      MOVE is_data-value TO is_zpwefa_ups-imp_tax_id.
    ENDIF.

****************

    IF is_data-col = '0148'.
      MOVE is_data-value TO is_zpwefa_ups-dec_num.
    ENDIF.

    IF is_data-col = '0149'.
      MOVE is_data-value TO is_zpwefa_ups-carr_nam.
    ENDIF.
    IF is_data-col = '0150'.
      MOVE is_data-value TO is_zpwefa_ups-cccd_num.
    ENDIF.

    IF is_data-col = '0151'.
      MOVE is_data-value TO is_zpwefa_ups-cycle_num.
    ENDIF.

    IF is_data-col = '0152'.
      MOVE is_data-value TO is_zpwefa_ups-frg_trd_ref_num.
    ENDIF.

    IF is_data-col = '0153'.
      MOVE is_data-value TO is_zpwefa_ups-job_num.
    ENDIF.

    IF is_data-col = '0154'.
      MOVE is_data-value TO is_zpwefa_ups-tran_mode.
    ENDIF.

    IF is_data-col = '0155'.
      MOVE is_data-value TO is_zpwefa_ups-tax_type.
    ENDIF.

    IF is_data-col = '0156'.
      MOVE is_data-value TO is_zpwefa_ups-tariff_code.
    ENDIF.

    IF is_data-col = '0157'.
      MOVE is_data-value TO is_zpwefa_ups-tariff_rate.
    ENDIF.

    IF is_data-col = '0158'.
      MOVE is_data-value TO is_zpwefa_ups-tariff_trt_num.
    ENDIF.

    IF is_data-col = '0159'.
      MOVE is_data-value TO is_zpwefa_ups-contact_name.
    ENDIF.

    IF is_data-col = '0160'.
      MOVE is_data-value TO is_zpwefa_ups-contact_num.
    ENDIF.

    IF is_data-col = '0161'.
      MOVE is_data-value TO is_zpwefa_ups-doc_type.
    ENDIF.

    IF is_data-col = '0162'.
      MOVE is_data-value TO is_zpwefa_ups-off_num.
    ENDIF.

    IF is_data-col = '0163'.
      MOVE is_data-value TO is_zpwefa_ups-doc_num.
    ENDIF.

    IF is_data-col = '0164'.
      MOVE is_data-value TO is_zpwefa_ups-duty_val.
    ENDIF.

    IF is_data-col = '0165'.
      MOVE is_data-value TO is_zpwefa_ups-tot_val_duty.
    ENDIF.

    IF is_data-col = '0166'.
      MOVE is_data-value TO is_zpwefa_ups-exc_tax_amnt.
    ENDIF.
    IF is_data-col = '0167'.
      MOVE is_data-value TO is_zpwefa_ups-exc_tax_rate.
    ENDIF.
    IF is_data-col = '0168'.
      MOVE is_data-value TO is_zpwefa_ups-gst_amnt.
    ENDIF.

    IF is_data-col = '0169'.
      MOVE is_data-value TO is_zpwefa_ups-gst_rate.
    ENDIF.
    IF is_data-col = '0170'.
      MOVE is_data-value TO is_zpwefa_ups-ord_cncl.
    ENDIF.
    IF is_data-col = '0171'.
      MOVE is_data-value TO is_zpwefa_ups-ord_cntry.
    ENDIF.

    IF is_data-col = '0172'.
      MOVE is_data-value TO is_zpwefa_ups-sima_access.
    ENDIF.

    IF is_data-col = '0173'.
*      MOVE is_data-value TO is_zpwefa_ups-tax_value.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-tax_value.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-tax_value.
      ENDIF.

    ENDIF.

    IF is_data-col = '0174'.
*      MOVE is_data-value TO is_zpwefa_ups-totl_cust_amnt.
      CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_ups-totl_cust_amnt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_ups-totl_cust_amnt.
      ENDIF.
    ENDIF.

    IF is_data-col = '0175'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_1.
    ENDIF.

    IF is_data-col = '0176'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_2.
    ENDIF.

    IF is_data-col = '0177'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_3.
    ENDIF.

    IF is_data-col = '0178'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_4.
    ENDIF.

    IF is_data-col = '0179'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_5.
    ENDIF.

    IF is_data-col = '0180'.
      MOVE is_data-value TO is_zpwefa_ups-pay_role_code.
    ENDIF.



    IF is_data-col = '0181'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_7.
    ENDIF.

    IF is_data-col = '0182'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_8.
    ENDIF.

    IF is_data-col = '0183'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_9.

    ENDIF.
    IF is_data-col = '0184'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_10.
    ENDIF.
    IF is_data-col = '0185'.
      MOVE is_data-value TO is_zpwefa_ups-misc_line_11.
    ENDIF.

    IF is_data-col = '0186'.
      MOVE is_data-value TO is_zpwefa_ups-duty_rate.
    ENDIF.

    IF is_data-col = '0187'.
      MOVE is_data-value TO is_zpwefa_ups-vat_basis_amnt.
    ENDIF.
    IF is_data-col = '0188'.
      MOVE is_data-value TO is_zpwefa_ups-vat_amnt.
    ENDIF.
    IF is_data-col = '0189'.
      MOVE is_data-value TO is_zpwefa_ups-vat_rate.
    ENDIF.
    IF is_data-col = '0190'.
      MOVE is_data-value TO is_zpwefa_ups-oth_basis_amnt.
    ENDIF.
    IF is_data-col = '0191'.
      MOVE is_data-value TO is_zpwefa_ups-oth_amnt.
    ENDIF.
    IF is_data-col = '0192'.
      MOVE is_data-value TO is_zpwefa_ups-oth_rate.
    ENDIF.
    IF is_data-col = '0193'.
      MOVE is_data-value TO is_zpwefa_ups-oth_cust_num_ind.
    ENDIF.
    IF is_data-col = '0194'.
      MOVE is_data-value TO is_zpwefa_ups-oth_cust_num.
    ENDIF.
    IF is_data-col = '0195'.
      MOVE is_data-value TO is_zpwefa_ups-oth_cust_name.
    ENDIF.




    IF is_data-col = '0196'.
      MOVE is_data-value TO is_zpwefa_ups-pckg_dim_uom.
    ENDIF.
    IF is_data-col = '0197'.
      MOVE is_data-value TO is_zpwefa_ups-org_shp_pkg_qty.
    ENDIF.
    IF is_data-col = '0198'.
      MOVE is_data-value TO is_zpwefa_ups-corr_zone.
    ENDIF.
    IF is_data-col = '0199'.
      MOVE is_data-value TO is_zpwefa_ups-tax_law_art_num.
    ENDIF.
    IF is_data-col = '0200'.
      MOVE is_data-value TO is_zpwefa_ups-tax_law_art_amnt.
    ENDIF.
    IF is_data-col = '0201'.
      MOVE is_data-value TO is_zpwefa_ups-org_track_num.
    ENDIF.
    IF is_data-col = '0202'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_28.
    ENDIF.
    IF is_data-col = '0203'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_29.
    ENDIF.
    IF is_data-col = '0204'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_30.
    ENDIF.
    IF is_data-col = '0205'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_31.
    ENDIF.
    IF is_data-col = '0206'.
      MOVE is_data-value TO is_zpwefa_ups-bol_1.
    ENDIF.
    IF is_data-col = '0207'.
      MOVE is_data-value TO is_zpwefa_ups-bol_2.
    ENDIF.
    IF is_data-col = '0208'.
      MOVE is_data-value TO is_zpwefa_ups-bol_3.
    ENDIF.
    IF is_data-col = '0209'.
      MOVE is_data-value TO is_zpwefa_ups-bol_4.
    ENDIF.
    IF is_data-col = '0210'.
      MOVE is_data-value TO is_zpwefa_ups-bol_5.
    ENDIF.
    IF is_data-col = '0211'.
      MOVE is_data-value TO is_zpwefa_ups-po_1.
    ENDIF.
    IF is_data-col = '0212'.
      MOVE is_data-value TO is_zpwefa_ups-po_2.
    ENDIF.
    IF is_data-col = '0213'.
      MOVE is_data-value TO is_zpwefa_ups-po_3.
    ENDIF.
    IF is_data-col = '0214'.
      MOVE is_data-value TO is_zpwefa_ups-po_4.
    ENDIF.
    IF is_data-col = '0215'.
      MOVE is_data-value TO is_zpwefa_ups-po_5.
    ENDIF.
    IF is_data-col = '0216'.
      MOVE is_data-value TO is_zpwefa_ups-po_6.
    ENDIF.
    IF is_data-col = '0217'.
      MOVE is_data-value TO is_zpwefa_ups-po_7.
    ENDIF.
    IF is_data-col = '0218'.
      MOVE is_data-value TO is_zpwefa_ups-po_8.
    ENDIF.
    IF is_data-col = '0219'.
      MOVE is_data-value TO is_zpwefa_ups-po_9.
    ENDIF.
    IF is_data-col = '0220'.
      MOVE is_data-value TO is_zpwefa_ups-po_10.
    ENDIF.
    IF is_data-col = '0221'.
      MOVE is_data-value TO is_zpwefa_ups-nmfc.
    ENDIF.
    IF is_data-col = '0222'.
      MOVE is_data-value TO is_zpwefa_ups-det_class.
    ENDIF.
    IF is_data-col = '0223'.
      MOVE is_data-value TO is_zpwefa_ups-frt_seq_num.
    ENDIF.
    IF is_data-col = '0224'.
      MOVE is_data-value TO is_zpwefa_ups-dec_frt_cls.
    ENDIF.
    IF is_data-col = '0225'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_34.
    ENDIF.
    IF is_data-col = '0226'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_35.
    ENDIF.
    IF is_data-col = '0227'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_36.
    ENDIF.
    IF is_data-col = '0228'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_37.
    ENDIF.
    IF is_data-col = '0229'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_38.
    ENDIF.
    IF is_data-col = '0230'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_39.
    ENDIF.
    IF is_data-col = '0231'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_40.
    ENDIF.
    IF is_data-col = '0232'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_41.
    ENDIF.
    IF is_data-col = '0233'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_42.
    ENDIF.
    IF is_data-col = '0234'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_43.
    ENDIF.
    IF is_data-col = '0235'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_44.
    ENDIF.
    IF is_data-col = '0236'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_45.
    ENDIF.
    IF is_data-col = '0237'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_46.
    ENDIF.
    IF is_data-col = '0238'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_47.
    ENDIF.
    IF is_data-col = '0239'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_48.
    ENDIF.
    IF is_data-col = '0240'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_49.
    ENDIF.
    IF is_data-col = '0241'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_50.
    ENDIF.
    IF is_data-col = '0242'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_51.
    ENDIF.
    IF is_data-col = '0243'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_52.
    ENDIF.
    IF is_data-col = '0244'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_53.
    ENDIF.
    IF is_data-col = '0245'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_54.
    ENDIF.
    IF is_data-col = '0246'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_55.
    ENDIF.
    IF is_data-col = '0247'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_56.
    ENDIF.
    IF is_data-col = '0248'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_57.
    ENDIF.
    IF is_data-col = '0249'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_58.
    ENDIF.
    IF is_data-col = '0250'.
      MOVE is_data-value TO is_zpwefa_ups-place_hold_59.
    ENDIF.

    IF is_data-col = '0251'.
      MOVE ps_plant TO is_zpwefa_ups-plant_id.
    ENDIF.
    IF is_data-col = '0252'.
      MOVE is_data-value TO is_zpwefa_ups-gsr_flag.
    ENDIF.
    IF is_data-col = '0253'.
      MOVE is_data-value TO is_zpwefa_ups-gsr_desc.
    ENDIF.
    IF is_data-col = '0254'.
      MOVE is_data-value TO is_zpwefa_ups-lost_flag.
    ENDIF.
    IF is_data-col = '0255'.
      MOVE is_data-value TO is_zpwefa_ups-lost_desc.
    ENDIF.
    IF is_data-col = '0256'.
      MOVE is_data-value TO is_zpwefa_ups-damage_flag.
    ENDIF.
    IF is_data-col = '0257'.
      MOVE is_data-value TO is_zpwefa_ups-damage_desc.
    ENDIF.
    IF is_data-col = '0258'.
      MOVE is_data-value TO is_zpwefa_ups-customer_id.

    ENDIF.



    AT END OF row.
*      break-point.


*  if not v_count is initial or v_count <> 0.
*    counter = v_count + 1.
*  endif.
*    if  v_count is initial or v_count = 0.
      counter = counter + 1.
*    endif.
      is_zpwefa_ups-mandt = sy-mandt.
      is_zpwefa_ups-counter = counter.
      is_zpwefa_ups-plant_id = ps_plant.
      is_zpwefa_ups-carrier = ps_carr.

      is_zpwefa_ups-create_date = sy-datum.
      is_zpwefa_ups-create_time = sy-uzeit.
      is_zpwefa_ups-create_user = sy-uname.
      APPEND  is_zpwefa_ups TO it_zpwefa_ups.
      WA_ZEFA_UPS-INVOICE_NUMBER = is_zpwefa_ups-invoice_number.
      APPEND WA_ZEFA_UPS TO IT_ZEFA_UPS.
      CLEAR : is_zpwefa_ups.
      clear :        WA_ZEFA_UPS.

*modify table ZEFA_FEDEX from IS_ZEFA_FEDEX.
      IF sy-subrc = 0.
        subrc = 0.
      ELSEIF sy-subrc <> 0.
        subrc = 4.
      ENDIF.
    ENDAT.
    CLEAR : is_data.

  ENDLOOP.
  DATA : it_del_ups TYPE STANDARD TABLE OF /pweaver/efa_ups,
        v_invoice_no TYPE /PWEAVER/INVOICE_NUMBER.

  delete adjacent duplicates from IT_ZEFA_UPS.
  REFRESH : IT_DEL_UPS.
  CLEAR :  V_INVOICE_NO.
  LOOP AT IT_ZEFA_UPS INTO WA_ZEFA_UPS.
    v_invoice_no = WA_ZEFA_UPS-invoice_number.
*    CLEAR is_zpwefa_ups.


    SELECT * FROM /pweaver/efa_ups INTO TABLE it_del_ups WHERE invoice_number = v_invoice_no.
    IF sy-subrc eq 0.
      DELETE /pweaver/efa_ups FROM TABLE it_del_ups.
      IF sy-subrc ne 0.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

****************************CHNGE**************************************


  DATA : ls_carrierconfig TYPE /pweaver/cconfig.

  DATA : it_ups_track TYPE STANDARD TABLE OF /pweaver/efa_trc,
         it_del_track TYPE STANDARD TABLE OF /pweaver/efa_trc,
         wa_ups_track TYPE /pweaver/efa_trc.

*  IF it_zpwefa_ups IS NOT INITIAL.
*    READ TABLE it_zpwefa_ups INTO is_zpwefa_ups INDEX 1.
*    v_invoice_no = is_zpwefa_ups-invoice_number.
*    CLEAR is_zpwefa_ups.
*
*    SELECT * FROM /pweaver/efa_ups INTO TABLE it_del_ups WHERE invoice_number = v_invoice_no.
*    IF sy-subrc EQ 0.
*      DELETE /pweaver/efa_ups FROM TABLE it_del_ups.
*      IF sy-subrc NE 0.
*        EXIT.
*      ENDIF.
*    ENDIF.

  INSERT /pweaver/efa_ups FROM TABLE it_zpwefa_ups.
**************************ENDOF CHANE****************************************************
  SELECT SINGLE * FROM /pweaver/cconfig INTO ls_carrierconfig WHERE carriertype = 'UPS' ."AND
*                                                                      plant       = ps_plant.

  LOOP AT it_zpwefa_ups INTO is_zpwefa_ups WHERE ( chrg_catg_code = 'SHP' OR chrg_catg_code = 'RTN' ) AND
                                                 ( chrg_class_code = 'FRT' OR chrg_class_code = 'INF' ) ."and CHRG_CATG_DET_CD ne 'FC' .

    IF is_zpwefa_ups-chrg_class_code = 'FRT'.
      wa_ups_track-mandt        = is_zpwefa_ups-mandt .
      wa_ups_track-tracking_no  = is_zpwefa_ups-trck_num.
      wa_ups_track-lead_ship_no = is_zpwefa_ups-lead_ship_number.
      wa_ups_track-account_no   = is_zpwefa_ups-account_number.
      wa_ups_track-plant        = ps_plant.
      wa_ups_track-invoice_no   = is_zpwefa_ups-invoice_number.
      wa_ups_track-invoice_date = is_zpwefa_ups-invoice_date.
      wa_ups_track-carrier      = 'UPS'.
      wa_ups_track-license_no   = ls_carrierconfig-licenseno.
      wa_ups_track-userid       = ls_carrierconfig-userid.
      wa_ups_track-password     = ls_carrierconfig-password.
      wa_ups_track-transaction_date  = is_zpwefa_ups-transaction_date.
      clear : v_poddate,
              v_expdate,
              v_podsig.
      select single POD_DATE POD_SIGNATURE EXP_DEL_DATE  into (v_poddate , v_podsig, v_expdate) from /PWEAVER/MANFEST where TRACKING_NUMBER = is_zpwefa_ups-trck_num.
      wa_ups_track-pod_date = v_poddate.
      wa_ups_track-EXP_DEL_DATE = v_expdate.
      wa_ups_track-pod_signature = v_podsig.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          I_DATE_FROM          = v_expdate
*             I_KEY_DAY_FROM       =
          I_DATE_TO            = v_poddate
*             I_KEY_DAY_TO         =
*             I_FLG_SEPARATE       = ' '
       IMPORTING
         E_DAYS               = v_diff_days.
*             E_MONTHS             =
*             E_YEARS              =
      .
      if v_diff_days is initial.



      else.

        wa_ups_TRACK-GSR_STATUS = 'X'."P_WA_UPS_GSR-GSR_STATUS'.

      endif.

*    endif.


      APPEND wa_ups_track TO it_ups_track.
      CLEAR  wa_ups_track.
    ELSEIF is_zpwefa_ups-chrg_class_code = 'INF' .
      READ TABLE it_ups_track INTO wa_ups_track WITH KEY tracking_no  = is_zpwefa_ups-trck_num
                                                         lead_ship_no = is_zpwefa_ups-lead_ship_number .
      IF sy-subrc NE 0.
        wa_ups_track-mandt        = is_zpwefa_ups-mandt .
        wa_ups_track-tracking_no  = is_zpwefa_ups-trck_num.
        wa_ups_track-lead_ship_no = is_zpwefa_ups-lead_ship_number.
        wa_ups_track-account_no   = is_zpwefa_ups-account_number.
        wa_ups_track-plant        = ps_plant.
        wa_ups_track-invoice_no   = is_zpwefa_ups-invoice_number.
        wa_ups_track-invoice_date = is_zpwefa_ups-invoice_date.
        wa_ups_track-carrier      = 'UPS'.
        wa_ups_track-license_no   = ls_carrierconfig-licenseno.
        wa_ups_track-userid       = ls_carrierconfig-userid.
        wa_ups_track-password     = ls_carrierconfig-password.
        wa_ups_track-transaction_date  = is_zpwefa_ups-transaction_date.
        clear : v_poddate,
            v_expdate.
        select single POD_DATE  POD_SIGNATURE EXP_DEL_DATE into (v_poddate , v_podsig , v_expdate) from /PWEAVER/MANFEST where TRACKING_NUMBER = is_zpwefa_ups-trck_num.
        wa_ups_track-pod_date = v_poddate.
        wa_ups_track-EXP_DEL_DATE = v_expdate.
        wa_ups_track-pod_signature = v_podsig.
        CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
          EXPORTING
            I_DATE_FROM          = v_expdate
*             I_KEY_DAY_FROM       =
            I_DATE_TO            = v_poddate
*             I_KEY_DAY_TO         =
*             I_FLG_SEPARATE       = ' '
         IMPORTING
           E_DAYS               = v_diff_days.
*             E_MONTHS             =
*             E_YEARS              =
        .
        if v_diff_days is initial.



        else.

          wa_ups_TRACK-GSR_STATUS = 'X'."P_WA_UPS_GSR-GSR_STATUS'.

        endif.

        APPEND wa_ups_track TO it_ups_track.
        CLEAR  wa_ups_track.

      ENDIF.
    ENDIF.
    CLEAR wa_ups_track.

  ENDLOOP.
  IF it_ups_track IS NOT INITIAL.
    REFRESH : IT_DEL_TRACK.
    loop at it_zefa_ups into wa_zefa_ups.
      SELECT * FROM /pweaver/efa_trc INTO TABLE it_del_track WHERE invoice_no = wa_zefa_ups-invoice_number.
      IF sy-subrc eq 0.
        DELETE /pweaver/efa_trc FROM TABLE it_del_track.
        IF sy-subrc ne 0.
          EXIT.
        ENDIF.
      ENDIF.
    endloop.
  ENDIF.
****************************
*    IF it_ups_track IS NOT INITIAL.
*      SELECT * FROM /pweaver/efa_trc INTO TABLE it_del_track WHERE invoice_no = v_invoice_no.
*      IF sy-subrc EQ 0.
*        DELETE /pweaver/efa_trc FROM TABLE it_del_track.
*        IF sy-subrc NE 0.
*          EXIT.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*********************************
  INSERT /pweaver/efa_trc FROM TABLE it_ups_track.



*  ENDIF.











* data : ls_carrierconfig type zpwcarrierconfig.
*
*  select single * from zpwcarrierconfig into ls_carrierconfig where carriertype = 'UPS' and
*                                                                    plant       = ps_plant.
*
*  LOOP AT it_ups INTO wa_ups.
*
*    perform ups_track USING wa_ups ls_carrierconfig.
*
**    perform ups_time_transit using wa_ups ls_carrierconfig.
*
*
*
*  ENDLOOP.







*  DATA : V_LEAD_SHPMENT TYPE ZPWEFA_UPS-LEAD_SHIP_NUMBER,
*        V_LEAD_SHPMENT_MEM TYPE ZPWEFA_UPS-LEAD_SHIP_NUMBER,
*        COUNTER(1) TYPE C,
*        ZCOUNTER(1) TYPE C,
*        V_TABIX(4) TYPE C,
*        V_ORGTAB(4) TYPE C,
*        V_MODTAB(4) TYPE C,
*        V_COMPTAX(4) TYPE C.
*  CLEAR : COUNTER,ZCOUNTER.
*
*  ZCOUNTER = 0.
*  LOOP AT IT_ZPWEFA_UPS INTO IS_ZPWEFA_UPS.
****Compare the current row of tracking number with the row below
*
*
*    IF SY-TABIX = 1.
*      V_LEAD_SHPMENT = IS_ZPWEFA_UPS-LEAD_SHIP_NUMBER.
*    ENDIF.
*
*    IF SY-TABIX > 1.
*      V_COMPTAX = SY-TABIX.
*      V_ORGTAB = SY-TABIX.
*      V_MODTAB = SY-TABIX.
*      V_COMPTAX  = V_COMPTAX  - 1.
*      READ TABLE IT_ZPWEFA_UPS INTO CM_ZPWEFA_UPS INDEX V_COMPTAX.
*      IF SY-SUBRC = 0.
*        V_LEAD_SHPMENT = CM_ZPWEFA_UPS-LEAD_SHIP_NUMBER.
*      ENDIF.
*    ENDIF.
*
*    IF V_LEAD_SHPMENT = IS_ZPWEFA_UPS-LEAD_SHIP_NUMBER AND V_MODTAB > 1.
**  v_lead_shpment = is_ZPWEFA_UPS-LEAD_SHIP_NUMBER.
*      COUNTER = 'X'.
*      ZCOUNTER = ZCOUNTER + 1.
*    ENDIF.
*    IF V_LEAD_SHPMENT <> IS_ZPWEFA_UPS-LEAD_SHIP_NUMBER.
*      COUNTER = SPACE.
*      ZCOUNTER = SPACE.
*    ENDIF.
*
*
*    IF V_LEAD_SHPMENT = IS_ZPWEFA_UPS-LEAD_SHIP_NUMBER  AND ZCOUNTER = 1.
*
*      V_ORGTAB = V_ORGTAB - 1.
*      V_MODTAB = V_MODTAB.
*
*      READ TABLE IT_ZPWEFA_UPS INTO WA_ZPWEFA_UPS INDEX V_ORGTAB.
*
*      WA_ZPWEFA_UPS-CHRG_CLASS_CODE1 = IS_ZPWEFA_UPS-CHRG_CLASS_CODE.
*      WA_ZPWEFA_UPS-CHRG_DESC_CODE1 = IS_ZPWEFA_UPS-CHRG_DESC_CODE.
*      WA_ZPWEFA_UPS-CHRG_DESC1 = IS_ZPWEFA_UPS-CHRG_DESC.
*      WA_ZPWEFA_UPS-CHRG_UNIT_QTY1 = IS_ZPWEFA_UPS-CHRG_UNIT_QTY.
*      WA_ZPWEFA_UPS-BAS_CURR_CODE1 = IS_ZPWEFA_UPS-BAS_CURR_CODE.
*      WA_ZPWEFA_UPS-BASIS_VALUE1 = IS_ZPWEFA_UPS-BASIS_VALUE.
*      WA_ZPWEFA_UPS-TAX_INDICATOR1 = IS_ZPWEFA_UPS-TAX_INDICATOR.
*      WA_ZPWEFA_UPS-TRANS_CURR_CODE1 = IS_ZPWEFA_UPS-TRANS_CURR_CODE.
*      WA_ZPWEFA_UPS-INCEN_AMT1 = IS_ZPWEFA_UPS-INCEN_AMT.
*      WA_ZPWEFA_UPS-NET_AMNT1 = IS_ZPWEFA_UPS-NET_AMNT.
*
*
*
*      MODIFY IT_ZPWEFA_UPS FROM WA_ZPWEFA_UPS INDEX V_ORGTAB.
*      CLEAR WA_ZPWEFA_UPS.
*
*      DELETE IT_ZPWEFA_UPS INDEX V_MODTAB.
*
*
**zcounter = zcounter + 1.
*      CONTINUE.
*    ELSEIF V_LEAD_SHPMENT = IS_ZPWEFA_UPS-LEAD_SHIP_NUMBER  AND ZCOUNTER = 2.
*
*      V_ORGTAB = V_ORGTAB - 1.
*      V_MODTAB = V_MODTAB.
*
*      READ TABLE IT_ZPWEFA_UPS INTO WA_ZPWEFA_UPS INDEX V_ORGTAB.
*
*      WA_ZPWEFA_UPS-CHRG_CLASS_CODE2 = IS_ZPWEFA_UPS-CHRG_CLASS_CODE.
*      WA_ZPWEFA_UPS-CHRG_DESC_CODE2 = IS_ZPWEFA_UPS-CHRG_DESC_CODE.
*      WA_ZPWEFA_UPS-CHRG_DESC2 = IS_ZPWEFA_UPS-CHRG_DESC.
*      WA_ZPWEFA_UPS-CHRG_UNIT_QTY2 = IS_ZPWEFA_UPS-CHRG_UNIT_QTY.
*      WA_ZPWEFA_UPS-BAS_CURR_CODE2 = IS_ZPWEFA_UPS-BAS_CURR_CODE.
*      WA_ZPWEFA_UPS-BASIS_VALUE2 = IS_ZPWEFA_UPS-BASIS_VALUE.
*      WA_ZPWEFA_UPS-TAX_INDICATOR2 = IS_ZPWEFA_UPS-TAX_INDICATOR.
*      WA_ZPWEFA_UPS-TRANS_CURR_CODE2 = IS_ZPWEFA_UPS-TRANS_CURR_CODE.
*      WA_ZPWEFA_UPS-INCEN_AMT2 = IS_ZPWEFA_UPS-INCEN_AMT.
*      WA_ZPWEFA_UPS-NET_AMNT2 = IS_ZPWEFA_UPS-NET_AMNT.
***Modify the internal table
*
*      MODIFY IT_ZPWEFA_UPS FROM WA_ZPWEFA_UPS INDEX V_ORGTAB.
*      CLEAR WA_ZPWEFA_UPS.
*
*      DELETE IT_ZPWEFA_UPS INDEX V_MODTAB.
**zcounter = zcounter + 1.
*      CONTINUE.
*    ELSEIF V_LEAD_SHPMENT = IS_ZPWEFA_UPS-LEAD_SHIP_NUMBER  AND ZCOUNTER = 3.
*
*      V_ORGTAB = V_ORGTAB - 1.
*      V_MODTAB = V_MODTAB.
*
*      READ TABLE IT_ZPWEFA_UPS INTO WA_ZPWEFA_UPS INDEX V_ORGTAB.
*
*      WA_ZPWEFA_UPS-CHRG_CLASS_CODE3 = IS_ZPWEFA_UPS-CHRG_CLASS_CODE.
*      WA_ZPWEFA_UPS-CHRG_DESC_CODE3 = IS_ZPWEFA_UPS-CHRG_DESC_CODE.
*      WA_ZPWEFA_UPS-CHRG_DESC3 = IS_ZPWEFA_UPS-CHRG_DESC.
*      WA_ZPWEFA_UPS-CHRG_UNIT_QTY3 = IS_ZPWEFA_UPS-CHRG_UNIT_QTY.
*      WA_ZPWEFA_UPS-BAS_CURR_CODE3 = IS_ZPWEFA_UPS-BAS_CURR_CODE.
*      WA_ZPWEFA_UPS-BASIS_VALUE3 = IS_ZPWEFA_UPS-BASIS_VALUE.
*      WA_ZPWEFA_UPS-TAX_INDICATOR3 = IS_ZPWEFA_UPS-TAX_INDICATOR.
*      WA_ZPWEFA_UPS-TRANS_CURR_CODE3 = IS_ZPWEFA_UPS-TRANS_CURR_CODE.
*      WA_ZPWEFA_UPS-INCEN_AMT3 = IS_ZPWEFA_UPS-INCEN_AMT.
*      WA_ZPWEFA_UPS-NET_AMNT3 = IS_ZPWEFA_UPS-NET_AMNT.
***Modify the internal table
*
*      MODIFY IT_ZPWEFA_UPS FROM WA_ZPWEFA_UPS INDEX V_ORGTAB.
*      CLEAR WA_ZPWEFA_UPS.
*
*      DELETE IT_ZPWEFA_UPS INDEX V_MODTAB.
**zcounter = zcounter + 1.
*      CONTINUE.
*    ELSEIF V_LEAD_SHPMENT = IS_ZPWEFA_UPS-LEAD_SHIP_NUMBER AND ZCOUNTER = 4.
*
*      V_ORGTAB = V_ORGTAB - 1.
*      V_MODTAB = V_MODTAB.
*
*      READ TABLE IT_ZPWEFA_UPS INTO WA_ZPWEFA_UPS INDEX V_ORGTAB.
*
*      WA_ZPWEFA_UPS-CHRG_CLASS_CODE4 = IS_ZPWEFA_UPS-CHRG_CLASS_CODE.
*      WA_ZPWEFA_UPS-CHRG_DESC_CODE4 = IS_ZPWEFA_UPS-CHRG_DESC_CODE.
*      WA_ZPWEFA_UPS-CHRG_DESC4 = IS_ZPWEFA_UPS-CHRG_DESC.
*      WA_ZPWEFA_UPS-CHRG_UNIT_QTY4 = IS_ZPWEFA_UPS-CHRG_UNIT_QTY.
*      WA_ZPWEFA_UPS-BAS_CURR_CODE4 = IS_ZPWEFA_UPS-BAS_CURR_CODE.
*      WA_ZPWEFA_UPS-BASIS_VALUE4 = IS_ZPWEFA_UPS-BASIS_VALUE.
*      WA_ZPWEFA_UPS-TAX_INDICATOR4 = IS_ZPWEFA_UPS-TAX_INDICATOR.
*      WA_ZPWEFA_UPS-TRANS_CURR_CODE4 = IS_ZPWEFA_UPS-TRANS_CURR_CODE.
*      WA_ZPWEFA_UPS-INCEN_AMT4 = IS_ZPWEFA_UPS-INCEN_AMT.
*      WA_ZPWEFA_UPS-NET_AMNT4 = IS_ZPWEFA_UPS-NET_AMNT.
*
*      MODIFY IT_ZPWEFA_UPS FROM WA_ZPWEFA_UPS INDEX V_ORGTAB.
*      CLEAR WA_ZPWEFA_UPS.
*
*      DELETE IT_ZPWEFA_UPS INDEX V_MODTAB.
*
*    ENDIF.
*  ENDLOOP.
*
*
*******Insert into ZPWEFA_UPS TABLE
*
*  LOOP AT IT_ZPWEFA_UPS INTO IS_ZPWEFA_UPS.
*    INSERT INTO ZPWEFA_UPS VALUES  IS_ZPWEFA_UPS.
*    CLEAR : IS_ZPWEFA_UPS.
*
*  ENDLOOP.






ENDFUNCTION.
