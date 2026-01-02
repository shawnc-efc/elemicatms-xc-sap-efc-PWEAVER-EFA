*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/EFA_INVOICE_HIST_FORM
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT
*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/EFA_INVOICE_HIST_FORM
*&---------------------------------------------------------------------*
FORM GET_DATA_INTERNAL_TABLE .


*  read table it_vrm_carrier into wa_vrm_carrier with key key = ps_carr.
*  if sy-subrc = 0.
*    ps_carr = wa_vrm_carrier-text.
*  endif.

  if ps_carr = 'FEDEX'.
***Get the distinct invoice numbers from Fedex
    refresh : it_output_fedex,it_invoice.

      select distinct invoice_number
                      from /pweaver/efa_fed into table it_invoice where invoice_date in s_date and
                                                                    invoice_number in invoice AND
                                                                    BILL_TO_ACC_NUM IN S_ACCT.

*****Gete the Invoice Date,customer ID and Carrier based on Invoice number
    if not it_invoice[] is initial.
      loop at it_invoice into wa_invoice.
        select single bill_to_acc_num
                      invoice_date
                      invoice_number
                      customer_id
                      carrier
                      create_date
                      create_time
                      create_user
          from /pweaver/efa_fed into wa_output_fedex
          where invoice_number = wa_invoice-invoice_number.

***Get the Net Sum for the Invoice
        data : test2(15) type p decimals 2,
               test3 type p decimals 2.
        clear : v_fedex_sum,
                TEST2.
        select sum( net_chrg_amnt ) from /pweaver/efa_fed into v_fedex_sum where invoice_number = wa_invoice-invoice_number.
        clear : test2.
        test2 = v_fedex_sum.
***Get the Earned Discount for the Invoice
        refresh : it_zpwefa_fedex.
        clear   : wa_zpwefa_fedex.
        select * from /pweaver/efa_fed into table it_zpwefa_fedex where invoice_number = wa_invoice-invoice_number.



        clear : v_earned_discount.
        clear : test3.
        loop at it_zpwefa_fedex into wa_zpwefa_fedex.

          if wa_zpwefa_fedex-trk_id_chg_des = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des = 'Performance Pricing'.
            v_earned_discount =  v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des1 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des1 ='Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt1.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des2 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des2 = 'Performance Pricing' .
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt2.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des3 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des3 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt3.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des4 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des4 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt4.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des5 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des5 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt5.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des6 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des6 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt6.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des7 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des7 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt7.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des8 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des8 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt8.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des9 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des9 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt9.
          endif.


          if wa_zpwefa_fedex-trk_id_chg_des10 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des10 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt10.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des11 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des11 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt11.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des12 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des12 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt12.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des13 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des13 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt13.
          endif.


          if wa_zpwefa_fedex-trk_id_chg_des14 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des14 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt14.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des15 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des15 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt15.
          endif.


          if wa_zpwefa_fedex-trk_id_chg_des16 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des16 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt16.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des17 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des17 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt17.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des18 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des18 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt18.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des19 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des19 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt19.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des20 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des20 ='Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt20.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des21 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des21 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt21.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des22 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des22 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt22.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des23 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des23 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt23.
          endif.

          if wa_zpwefa_fedex-trk_id_chg_des24 = 'Discount' OR wa_zpwefa_fedex-trk_id_chg_des24 = 'Performance Pricing'.
            v_earned_discount =   v_earned_discount + wa_zpwefa_fedex-trk_id_chg_amt24.
          endif.
          clear : wa_zpwefa_fedex.
        endloop.


        if sy-subrc = 0.
          wa_output-account_no =    wa_output_fedex-account_no.
          wa_output-invoice_date = wa_output_fedex-invoice_date.
          wa_output-invoice_number = wa_output_fedex-invoice_number.
          wa_output-customer_id = wa_output_fedex-customer_id.
          wa_output-carrier = wa_output_fedex-carrier.
          wa_output-create_date = wa_output_fedex-create_date.
          wa_output-create_time = wa_output_fedex-create_time.
          wa_output-create_user = wa_output_fedex-create_user.
*          wa_output-net_value = v_fedex_sum.
          wa_output-net_value = test2.
          test3 = v_earned_discount.
          test3 = test3 * ( -1 ).
          wa_output-earned_discount = test3.


          append  wa_output to it_output.
          clear wa_output.
        endif.
      endloop.
    endif.
     IF IT_OUTPUT[] IS NOT INITIAL.
       SORT IT_OUTPUT by invoice_date descending.
      ENDIF.

  endif.

  if ps_carr = 'UPS'.
** Get the distinct invoice number from UPS

    refresh : it_output_ups,it_invoice.
    select distinct invoice_number invoice_date
           from /pweaver/efa_ups into table it_invoice where invoice_date in s_date and
                                                                  invoice_number in invoice AND
                                                                  ACCOUNT_NUMBER IN S_ACCT.

    data : v_handling_charges type /pweaver/efa_fed-net_chrg_amnt,
           v_discount TYPE /pweaver/efa_ups-incen_amt,
           v_discount_ship TYPE /pweaver/efa_ups-incen_amt,
           v_discount_handle TYPE /pweaver/efa_ups-incen_amt,
           v_discount_chandle TYPE /pweaver/efa_ups-incen_amt,
           v_discount_cship TYPE /pweaver/efa_ups-incen_amt,
           v_discount_mincent TYPE /pweaver/efa_ups-incen_amt,
           v_discount_inv TYPE /pweaver/efa_ups-incen_amt   .
     data : test4 type p decimals 2,
               test5 type p decimals 2.

    SORT it_invoice by invoice_date DESCENDING.

    LOOP AT it_invoice into wa_invoice.

      clear : v_handling_charges,
            test4.
      select single invoice_amount from /pweaver/efa_ups into v_handling_charges
                                   where invoice_number = wa_invoice-invoice_number.
***************** Discount
        clear :  v_discount,
                 test5.
        select sum( incen_amt ) from /pweaver/efa_ups into v_discount where invoice_number = wa_invoice-invoice_number.

*        clear : v_discount_ship, v_discount_handle,v_discount_chandle,v_discount_cship,v_discount_mincent   .
*        select sum( SHIP_INCENT_AMNT ) from /pweaver/efa_ups_summ into v_discount_ship
*          where invoice_number = wa_invoice-invoice_number.
*
*        select sum( HANDLE_INCEN_AMT ) from /pweaver/efa_ups_summ into v_discount_handle
*          where invoice_number = wa_invoice-invoice_number.
*
*        select sum( CORR_HANDLE_DISC ) from /pweaver/efa_ups_summ into v_discount_chandle
*          where invoice_number = wa_invoice-invoice_number.
*
*        select sum( CORR_SHIP_INCENT ) from /pweaver/efa_ups_summ into v_discount_cship
*          where invoice_number = wa_invoice-invoice_number.
*
*        select sum( MISC_INCENT_CHRG ) from /pweaver/efa_ups_summ into v_discount_mincent
*          where invoice_number = wa_invoice-invoice_number.
*
*        select sum( INV_ADD_DISC ) from /pweaver/efa_ups_summ into v_discount_inv
*          where invoice_number = wa_invoice-invoice_number.
*
*          v_discount =  v_discount_ship + v_discount_handle "+ v_discount_chandle + v_discount_cship
*                      + v_discount_mincent + v_discount_inv  .

      select single account_number
                    invoice_date
                    invoice_number
                    customer_id
                    carrier
                    create_date
                    create_time
                    create_user
              from  /pweaver/efa_ups into wa_output_ups
              where invoice_number = wa_invoice-invoice_number.

      if sy-subrc = 0.
        wa_output-account_no =    wa_output_ups-account_no.
        wa_output-invoice_date = wa_output_ups-invoice_date.
        wa_output-invoice_number = wa_output_ups-invoice_number.
        wa_output-customer_id = wa_output_ups-customer_id.
        wa_output-carrier = wa_output_ups-carrier.
        wa_output-create_date = wa_output_ups-create_date.
        wa_output-create_time = wa_output_ups-create_time.
        wa_output-create_user = wa_output_ups-create_user.
        test4 = v_handling_charges.
        wa_output-net_value   = test4.
        test5 = v_discount.
        wa_output-earned_discount = v_discount.
        append  wa_output to it_output.
        clear wa_output.
      endif.
    ENDLOOP.
    IF IT_OUTPUT[] IS NOT INITIAL.
       SORT IT_OUTPUT by invoice_date descending.
      ENDIF.

    IF p_cons IS NOT INITIAL.

      LOOP AT it_output INTO wa_output.
        wa_output_temp = wa_output.
        at END OF account_no.
        sum.
        wa_cons_output-account_no = wa_output_temp-account_no.
        IF s_date IS NOT INITIAL.
        wa_cons_output-low   = s_date-low.
        wa_cons_output-high  = s_date-high.
        wa_cons_output-date_low  = s_date-low.
        wa_cons_output-date_high = s_date-high.
        ELSEIF invoice IS NOT INITIAL.
        wa_cons_output-low   = invoice-low.
        wa_cons_output-high  = invoice-high.
        wa_cons_output-invoice_low   = invoice-low.
        wa_cons_output-invoice_high  = invoice-high.
        ELSE.
        wa_cons_output-low   = 'NA'.
        wa_cons_output-high  = 'NA'.
        ENDIF.
        wa_cons_output-net_value  = wa_output-net_value.
        wa_cons_output-carrier    = wa_output_temp-carrier.
        APPEND wa_cons_output to it_cons_output.
        clear  wa_cons_output.
        endat.
      clear wa_output.
      ENDLOOP.

    ENDIF.

***************** Invoice Amount
*      clear v_handling_charges.
*      select single invoice_amount from /pweaver/efa_ups into v_handling_charges where invoice_date in s_date and
*                                                                                invoice_number in invoice.
*
****************** Discount
*        clear v_discount.
*        select sum( incen_amt ) from /pweaver/efa_ups into v_discount where invoice_date in s_date and
*                                                                       invoice_number in invoice.


*****Gete the Invoice Date,customer ID and Carrier based on Invoice number
*    if not it_invoice[] is initial.
*      loop at it_invoice into wa_invoice.
*
*
*
*
*
*      endloop.
*    endif.
  endif.
  if ps_carr = 'FEDEX FRT'.
* select distinct PRO_NUMBER PICKUP_DATE
*           from /PWEAVER/EFA_FFR into table it_PRONUMBER where PICKUP_DATE  in s_Pdate .
 LOOP AT it_PRONUMBER into wa_PRONUMBER.



 ENDLOOP.

  ENDIF.
*****Append to final intenal table



endform.                    " GET_DATA_INTERNAL_TABLE
*&---------------------------------------------------------------------*
*&      Form  WRITE_REPORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form write_report .

IF p_cons IS INITIAL.

  if not it_output[] is initial.
    loop at it_output into wa_output.
*WRITE:/ GV_CB AS CHECKBOX,
*wa_output-account_no.
      data : v_date(10) type c.
      clear : v_date.

*      concatenate wa_output-invoice_date+0(4)  wa_output-invoice_date+4(2)   wa_output-invoice_date+6(2) into v_date.
     concatenate  wa_output-invoice_date+4(2) '/'  wa_output-invoice_date+6(2) '/' wa_output-invoice_date+0(4) into v_date.
*    write : / sy-uline.
      write: /1 sy-vline,
      2 gv_cb as checkbox,
      4 sy-vline,
      5 wa_output-invoice_number,
      21  sy-vline,
      23 v_date,
      35 sy-vline,
      36 wa_output-account_no,
      48 sy-vline,
      49 wa_output-net_value right-justified,
*      49 wa_output-net_value ,
      66 sy-vline,
      70 wa_output-earned_discount right-justified,
      90 sy-vline.
*      64 sy-vline,
*      65 wa_output-earned_discount right-justified,
*      80 sy-vline.
*      81 WA_OUTPUT-CUSTOMER_ID,
*      93 SY-VLINE,
*      94 WA_OUTPUT-ACCOUNT_NO,
*      105 SY-VLINE.
      write : /1(90) sy-uline.
      clear : wa_output.
    endloop.
    clear : wa_output.
  else.
*  perform page_header.
    write :/ 'The report doesn''t contain any values'.
  endif.
ELSE.

  if not it_cons_output is initial.
    loop at it_cons_output into wa_cons_output.
*    write : / sy-uline.
      write: /1 sy-vline,
      2 gv_cb as checkbox,
      4 sy-vline,
      5 wa_cons_output-account_no,
      21  sy-vline,
      23 wa_cons_output-low,
      40 sy-vline,
      42 wa_cons_output-high,
      60 sy-vline,
      61 wa_cons_output-net_value right-justified,
      84 sy-vline.
*      70 wa_output-earned_discount right-justified,
*      90 sy-vline.
*      64 sy-vline,
*      65 wa_output-earned_discount right-justified,
*      80 sy-vline.
*      81 WA_OUTPUT-CUSTOMER_ID,
*      93 SY-VLINE,
*      94 WA_OUTPUT-ACCOUNT_NO,
*      105 SY-VLINE.
      write : /1(84) sy-uline.
    endloop.
    clear : wa_output.
  else.
*  perform page_header.
    write :/ 'The report doesn''t contain any values'.
  endif.
ENDIF.

endform.                    " WRITE_REPORT
*&---------------------------------------------------------------------*
*&      Form  PAGE_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form page_header .
*if ps_carr = 'UPS' OR PS_CARR = 'FEDEX'.
IF p_cons IS INITIAL.

  write : /1(90) sy-uline.
  write: /1 sy-vline,

  4 sy-vline,
  5 'Invoice #',
  21  sy-vline,
  22 'Invoice Date',
  35 sy-vline,
  36 'Account #',
  48 sy-vline,
  49 '      Net Charge',
  66  sy-vline,
  70 '     Earned Discount',
  90 sy-vline.
*  64  sy-vline,
*  65 'Earned Discount',
*  80 sy-vline.
*  81 'Customer Id',
*  93 SY-VLINE,
*  94 'Account No',
*  105 SY-VLINE.
  write : /1(90) sy-uline.

ELSE.

  write : /1(84) sy-uline.
  write: /1 sy-vline,

  4 sy-vline,
  5 'Account #' COLOR COL_GROUP,
  21  sy-vline,
  22 'From' COLOR COL_GROUP,
  40 sy-vline,
  41 'To' COLOR COL_GROUP,
  60 sy-vline,
  61 '           Net Charge' COLOR COL_GROUP,
  84  sy-vline.
*  70 '     Earned Discount',
*  90 sy-vline.
*  64  sy-vline,
*  65 'Earned Discount',
*  80 sy-vline.
*  81 'Customer Id',
*  93 SY-VLINE,
*  94 'Account No',
*  105 SY-VLINE.
  write : /1(84) sy-uline.

ENDIF.
*ELSEIF  PS_CARR = 'FEDEX FRT'.
*   write : /1(90) sy-uline.
*  write: /1 sy-vline,
*
*  4 sy-vline,
*  5 'Pro Number',
*  21  sy-vline,
*  22 'Delivery Date',
*  35 sy-vline,
*  36 'PO Number',
*  48 sy-vline,
*  49 '          NetCharges',
*  69  sy-vline,
*  70 '    Discount',
*  90 sy-vline.
**  64  sy-vline,
**  65 'Earned Discount',
**  80 sy-vline.
**  81 'Customer Id',
**  93 SY-VLINE,
**  94 'Account No',
**  105 SY-VLINE.
*  write : /1(90) sy-uline.
*
*
*ENDIF.


endform.                    " PAGE_HEADER
*&---------------------------------------------------------------------*
*&      Form  delete_rows
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form delete_rows .
  do lines times.
    sy_index = sy-index.
* read line sy-index field value  GV_CB into V_CH.

    clear : v_ch,v_inv_no,v_carr.
    read line sy-index field value gv_cb into v_ch.
    if v_ch = 'X'.

      clear :wa_output.
      read line sy-index field value wa_output-invoice_number into  v_inv_no.
      read line sy-index field value wa_output-carrier into  v_carr.


      read table it_output into wa_output with  key invoice_number = v_inv_no
                                                     carrier = v_carr.

      if sy-subrc = 0.

        delete it_output index sy-tabix.
        if v_carr = 'UPS'.
          delete from /pweaver/efa_ups where invoice_number = v_inv_no.

          submit zefa_inv_history.
        endif.

        if v_carr = 'FEDEX'.
          delete  from /pweaver/efa_fed where invoice_number = v_inv_no.
          submit zefa_inv_history.
        endif.

***Delete the values from the table.


      endif.

    endif.
  enddo.

endform.                    " delete_rows
*&---------------------------------------------------------------------*
*&      Form  diplay_efa
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form display_efa .

  clear : it_export_output[].
  refresh : it_export_output[].
  delete  from memory id 'ZPWEFA_DIS'.
  free memory id 'ZPWEFA_DIS'.

  data : v_lines(3) type c.

  IF p_cons IS NOT INITIAL.

  describe table it_cons_output lines v_lines.
  do lines times. "lines
    sy_index = sy-index.

    clear : v_ch,v_inv_no,v_carr,v_acct_no.
    read line sy-index field value gv_cb into v_ch.
    clear :wa_output.
    read line sy-index field value wa_cons_output-account_no into  v_acct_no.


    read table it_cons_output into wa_cons_output with  key account_no = v_acct_no.
    if sy-subrc eq 0.
      if v_ch eq 'X'.
        LOOP AT it_output into wa_output where account_no = v_acct_no.
        wa_export_output-gv_cb  = 'X'.
        wa_export_output-account_no   = wa_output-account_no.
        wa_export_output-invoice_date  = wa_output-invoice_date.
        wa_export_output-invoice_number = wa_output-invoice_number.
        wa_export_output-customer_id =  wa_output-customer_id.
        wa_export_output-carrier = wa_output-carrier.
        wa_export_output-create_date =  wa_output-create_date.
        wa_export_output-create_time = wa_output-create_time.
        wa_export_output-create_user = wa_output-create_user.
        wa_export_output-net_value = wa_output-net_value.
        wa_export_output-earned_discount = wa_output-earned_discount.
        append wa_export_output to it_export_output.
        clear : wa_export_output.
        ENDLOOP.

       wa_cons_export-gv_cb = 'X'.
       wa_cons_export-account_no  = wa_cons_output-account_no.
       wa_cons_export-low         = wa_cons_output-low.
       wa_cons_export-high        = wa_cons_output-high.
       wa_cons_export-date_low    = wa_cons_output-date_low.
       wa_cons_export-date_high   = wa_cons_output-date_high.
       wa_cons_export-invoice_low = wa_cons_output-invoice_low.
       wa_cons_export-invoice_high = wa_cons_output-invoice_high.
       wa_cons_export-carrier    = wa_cons_output-carrier.
       wa_cons_export-net_value   = wa_cons_output-net_value.
       append wa_cons_export to it_cons_export.
       clear : wa_cons_export.

      endif.
    endif.
    enddo.
  else.
  describe table it_output lines v_lines.
  do lines times.  "lines
    sy_index = sy-index.
* read line sy-index field value  GV_CB into V_CH.

    clear : v_ch,v_inv_no,v_carr.
    read line sy-index field value gv_cb into v_ch.
    clear :wa_output.
    read line sy-index field value wa_output-invoice_number into  v_inv_no.
*      READ LINE SY-INDEX FIELD VALUE WA_OUTPUT-CARRIER INTO  V_CARR.


    read table it_output into wa_output with  key invoice_number = v_inv_no.
    if sy-subrc = 0.
      if v_ch = 'X'.
        wa_export_output-gv_cb  = 'X'.
        wa_export_output-account_no   = wa_output-account_no.
        wa_export_output-invoice_date  = wa_output-invoice_date.
        wa_export_output-invoice_number = wa_output-invoice_number.
        wa_export_output-customer_id =  wa_output-customer_id.
        wa_export_output-carrier = wa_output-carrier.
        wa_export_output-create_date =  wa_output-create_date.
        wa_export_output-create_time = wa_output-create_time.
        wa_export_output-create_user = wa_output-create_user.
        wa_export_output-net_value = wa_output-net_value.
        wa_export_output-earned_discount = wa_output-earned_discount.
        append wa_export_output to it_export_output.
        clear : wa_export_output.

      else.

        wa_export_output-gv_cb  = space.
        wa_export_output-account_no   = wa_output-account_no.
        wa_export_output-invoice_date  = wa_output-invoice_date.
        wa_export_output-invoice_number = wa_output-invoice_number.
        wa_export_output-customer_id =  wa_output-customer_id.
        wa_export_output-carrier = wa_output-carrier.
        wa_export_output-create_date =  wa_output-create_date.
        wa_export_output-create_time = wa_output-create_time.
        wa_export_output-create_user = wa_output-create_user.
        wa_export_output-net_value = wa_output-net_value.
        wa_export_output-earned_discount = wa_output-earned_discount.
        append wa_export_output to it_export_output.
        clear : wa_export_output.


      endif.
    endif.
  enddo.
  ENDIF.


  export it_cons_export to memory id 'ZPWEFA_CONS'.
  export it_export_output[] to memory id 'ZPWEFA_DIS'.

  submit /pweaver/efa_03 and return.
  endform.
*&---------------------------------------------------------------------*
*&      Form  SELECTALL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECTALL .
  IF p_cons IS INITIAL.
     write : /1(90) sy-uline.
  write: /1 sy-vline,

  4 sy-vline,
  5 'Invoice #',
  21  sy-vline,
  22 'Invoice Date',
  35 sy-vline,
  36 'Account #',
  48 sy-vline,
  49 '      Net Charge',
  66  sy-vline,
  70 '     Earned Discount',
  90 sy-vline.
*  64  sy-vline,
*  65 'Earned Discount',
*  80 sy-vline.
*  81 'Customer Id',
*  93 SY-VLINE,
*  94 'Account No',
*  105 SY-VLINE.
  write : /1(90) sy-uline.

  if not it_output[] is initial.
    gv_cb =   'X'.
    loop at it_output into wa_output.
*WRITE:/ GV_CB AS CHECKBOX,
*wa_output-account_no.
      data : v_date(10) type c.
      clear : v_date.

*      concatenate wa_output-invoice_date+0(4)  wa_output-invoice_date+4(2)   wa_output-invoice_date+6(2) into v_date.
     concatenate  wa_output-invoice_date+4(2) '/'  wa_output-invoice_date+6(2) '/' wa_output-invoice_date+0(4) into v_date.
*    write : / sy-uline.
      write: /1 sy-vline,
      2 gv_cb as checkbox,
      4 sy-vline,
      5 wa_output-invoice_number,
      21  sy-vline,
      23 v_date,
      35 sy-vline,
      36 wa_output-account_no,
      48 sy-vline,
      49 wa_output-net_value right-justified,
*      49 wa_output-net_value ,
      66 sy-vline,
      70 wa_output-earned_discount right-justified,
      90 sy-vline.
*      64 sy-vline,
*      65 wa_output-earned_discount right-justified,
*      80 sy-vline.
*      81 WA_OUTPUT-CUSTOMER_ID,
*      93 SY-VLINE,
*      94 WA_OUTPUT-ACCOUNT_NO,
*      105 SY-VLINE.
      write : /1(90) sy-uline.
      clear : wa_output.
    endloop.
    clear : wa_output.
  else.
*  perform page_header.
    write :/ 'The report doesn''t contain any values'.
  endif.
ELSE.

  if not it_cons_output is initial.
    loop at it_cons_output into wa_cons_output.
*    write : / sy-uline.
      write: /1 sy-vline,
      2 gv_cb as checkbox,
      4 sy-vline,
      5 wa_cons_output-account_no,
      21  sy-vline,
      23 wa_cons_output-low,
      40 sy-vline,
      42 wa_cons_output-high,
      60 sy-vline,
      61 wa_cons_output-net_value right-justified,
      84 sy-vline.
*      70 wa_output-earned_discount right-justified,
*      90 sy-vline.
*      64 sy-vline,
*      65 wa_output-earned_discount right-justified,
*      80 sy-vline.
*      81 WA_OUTPUT-CUSTOMER_ID,
*      93 SY-VLINE,
*      94 WA_OUTPUT-ACCOUNT_NO,
*      105 SY-VLINE.
      write : /1(84) sy-uline.
    endloop.
    clear : wa_output.
  else.
*  perform page_header.
    write :/ 'The report doesn''t contain any values'.
  endif.
ENDIF.

ENDFORM.                    " SELECTALL
*&---------------------------------------------------------------------*
*&      Form  DESELECT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM DESELECT .
  IF p_cons IS INITIAL.
     write : /1(90) sy-uline.
  write: /1 sy-vline,

  4 sy-vline,
  5 'Invoice #',
  21  sy-vline,
  22 'Invoice Date',
  35 sy-vline,
  36 'Account #',
  48 sy-vline,
  49 '      Net Charge',
  66  sy-vline,
  70 '     Earned Discount',
  90 sy-vline.
*  64  sy-vline,
*  65 'Earned Discount',
*  80 sy-vline.
*  81 'Customer Id',
*  93 SY-VLINE,
*  94 'Account No',
*  105 SY-VLINE.
  write : /1(90) sy-uline.

  if not it_output[] is initial.
*    gv_cb =   .
    loop at it_output into wa_output.
*WRITE:/ GV_CB AS CHECKBOX,
*wa_output-account_no.
      data : v_date(10) type c.
      clear : v_date,
              GV_CB.

*      concatenate wa_output-invoice_date+0(4)  wa_output-invoice_date+4(2)   wa_output-invoice_date+6(2) into v_date.
     concatenate  wa_output-invoice_date+4(2) '/'  wa_output-invoice_date+6(2) '/' wa_output-invoice_date+0(4) into v_date.
*    write : / sy-uline.
      write: /1 sy-vline,
      2 gv_cb as checkbox,
      4 sy-vline,
      5 wa_output-invoice_number,
      21  sy-vline,
      23 v_date,
      35 sy-vline,
      36 wa_output-account_no,
      48 sy-vline,
      49 wa_output-net_value right-justified,
*      49 wa_output-net_value ,
      66 sy-vline,
      70 wa_output-earned_discount right-justified,
      90 sy-vline.
*      64 sy-vline,
*      65 wa_output-earned_discount right-justified,
*      80 sy-vline.
*      81 WA_OUTPUT-CUSTOMER_ID,
*      93 SY-VLINE,
*      94 WA_OUTPUT-ACCOUNT_NO,
*      105 SY-VLINE.
      write : /1(90) sy-uline.
      clear : wa_output.
    endloop.
    clear : wa_output.
  else.
*  perform page_header.
    write :/ 'The report doesn''t contain any values'.
  endif.
ELSE.

  if not it_cons_output is initial.
    loop at it_cons_output into wa_cons_output.
*    write : / sy-uline.
      write: /1 sy-vline,
      2 gv_cb as checkbox,
      4 sy-vline,
      5 wa_cons_output-account_no,
      21  sy-vline,
      23 wa_cons_output-low,
      40 sy-vline,
      42 wa_cons_output-high,
      60 sy-vline,
      61 wa_cons_output-net_value right-justified,
      84 sy-vline.
*      70 wa_output-earned_discount right-justified,
*      90 sy-vline.
*      64 sy-vline,
*      65 wa_output-earned_discount right-justified,
*      80 sy-vline.
*      81 WA_OUTPUT-CUSTOMER_ID,
*      93 SY-VLINE,
*      94 WA_OUTPUT-ACCOUNT_NO,
*      105 SY-VLINE.
      write : /1(84) sy-uline.
    endloop.
    clear : wa_output.
  else.
*  perform page_header.
    write :/ 'The report doesn''t contain any values'.
  endif.
ENDIF.


ENDFORM.                    " DESELECT
