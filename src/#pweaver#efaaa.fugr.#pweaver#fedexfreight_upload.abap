FUNCTION /PWEAVER/FEDEXFREIGHT_UPLOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(PS_PLANT) TYPE  MARC-WERKS
*"     REFERENCE(P_DATE) TYPE  SY-DATUM
*"     REFERENCE(PS_CARR) TYPE  /PWEAVER/CCONFIG-CARRIERTYPE
*"  EXPORTING
*"     REFERENCE(SUBRC) TYPE  SY-SUBRC
*"  TABLES
*"      IT_DATA STRUCTURE  ALSMEX_TABLINE
*"----------------------------------------------------------------------

  DATA : is_data TYPE alsmex_tabline.
  DATA : it_zpwefa_fedexfr TYPE TABLE OF /pweaver/efa_ffr INITIAL SIZE 0,
         is_zpwefa_fedexfr TYPE /pweaver/efa_ffr.
    DATA : date(2),
         date_year(7),
         month(2),
         year(4),
         HH(2),
         MM(2),
         SS(2).
*    DELETE FROM /pweaver/efa_fFR.

LOOP AT IT_DATA INTO IS_DATA.

 IF IS_DATA-ROW = '0001'.
       CONTINUE.
     ENDIF.
     AT NEW ROW.
       CLEAR IS_ZPWEFA_FEDEXFR.
     ENDAT.
  IF IS_DATA-COL = '0001'.
    SPLIT is_data-value AT '.' INTO month date year.
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

       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXfr-PICKUP_DATE.
     ENDIF.
     IF IS_DATA-COL = '0002'.
       SPLIT is_data-value AT ':' INTO HH MM SS.
       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = HH
        IMPORTING
          output = HH.
         CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = MM
        IMPORTING
          output = MM.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = SS
        IMPORTING
          output = SS.
          if ss is INITIAL.
            ss = 00.
            endif.
         CONCATENATE HH ':'MM ':'ss INTO is_data-value.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-PICKUP_TIME.
     ENDIF.
     IF IS_DATA-COL = '0003'.
       SPLIT is_data-value AT '.' INTO month date year.
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
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-DELIVERY_DATE.
     ENDIF.
     IF IS_DATA-COL = '0004'.
        SPLIT is_data-value AT ':' INTO HH MM SS.
       CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = HH
        IMPORTING
          output = HH.
         CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = MM
        IMPORTING
          output = MM.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = SS
        IMPORTING
          output = SS.
         CONCATENATE HH MM  INTO is_data-value.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-DELIVERY_TIME.
     ENDIF.
     IF IS_DATA-COL = '0005'.
*        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
*        MOVE is_data-value TO is_zpwefa_fedex-org_amt_due.
*      ENDCATCH.

*      IF sy-subrc = 1.
*        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexfr-PRO_NUMBER.
*      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_AMT_DUE.
*     ENDIF.
     IF IS_DATA-COL = '0006'.
*        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
*        MOVE is_data-value TO is_zpwefa_fedex-current_balance.
*      ENDCATCH.

*      IF sy-subrc = 1.
*        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexfr-BOL_NUMBER.
*      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CURRENT_BALANCE.
*     ENDIF.
     IF IS_DATA-COL = '0007'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXfr-PO_NUMBER.
     ENDIF.
     IF IS_DATA-COL = '0008'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXfr-SHIPPERNAME.
     ENDIF.
     IF IS_DATA-COL = '0009'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXfr-CONSIGNEENAME.

     ENDIF.
     IF IS_DATA-COL = '0010'.
*        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
*        MOVE is_data-value TO is_zpwefa_fedex-tran_charg_amnt.
**      ENDCATCH.

*      IF sy-subrc = 1.
*        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexfr-SHIPPER_CITY.
*      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRAN_CHARG_AMNT.
*     ENDIF.
     IF IS_DATA-COL = '0011'.
*        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
*        MOVE is_data-value TO is_zpwefa_fedex-net_chrg_amnt.
*      ENDCATCH.

*      IF sy-subrc = 1.
*        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexfr-SHIPPER_STATE.
*      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-NET_CHRG_AMNT.
*     ENDIF.
     IF IS_DATA-COL = '0012'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXfr-SHIPPER_ZIP.
     ENDIF.
     IF IS_DATA-COL = '0013'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXfr-CONSIGNEE_CITY.
     ENDIF.
     IF IS_DATA-COL = '0014'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXfr-CONSIGNEE_STATE.
     ENDIF.
     IF IS_DATA-COL = '0015'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-CONSIGNEE_ZIP.
     ENDIF.
     IF IS_DATA-COL = '0016'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-HDLG_UNITS.
     ENDIF.
     IF IS_DATA-COL = '0017'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-SERVICE.
     ENDIF.
     IF IS_DATA-COL = '0018'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-PIECES.
     ENDIF.
     IF IS_DATA-COL = '0019'.
        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedexFR-WEIGHT.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexFR-WEIGHT.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ACTL_WGT_AMT.
*     ENDIF.
     IF IS_DATA-COL = '0020'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-TERMS.
     ENDIF.
     IF IS_DATA-COL = '0021'.
*       CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
*        MOVE is_data-value TO is_zpwefa_fedex-rated_wgt_amt.
*      ENDCATCH.

*      IF sy-subrc = 1.
*        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexFR-STD_DAYS.
      ENDIF.
*    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RATED_WGT_AMT.
*     ENDIF.
     IF IS_DATA-COL = '0022'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-ACT_DAYS.
     ENDIF.
     IF IS_DATA-COL = '0023'.
           CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedexFR-FSC.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexFR-FSC.
      ENDIF.

     ENDIF.
     IF IS_DATA-COL = '0024'.
               CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedexFR-NETCHARGES.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexFR-NETCHARGES.
      ENDIF.
     ENDIF.
     IF IS_DATA-COL = '0025'.
           CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedexFR-DISCOUNT.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedexFR-DISCOUNT.
      ENDIF.
     ENDIF.
      IF IS_DATA-COL = '0026'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-SHIPPER_1.
     ENDIF.
      IF IS_DATA-COL = '0027'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEXFR-SHIPPER_1.
     ENDIF.
     AT END OF ROW.
       APPEND IS_ZPWEFA_FEDEXFR TO IT_ZPWEFA_FEDEXFR.
       CLEAR : IS_ZPWEFA_FEDEXFR.
        IF SY-SUBRC = 0.
         SUBRC = 0.
       ELSEIF SY-SUBRC <> 0.
         SUBRC = 4.
       ENDIF.
     ENDAT.
     CLEAR : IS_DATA.
   ENDLOOP.
    LOOP AT IT_ZPWEFA_FEDEXFR INTO IS_ZPWEFA_FEDEXFR.
     IS_ZPWEFA_FEDEXFR-MANDT = SY-MANDT.
      IS_ZPWEFA_FEDEXfr-PLANT_ID = PS_PLANT.
     IS_ZPWEFA_FEDEXfr-CARRIER = PS_CARR.




     MODIFY /PWEAVER/EFA_FFR FROM IS_ZPWEFA_FEDEXFR.
*     endif.
     CLEAR IS_ZPWEFA_FEDEXFR.



   ENDLOOP.




ENDFUNCTION.
