FUNCTION /PWEAVER/FEDEX_INVOICE_UPLOAD.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(PS_PLANT) TYPE  MARC-WERKS OPTIONAL
*"     REFERENCE(P_DATE) TYPE  SY-DATUM
*"     REFERENCE(PS_CARR) TYPE  /PWEAVER/CCONFIG-CARRIERTYPE
*"  EXPORTING
*"     REFERENCE(SUBRC) TYPE  SY-SUBRC
*"  TABLES
*"      IT_DATA STRUCTURE  KCDE_CELLS
*"----------------------------------------------------------------------

  TABLES : /PWEAVER/EFA_FED,
           /PWEAVER/MANFEST.
*  DELETE FROM /pweaver/efa_fed.

   DATA : IS_DATA TYPE KCDE_CELLS.

   DATA : IT_ZPWEFA_FEDEX TYPE TABLE OF /PWEAVER/EFA_FED INITIAL SIZE 0,
          IS_ZPWEFA_FEDEX TYPE /PWEAVER/EFA_FED.
   DATA : IT_ZEFA_FEDEX TYPE STANDARD TABLE OF /PWEAVER/EFA_FED,
          WA_ZEFA_FEDEX TYPE /PWEAVER/EFA_FED.
  DATA : IT_zpwefa TYPE TABLE OF /PWEAVER/EFA_FED,
         IS_ZPWEFA TYPE /PWEAVER/EFA_FED.
  data :  is_zpwefa_fedextrc type /pweaver/manfest.
  data  :      v_diff_days type VTBBEWE-ATAGE.
  data : it_del_fedex type standard table of /PWEAVER/EFA_FED.

   DATA : V_LET(52) TYPE C VALUE  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.

   DATA : V_INVOICE_NUMBER TYPE /PWEAVER/EFA_FED-INVOICE_NUMBER.

   CLEAR : V_INVOICE_NUMBER.
   refresh : it_del_fedex.
*   delete from /PWEAVER/EFA_FED.

LOOP AT IT_DATA INTO IS_DATA.

     IF IS_DATA-ROW = '0001'.
       CONTINUE.
     ENDIF.
     AT NEW ROW.
       CLEAR IS_ZPWEFA_FEDEX.
     ENDAT.



     IF IS_DATA-COL = '0001'.

       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-BILL_TO_ACC_NUM.
     ENDIF.
     IF IS_DATA-COL = '0002'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-INVOICE_DATE.
     ENDIF.
     IF IS_DATA-COL = '0003'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-INVOICE_NUMBER.
     ENDIF.
     IF IS_DATA-COL = '0004'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-STORE_ID.
     ENDIF.
     IF IS_DATA-COL = '0005'.
        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-org_amt_due.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-org_amt_due.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_AMT_DUE.
*     ENDIF.
     IF IS_DATA-COL = '0006'.
        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-current_balance.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-current_balance.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CURRENT_BALANCE.
*     ENDIF.
     IF IS_DATA-COL = '0007'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-PAYOR.
     ENDIF.
     IF IS_DATA-COL = '0008'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRD_TRCK_ID.
     ENDIF.
     IF IS_DATA-COL = '0009'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-EXP_GRD_TRCK_ID.

     ENDIF.
     IF IS_DATA-COL = '0010'.
        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-tran_charg_amnt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-tran_charg_amnt.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRAN_CHARG_AMNT.
*     ENDIF.
     IF IS_DATA-COL = '0011'.
        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-net_chrg_amnt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-net_chrg_amnt.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-NET_CHRG_AMNT.
*     ENDIF.
     IF IS_DATA-COL = '0012'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SERVICE_TYPE.
     ENDIF.
     IF IS_DATA-COL = '0013'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRND_SERVICE.
     ENDIF.
     IF IS_DATA-COL = '0014'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPMENT_DATE.
     ENDIF.
     IF IS_DATA-COL = '0015'.
       if IS_DATA-VALUE ne '0'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-POD_DEL_DATE.
       endif.
     ENDIF.
     IF IS_DATA-COL = '0016'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-POD_DEL_TIME.
     ENDIF.
     IF IS_DATA-COL = '0017'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-POD_SER_AREA_COD.
     ENDIF.
     IF IS_DATA-COL = '0018'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-POD_SIGN_DESC.
     ENDIF.
     IF IS_DATA-COL = '0019'.
        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-actl_wgt_amt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-actl_wgt_amt.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ACTL_WGT_AMT.
*     ENDIF.
     IF IS_DATA-COL = '0020'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ACTL_WGT_UNITS.
     ENDIF.
     IF IS_DATA-COL = '0021'.
       CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-rated_wgt_amt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-rated_wgt_amt.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RATED_WGT_AMT.
*     ENDIF.
     IF IS_DATA-COL = '0022'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RATED_WGT_UNTS.
     ENDIF.
     IF IS_DATA-COL = '0023'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-NUM_PIECES.
     ENDIF.
     IF IS_DATA-COL = '0024'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-BUN_NUM.
     ENDIF.
     IF IS_DATA-COL = '0025'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-METER_NUM.
     ENDIF.
     IF IS_DATA-COL = '0026'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECIPEINT_NAME.
     ENDIF.
     IF IS_DATA-COL = '0027'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECIPEINT_COMP.
     ENDIF.
     IF IS_DATA-COL = '0028'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECP_ADDR_LINE1.
     ENDIF.
     IF IS_DATA-COL = '0029'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECP_ADDR_LINE2.
     ENDIF.
     IF IS_DATA-COL = '0030'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECIPIENT_CITY.
     ENDIF.
     IF IS_DATA-COL = '0031'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECIPIENT_STATE.
     ENDIF.
     IF IS_DATA-COL = '0032'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-REC_ZIP_CODE.
     ENDIF.
     IF IS_DATA-COL = '0033'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-REC_COUNTRY.
     ENDIF.
     IF IS_DATA-COL = '0034'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_COMPANY.
     ENDIF.
     IF IS_DATA-COL = '0035'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_NAME.
     ENDIF.
     IF IS_DATA-COL = '0036'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_ADD_LIN1.
     ENDIF.
     IF IS_DATA-COL = '0037'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_ADD_LIN2.
     ENDIF.
     IF IS_DATA-COL = '0038'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_CITY.
     ENDIF.
     IF IS_DATA-COL = '0039'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_STATE.
     ENDIF.
     IF IS_DATA-COL = '0040'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_ZIP_CODE.
     ENDIF.
     IF IS_DATA-COL = '0041'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_COUNTRY.
     ENDIF.
     IF IS_DATA-COL = '0042'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_CUST_REF.
     ENDIF.
     IF IS_DATA-COL = '0043'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_REF_2.
     ENDIF.
     IF IS_DATA-COL = '0044'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_REF_3.
     ENDIF.
     IF IS_DATA-COL = '0045'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_DEP_REF_DESC.
     ENDIF.
     IF IS_DATA-COL = '0046'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-UPD_CUST_REF.
     ENDIF.
     IF IS_DATA-COL = '0047'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-UPD_REF_2.
     ENDIF.
     IF IS_DATA-COL = '0048'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-UPD_REF_3.
     ENDIF.
     IF IS_DATA-COL = '0049'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_DEPT_REF_DES.
     ENDIF.
     IF IS_DATA-COL = '0050'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-UPD_DEPT_REF_DES.
     ENDIF.
     IF IS_DATA-COL = '0051'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_REC_ADD_LN1.
     ENDIF.
     IF IS_DATA-COL = '0052'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_REC_ADD_LN2.
     ENDIF.
     IF IS_DATA-COL = '0053'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_RECP_CITY.
     ENDIF.
     IF IS_DATA-COL = '0054'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_RECP_STATE.
     ENDIF.

     IF IS_DATA-COL = '0055'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_RECP_ZIP_COD.
     ENDIF.
     IF IS_DATA-COL = '0056'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_RECP_COUNTY.
     ENDIF.
     IF IS_DATA-COL = '0057'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ZONE_CODE.
     ENDIF.
     IF IS_DATA-COL = '0058'.
       if is_data-value ne '0'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ENTRY_DATE.
       endif.
     ENDIF.
     IF IS_DATA-COL = '0059'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ENTRY_NUMBER.
     ENDIF.
     IF IS_DATA-COL = '0060'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CUSTOMS_VALUE.
     ENDIF.
     IF IS_DATA-COL = '0061'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CUSTOM_VAL_CURR.
     ENDIF.
     IF IS_DATA-COL = '0062'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-DECLARED_VALUE.
     ENDIF.
     IF IS_DATA-COL = '0063'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-DEC_VAL_CURR_COD.
     ENDIF.
     IF IS_DATA-COL = '0064'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_DESC.
     ENDIF.
     IF IS_DATA-COL = '0065'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_CNTRY_CODE.
     ENDIF.
     IF IS_DATA-COL = '0066'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_DESC_1.
     ENDIF.
     IF IS_DATA-COL = '0067'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_CNTRY_CODE1.
     ENDIF.
     IF IS_DATA-COL = '0068'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_DESC_2.
     ENDIF.
     IF IS_DATA-COL = '0069'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_CNTRY_CODE2.
     ENDIF.

     IF IS_DATA-COL = '0070'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_DESC_3.
     ENDIF.
     IF IS_DATA-COL = '0071'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_CNTRY_CODE3.
     ENDIF.
     IF IS_DATA-COL = '0072'.
       if IS_DATA-VALUE ne '0'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CURR_CONV_DAT.
       endif.
     ENDIF.
     IF IS_DATA-COL = '0073'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CURR_CONV_RATE.
     ENDIF.
     IF IS_DATA-COL = '0074'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULTI_WGT_NUM.
     ENDIF.
     IF IS_DATA-COL = '0075'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULWGTTOTWGTUNT.
     ENDIF.
     IF IS_DATA-COL = '0076'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULWGTTOTWGTWGT.
     ENDIF.
     IF IS_DATA-COL = '0077'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULWGTTOTSHPAMT.
     ENDIF.
     IF IS_DATA-COL = '0078'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULWGTTOTSHPWGT.
     ENDIF.
     IF IS_DATA-COL = '0079'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRDTRCKIDADDCORR.
     ENDIF.
     IF IS_DATA-COL = '0080'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRDTRCKIDADDGRSS.
     ENDIF.
     IF IS_DATA-COL = '0081'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES.
     ENDIF.
     IF IS_DATA-COL = '0082'.
        CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT.
*     ENDIF.
     IF IS_DATA-COL = '0083'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES1.
     ENDIF.
     IF IS_DATA-COL = '0084'.
       CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt1.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt1.
      ENDIF.
    ENDIF.

*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT1.
*     ENDIF.
     IF IS_DATA-COL = '0085'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES2.
     ENDIF.
     IF IS_DATA-COL = '0086'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt2.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt2.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT2.
*     ENDIF.
     IF IS_DATA-COL = '0087'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES3.
     ENDIF.
     IF IS_DATA-COL = '0088'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt3.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt3.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT3.
*     ENDIF.
     IF IS_DATA-COL = '0089'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES4.
     ENDIF.
     IF IS_DATA-COL = '0090'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt4.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt4.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT4.
*     ENDIF.
     IF IS_DATA-COL = '0091'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES5.
     ENDIF.
     IF IS_DATA-COL = '0092'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt5.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt5.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT5.
*     ENDIF.
     IF IS_DATA-COL = '0093'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES6.
     ENDIF.
     IF IS_DATA-COL = '0094'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt6.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt6.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT6.
*     ENDIF.
     IF IS_DATA-COL = '0095'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES7.
     ENDIF.
     IF IS_DATA-COL = '0096'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt7.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt7.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT7.
*     ENDIF.
     IF IS_DATA-COL = '0097'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES8.
     ENDIF.
     IF IS_DATA-COL = '0098'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt8.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt8.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT8.
*     ENDIF.
     IF IS_DATA-COL = '0099'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES9.
     ENDIF.
     IF IS_DATA-COL = '0100'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt9.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt9.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT9.
*     ENDIF.
     IF IS_DATA-COL = '0101'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES10.
     ENDIF.
     IF IS_DATA-COL = '0102'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt10.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt10.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT10.
*     ENDIF.
     IF IS_DATA-COL = '0103'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES11.
     ENDIF.
     IF IS_DATA-COL = '0104'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt11.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt11.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT11.
*     ENDIF.
     IF IS_DATA-COL = '0105'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES12.
     ENDIF.
     IF IS_DATA-COL = '0106'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt12.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt12.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT12.
*     ENDIF.
     IF IS_DATA-COL = '0107'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES13.
     ENDIF.
     IF IS_DATA-COL = '0108'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt13.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt13.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT13.
*     ENDIF.
     IF IS_DATA-COL = '0109'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES14.
     ENDIF.
     IF IS_DATA-COL = '0110'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt14.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt14.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT14.
*     ENDIF.
     IF IS_DATA-COL = '0111'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES15.
     ENDIF.
     IF IS_DATA-COL = '0112'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt15.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt15.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT15.
*     ENDIF.
     IF IS_DATA-COL = '0113'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES16.
     ENDIF.
     IF IS_DATA-COL = '0114'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt16.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt16.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT16.
*     ENDIF.
     IF IS_DATA-COL = '0115'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES17.
     ENDIF.
     IF IS_DATA-COL = '0116'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt17.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt17.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT17.
*     ENDIF.
     IF IS_DATA-COL = '0117'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES18.
     ENDIF.
     IF IS_DATA-COL = '0118'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 18.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt1.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt18.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT18.
*     ENDIF.
     IF IS_DATA-COL = '0119'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES19.
     ENDIF.
     IF IS_DATA-COL = '0120'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt19.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt19.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT19.
*     ENDIF.
     IF IS_DATA-COL = '0121'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES20.
     ENDIF.
     IF IS_DATA-COL = '0122'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt20.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt20.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT20.
*     ENDIF.
     IF IS_DATA-COL = '0123'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES21.
     ENDIF.
     IF IS_DATA-COL = '0124'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt21.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt21.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT21.
*     ENDIF.
     IF IS_DATA-COL = '0125'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES22.
     ENDIF.
     IF IS_DATA-COL = '0126'.
          CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt22.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt22.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT22.
*     ENDIF.
     IF IS_DATA-COL = '0127'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES23.
     ENDIF.
     IF IS_DATA-COL = '0128'.
            CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt23.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt23.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT23.
*     ENDIF.
     IF IS_DATA-COL = '0129'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES24.
     ENDIF.
     IF IS_DATA-COL = '0130'.
            CATCH SYSTEM-EXCEPTIONS convt_no_number = 1.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt24.
      ENDCATCH.

      IF sy-subrc = 1.
        REPLACE ALL OCCURRENCES OF ',' IN is_data-value WITH '.'.
        MOVE is_data-value TO is_zpwefa_fedex-trk_id_chg_amt24.
      ENDIF.
    ENDIF.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT24.
*     ENDIF.
     IF IS_DATA-COL = '0131'.
       MOVE PS_PLANT TO IS_ZPWEFA_FEDEX-PLANT_ID.
     ENDIF.
     IF IS_DATA-COL = '0132'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRSFLAG.
     ENDIF.
     IF IS_DATA-COL = '0133'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRS_DESC.
     ENDIF.
     IF IS_DATA-COL = '0134'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-LOST_FLAG.
     ENDIF.
     IF IS_DATA-COL = '0135'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-LOST_DESC.
     ENDIF.
     IF IS_DATA-COL = '0136'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-DAM_FLAG.
     ENDIF.
     IF IS_DATA-COL = '0137'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-DAM_DESC.
     ENDIF.
     IF IS_DATA-COL = '0138'.
       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CUSTOMER_ID.
     ENDIF.

     AT END OF ROW.

*       IS_ZPWEFA_FEDEX-MANDT = SY-MANDT.
*       IS_ZPWEFA_FEDEX-PLANT_ID = PS_PLANT.
*       IS_ZPWEFA_FEDEX-CARRIER = PS_CARR.
*       IS_ZPWEFA_FEDEX-CREATE_DATE = SY-DATUM.
*       IS_ZPWEFA_FEDEX-CREATE_TIME = SY-UZEIT.
*       IS_ZPWEFA_FEDEX-CREATE_USER = SY-UNAME.
*       MODIFY ZPWEFA_FEDEX FROM IS_ZPWEFA_FEDEX.
       APPEND IS_ZPWEFA_FEDEX TO IT_ZPWEFA_FEDEX.
       WA_ZEFA_FEDEX-INVOICE_NUMBER = IS_ZPWEFA_FEDEX-INVOICE_NUMBER.
       append wa_zefa_fedex to it_zefa_fedex.
       CLEAR : IS_ZPWEFA_FEDEX,
               wa_zefa_fedex.

*modify table ZPWEFA_FEDEX from IS_ZPWEFA_FEDEX.
       IF SY-SUBRC = 0.
         SUBRC = 0.
       ELSEIF SY-SUBRC <> 0.
         SUBRC = 4.
       ENDIF.
     ENDAT.
     CLEAR : IS_DATA.
   ENDLOOP.
sort it_zpwefa_fedex by EXP_GRD_TRCK_ID.
   LOOP AT IT_ZPWEFA_FEDEX INTO IS_ZPWEFA_FEDEX.
     IS_ZPWEFA_FEDEX-MANDT = SY-MANDT.
     IS_ZPWEFA_FEDEX-PLANT_ID = PS_PLANT.
     IS_ZPWEFA_FEDEX-CARRIER = PS_CARR.
     IS_ZPWEFA_FEDEX-CREATE_DATE = SY-DATUM.
     IS_ZPWEFA_FEDEX-CREATE_TIME = SY-UZEIT.
     IS_ZPWEFA_FEDEX-CREATE_USER = SY-UNAME.
     clear : is_zpwefa_fedextrc.
     select single * from /PWEAVER/MANFEST into is_zpwefa_fedextrc where tracking_number = is_zpwefa_fedex-EXP_GRD_TRCK_ID.

      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
            EXPORTING
              I_DATE_FROM          = is_zpwefa_fedextrc-POD_DATE
*             I_KEY_DAY_FROM       =
              I_DATE_TO            = is_zpwefa_fedextrc-EXP_DEL_DATE
*             I_KEY_DAY_TO         =
*             I_FLG_SEPARATE       = ' '
           IMPORTING
             E_DAYS               = v_diff_days.
*             E_MONTHS             =
*             E_YEARS              =
                    .
          if v_diff_days is initial.
*
*
*
  else.
*
IS_ZPWEFA_FEDEX-GRSFLAG = 'X'."P_WA_UPS_GSR-GSR_STATUS'.
*
    endif.

*     select single * from /pweaver/efa_fed INTO is_zpwefa where EXP_GRD_TRCK_ID  = is_zpwefa_fedex-EXP_GRD_TRCK_ID.
*       if sy-subrc = 0.
*    append is_zpwefa to it_zpwefa.
*    CLEAR : IS_ZPWEFA.
*    ENDIF.
     MODIFY IT_ZPWEFA_FEDEX FROM IS_ZPWEFA_FEDEX.
*     endif.
     CLEAR IS_ZPWEFA_FEDEX.



   ENDLOOP.
   clear :  WA_ZEFA_FEDEX.
DELETE ADJACENT DUPLICATES FROM IT_ZEFA_FEDEX.
CLEAR IS_ZPWEFA_FEDEX.
*     READ TABLE IT_ZPWEFA_FEDEX INTO IS_ZPWEFA_FEDEX INDEX 2.
     loop at it_zefa_fedex into WA_ZEFA_FEDEX.
     V_INVOICE_NUMBER = WA_ZEFA_FEDEX-INVOICE_NUMBER.
 SELECT * FROM /PWEAVER/EFA_FED INTO TABLE it_del_fedex WHERE invoice_number = v_invoice_nUMBER.
   IF sy-subrc eq 0.
      DELETE /PWEAVER/EFA_FED FROM TABLE it_del_fedex.
      IF sy-subrc ne 0.
        EXIT.
      ENDIF.
    ENDIF.
   CLEAR : WA_ZEFA_FEDEX.
*          V_INVOICE_NUMBER.
*     endloop.

     EXPORT V_INVOICE_NUMBER  to memory id 'ZINVOICE'.
     CLEAR : V_INVOICE_NUMBER.
     endloop.
*      sort it_zpwefa_fedex by EXP_GRD_TRCK_ID.

* read table it_zpwefa_fedex into is_zpwefa_fedex
*INSERT /pweaver/efa_ups FROM TABLE it_zpwefa_ups.
  insert /PWEAVER/EFA_FED FROM TABLE it_zpwefa_FEDEX ." accepting duplicate keys.
*   IF SY-SUBRC = 0.
*     CLEAR IS_ZPWEFA_FEDEX.
*     READ TABLE IT_ZPWEFA_FEDEX INTO IS_ZPWEFA_FEDEX INDEX 2.
*     V_INVOICE_NUMBER = IS_ZPWEFA_FEDEX-INVOICE_NUMBER.
*     EXPORT V_INVOICE_NUMBER  to memory id 'ZINVOICE'.
*
* endif.



*   LOOP AT IT_DATA INTO IS_DATA.
*
*     IF IS_DATA-ROW = '0001'.
*       CONTINUE.
*     ENDIF.
*     AT NEW ROW.
*       CLEAR IS_ZPWEFA_FEDEX.
*     ENDAT.
*
*
*
*     IF IS_DATA-COL = '0001'.
*
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-BILL_TO_ACC_NUM.
*     ENDIF.
*     IF IS_DATA-COL = '0002'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-INVOICE_DATE.
*     ENDIF.
*     IF IS_DATA-COL = '0003'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-INVOICE_NUMBER.
*     ENDIF.
*     IF IS_DATA-COL = '0004'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-STORE_ID.
*     ENDIF.
*     IF IS_DATA-COL = '0005'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_AMT_DUE.
*     ENDIF.
*     IF IS_DATA-COL = '0006'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CURRENT_BALANCE.
*     ENDIF.
*     IF IS_DATA-COL = '0007'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-PAYOR.
*     ENDIF.
*     IF IS_DATA-COL = '0008'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRD_TRCK_ID.
*     ENDIF.
*     IF IS_DATA-COL = '0009'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-EXP_GRD_TRCK_ID.
*
*     ENDIF.
*     IF IS_DATA-COL = '0010'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRAN_CHARG_AMNT.
*     ENDIF.
*     IF IS_DATA-COL = '0011'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-NET_CHRG_AMNT.
*     ENDIF.
*     IF IS_DATA-COL = '0012'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SERVICE_TYPE.
*     ENDIF.
*     IF IS_DATA-COL = '0013'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRND_SERVICE.
*     ENDIF.
*     IF IS_DATA-COL = '0014'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPMENT_DATE.
*     ENDIF.
*     IF IS_DATA-COL = '0015'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-POD_DEL_DATE.
*     ENDIF.
*     IF IS_DATA-COL = '0016'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-POD_DEL_TIME.
*     ENDIF.
*     IF IS_DATA-COL = '0017'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-POD_SER_AREA_COD.
*     ENDIF.
*     IF IS_DATA-COL = '0018'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-POD_SIGN_DESC.
*     ENDIF.
*     IF IS_DATA-COL = '0019'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ACTL_WGT_AMT.
*     ENDIF.
*     IF IS_DATA-COL = '0020'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ACTL_WGT_UNITS.
*     ENDIF.
*     IF IS_DATA-COL = '0021'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RATED_WGT_AMT.
*     ENDIF.
*     IF IS_DATA-COL = '0022'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RATED_WGT_UNTS.
*     ENDIF.
*     IF IS_DATA-COL = '0023'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-NUM_PIECES.
*     ENDIF.
*     IF IS_DATA-COL = '0024'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-BUN_NUM.
*     ENDIF.
*     IF IS_DATA-COL = '0025'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-METER_NUM.
*     ENDIF.
*     IF IS_DATA-COL = '0026'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECIPEINT_NAME.
*     ENDIF.
*     IF IS_DATA-COL = '0027'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECIPEINT_COMP.
*     ENDIF.
*     IF IS_DATA-COL = '0028'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECP_ADDR_LINE1.
*     ENDIF.
*     IF IS_DATA-COL = '0029'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECP_ADDR_LINE2.
*     ENDIF.
*     IF IS_DATA-COL = '0030'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECIPIENT_CITY.
*     ENDIF.
*     IF IS_DATA-COL = '0031'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-RECIPIENT_STATE.
*     ENDIF.
*     IF IS_DATA-COL = '0032'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-REC_ZIP_CODE.
*     ENDIF.
*     IF IS_DATA-COL = '0033'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-REC_COUNTRY.
*     ENDIF.
*     IF IS_DATA-COL = '0034'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_COMPANY.
*     ENDIF.
*     IF IS_DATA-COL = '0035'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_NAME.
*     ENDIF.
*     IF IS_DATA-COL = '0036'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_ADD_LIN1.
*     ENDIF.
*     IF IS_DATA-COL = '0037'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_ADD_LIN2.
*     ENDIF.
*     IF IS_DATA-COL = '0038'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_CITY.
*     ENDIF.
*     IF IS_DATA-COL = '0039'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_STATE.
*     ENDIF.
*     IF IS_DATA-COL = '0040'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_ZIP_CODE.
*     ENDIF.
*     IF IS_DATA-COL = '0041'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-SHIPPER_COUNTRY.
*     ENDIF.
*     IF IS_DATA-COL = '0042'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_CUST_REF.
*     ENDIF.
*     IF IS_DATA-COL = '0043'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_REF_2.
*     ENDIF.
*     IF IS_DATA-COL = '0044'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_REF_3.
*     ENDIF.
*     IF IS_DATA-COL = '0045'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_DEP_REF_DESC.
*     ENDIF.
*     IF IS_DATA-COL = '0046'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-UPD_CUST_REF.
*     ENDIF.
*     IF IS_DATA-COL = '0047'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-UPD_REF_2.
*     ENDIF.
*     IF IS_DATA-COL = '0048'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-UPD_REF_3.
*     ENDIF.
*     IF IS_DATA-COL = '0049'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_DEPT_REF_DES.
*     ENDIF.
*     IF IS_DATA-COL = '0050'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-UPD_DEPT_REF_DES.
*     ENDIF.
*     IF IS_DATA-COL = '0051'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_REC_ADD_LN1.
*     ENDIF.
*     IF IS_DATA-COL = '0052'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_REC_ADD_LN2.
*     ENDIF.
*     IF IS_DATA-COL = '0053'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_RECP_CITY.
*     ENDIF.
*     IF IS_DATA-COL = '0054'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_RECP_STATE.
*     ENDIF.
*
*     IF IS_DATA-COL = '0055'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_RECP_ZIP_COD.
*     ENDIF.
*     IF IS_DATA-COL = '0056'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ORG_RECP_COUNTY.
*     ENDIF.
*     IF IS_DATA-COL = '0057'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ZONE_CODE.
*     ENDIF.
*     IF IS_DATA-COL = '0058'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ENTRY_DATE.
*     ENDIF.
*     IF IS_DATA-COL = '0059'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-ENTRY_NUMBER.
*     ENDIF.
*     IF IS_DATA-COL = '0060'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CUSTOMS_VALUE.
*     ENDIF.
*     IF IS_DATA-COL = '0061'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CUSTOM_VAL_CURR.
*     ENDIF.
*     IF IS_DATA-COL = '0062'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-DECLARED_VALUE.
*     ENDIF.
*     IF IS_DATA-COL = '0063'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-DEC_VAL_CURR_COD.
*     ENDIF.
*     IF IS_DATA-COL = '0064'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_DESC.
*     ENDIF.
*     IF IS_DATA-COL = '0065'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_CNTRY_CODE.
*     ENDIF.
*     IF IS_DATA-COL = '0066'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_DESC_1.
*     ENDIF.
*     IF IS_DATA-COL = '0067'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_CNTRY_CODE1.
*     ENDIF.
*     IF IS_DATA-COL = '0068'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_DESC_2.
*     ENDIF.
*     IF IS_DATA-COL = '0069'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_CNTRY_CODE2.
*     ENDIF.
*
*     IF IS_DATA-COL = '0070'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_DESC_3.
*     ENDIF.
*     IF IS_DATA-COL = '0071'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-COMM_CNTRY_CODE3.
*     ENDIF.
*     IF IS_DATA-COL = '0072'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CURR_CONV_DAT.
*     ENDIF.
*     IF IS_DATA-COL = '0073'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CURR_CONV_RATE.
*     ENDIF.
*     IF IS_DATA-COL = '0074'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULTI_WGT_NUM.
*     ENDIF.
*     IF IS_DATA-COL = '0075'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULWGTTOTWGTUNT.
*     ENDIF.
*     IF IS_DATA-COL = '0076'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULWGTTOTWGTWGT.
*     ENDIF.
*     IF IS_DATA-COL = '0077'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULWGTTOTSHPAMT.
*     ENDIF.
*     IF IS_DATA-COL = '0078'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-MULWGTTOTSHPWGT.
*     ENDIF.
*     IF IS_DATA-COL = '0079'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRDTRCKIDADDCORR.
*     ENDIF.
*     IF IS_DATA-COL = '0080'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRDTRCKIDADDGRSS.
*     ENDIF.
*     IF IS_DATA-COL = '0081'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES.
*     ENDIF.
*     IF IS_DATA-COL = '0082'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT.
*     ENDIF.
*     IF IS_DATA-COL = '0083'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES1.
*     ENDIF.
*     IF IS_DATA-COL = '0084'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT1.
*     ENDIF.
*     IF IS_DATA-COL = '0085'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES2.
*     ENDIF.
*     IF IS_DATA-COL = '0086'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT2.
*     ENDIF.
*     IF IS_DATA-COL = '0087'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES3.
*     ENDIF.
*     IF IS_DATA-COL = '0088'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT3.
*     ENDIF.
*     IF IS_DATA-COL = '0089'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES4.
*     ENDIF.
*     IF IS_DATA-COL = '0090'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT4.
*     ENDIF.
*     IF IS_DATA-COL = '0091'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES5.
*     ENDIF.
*     IF IS_DATA-COL = '0092'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT5.
*     ENDIF.
*     IF IS_DATA-COL = '0093'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES6.
*     ENDIF.
*     IF IS_DATA-COL = '0094'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT6.
*     ENDIF.
*     IF IS_DATA-COL = '0095'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES7.
*     ENDIF.
*     IF IS_DATA-COL = '0096'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT7.
*     ENDIF.
*     IF IS_DATA-COL = '0097'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES8.
*     ENDIF.
*     IF IS_DATA-COL = '0098'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT8.
*     ENDIF.
*     IF IS_DATA-COL = '0099'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES9.
*     ENDIF.
*     IF IS_DATA-COL = '0100'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT9.
*     ENDIF.
*     IF IS_DATA-COL = '0101'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES10.
*     ENDIF.
*     IF IS_DATA-COL = '0102'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT10.
*     ENDIF.
*     IF IS_DATA-COL = '0103'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES11.
*     ENDIF.
*     IF IS_DATA-COL = '0104'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT11.
*     ENDIF.
*     IF IS_DATA-COL = '0105'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES12.
*     ENDIF.
*     IF IS_DATA-COL = '0106'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT12.
*     ENDIF.
*     IF IS_DATA-COL = '0107'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES13.
*     ENDIF.
*     IF IS_DATA-COL = '0108'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT13.
*     ENDIF.
*     IF IS_DATA-COL = '0109'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES14.
*     ENDIF.
*     IF IS_DATA-COL = '0110'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT14.
*     ENDIF.
*     IF IS_DATA-COL = '0111'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES15.
*     ENDIF.
*     IF IS_DATA-COL = '0112'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT15.
*     ENDIF.
*     IF IS_DATA-COL = '0113'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES16.
*     ENDIF.
*     IF IS_DATA-COL = '0114'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT16.
*     ENDIF.
*     IF IS_DATA-COL = '0115'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES17.
*     ENDIF.
*     IF IS_DATA-COL = '0116'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT17.
*     ENDIF.
*     IF IS_DATA-COL = '0117'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES18.
*     ENDIF.
*     IF IS_DATA-COL = '0118'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT18.
*     ENDIF.
*     IF IS_DATA-COL = '0119'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES19.
*     ENDIF.
*     IF IS_DATA-COL = '0120'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT19.
*     ENDIF.
*     IF IS_DATA-COL = '0121'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES20.
*     ENDIF.
*     IF IS_DATA-COL = '0122'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT20.
*     ENDIF.
*     IF IS_DATA-COL = '0123'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES21.
*     ENDIF.
*     IF IS_DATA-COL = '0124'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT21.
*     ENDIF.
*     IF IS_DATA-COL = '0125'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES22.
*     ENDIF.
*     IF IS_DATA-COL = '0126'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT22.
*     ENDIF.
*     IF IS_DATA-COL = '0127'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES23.
*     ENDIF.
*     IF IS_DATA-COL = '0128'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT23.
*     ENDIF.
*     IF IS_DATA-COL = '0129'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_DES24.
*     ENDIF.
*     IF IS_DATA-COL = '0130'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-TRK_ID_CHG_AMT24.
*     ENDIF.
*     IF IS_DATA-COL = '0131'.
*       MOVE PS_PLANT TO IS_ZPWEFA_FEDEX-PLANT_ID.
*     ENDIF.
*     IF IS_DATA-COL = '0132'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRSFLAG.
*     ENDIF.
*     IF IS_DATA-COL = '0133'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-GRS_DESC.
*     ENDIF.
*     IF IS_DATA-COL = '0134'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-LOST_FLAG.
*     ENDIF.
*     IF IS_DATA-COL = '0135'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-LOST_DESC.
*     ENDIF.
*     IF IS_DATA-COL = '0136'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-DAM_FLAG.
*     ENDIF.
*     IF IS_DATA-COL = '0137'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-DAM_DESC.
*     ENDIF.
*     IF IS_DATA-COL = '0138'.
*       MOVE IS_DATA-VALUE TO IS_ZPWEFA_FEDEX-CUSTOMER_ID.
*     ENDIF.
*
*     AT END OF ROW.
*
**       IS_ZPWEFA_FEDEX-MANDT = SY-MANDT.
**       IS_ZPWEFA_FEDEX-PLANT_ID = PS_PLANT.
**       IS_ZPWEFA_FEDEX-CARRIER = PS_CARR.
**       IS_ZPWEFA_FEDEX-CREATE_DATE = SY-DATUM.
**       IS_ZPWEFA_FEDEX-CREATE_TIME = SY-UZEIT.
**       IS_ZPWEFA_FEDEX-CREATE_USER = SY-UNAME.
**       MODIFY ZPWEFA_FEDEX FROM IS_ZPWEFA_FEDEX.
*       APPEND IS_ZPWEFA_FEDEX TO IT_ZPWEFA_FEDEX.
*       CLEAR : IS_ZPWEFA_FEDEX.
*
**modify table ZPWEFA_FEDEX from IS_ZPWEFA_FEDEX.
*       IF SY-SUBRC = 0.
*         SUBRC = 0.
*       ELSEIF SY-SUBRC <> 0.
*         SUBRC = 4.
*       ENDIF.
*     ENDAT.
*     CLEAR : IS_DATA.
*   ENDLOOP.
*
*   LOOP AT IT_ZPWEFA_FEDEX INTO IS_ZPWEFA_FEDEX.
*     IS_ZPWEFA_FEDEX-MANDT = SY-MANDT.
*     IS_ZPWEFA_FEDEX-PLANT_ID = PS_PLANT.
*     IS_ZPWEFA_FEDEX-CARRIER = PS_CARR.
*     IS_ZPWEFA_FEDEX-CREATE_DATE = SY-DATUM.
*     IS_ZPWEFA_FEDEX-CREATE_TIME = SY-UZEIT.
*     IS_ZPWEFA_FEDEX-CREATE_USER = SY-UNAME.
*     MODIFY ZPWEFA_FEDEX FROM IS_/PWEAVER/EFA_FED.
*     CLEAR IS_/PWEAVER/EFA_FED.
*
*
*
*   ENDLOOP.
*   IF SY-SUBRC = 0.
*     CLEAR IS_/PWEAVER/EFA_FED.
*     READ TABLE IT_/PWEAVER/EFA_FED INTO IS_/PWEAVER/EFA_FED INDEX 2.
*     V_INVOICE_NUMBER = IS_/PWEAVER/EFA_FED-INVOICE_NUMBER.
*     EXPORT V_INVOICE_NUMBER  to memory id 'ZINVOICE'.
*
*   ENDIF.







ENDFUNCTION.
