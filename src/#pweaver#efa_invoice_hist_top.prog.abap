*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/EFA_INVOICE_HIST_TOP
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT

TABLES : /pweaver/efa_fed,/pweaver/efa_ups,/PWEAVER/EFA_FFR.

TYPES : BEGIN OF ty_output,
        account_no      TYPE /pweaver/efa_fed-bill_to_acc_num,
        invoice_date    TYPE /pweaver/efa_fed-invoice_date,
        invoice_number  TYPE /pweaver/efa_fed-invoice_number,
        customer_id     TYPE /pweaver/efa_fed-customer_id,
        carrier         TYPE /pweaver/efa_fed-service_type,
        create_date     TYPE /pweaver/efa_fed-create_date,
        create_time     TYPE /pweaver/efa_fed-create_time,
        create_user     TYPE /pweaver/efa_fed-create_user,
*        net_value       TYPE /pweaver/efa_fed-net_chrg_amnt,
        net_value       TYPE p decimals 2,
*        test_VALUE(15) type p decimals 2,
*        earned_discount TYPE /pweaver/efa_fed-net_chrg_amnt,
         earned_discount TYPE p decimals 2,
        END OF ty_output.
TYPES : BEGIN OF TY_FFROUTPUT,
        PRO_NUMBER TYPE /PWEAVER/EFA_FFR-PRO_NUMBER,
        PICKUP_DATE TYPE /PWEAVER/EFA_FFR-PICKUP_DATE,
        DELIVERY_DATE TYPE /PWEAVER/EFA_FFR-DELIVERY_DATE,
        HDLG_UNITS TYPE /PWEAVER/EFA_FFR-HDLG_UNITS,
        SERVICE TYPE /PWEAVER/EFA_FFR-SERVICE,
        TERMS TYPE /PWEAVER/EFA_FFR-TERMS,
        NETCHARGES TYPE /PWEAVER/EFA_FFR-NETCHARGES,
        Discount TYPE /PWEAVER/EFA_FFR-Discount,
        END OF TY_FFROUTPUT.


TYPES : BEGIN OF ty_invoice,
         invoice_number TYPE /pweaver/efa_fed-invoice_number,
         invoice_date   TYPE /pweaver/efa_fed-invoice_date,
        END OF ty_invoice.
        TYPES : BEGIN OF ty_pronumber,
         PRO_NUMBER TYPE /PWEAVER/EFA_FFR-PRO_NUMBER,
         PICKUP_DATE TYPE /PWEAVER/EFA_FFR-PICKUP_DATE,

        END OF ty_pronumber.

TYPES : BEGIN OF ty_export_output,
        gv_cb           TYPE c,
        account_no      TYPE /pweaver/efa_fed-bill_to_acc_num,
        invoice_date    TYPE /pweaver/efa_fed-invoice_date,
        invoice_number  TYPE /pweaver/efa_fed-invoice_number,
        customer_id     TYPE /pweaver/efa_fed-customer_id,
        carrier         TYPE /pweaver/efa_fed-service_type,
        create_date     TYPE /pweaver/efa_fed-create_date,
        create_time     TYPE /pweaver/efa_fed-create_time,
        create_user     TYPE /pweaver/efa_fed-create_user,
        net_value       TYPE /pweaver/efa_fed-net_chrg_amnt,
        earned_discount TYPE /pweaver/efa_fed-net_chrg_amnt,
        END OF ty_export_output.

TYPES : BEGIN OF ty_cons_output,
         account_no     TYPE /pweaver/efa_fed-bill_to_acc_num,
         low            TYPE /pweaver/efa_fed-invoice_number,
         high           TYPE /pweaver/efa_fed-invoice_number,
         date_low       TYPE /pweaver/efa_fed-invoice_date,
         date_high      TYPE /pweaver/efa_fed-invoice_date,
         invoice_low    TYPE /pweaver/efa_fed-invoice_number,
         invoice_high   TYPE /pweaver/efa_fed-invoice_number,
         carrier        TYPE /pweaver/efa_fed-service_type,
         net_value      TYPE /pweaver/efa_fed-net_chrg_amnt,
        END OF ty_cons_output.

TYPES : BEGIN OF ty_cons_output_export,
         gv_cb          TYPE c,
         account_no     TYPE /pweaver/efa_fed-bill_to_acc_num,
         low            TYPE /pweaver/efa_fed-invoice_number,
         high           TYPE /pweaver/efa_fed-invoice_number,
         date_low       TYPE /PWEAVER/EFA_FED-invoice_date,
         date_high      TYPE /PWEAVER/EFA_FED-invoice_date,
         invoice_low    TYPE /PWEAVER/EFA_FED-invoice_number,
         invoice_high   TYPE /PWEAVER/EFA_FED-invoice_number,
         carrier        TYPE /PWEAVER/EFA_FED-service_type,
         net_value      TYPE /PWEAVER/EFA_FED-net_chrg_amnt,
       END OF ty_cons_output_export.

DATA  : it_cons_output TYPE STANDARD TABLE OF ty_cons_output,
        wa_cons_output TYPE ty_cons_output.

DATA  : it_cons_export TYPE STANDARD TABLE OF ty_cons_output_export,
        wa_cons_export TYPE ty_cons_output_export.

DATA : it_output_fedex  TYPE STANDARD TABLE OF ty_output,
       it_export_output TYPE STANDARD TABLE OF ty_export_output,
       it_output_ups    TYPE STANDARD TABLE OF ty_output,
       it_output        TYPE STANDARD TABLE OF ty_output,
       it_invoice       TYPE STANDARD TABLE OF ty_invoice,
       it_pronumber type STANDARD TABLE OF ty_pronumber,
       it_zpwefa_fedex  TYPE STANDARD TABLE OF /PWEAVER/EFA_FED,
       wa_zpwefa_fedex  TYPE /PWEAVER/EFA_FED,
       wa_output_fedex  TYPE ty_output,
       wa_output_ups    TYPE ty_output,
       WA_OUTPUT_FFRT TYPE TY_FFROUTPUT,
       wa_export_output TYPE ty_export_output,
       wa_output        TYPE ty_output,
       wa_output_temp   TYPE ty_output,
       wa_invoice       TYPE ty_invoice,
       wa_pronumber type ty_pronumber.
DATA : v_ch,
     v_inv_no   TYPE /pweaver/efa_ups-invoice_number,
     v_acct_no  TYPE /pweaver/efa_ups-account_number,
     v_carr     TYPE /pweaver/efa_ups-carrier.


DATA : sy_index TYPE i,
       gv_cb,
       lines TYPE i.




TABLES : /pweaver/cconfig,sscrfields.


***Type-Pools
TYPE-POOLS  vrm.


* DATA DICLARATION.
DATA : plant        TYPE vrm_id,
      v_number(2)   TYPE c,
      v_tabix(2)    TYPE c,
      v_carrier(8)  TYPE c,
      v_fedex_sum   TYPE /PWEAVER/EFA_FED-net_chrg_amnt,
      v_earned_discount TYPE /PWEAVER/EFA_FED-trk_id_chg_amt.
*plant_list TYPE vrm_values,
*plant_value like line of list.


****Internal tables



TYPES : BEGIN OF ty_carrier,
  carriertype TYPE /pweaver/cconfig-carriertype,
  END OF ty_carrier.

DATA : it_vrm_invoice TYPE vrm_values,
       wa_vrm_invoice TYPE vrm_value.


DATA : it_vrm_carrier TYPE vrm_values,
       wa_vrm_carrier TYPE vrm_value,
       it_carrier     TYPE STANDARD TABLE OF ty_carrier,
       wa_carrier     TYPE ty_carrier.

DATA : it_data TYPE TABLE OF alsmex_tabline INITIAL SIZE 0,
       is_data TYPE alsmex_tabline.
TYPES : BEGIN OF ty_plant,
         werks TYPE marc-werks,
        END OF ty_plant.

DATA : it_vrm_plant TYPE vrm_values,
       wa_vrm_plant TYPE vrm_value,
       it_plant TYPE STANDARD TABLE OF ty_plant,
       wa_plant TYPE ty_plant.




SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
PARAMETERS     : ps_carr TYPE /pweaver/cconfig-carriertype AS LISTBOX VISIBLE LENGTH 10 OBLIGATORY.
*  ps_plant TYPE marc-werks AS LISTBOX VISIBLE LENGTH 10 ."obligatory.

SELECT-OPTIONS : s_date FOR sy-datum NO-EXTENSION.
SELECT-OPTIONS : invoice FOR /PWEAVER/EFA_FED-invoice_number NO-EXTENSION.
*SELECT-OPTIONS : s_pdate FOR sy-datum NO-EXTENSION.
SELECT-OPTIONS : S_ACCT FOR /PWEAVER/EFA_FED-BILL_TO_ACC_NUM NO-EXTENSION.
*PARAMETERS     : p_cons AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

DATA p_cons.
