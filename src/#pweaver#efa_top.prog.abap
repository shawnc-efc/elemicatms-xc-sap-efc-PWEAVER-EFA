*&---------------------------------------------------------------------*
*& Include /PWEAVER/EFA_TOP                                  Module Pool      /PWEAVER/EFA
*&
*&---------------------------------------------------------------------*


  TYPE-POOLS: CNDP, GFW, VRM.
  CLASS LCL_APPLICATION DEFINITION deferred.

  CLASS CL_GUI_CFW DEFINITION LOAD.
  CLASS CL_GFW_MUX DEFINITION LOAD.

  DATA: G_APPLICATION TYPE REF TO LCL_APPLICATION.

  DATA : IT_VRM_CARRIER TYPE VRM_VALUES,
         WA_VRM_CARRIER LIKE LINE OF IT_VRM_CARRIER.
*********************   Screen Elements of 0900   ***********************

  TYPES : BEGIN OF TY_INVOICE,
           DATE     TYPE DATUM,
           NUMBER   TYPE /PWEAVER/DEACCOUNTNUMBER, " change this once the table is completed
           AMOUNT   TYPE NETWR,
           DISCOUNT TYPE NETWR,
           SEL      TYPE C,
          END OF TY_INVOICE.

  CONTROLS : TC_INVOICES TYPE TABLEVIEW USING SCREEN 0900.

  DATA : SCR_CARRIER TYPE /PWEAVER/DECARRIERTYPE,
         SCR_INV_LDATE TYPE SY-DATUM,
         SCR_INV_HDATE TYPE SY-DATUM,
         SCR_CONS TYPE C,

         SCR_CONS_FDATE TYPE SY-DATUM,
         SCR_CONS_TDATE TYPE SY-DATUM,
         SCR_CONS_AMOUNT TYPE NETWR,
         SCR_CONS_DISCOUNT TYPE NETWR.

  DATA : IT_INVOICE TYPE STANDARD TABLE OF TY_INVOICE,
         WA_INVOICE TYPE TY_INVOICE.

  DATA : OKCODE_900 TYPE SY-UCOMM.
