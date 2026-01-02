*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/FREIGHT_AUDIT_TOP
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT

*include <icon>.
TYPE-POOLS: icon.
type-pools: gfw.
*



*tables: zpwefainvoice,zpwefachargesumm.
*tables : zpwefa_invc_ups,zpwefa_inv_fedex,zpwefa_gsr.

class lcl_application definition deferred.

class cl_gui_cfw definition load.

data: g_application type ref to lcl_application,
      g_custom_container type ref to cl_gui_custom_container,
      g_tree type ref to cl_simple_tree_model,
      g_ok_code type sy-ucomm.

data : g_docking_container type ref to cl_gui_docking_container,
       g_splitter_container type ref to cl_gui_splitter_container,
       g_container_1 type ref to cl_gui_container,
       g_container_2 type ref to cl_gui_container.

  data : first.
  data   repid type sy-repid.
  data   dynnr type sy-dynnr.


* Fields on Dynpro 100
data: g_event(30),
      g_node_key(30) type c,
      g_item_name type tv_itmname,
      g_header_name type tv_hdrname.

data: go_grid type ref to cl_gui_alv_grid,
      go_custom_container type ref to cl_gui_custom_container,
      gs_layout type lvc_s_layo.

data: val type f, val1 type i,val2 type string,
      val_str type amount, val3 type i.
data: column_texts type table of gprtxt with header line,
      column_texts1 type table of gprtxt with header line,
      values type table of gprval with header line.

*
* Fields on Dynpro 100

*DATA: g_event(30),
*      g_node_key(30) TYPE c.

data : week_dt  type dats ,"zpwefainvoice-invoiceweekno,
     service(30) type c,
      temp_service(30).


****Begin of changes jan 07 2010
********** 1 . Service Type ***FEDEX
****Internal Table for Service Type
*TYPES: BEGIN OF TY_SERVICE_TYPE,
*     SERVICE_TYPE     TYPE ZPWSERVICETYPE,   " Service type
*     TOTAL_LETERS     TYPE I,        " no. of total letters
*     TOTAL_SHIPMENTS  TYPE NETPR,     " total no.of packages
*     RATEDWEIGHT     TYPE BRGEW_AP,  " Billed weight
*     ZPACKAGES        TYPE NETPR,     " total pkgs:2
*     V_TOTAL_WEIGHT   TYPE BRGEW_AP,  " total billed weight
*     DISCOUNT         TYPE NETWR,     " CREDITAPPROVED
*     TOTAL_CHARGE           TYPE NETWR,     " NETDUE
*     V_DUE            TYPE NETWR,                           " netdue:2
*     AMOUNT_PERCENT   TYPE NETWR,     " total net cost as percentage
*     PKG_AVG_COST     TYPE NETWR,     " Avg. cost per package
*     WEIGHT_AVG_COST  TYPE NETWR,     " Avg. Cost per weight
*     V_TOTAL          TYPE NETWR,     " grand total weight
*     LINE_COLOR       TYPE CHAR4,
*     END OF TY_SERVICE_TYPE.
*
** declare internal tables for Service Type
*DATA : IT_FINAL_SERVICE_TYPE TYPE TABLE OF TY_SERVICE_TYPE,
*       WA_FINAL_SERVICE_TYPE TYPE TY_SERVICE_TYPE,
*       IT_OUTPUT_SERVICE_TYPE TYPE TABLE OF TY_SERVICE_TYPE,
*       WA_OUTPUT_SERVICE_TYPE TYPE TY_SERVICE_TYPE.
*
*DATA: IT_FIELDCAT_SERVICE_TYPE TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_SERVICE_TYPE TYPE LVC_S_FCAT.
*
*
*********** 1 . Service Type ***UPS
*****Internal Table for Service Type
*TYPES: BEGIN OF TY_SERVICE_TYPE_UPS,
*     SERVICE_TYPE     TYPE ZPWSERVICETYPE,   " Service type
*     TOTAL_LETERS     TYPE I,        " no. of total letters
*     TOTAL_SHIPMENTS  TYPE NETPR,     " total no.of packages
*     ACTUALWEIGHT     TYPE BRGEW_AP,  " Billed weight
*     ZPACKAGES        TYPE NETPR,     " total pkgs:2
*     V_TOTAL_WEIGHT   TYPE BRGEW_AP,  " total billed weight
*     TOTAL_INC_CREDIT TYPE NETWR,     " CREDITAPPROVED
*     TOTAL_BILL_CHARG           TYPE NETWR,     " NETDUE
*     V_DUE            TYPE NETWR,                           " netdue:2
*     AMOUNT_PERCENT   TYPE NETWR,     " total net cost as percentage
*     PKG_AVG_COST     TYPE NETWR,     " Avg. cost per package
*     WEIGHT_AVG_COST  TYPE NETWR,     " Avg. Cost per weight
*     V_TOTAL          TYPE NETWR,     " grand total weight
*     LINE_COLOR       TYPE CHAR4,
*     END OF TY_SERVICE_TYPE_UPS.
*
** declare internal tables for Service Type
*DATA : IT_FINAL_SERVICE_TYPE_UPS TYPE TABLE OF TY_SERVICE_TYPE_UPS,
*       WA_FINAL_SERVICE_TYPE_UPS TYPE TY_SERVICE_TYPE_UPS,
*       IT_OUTPUT_SERVICE_TYPE_UPS TYPE TABLE OF TY_SERVICE_TYPE_UPS,
*       WA_OUTPUT_SERVICE_TYPE_UPS TYPE TY_SERVICE_TYPE_UPS.
*
*DATA: IT_FIELDCAT_SERVICE_TYPE_UPS TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_SERVICE_TYPE_UPS TYPE LVC_S_FCAT.
*
*
*
*
***************Package Type Internal Table declaration*************FEDEX
****************2.Package Type
*TYPES: BEGIN OF TY_PACKAGE_TYPE,
*       PACKAGE_TYPE      TYPE ZPWPACKAGETYPE,   " Package type
*       TOTAL_LETERS     TYPE I ,        " no. of total letters
*       TOTAL_SHIPMENTS  TYPE NETPR,     " total no.of packages
*       RATEDWEIGHT     TYPE BRGEW_AP,  " Billed weight
*       TOTAL_SHIPMENTS1 TYPE NETPR,     " total pkgs:2
*       V_TOTAL_WEIGHT   TYPE BRGEW_AP,  " total billed weight
*       DISCOUNT         TYPE NETWR,     " CREDITAPPROVED
*       TOTAL_CHARGE     TYPE NETWR,     " NETDUE
*       V_DUE            TYPE NETWR,                         " netdue:2
*       AMOUNT_PERCENT   TYPE NETWR,     " total net cost as percentage
*       PKG_AVG_COST     TYPE NETWR,     " Avg. cost per package
*       WEIGHT_AVG_COST  TYPE NETWR,     " Avg. Cost per weight
*       V_TOTAL          TYPE NETWR,     " grand total weight
*       LINE_COLOR       TYPE CHAR4,
*       END OF TY_PACKAGE_TYPE.
*
*
*
** declare internal tables for Service Type*********FEDEX
*DATA : IT_FINAL_PACKAGE_TYPE TYPE TABLE OF TY_PACKAGE_TYPE,
*       WA_FINAL_PACKAGE_TYPE TYPE TY_PACKAGE_TYPE,
*       IT_OUTPUT_PACKAGE_TYPE TYPE TABLE OF TY_PACKAGE_TYPE,
*       WA_OUTPUT_PACKAGE_TYPE TYPE TY_PACKAGE_TYPE.
*
*DATA: IT_FIELDCAT_PACKAGE_TYPE TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_PACKAGE_TYPE TYPE LVC_S_FCAT.
*
*
***************Package Type Internal Table declaration*************UPS
****************2.Package Type
*TYPES: BEGIN OF TY_PACKAGE_TYPE_UPS,
*       PACKAGE_TYPE      TYPE ZPWPACKAGETYPE,   " Package type
*       TOTAL_LETERS     TYPE I ,        " no. of total letters
*       TOTAL_SHIPMENTS  TYPE NETPR,     " total no.of packages
*       ACTUALWEIGHT     TYPE BRGEW_AP,  " Billed weight
*       TOTAL_SHIPMENTS1 TYPE NETPR,     " total pkgs:2
*       V_TOTAL_WEIGHT   TYPE BRGEW_AP,  " total billed weight
*       TOTAL_INC_CREDIT  TYPE NETWR,     " CREDITAPPROVED
*       TOTAL_BILL_CHARG TYPE NETWR,     " NETDUE
*       V_DUE            TYPE NETWR,                         " netdue:2
*       AMOUNT_PERCENT   TYPE NETWR,     " total net cost as percentage
*       PKG_AVG_COST     TYPE NETWR,     " Avg. cost per package
*       WEIGHT_AVG_COST  TYPE NETWR,     " Avg. Cost per weight
*       V_TOTAL          TYPE NETWR,     " grand total weight
*       LINE_COLOR       TYPE CHAR4,
*       END OF TY_PACKAGE_TYPE_UPS.
*
*
*
** declare internal tables for Service Type*********FEDEX
*DATA : IT_FINAL_PACKAGE_TYPE_UPS TYPE TABLE OF TY_PACKAGE_TYPE_UPS,
*       WA_FINAL_PACKAGE_TYPE_UPS TYPE TY_PACKAGE_TYPE_UPS,
*       IT_OUTPUT_PACKAGE_TYPE_UPS TYPE TABLE OF TY_PACKAGE_TYPE_UPS,
*       WA_OUTPUT_PACKAGE_TYPE_UPS TYPE TY_PACKAGE_TYPE_UPS.
*
*DATA: IT_FIELDCAT_PACKAGE_TYPE_UPS TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_PACKAGE_TYPE_UPS TYPE LVC_S_FCAT.
*
*
*
***************Zone Internal Table Declaration
***************3.Zone
*
**TYPES :    BEGIN OF ty_zone_type,
**          zone1 TYPE regio,
**          zpackages TYPE i,
**          totalletters TYPE name1,
**          ratedweight TYPE brgew_ap,
**          discount TYPE netwr,
**          total_charge TYPE netwr,
**          totalnetcostper TYPE name1,
**          avgpkgcost TYPE netwr,
**          avgpndcost TYPE netwr,
**          line_color TYPE char4,
**          END OF ty_zone_type.
**
*** declare internal tables for Zone Type
**DATA : it_final_zone_type TYPE TABLE OF ty_zone_type,
**       wa_final_zone_type TYPE ty_zone_type,
**       it_output_zone_type TYPE TABLE OF ty_zone_type,
**       wa_output_zone_type TYPE ty_zone_type.
**
**DATA: it_fieldcat_zone_type TYPE TABLE OF lvc_s_fcat,
**      wa_fieldcat_zone_type TYPE lvc_s_fcat.
*
*
*
***************Zone Internal Table Declaration
***************3.Zone  **********UPS
*
*TYPES :    BEGIN OF TY_ZONE_TYPE_UPS,
*          ZONE1 TYPE REGIO,
*          ZPACKAGES TYPE I,
*          TOTALLETTERS TYPE NAME1,
*          ACTUALWEIGHT TYPE BRGEW_AP,
*          TOTAL_INC_CREDIT TYPE NETWR,
*          TOTAL_BILL_CHARG TYPE NETWR,
*          TOTALNETCOSTPER TYPE NAME1,
*          AVGPKGCOST TYPE NETWR,
*          AVGPNDCOST TYPE NETWR,
*          LINE_COLOR TYPE CHAR4,
*          END OF TY_ZONE_TYPE_UPS.
*
** declare internal tables for Zone Type
*DATA : IT_FINAL_ZONE_TYPE_UPS TYPE TABLE OF TY_ZONE_TYPE_UPS,
*       WA_FINAL_ZONE_TYPE_UPS TYPE TY_ZONE_TYPE_UPS,
*       IT_OUTPUT_ZONE_TYPE_UPS TYPE TABLE OF TY_ZONE_TYPE_UPS,
*       WA_OUTPUT_ZONE_TYPE_UPS TYPE TY_ZONE_TYPE_UPS.
*
*DATA: IT_FIELDCAT_ZONE_TYPE_UPS TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_ZONE_TYPE_UPS TYPE LVC_S_FCAT.
*
***********Internal table for Payment Method Declaration
**************4.Payment Method *************FEDEX
*
*TYPES: BEGIN OF TY_PAYMENT_METHOD,
*        PAYMENT_METHOD    TYPE  CHAR20,    " Payment Method
*        TOTAL_LETERS     TYPE  I ,                " no. of total letters
*        TOTAL_SHIPMENTS  TYPE  NETPR,             " total no.of packages
*        RATEDWEIGHT      TYPE  BRGEW_AP,          " Billed weight
*        ZPACKAGES        TYPE  NETPR,             " total pkgs:2
*        V_TOTAL_WEIGHT   TYPE  BRGEW_AP,          " total billed weight
*        DISCOUNT         TYPE  NETWR,             " CREDITAPPROVED
*        TOTAL_CHARGE     TYPE  NETWR,             " NETDUE
*        V_DUE            TYPE  NETWR,                       " netdue:2
*        AMOUNT_PERCENT   TYPE  NETWR,             " total net cost as percentage
*        PKG_AVG_COST     TYPE  NETWR,             " Avg. cost per package
*        WEIGHT_AVG_COST  TYPE  NETWR,             " Avg. Cost per weight
*        V_TOTAL          TYPE  NETWR,             " grand total weight
*        LINE_COLOR       TYPE  CHAR4,
*       END OF TY_PAYMENT_METHOD.
*
*
** declare internal tables for Zone Type
*DATA : IT_FINAL_PAYMENT_METHOD TYPE TABLE OF TY_PAYMENT_METHOD,
*       WA_FINAL_PAYMENT_METHOD TYPE TY_PAYMENT_METHOD,
*       IT_OUTPUT_PAYMENT_METHOD TYPE TABLE OF TY_PAYMENT_METHOD,
*       WA_OUTPUT_PAYMENT_METHOD TYPE TY_PAYMENT_METHOD.
*
*DATA: IT_FIELDCAT_PAYMENT_METHOD TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_PAYMENT_METHOD TYPE LVC_S_FCAT.
*
*
***********Internal table for Payment Method Declaration
**************4.Payment Method  ************UPS
*
*TYPES: BEGIN OF TY_PAYMENT_METHOD_UPS,
*        PAYMENT_METHOD    TYPE  CHAR20,    " Payment Method
*        TOTAL_LETERS     TYPE  I ,                " no. of total letters
*        TOTAL_SHIPMENTS  TYPE  NETPR,             " total no.of packages
*        ACTUALWEIGHT      TYPE  BRGEW_AP,          " Billed weight
*        ZPACKAGES        TYPE  NETPR,             " total pkgs:2
*        V_TOTAL_WEIGHT   TYPE  BRGEW_AP,          " total billed weight
*        TOTAL_INC_CREDIT         TYPE  NETWR,             " CREDITAPPROVED
*        TOTAL_BILL_CHARG    TYPE  NETWR,             " NETDUE
*        V_DUE            TYPE  NETWR,                       " netdue:2
*        AMOUNT_PERCENT   TYPE  NETWR,             " total net cost as percentage
*        PKG_AVG_COST     TYPE  NETWR,             " Avg. cost per package
*        WEIGHT_AVG_COST  TYPE  NETWR,             " Avg. Cost per weight
*        V_TOTAL          TYPE  NETWR,             " grand total weight
*        LINE_COLOR       TYPE  CHAR4,
*       END OF TY_PAYMENT_METHOD_UPS.
*
*
** declare internal tables for Zone Type
*DATA : IT_FINAL_PAYMENT_METHOD_UPS TYPE TABLE OF TY_PAYMENT_METHOD_UPS,
*       WA_FINAL_PAYMENT_METHOD_UPS TYPE TY_PAYMENT_METHOD_UPS,
*       IT_OUTPUT_PAYMENT_METHOD_UPS TYPE TABLE OF TY_PAYMENT_METHOD_UPS,
*       WA_OUTPUT_PAYMENT_METHOD_UPS TYPE TY_PAYMENT_METHOD_UPS.
*
*DATA: IT_FIELDCAT_PAYMENT_METHOD_UPS TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_PAYMENT_METHOD_UPS TYPE LVC_S_FCAT.
*
*
*
**************Internal table Declaration for Ship Day
**********5. Ship Day*************FEDEX
*
*TYPES: BEGIN OF TY_SHIP_DAY,
**       PICKUP_DATE  TYPE ZPICKUPDATE,
*       TOTAL_LETERS     TYPE I ,        " no. of total letters
*       TOTAL_SHIPMENTS  TYPE NETPR,     " total no.of packages
*       RATEDWEIGHT     TYPE BRGEW_AP,  " Billed weight
*       TOTAL_SHIPMENTS1 TYPE NETPR,     " total pkgs:2
*       V_TOTAL_WEIGHT   TYPE BRGEW_AP,  " total billed weight
*       DISCOUNT   TYPE NETWR,     " CREDITAPPROVED
*       TOTAL_CHARGE     TYPE NETWR,     " NETDUE
*       V_DUE            TYPE NETWR,                         " netdue:2
*       AMOUNT_PERCENT   TYPE NETWR,     " total net cost as percentage
*       PKG_AVG_COST     TYPE NETWR,     " Avg. cost per package
*       WEIGHT_AVG_COST  TYPE NETWR,     " Avg. Cost per weight
*       V_TOTAL          TYPE NETWR,     " grand total weight
*       LINE_COLOR       TYPE CHAR4,
*       END OF TY_SHIP_DAY.
*
** declare internal tables for Zone Type
*DATA : IT_FINAL_SHIP_DAY TYPE TABLE OF TY_SHIP_DAY,
*       WA_FINAL_SHIP_DAY TYPE TY_SHIP_DAY,
*       IT_OUTPUT_SHIP_DAY TYPE TABLE OF TY_SHIP_DAY,
*       WA_OUTPUT_SHIP_DAY TYPE TY_SHIP_DAY.
*
*DATA: IT_FIELDCAT_SHIP_DAY TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_SHIP_DAY TYPE LVC_S_FCAT.
*
**************Internal table Declaration for Ship Day
**********5. Ship Day*************UPS
*
*TYPES: BEGIN OF TY_SHIP_DAY_UPS,
**       PICKUP_DATE  TYPE ZPICKUPDATE,
*       TOTAL_LETERS     TYPE I ,        " no. of total letters
*       TOTAL_SHIPMENTS  TYPE NETPR,     " total no.of packages
*       ACTUALWEIGHT     TYPE BRGEW_AP,  " Billed weight
*       TOTAL_SHIPMENTS1 TYPE NETPR,     " total pkgs:2
*       V_TOTAL_WEIGHT   TYPE BRGEW_AP,  " total billed weight
*       TOTAL_INC_CREDIT   TYPE NETWR,     " CREDITAPPROVED
*       TOTAL_BILL_CHARG     TYPE NETWR,     " NETDUE
*       V_DUE            TYPE NETWR,                         " netdue:2
*       AMOUNT_PERCENT   TYPE NETWR,     " total net cost as percentage
*       PKG_AVG_COST     TYPE NETWR,     " Avg. cost per package
*       WEIGHT_AVG_COST  TYPE NETWR,     " Avg. Cost per weight
*       V_TOTAL          TYPE NETWR,     " grand total weight
*       LINE_COLOR       TYPE CHAR4,
*       END OF TY_SHIP_DAY_UPS.
*
** declare internal tables for Zone Type
*DATA : IT_FINAL_SHIP_DAY_UPS TYPE TABLE OF TY_SHIP_DAY_UPS,
*       WA_FINAL_SHIP_DAY_UPS TYPE TY_SHIP_DAY_UPS,
*       IT_OUTPUT_SHIP_DAY_UPS TYPE TABLE OF TY_SHIP_DAY_UPS,
*       WA_OUTPUT_SHIP_DAY_UPS TYPE TY_SHIP_DAY_UPS.
*
*DATA: IT_FIELDCAT_SHIP_DAY_UPS TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_SHIP_DAY_UPS TYPE LVC_S_FCAT.
*
*
**************Internal table Declaration for Ship Day
**********5. Incorrect Address
**DATA: IT_INCT_ADDR TYPE TABLE OF ZEFA_INVC_FEDEX,
**      WA_INCT_ADDR TYPE ZEFA_INVC_FEDEX.
*DATA: IT_FIELDCAT_INCT_ADDR TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_INCT_ADDR TYPE LVC_S_FCAT.
*
*
*****************Internal table for GSR's ( Guaranteed Service Refunds )********
**********6.Guarnateed Service Refunds (GSR's)************************
*
*TYPES: BEGIN OF TY_GSR,
*       INVOICE_NO          TYPE VBELN_VF,
*       SERVICE_TYPE  TYPE ZPWDESC,   " Service type
*       PICKUP_DATE  TYPE DATS,
*       TRACKINGNUMBER   TYPE CHAR30,
**       pod              TYPE char18,
**       packagesignature TYPE zpacksign,
**       packagesignature TYPE char30,
*       SUMCHARGE        TYPE NETWR,
*       DISCOUNT   TYPE NETWR,     " CREDITAPPROVED
*       TOTAL_CHARGE           TYPE NETWR,     " NETDUE
*       ERRORCODE        TYPE CHAR30,
*       CARRIER          TYPE CHAR20,
*       END OF TY_GSR.
*
*
** declare internal tables for Zone Type
*DATA : IT_FINAL_GSR TYPE TABLE OF TY_GSR,
*       WA_FINAL_GSR TYPE TY_GSR,
*       IT_OUTPUT_GSR TYPE TABLE OF TY_GSR,
*       WA_OUTPUT_GSR TYPE TY_GSR.
*
*DATA: IT_FIELDCAT_GSR TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_GSR TYPE LVC_S_FCAT.
*
*
*****************Internal table for GSR's ( Guaranteed Service Refunds )********
**********6.Guarnateed Service Refunds (GSR's)************************UPS
*
*TYPES: BEGIN OF TY_GSR_UPS,
*       INVOICE_NO          TYPE VBELN_VF,
*       SERVICE_TYPE  TYPE ZPWDESC,   " Service type
*       PICKUP_DATE  TYPE DATS,
*       TRACKINGNUMBER   TYPE CHAR30,
**       pod              TYPE char18,
**       packagesignature TYPE zpacksign,
**       packagesignature TYPE char30,
*       SUMCHARGE        TYPE NETWR,
*       TOTAL_INC_CREDIT   TYPE NETWR,     " CREDITAPPROVED
*       TOTAL_BILL_CHARG           TYPE NETWR,     " NETDUE
*       ERRORCODE        TYPE CHAR30,
*       CARRIER          TYPE CHAR20,
*       END OF TY_GSR_UPS.
*
*
** declare internal tables for Zone Type
*DATA : IT_FINAL_GSR_UPS TYPE TABLE OF TY_GSR_UPS,
*       WA_FINAL_GSR_UPS TYPE TY_GSR_UPS,
*       IT_OUTPUT_GSR_UPS TYPE TABLE OF TY_GSR_UPS,
*       WA_OUTPUT_GSR_UPS TYPE TY_GSR_UPS.
*
*DATA: IT_FIELDCAT_GSR_UPS TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_GSR_UPS TYPE LVC_S_FCAT.
*
*
*
*
*
*
*******************Internal table for All Detail's
*******************7.All Details**********FEDEX
*
**DATA : IT_FINAL_ALL_DETAILS TYPE TABLE OF ZEFA_INVC_FEDEX,
**       WA_FINAL_ALL_DETAILS TYPE ZEFA_INVC_FEDEX.
*
*DATA: IT_FIELDCAT_ALL_DETAILS TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_ALL_DETAILS TYPE LVC_S_FCAT.
*
*
*******************Internal table for All Detail's
*******************7.All Details ***********UPS
*
**DATA : IT_FINAL_ALL_DETAILS_UPS TYPE TABLE OF ZEFA_INVC_UPS,
**       WA_FINAL_ALL_DETAILS_UPS TYPE ZEFA_INVC_UPS.
*
*DATA: IT_FIELDCAT_ALL_DETAILS_UPS TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_ALL_DETAILS_UPS TYPE LVC_S_FCAT.
*
*
*
*
*
*
**
**
**
*********** for Packtype_display
**
**TYPES: BEGIN OF ty_final1,
**       packagetype      TYPE zpwpackagetype,   " Package type
**       total_leters     TYPE i ,        " no. of total letters
**       total_shipments  TYPE netpr,     " total no.of packages
**       billedweight     TYPE brgew_ap,  " Billed weight
**       total_shipments1 TYPE netpr,     " total pkgs:2
**       v_total_weight   TYPE brgew_ap,  " total billed weight
**       creditapproved   TYPE netwr,     " CREDITAPPROVED
**       netdue           TYPE netwr,     " NETDUE
**       v_due            TYPE netwr,                         " netdue:2
**       amount_percent   TYPE netwr,     " total net cost as percentage
**       pkg_avg_cost     TYPE netwr,     " Avg. cost per package
**       weight_avg_cost  TYPE netwr,     " Avg. Cost per weight
**       v_total          TYPE netwr,     " grand total weight
**       line_color       TYPE char4,
**       END OF ty_final1.
**
*** declare internal tables
**DATA : it_final1 TYPE TABLE OF ty_final1,
**       wa_final1 TYPE ty_final1,
**       it_output1 TYPE TABLE OF ty_final1,
**       wa_output1 TYPE ty_final1.
**DATA: it_fieldcat1 TYPE TABLE OF lvc_s_fcat,
**      wa_fieldcat1 TYPE lvc_s_fcat.
**
**
**DATA : it_zefa_invc_fedex TYPE TABLE OF zefa_invc_fedex,
**       wa_zefa_invc_fedex TYPE zefa_invc_fedex,
**       it_zefa_invc_ups TYPE TABLE OF zefa_invc_ups,
**       wa_zefa_invc_ups TYPE zefa_invc_ups.
**
**
*********** for ZONE.
**DATA :    BEGIN OF zone_structure,
**          zone TYPE regio,
**          totalpackages TYPE i,
**          totalletters TYPE name1,
**          totalweight TYPE brgew_ap,
**          totalsaving TYPE netwr,
**          totalcost TYPE netwr,
**          totalnetcostper TYPE name1,
**          avgpkgcost TYPE netwr,
**          avgpndcost TYPE netwr,
**          line_color TYPE char4,
**      END OF zone_structure.
**
**
**DATA it_zone LIKE TABLE OF zone_structure.
**
**DATA zone_it_fieldcat TYPE TABLE OF lvc_s_fcat.
**DATA zone_wa_fieldcat TYPE lvc_s_fcat.
**
**
************ for shipday
**
**TYPES: BEGIN OF ty_final3,
**       actualpickup_dt  TYPE dats,
**       total_leters     TYPE i ,        " no. of total letters
**       total_shipments  TYPE netpr,     " total no.of packages
**       billedweight     TYPE brgew_ap,  " Billed weight
**       total_shipments1 TYPE netpr,     " total pkgs:2
**       v_total_weight   TYPE brgew_ap,  " total billed weight
**       creditapproved   TYPE netwr,     " CREDITAPPROVED
**       netdue           TYPE netwr,     " NETDUE
**       v_due            TYPE netwr,                         " netdue:2
**       amount_percent   TYPE netwr,     " total net cost as percentage
**       pkg_avg_cost     TYPE netwr,     " Avg. cost per package
**       weight_avg_cost  TYPE netwr,     " Avg. Cost per weight
**       v_total          TYPE netwr,     " grand total weight
**       line_color       TYPE char4,
**
**       END OF ty_final3.
**
*** declare internal tables
**DATA : it_final3 TYPE TABLE OF ty_final3,
**       wa_final3 TYPE ty_final3,
**       it_output3 TYPE TABLE OF ty_final3,
**       wa_output3 TYPE ty_final3.
**DATA: it_fieldcat3 TYPE TABLE OF lvc_s_fcat,
**      wa_fieldcat3 TYPE lvc_s_fcat.
**
**
**
**
**
**
*** declare internal tables
***DATA : it_final2 TYPE TABLE OF ty_final2,
***       wa_final2 TYPE ty_final2,
***       it_output2 TYPE TABLE OF ty_final2,
***       wa_output2 TYPE ty_final2.
**DATA: it_fieldcat2 TYPE TABLE OF lvc_s_fcat,
**      wa_fieldcat2 TYPE lvc_s_fcat.
**
**
************** for Payment Method.
**
**TYPES: BEGIN OF ty_final,
***        paymentmethodes  TYPE  zpaymentmethod,    " Payment Method
**        paymentmethodes  TYPE  char20,    " Payment Method
**        total_leters     TYPE  i ,                " no. of total letters
**        total_shipments  TYPE  netpr,             " total no.of packages
**        billedweight     TYPE  brgew_ap,          " Billed weight
**        tshs             TYPE  netpr,             " total pkgs:2
**        v_total_weight   TYPE  brgew_ap,          " total billed weight
**        creditapproved   TYPE  netwr,             " CREDITAPPROVED
**        netdue           TYPE  netwr,             " NETDUE
**        v_due            TYPE  netwr,                       " netdue:2
**        amount_percent   TYPE  netwr,             " total net cost as percentage
**        pkg_avg_cost     TYPE  netwr,             " Avg. cost per package
**        weight_avg_cost  TYPE  netwr,             " Avg. Cost per weight
**        v_total          TYPE  netwr,             " grand total weight
**        line_color       TYPE  char4,
**       END OF ty_final.
**
*** declare internal tables
**
**DATA : it_final TYPE TABLE OF ty_final,
**       wa_final TYPE ty_final,
**       it_output TYPE TABLE OF ty_final,
**       wa_output TYPE ty_final.
**
**DATA: it_fieldcat TYPE TABLE OF lvc_s_fcat,
**      wa_fieldcat TYPE lvc_s_fcat.
**
************** for Incorrect address
**
**DATA: it_zpwefainvoice TYPE TABLE OF zpwefainvoice,
**      wa_zpwefainvoice TYPE zpwefainvoice.
**DATA: it_fieldcat6 TYPE TABLE OF lvc_s_fcat,
**      wa_fieldcat6 TYPE lvc_s_fcat.
**
************** for GSR
**TYPES: BEGIN OF ty_final7,
**       invoice          TYPE vbeln_vf,
**       servicetypedesc  TYPE zpwdesc,   " Service type
**       actualpickup_dt  TYPE dats,
***       trackingnumber   TYPE zpwtrack,
**       trackingnumber   TYPE char30,
**       pod              TYPE char18,
***       packagesignature TYPE zpacksign,
**       packagesignature TYPE char30,
**       sumcharge        TYPE netwr,
**       creditapproved   TYPE netwr,     " CREDITAPPROVED
**       netdue           TYPE netwr,     " NETDUE
***       errorcode        TYPE zerrorcode,
**        errorcode        TYPE char30,
***       CARRIER          TYPE ZCARRIER,
**       carrier          TYPE char20,
**       END OF ty_final7.
*** declare internal tables
**DATA : it_final7 TYPE TABLE OF ty_final7,
**       wa_final7 TYPE ty_final7,
**       it_output7 TYPE TABLE OF ty_final7,
**       wa_output7 TYPE ty_final7.
**DATA: it_fieldcat7 TYPE TABLE OF lvc_s_fcat,
**      wa_fieldcat7 TYPE lvc_s_fcat.
**
**
************************************************************************
*******Begin of Data Declaration for Carrier Remittance******FEDEX
*TYPES: BEGIN OF TY_CARRIER_REMITTANCE,
*       INVOICE_NO          TYPE VBELN_VF," Invoice Number
*       SERVICE_TYPE  TYPE ZPWDESC, " Package Type
*       ACCTNO           TYPE CHAR10, " Account Number
*       INVOICE_DATE    TYPE ERZDT,   " Invoice WeekNumber
*       TOTAL_AMOUNT     TYPE NETPR,     " Total Count of Packages
*       TOTAL_SHIPMENTS  TYPE NETPR,     " total no.of packages
*       TOTAL_NON_SHIPMENTS  TYPE NETPR, " total no.of non packages
*       TOTAL_CHARGE       TYPE NETWR,     " Sum chage
*       DISCOUNT   TYPE NETWR,     " CREDITAPPROVED
*       NETDUE           TYPE NETWR,     " NETDUE
*       LINE_COLOR       TYPE CHAR4,
*       END OF TY_CARRIER_REMITTANCE.
** declare internal tables
*DATA : IT_CARRIER_REMITTANCE_FINAL TYPE TABLE OF TY_CARRIER_REMITTANCE,
*       WA_CARRIER_REMITTANCE_FINAL TYPE TY_CARRIER_REMITTANCE,
*       IT_CARRIER_REMITTANCE_OUTPUT TYPE TABLE OF TY_CARRIER_REMITTANCE,
*       WA_CARRIER_REMITTANCE_OUTPUT TYPE TY_CARRIER_REMITTANCE.
*DATA: IT_CARRIER_REMITTANCE_FIELDCAT TYPE TABLE OF LVC_S_FCAT,
*      WA_CARRIER_REMITTANCE_FIELDCAT TYPE LVC_S_FCAT.
*
*
*******Begin of Data Declaration for Carrier Remittance***UPS
*TYPES: BEGIN OF TY_CARRIER_REMITTANCE_UPS,
*       INVOICE_NO          TYPE VBELN_VF," Invoice Number
*       SERVICE_TYPE  TYPE ZPWDESC, " Package Type
*       ACCTNO           TYPE CHAR10, " Account Number
*       INVOICE_DATE    TYPE ERZDT,   " Invoice WeekNumber
*       TOTAL_AMOUNT     TYPE NETPR,     " Total Count of Packages
*       TOTAL_SHIPMENTS  TYPE NETPR,     " total no.of packages
*       TOTAL_NON_SHIPMENTS  TYPE NETPR, " total no.of non packages
*       TOTAL_BILL_CHARG       TYPE NETWR,     " Sum chage
*       TOTAL_INC_CREDIT   TYPE NETWR,     " CREDITAPPROVED
*       NETDUE           TYPE NETWR,     " NETDUE
*       LINE_COLOR       TYPE CHAR4,
*       END OF TY_CARRIER_REMITTANCE_UPS.
** declare internal tables
*DATA : IT_CARRIER_REMITT_FINAL_UPS TYPE TABLE OF TY_CARRIER_REMITTANCE_UPS,
*       WA_CARRIER_REMITT_FINAL_UPS TYPE TY_CARRIER_REMITTANCE_UPS,
*       IT_CARRIER_REMITT_OUTPUT_UPS TYPE TABLE OF TY_CARRIER_REMITTANCE_UPS,
*       WA_CARRIER_REMITT_OUTPUT_UPS TYPE TY_CARRIER_REMITTANCE_UPS.
*DATA: IT_CARRIER_REMITT_FIELDCAT_UPS TYPE TABLE OF LVC_S_FCAT,
*      WA_CARRIER_REMITT_FIELDCAT_UPS TYPE LVC_S_FCAT.
*
*
*
*
*
*
*******End of Data Declaration ofr Carrier Remittance
**************for All Details
**
*** declare internal tables
**DATA : it_final8 TYPE TABLE OF zpwefainvoice,
**       wa_final8 TYPE zpwefainvoice.
**
**DATA: it_fieldcat8 TYPE TABLE OF lvc_s_fcat,
**      wa_fieldcat8 TYPE lvc_s_fcat.
*
*************** Charge Summary type
*
*TYPES: BEGIN OF TY_SUMM_FINAL,
**       origin TYPE zorigin,
*        ORIGIN TYPE CHAR15,
*       CHARGEDESC TYPE CHAR30,
*       CHGCODEDESC  TYPE CHAR40,
*       TOTAL_CNT  TYPE NETWR,
*       ADDRCORRCRG TYPE NETWR,
*       TOTAL_COST TYPE NETWR,
*       AVG_COST   TYPE NETWR,
*       END OF TY_SUMM_FINAL.
** declare internal tables
*DATA : IT_ZPWEFAINVOICE1 TYPE TABLE OF TY_SUMM_FINAL,
*       WA_ZPWEFAINVOICE1 TYPE TY_SUMM_FINAL,
*       IT_SUMM_OUTPUT TYPE TABLE OF TY_SUMM_FINAL,
*       WA_SUMM_OUTPUT TYPE TY_SUMM_FINAL.
*DATA: IT_SUMM_FIELDCAT TYPE TABLE OF LVC_S_FCAT,
*      WA_SUMM_FIELDCAT TYPE LVC_S_FCAT.
data : sel type  c,
  g_but  type c.

********************************
*********************for charge summary type


* declare internal tables
*data : it_final9 type table of zpwefachargesumm,
*       wa_final9 type zpwefachargesumm.

data: it_fieldcat9 type table of lvc_s_fcat,
      wa_fieldcat9 type lvc_s_fcat.


*******************hotspot delclaration
*DATA : init TYPE C,
*      document_viewer TYPE REF TO i_oi_document_viewer.

types pict_line(256) type c.

data :init,

container type ref to cl_gui_custom_container,

editor type ref to cl_gui_textedit,

picture type ref to cl_gui_picture,

pict_tab type table of pict_line,

document_viewer type ref to i_oi_document_viewer.
data: pw_url(200) type c .

*DATA : Zcarriertype TYPE zcarrier.

data : zcarriertype type char20.

data : approve(1) type c.
****************************************************************Begin of new changes************

types : begin of ty_output,
  gv_cb type c,
  account_no type   /pweaver/efa_fed-bill_to_acc_num,
  invoice_date type /pweaver/efa_fed-invoice_date,
  invoice_number type /pweaver/efa_fed-invoice_number,
  customer_id type /pweaver/efa_fed-customer_id,
  carrier  type  /pweaver/efa_fed-service_type,
  create_date type /pweaver/efa_fed-create_date,
  create_time type /pweaver/efa_fed-create_time,
  create_user type /pweaver/efa_fed-create_user,
    net_value type /pweaver/efa_fed-net_chrg_amnt,
  earned_discount type /pweaver/efa_fed-net_chrg_amnt,
  end of ty_output.

TYPES : begin of ty_cons_output_export,
         gv_cb type c,
         account_no type   /pweaver/efa_fed-bill_to_acc_num,
         low type /PWEAVER/EFA_FED-INVOICE_NUMBER,
         high type /PWEAVER/EFA_FED-INVOICE_NUMBER,
         date_low type /pweaver/efa_fed-invoice_date,
         date_high type /pweaver/efa_fed-invoice_date,
         invoice_low type /pweaver/efa_fed-invoice_number,
         invoice_high type /pweaver/efa_fed-invoice_number,
         carrier  type  /pweaver/efa_fed-service_type,
         net_value type /pweaver/efa_fed-net_chrg_amnt,
       end of ty_cons_output_export.

types : begin of ty_invoice,
invoice_number type /pweaver/efa_fed-invoice_number,
     end of ty_invoice.

data :it_export_output type standard table of ty_output,
       wa_export_output type ty_output.

data :it_cons_export type standard table of ty_cons_output_export,
       wa_cons_export type ty_cons_output_export.

  DATA : it_zpwefa_ups TYPE STANDARD TABLE OF /pweaver/efa_UPS,
         is_zpwefa_ups TYPE /pweaver/efa_UPS.
   DATA : it_zpwefa_ups1 TYPE STANDARD TABLE OF /pweaver/efa_UPS,
         is_zpwefa_ups1 TYPE /pweaver/efa_UPS.
   DATA : it_zpwefa_track TYPE STANDARD TABLE OF /pweaver/efa_UPS,
         is_zpwefa_track TYPE /pweaver/efa_UPS.


  data : wa_ups type /PWEAVER/EFA_UPS_SUMM,
         it_ups type standard table of /PWEAVER/EFA_UPS_SUMM.
    data : wa_ups1 type /PWEAVER/EFA_UPS_SUMM,
         it_ups1 type standard table of /PWEAVER/EFA_UPS_SUMM.
    data : wa_upsd type /PWEAVER/EFA_UPS_SUMM,
         it_upsd type standard table of /PWEAVER/EFA_UPS_SUMM.
****testing ****Delete this
data : fedex(1) type c,
      ups(1) type c.

data : v_invoice_number type char20,
       temp_invoice type char20.
*************FedEx - service type *******
types : begin of ty_ZPWEFA_FEDEX_serv_type,
service_type  type /pweaver/efa_fed-service_type,
packages type /PWEAVER/EFA_FED-num_pieces,
rated_weight type brgew_ap,    "zpwefa_fedex-rated_wgt_amt,
transport_charge type netwr,   "ZPWTRK_ID_CHG_AMT,
handling_charge type  netwr,   "ZPWTRK_ID_CHG_AMT,
discount type netwr,           "ZPWTRK_ID_CHG_AMT,
net_charge type  netwr,        "ZPWTRK_ID_CHG_AMT,
  line_color       type char4,
  end of ty_zpwefa_fedex_serv_type.

data : it_output_serv_type type standard table of ty_zpwefa_fedex_serv_type,
       wa_output_serv_type type                   ty_zpwefa_fedex_serv_type,
       it_final_serv_type type standard table of  ty_zpwefa_fedex_serv_type,
       wa_final_serv_type type                    ty_zpwefa_fedex_serv_type,
       it_serv_type type standard table of        /pweaver/efa_fed,
       wa_serv_type type                          /pweaver/efa_fed.


data: it_fieldcat_serv_type type table of lvc_s_fcat,
      wa_fieldcat_serv_type type lvc_s_fcat.



**************end of FedEx - service type ************

****Begin of UPS - Service Type ***

types : begin of ty_zpwefa_ups_serv_type,
service_type  type /pweaver/efa_UPS-chrg_desc,
  tracking_number type /pweaver/efa_UPS-trck_num,
packages type /pweaver/efa_UPS-pckg_qty,
rated_weight type /pweaver/efa_UPS-billed_weight,
transport_charge type /pweaver/efa_UPS-basis_value,
handling_charge type  /pweaver/efa_UPS-basis_value,
discount type /pweaver/efa_UPS-incen_amt,
net_charge type /pweaver/efa_UPS-net_amnt,
chrg_class_code type /pweaver/efa_UPS-chrg_class_code,
chrg_desc_code type /pweaver/efa_UPS-chrg_desc_code,
  line_color       type char4,
  end of ty_zpwefa_ups_serv_type.

types : begin of ty_ups_track,
tracking_number type /pweaver/efa_UPS-trck_num,
end of ty_ups_track.

types : begin of ty_ups_service,
chrg_desc type /pweaver/efa_UPS-chrg_desc,
end of ty_ups_service.

TYPES : begin of ty_zpwefa_ups,
  service_type  type /pweaver/efa_UPS-chrg_desc,
  zone  type /pweaver/efa_fed-zone_code,
  payor TYPE /pweaver/efa_fed-payor,
  tracking_number type /pweaver/efa_UPS-trck_num,
  packages type /pweaver/efa_UPS-pckg_qty,
  rated_weight type /pweaver/efa_UPS-billed_weight,
  transport_charge type /pweaver/efa_UPS-basis_value,
  handling_charge type  /pweaver/efa_UPS-basis_value,
  discount type /pweaver/efa_UPS-incen_amt,
  net_charge type /pweaver/efa_UPS-net_amnt,
  chrg_class_code type /pweaver/efa_UPS-chrg_class_code,
  chrg_desc_code type /pweaver/efa_UPS-chrg_desc_code,
  line_color       type char4,
 END OF ty_zpwefa_ups.

TYPES : begin of ty_zpwefa_ups_service,
  service_type  type /pweaver/efa_UPS-chrg_desc,
  tracking_number type /pweaver/efa_UPS-trck_num,
  packages type /pweaver/efa_UPS-pckg_qty,
  rated_weight type /pweaver/efa_UPS-billed_weight,
  transport_charge type /pweaver/efa_UPS-basis_value,
  handling_charge type  /pweaver/efa_UPS-basis_value,
  discount type /pweaver/efa_UPS-incen_amt,
  net_charge type /pweaver/efa_UPS-net_amnt,
  chrg_class_code type /pweaver/efa_UPS-chrg_class_code,
  chrg_desc_code type /pweaver/efa_UPS-chrg_desc_code,
  line_color       type char4,
 END OF ty_zpwefa_ups_service.

TYPES : begin of ty_zpwefa_ups_zone,
  zone  type /pweaver/efa_fed-zone_code,
  tracking_number type /pweaver/efa_UPS-trck_num,
  packages type /pweaver/efa_UPS-pckg_qty,
  rated_weight type /pweaver/efa_UPS-billed_weight,
  transport_charge type /pweaver/efa_UPS-basis_value,
  handling_charge type  /pweaver/efa_UPS-basis_value,
  discount type /pweaver/efa_UPS-incen_amt,
  net_charge type /pweaver/efa_UPS-net_amnt,
  chrg_class_code type /pweaver/efa_UPS-chrg_class_code,
  chrg_desc_code type /pweaver/efa_UPS-chrg_desc_code,
  line_color       type char4,
 END OF ty_zpwefa_ups_zone.

TYPES : begin of ty_zpwefa_ups_payor,
  payor TYPE /pweaver/efa_fed-payor,
  tracking_number type /pweaver/efa_UPS-trck_num,
  packages type /pweaver/efa_UPS-pckg_qty,
  rated_weight type /pweaver/efa_UPS-billed_weight,
  transport_charge type /pweaver/efa_UPS-basis_value,
  handling_charge type  /pweaver/efa_UPS-basis_value,
  discount type /pweaver/efa_UPS-incen_amt,
  net_charge type /pweaver/efa_UPS-net_amnt,
  chrg_class_code type /pweaver/efa_UPS-chrg_class_code,
  chrg_desc_code type /pweaver/efa_UPS-chrg_desc_code,
  line_color       type char4,
 END OF ty_zpwefa_ups_payor.



data : it_output_ups_serv_type type standard table of ty_zpwefa_ups_serv_type,
       wa_output_ups_serv_type type ty_zpwefa_ups_serv_type,
       it_final_ups_serv_type type standard table of ty_zpwefa_ups_serv_type,
       wa_final_ups_serv_type type ty_zpwefa_ups_serv_type,
       it_temp_ups_serv_type type standard table of ty_zpwefa_ups_serv_type,
       wa_temp_ups_serv_type type ty_zpwefa_ups_serv_type,
       it_serv_ups_type type standard table of /pweaver/efa_UPS,
       wa_serv_ups_type type /pweaver/efa_UPS,

       it_ups_track type standard table of ty_ups_track,
       wa_ups_track type ty_ups_track,
       it_ups_service type standard table of ty_ups_service,
       wa_ups_service type ty_ups_service,
       it_serv_temp_ups type standard table of /pweaver/efa_UPS,
       wa_serv_temp_ups type /pweaver/efa_UPS.


data : it_ups_data TYPE STANDARD TABLE OF /pweaver/efa_UPS,
       it_ups_data_temp TYPE STANDARD TABLE OF /pweaver/efa_UPS,
       it_final_ups_service TYPE STANDARD TABLE OF ty_zpwefa_ups_service,
       it_final_ups_zone TYPE STANDARD TABLE OF ty_zpwefa_ups_zone,
       it_final_ups_payor TYPE STANDARD TABLE OF ty_zpwefa_ups_payor,

       wa_ups_data_t_temp TYPE /pweaver/efa_UPS,
       wa_ups_data type /pweaver/efa_UPS,
       wa_ups_data_temp type ty_zpwefa_ups,
       wa_final_ups_service TYPE ty_zpwefa_ups_service,
       wa_final_ups_zone TYPE ty_zpwefa_ups_zone,
       wa_final_ups_payor TYPE ty_zpwefa_ups_payor,
       wa_output_ups_payor_type TYPE ty_zpwefa_ups_payor,
       it_output_ups_payor_type TYPE STANDARD TABLE OF ty_zpwefa_ups_payor.


data: it_fieldcat_serv_ups_type type table of lvc_s_fcat,
      wa_fieldcat_serv_ups_type type lvc_s_fcat,

      it_fieldcat_payor_ups_type TYPE STANDARD TABLE OF lvc_s_fcat,
      wa_fieldcat_payor_ups_type TYPE lvc_s_fcat.


***End of UPS - Service Type ***

*************Zone *******
types : begin of ty_zpwefa_fedex_zone_type,
zone  type /pweaver/efa_fed-zone_code,
packages type /pweaver/efa_fed-num_pieces,
rated_weight type brgew_ap,"/pweaver/efa_fed-rated_wgt_amt,
transport_charge type netwr," /pweaver/efa_fed-tran_charg_amnt,
handling_charge type  netwr,"/pweaver/efa_fed-tran_charg_amnt,
discount type netwr,"/pweaver/efa_fed-tran_charg_amnt,
net_charge type netwr," /pweaver/efa_fed-net_chrg_amnt,
  line_color       type char4,
  end of ty_zpwefa_fedex_zone_type.


data : it_output_zone_type type standard table of ty_zpwefa_fedex_zone_type,
       wa_output_zone_type type ty_zpwefa_fedex_zone_type,
       it_final_zone_type type standard table of ty_zpwefa_fedex_zone_type,
       wa_final_zone_type type ty_zpwefa_fedex_zone_type,
       it_zone_type type standard table of /pweaver/efa_fed,
       wa_zone_type type /pweaver/efa_fed.


data: it_fieldcat_zone_type type table of lvc_s_fcat,
      wa_fieldcat_zone_type type lvc_s_fcat.



**************Zone ************

types : begin of ty_zpwefa_ups_zone_type,
zone  type /pweaver/efa_fed-zone_code,
packages type /pweaver/efa_fed-num_pieces,
rated_weight type brgew_ap,"/pweaver/efa_fed-rated_wgt_amt,
transport_charge type netwr,"zpwefa_fedex-tran_charg_amnt,
handling_charge type  netwr,"zpwefa_fedex-tran_charg_amnt,
discount type netwr,"zpwefa_fedex-tran_charg_amnt,
net_charge type netwr,"zpwefa_fedex-net_chrg_amnt,
  line_color       type char4,
  end of ty_zpwefa_ups_zone_type.

data : it_output_ups_zone_type type standard table of ty_zpwefa_ups_zone_type,
       wa_output_ups_zone_type type ty_zpwefa_ups_zone_type . " ty_zpwefa_fedex_zone_type.

data: it_fieldcat_zone_ups_type type table of lvc_s_fcat,
      wa_fieldcat_zone_ups_type type lvc_s_fcat.


*************Payment Method *******
types : begin of ty_zpwefa_fedex_payor_type,
payor  type /pweaver/efa_fed-payor,
packages type /pweaver/efa_fed-num_pieces,
rated_weight type brgew_ap,       " zpwefa_fedex-rated_wgt_amt,
transport_charge type netwr,   "zpwefa_fedex-tran_charg_amnt,
handling_charge type netwr,    " zpwefa_fedex-tran_charg_amnt,
discount type netwr,           "zpwefa_fedex-tran_charg_amnt,
net_charge type netwr,         " zpwefa_fedex-net_chrg_amnt,
  line_color       type char4,
  end of ty_zpwefa_fedex_payor_type.


data : it_output_payor_type type standard table of ty_zpwefa_fedex_payor_type,
       wa_output_payor_type type ty_zpwefa_fedex_payor_type,
       it_final_payor_type type standard table of ty_zpwefa_fedex_payor_type,
       wa_final_payor_type type ty_zpwefa_fedex_payor_type,
       it_payor_type type standard table of /pweaver/efa_fed,
       wa_payor_type type /pweaver/efa_fed.


data: it_fieldcat_payor_type type table of lvc_s_fcat,
      wa_fieldcat_payor_type type lvc_s_fcat.



**************Payment Method ************


************Shipment date *************fedex*
types : begin of ty_zpwefa_fedex_shipdate_type,
shipdate  type /pweaver/efa_fed-shipment_date,
 packages type /pweaver/efa_fed-num_pieces,
 rated_weight type  brgew_ap, " zpwefa_fedex-rated_wgt_amt,
 transport_charge type netwr," zpwefa_fedex-tran_charg_amnt,
handling_charge type  netwr,"zpwefa_fedex-tran_charg_amnt,
discount type netwr,"zpwefa_fedex-tran_charg_amnt,
 net_charge type netwr,"zpwefa_fedex-net_chrg_amnt,
   line_color       type char4,
   end of ty_zpwefa_fedex_shipdate_type.


data : it_output_shipdate_type type standard table of ty_zpwefa_fedex_shipdate_type,
       wa_output_shipdate_type type ty_zpwefa_fedex_shipdate_type,
       it_final_shipdate_type type standard table of ty_zpwefa_fedex_shipdate_type,
       wa_final_shipdate_type type ty_zpwefa_fedex_shipdate_type,
       it_shipdate_type type standard table of /pweaver/efa_fed,
       wa_shipdate_type type /pweaver/efa_fed.


data: it_fieldcat_shipdate_type type table of lvc_s_fcat,
      wa_fieldcat_shipdate_type type lvc_s_fcat.


************End of shipment Date**************
*********************All Details**************
types : begin of ty_zpwefa_fedex_alldet_type,
shipment_date type /pweaver/efa_fed-shipment_date,
service_type type /pweaver/efa_fed-service_type,
exp_grd_trck_id type /pweaver/efa_fed-exp_grd_trck_id,
org_cust_ref type /pweaver/efa_fed-org_cust_ref,
org_ref_3 type /pweaver/efa_fed-org_ref_3,
pod(20) type c,
pod_del_date type /pweaver/efa_fed-pod_del_date,
pod_del_time type /pweaver/efa_fed-pod_del_time,
pod_sign_desc type /pweaver/efa_fed-pod_sign_desc,
zone_code type /pweaver/efa_fed-zone_code,
num_pieces type /pweaver/efa_fed-num_pieces,
rated_weight type brgew_ap," /pweaver/efa_fed-rated_wgt_amt,
shipper_name type /PWEAVER/EFA_FED-shipper_name,
 transport_charge  type netwr,"type zpwefa_fedex-tran_charg_amnt,
 fuel_surcharge type netwr," zpwefa_fedex-tran_charg_amnt,
handling_charge type  netwr,"zpwefa_fedex-tran_charg_amnt,
discount type netwr,"zpwefa_fedex-tran_charg_amnt,
perform_price type netwr,"zpwefa_fedex-net_chrg_amnt,
net_charge type  netwr,"zpwefa_fedex-net_chrg_amnt,
credit_amount type  netwr,"zpwefa_fedex-net_chrg_amnt,
error_code type  netwr,"zpwefa_fedex-net_chrg_amnt,
   line_color       type char4,
   end of ty_zpwefa_fedex_alldet_type.


data : it_output_alldet_type type standard table of ty_zpwefa_fedex_alldet_type,
       wa_output_alldet_type type ty_zpwefa_fedex_alldet_type,
       it_final_alldet_type type standard table of ty_zpwefa_fedex_alldet_type,
       wa_final_alldet_type type ty_zpwefa_fedex_alldet_type,
       it_alldet_type type standard table of /pweaver/efa_fed,
       wa_alldet_type type /pweaver/efa_fed.


data: it_fieldcat_alldet_type type table of lvc_s_fcat,
      wa_fieldcat_alldet_type type lvc_s_fcat.


********************end of All Details





*****begin of charge type summary - fedex
types : begin of ty_zpwefa_fedex_csr_type,
charg_desc type /pweaver/efa_fed-trk_id_chg_des,
packages type /pweaver/efa_fed-num_pieces,
rated_weight type /pweaver/efa_fed-rated_wgt_amt,
 transport_charge type /pweaver/efa_fed-tran_charg_amnt,
  handling_charge type  /pweaver/efa_fed-tran_charg_amnt,
  NET_CHRG_AMNT type /pweaver/efa_fed-NET_CHRG_AMNT,
   line_color       type char4,
   end of ty_zpwefa_fedex_csr_type.


data : it_output_csr_type type standard table of ty_zpwefa_fedex_csr_type,
       wa_output_csr_type type ty_zpwefa_fedex_csr_type,
       it_final_csr_type type standard table of ty_zpwefa_fedex_csr_type,
       wa_final_csr_type type ty_zpwefa_fedex_csr_type,
       it_csr_type type standard table of /pweaver/efa_fed,
       wa_csr_type type /pweaver/efa_fed.


data: it_fieldcat_csr_type type table of lvc_s_fcat,
      wa_fieldcat_csr_type type lvc_s_fcat.



******end of charge type  summary - fedex


*********************Address Correctoin - Fedex **************
types : begin of ty_zpwefa_fedex_addcorr_type,
shipment_date type char10,"zpwefa_fedex-shipment_date,
service_type type /pweaver/efa_fed-service_type,
exp_grd_trck_id type /pweaver/efa_fed-exp_grd_trck_id,
org_cust_ref type /pweaver/efa_fed-org_cust_ref,
org_ref_3 type /pweaver/efa_fed-org_ref_3,
pod(20) type c,
pod_del_date type char10, "zpwefa_fedex-pod_del_date,
pod_del_time type char8,"zpwefa_fedex-pod_del_time,
pod_sign_desc type /pweaver/efa_fed-pod_sign_desc,
 transport_charge type netwr,"zpwefa_fedex-tran_charg_amnt,
  net_charge type  netwr,"zpwefa_fedex-net_chrg_amnt,
  addr_charge type  netwr,"zpwefa_fedex-tran_charg_amnt,
recipeint_name type  /pweaver/efa_fed-recipeint_name,
recipeint_comp type  /pweaver/efa_fed-recipeint_comp,
recp_addr_line1 type  /pweaver/efa_fed-recp_addr_line1,
recp_addr_line2 type  /pweaver/efa_fed-recp_addr_line2,
recipient_city type  /pweaver/efa_fed-recipient_city,
recipient_state type  /pweaver/efa_fed-recipient_state,
rec_zip_code type  /pweaver/efa_fed-rec_zip_code,
rec_country type  /pweaver/efa_fed-rec_country,
   line_color       type char4,
   end of ty_zpwefa_fedex_addcorr_type.


data : it_output_addcorr_type type standard table of ty_zpwefa_fedex_addcorr_type,
       wa_output_addcorr_type type ty_zpwefa_fedex_addcorr_type,
       it_final_addcorr_type type standard table of ty_zpwefa_fedex_addcorr_type,
       wa_final_addcorr_type type ty_zpwefa_fedex_addcorr_type,
       it_addcorr_type type standard table of /pweaver/efa_fed,
       wa_addcorr_type type /pweaver/efa_fed.


data: it_fieldcat_addcorr_type type table of lvc_s_fcat,
      wa_fieldcat_addcorr_type type lvc_s_fcat.


********************end of address correction

*********************Begin of Comparision **************
types : begin of ty_zpwefa_fedex_comp_type,
shipment_date type /pweaver/efa_fed-shipment_date,
service_type type /pweaver/efa_fed-service_type,
exp_grd_trck_id type /pweaver/efa_fed-exp_grd_trck_id,
org_cust_ref type /pweaver/efa_fed-org_cust_ref,
org_ref_3 type /pweaver/efa_fed-org_ref_3,
pod(20) type c,
pod_del_date type /pweaver/efa_fed-pod_del_date,
pod_del_time type /pweaver/efa_fed-pod_del_time,
pod_sign_desc type /pweaver/efa_fed-pod_sign_desc,
zone_code type /pweaver/efa_fed-zone_code,
num_pieces type /pweaver/efa_fed-num_pieces,
rated_weight type brgew_ap,"zpwefa_fedex-rated_wgt_amt,
shipper_name type /pweaver/efa_fed-shipper_name,
 transport_charge type netwr,"zpwefa_fedex-tran_charg_amnt,
 fuel_surcharge type netwr,"zpwefa_fedex-tran_charg_amnt,
handling_charge type  netwr,"zpwefa_fedex-tran_charg_amnt,
discount type netwr,"zpwefa_fedex-tran_charg_amnt,
perform_price type netwr," zpwefa_fedex-net_chrg_amnt,
net_charge type  netwr,"zpwefa_fedex-net_chrg_amnt,
credit_amount type  netwr,"zpwefa_fedex-net_chrg_amnt,
error_code type netwr,"zpwefa_fedex-net_chrg_amnt,
***SAP Data
date_added type /PWEAVER/MANFEST-date_added,
time_added  type /PWEAVER/MANFEST-time_added,
plant  type /PWEAVER/MANFEST-plant,
vbeln  type /PWEAVER/MANFEST-vbeln,
pkgcount  type /PWEAVER/MANFEST-pkgcount,
totalpkg  type /PWEAVER/MANFEST-totalpkg,
handling_unit  type /PWEAVER/MANFEST-handling_unit,
tracking_number  type /PWEAVER/MANFEST-tracking_number,
package_weight  type brgew_ap,"/PWEAVER/MANFEST-package_weight,
carrier_code  type /PWEAVER/MANFEST-carrier_code,
carriertype  type /PWEAVER/MANFEST-carriertype,
freightamt  type netwr,"/PWEAVER/MANFEST-freightamt,
discountamt  type netwr,"/PWEAVER/MANFEST-discountamt,
insurance  type /PWEAVER/MANFEST-insurance,
canc_dt  type /PWEAVER/MANFEST-canc_dt,
canc_tim  type /PWEAVER/MANFEST-canc_tim,
company  type /PWEAVER/MANFEST-company,
contact  type /PWEAVER/MANFEST-contact,
address1  type /PWEAVER/MANFEST-address1,
address2  type /PWEAVER/MANFEST-address2,
city  type /PWEAVER/MANFEST-city,
region  type /PWEAVER/MANFEST-region,
postalcode  type /PWEAVER/MANFEST-postalcode,
country  type /PWEAVER/MANFEST-country,
phone  type /PWEAVER/MANFEST-phone,
comments(40) type c,
   line_color       type char4,

   end of ty_zpwefa_fedex_comp_type.


data : it_output_comp_type type standard table of ty_zpwefa_fedex_comp_type,
       wa_output_comp_type type ty_zpwefa_fedex_comp_type,
       it_final_comp_type type standard table of ty_zpwefa_fedex_comp_type,
       wa_final_comp_type type ty_zpwefa_fedex_comp_type,
       it_comp_type type standard table of /pweaver/efa_fed,
       wa_comp_type type /pweaver/efa_fed,
       it_sap_comp_type type standard table of /PWEAVER/MANFEST,
       wa_sap_comp_type type /PWEAVER/MANFEST.


data: it_fieldcat_comp_type type table of lvc_s_fcat,
      wa_fieldcat_comp_type type lvc_s_fcat.


********************end of Comparison

********begin of guaranteed service refund****FedEx


types : begin of ty_zpwefa_fedex_gsr_type,
shipment_date type /pweaver/efa_fed-shipment_date,
service_type type /pweaver/efa_fed-service_type,
exp_grd_trck_id type /pweaver/efa_fed-exp_grd_trck_id,
org_cust_ref type /pweaver/efa_fed-org_cust_ref,
org_ref_3 type /pweaver/efa_fed-org_ref_3,
pod(20) type c,
pod_del_date type /pweaver/efa_fed-pod_del_date,
pod_del_time type /pweaver/efa_fed-pod_del_time,
pod_sign_desc type /pweaver/efa_fed-pod_sign_desc,
 transport_charge type /pweaver/efa_fed-tran_charg_amnt,
   line_color       type char4,

   end of ty_zpwefa_fedex_gsr_type.


data : it_output_gsr_type type standard table of ty_zpwefa_fedex_gsr_type,
       wa_output_gsr_type type ty_zpwefa_fedex_gsr_type,
       it_final_gsr_type type standard table of ty_zpwefa_fedex_gsr_type,
       wa_final_gsr_type type ty_zpwefa_fedex_gsr_type,
       it_gsr_type type standard table of /pweaver/efa_fed,
       wa_gsr_type type /pweaver/efa_fed.

data: it_fieldcat_gsr_type type table of lvc_s_fcat,
      wa_fieldcat_gsr_type type lvc_s_fcat.


********************end of guaranteed service refund


******************Begin of Reminttance report
types : begin of ty_zpwefa_fedex_remit_type,
    service_type type /pweaver/efa_fed-service_type,
  invoice_number type /pweaver/efa_fed-invoice_number,
bill_to_acc_num(10) type c,
invoice_date(10) type c,
packages type /pweaver/efa_fed-num_pieces,
invoice_amount type /pweaver/efa_fed-num_pieces,
transport_charge type /pweaver/efa_fed-tran_charg_amnt,
  handling_charge  type  /pweaver/efa_fed-num_pieces,
discount type /pweaver/efa_fed-num_pieces,
performance type /pweaver/efa_fed-num_pieces,
 credit_amount type /pweaver/efa_fed-num_pieces,
net_charge type /pweaver/efa_fed-num_pieces,
   line_color       type char4,
   end of ty_zpwefa_fedex_remit_type.


data : it_output_remit_type type standard table of ty_zpwefa_fedex_remit_type,
       wa_output_remit_type type ty_zpwefa_fedex_remit_type,
       it_final_remit_type type standard table of ty_zpwefa_fedex_remit_type,
       wa_final_remit_type type ty_zpwefa_fedex_remit_type,
       it_remit_type type standard table of /pweaver/efa_fed,
       wa_remit_type type /pweaver/efa_fed.

data: it_fieldcat_remit_type type table of lvc_s_fcat,
      wa_fieldcat_remit_type type lvc_s_fcat.

*****************End of Remittance Report

****************Begin of internal table for samsung
types : begin of ty_samsung,
sender_identifier(8) type c,
receiver_identifier(8) type c,
invoice_number(40) type c,
billing_date(8) type c,
division(5) type c,
business_type(5) type c,
order_type(5) type c,
transportation_type(10) type c,
actual_date(8) type c,
reference_type(5) type c,
reference_message(10) type c,
reference_document(10) type c,
reference_date type sy-datum,
related_document_1(10) type c,
related_document_2(40) type c,
service_type(10) type c,
incoterms_code(10) type c,
incoterms_place(10) type c,
plant(10) type c,
wh_location(10) type c,
carrier(10) type c,
forwarder(10) type c,
from_place(10) type c,
country_from(10) type c,
from_port(10) type c,
country_port(10) type c,
to_place(10) type c,
country_place(10) type c,
to_port(10) type c,
country_of_port(10) type c,
general_info_1(10) type c,
general_info_2(10) type c,
general_info_3(10) type c,
sequence_num(10) type c,
cargo_measurement(10) type c,
mesasure_amount type bseg-dmbtr,
  mesasure_unit(5) type c,
sequence_amount(10) type c,
truck_classificatin(10) type c,
truck_number(10) type c,
truck_type(10) type c,
truck_round(10) type c,
consolidation_number(10) type c,
consolidation_count(10) type c,
multi_stop(10) type c,
sequence(5) type c,
logistic_code(6) type c,
sub_code(6) type c,
supply_amount type /pweaver/efa_fed-num_pieces,
tax_amount(8) type c,
request_amount type /pweaver/efa_fed-num_pieces,
currency(5) type c,
fr_issue_date type sy-datum,
rate_exchange type bseg-dmbtr,
basis_currency type bseg-wrbtr,
destination_currency type bseg-wrbtr,
rate_exchange_date type sy-datum,
remark_1(10) type c,
remark_2(10) type c,
remark_3(10) type c,
sequence_number(10) type c,
reference_doc_code(10) type c,
reference_doc_number(10) type c,
reference_doc_date(10) type c,
material type mara-matnr,
plant_1 type marc-werks,
pack_quantity(10) type c,
pack_quan_code(10) type c,
quantity(10) type c,
quantity_code(10) type c,
volume(10) type c,
volume_code(10) type c,
gross_weight(10) type c,
gross_weight_code(10) type c,
net_weight(10) type c,
net_weight_code(10) type c,
chargable_weight(10) type c,
chargable_weight_code(10) type c,
    line_color       type char4,
end of ty_samsung.

data : it_output_sam_fedex_type type standard table of ty_samsung,
       wa_output_sam_fedex_type type ty_samsung,
       it_final_sam_fedex_type type standard table of ty_samsung,
       wa_final_sam_fedex_type type ty_samsung,
       it_sam_fedex_type type standard table of /pweaver/efa_fed,
       wa_sam_fedex_type type /pweaver/efa_fed.

data: it_fieldcat_samsung_type type table of lvc_s_fcat,
      wa_fieldcat_samsung_type type lvc_s_fcat.




****************End of internal table for samsung



TYPES : BEGIN OF ty_service_type,
          servicetype type /pweaver/efa_ups-CHRG_DESC,
          packages    type /pweaver/efa_ups-PCKG_QTY,
          actualweight TYPE ntgew_ap,
          ratedweight type NTGEW_AP,
          SHIP_INCENT_AMNT type netwr,
SHIP_NET_AMOUNT type netwr,
HANDLE_INCEN_AMT type netwr,
HANDLE_NET_AMNT type netwr,
CORR_SHIP_INCENT type netwr,
CORR_SHIP_NET type netwr,
CORR_HANDLE_DISC type netwr,
CORR_HANDLE_NET type netwr,
MISC_INCENT_CHRG type netwr,
MISC_NET_CHRG type netwr,
INV_ADD_DISC type  netwr,
INV_ADD_NET type netwr,
INV_MISC_DISC type netwr,
INV_MISC_NET type netwr,
ADDR_CORR_CHARGE type netwr,
 chrg_catg_detail type /pweaver/efa_ups-chrg_catg_det_cd,
  end of ty_service_type.

data : it_service_type TYPE STANDARD TABLE OF ty_service_type,
       wa_service_type type ty_service_type.
DATA : it_ups_summ type STANDARD TABLE OF /PWEAVER/EFA_UPS_SUMM,
       wa_ups_summ type /PWEAVER/EFA_UPS_SUMM.
TYPES : BEGIN OF TY_SUMM,

        DESC(50) TYPE C,
        CODE TYPE /PWEAVER/EFA_UPS-CHRG_CATG_DET_CD,
        net(8)      TYPE C,
        END OF TY_SUMM.
DATA :  IT_SUMM TYPE STANDARD TABLE OF TY_SUMM,
        WA_SUMM TYPE TY_SUMM.

TYPES : BEGIN OF ty_output_service_type,
          service_type type /pweaver/efa_ups-CHRG_DESC,
          total_packs  type /pweaver/efa_ups-PCKG_QTY,
          actual_weight type ntgew_ap,
          rated_weight type ntgew_ap,
          transportation_charge type netwr,
          handling_charge type netwr,
          total_discounts type netwr,
          adjustment_charge type netwr,
          correction_charge type netwr,
          net_charge type netwr,
          line_color type char4,
        END OF ty_output_service_type.

data : it_output_service_type TYPE STANDARD TABLE OF ty_output_service_type,
       wa_output_service_type TYPE ty_output_service_type.

data : it_fieldcat type table of lvc_s_fcat,
       wa_fieldcat type lvc_s_fcat.

**************** For ZONE Type  **************

  TYPES : BEGIN OF ty_zone_type,
            zzone type /PWEAVER/Z_ONE,
            packages    type /pweaver/efa_ups-PCKG_QTY,
            actualweight type ntgew_ap,
            ratedweight type NTGEW_AP,
            SHIP_INCENT_AMNT type netwr,
            SHIP_NET_AMOUNT type netwr,
            HANDLE_INCEN_AMT type netwr,
            HANDLE_NET_AMNT type netwr,
            CORR_SHIP_INCENT type netwr,
            CORR_SHIP_NET type netwr,
            CORR_HANDLE_DISC type netwr,
            CORR_HANDLE_NET type netwr,
            MISC_INCENT_CHRG type netwr,
            MISC_NET_CHRG type netwr,
            INV_ADD_DISC type  netwr,
            INV_ADD_NET type netwr,
            INV_MISC_DISC type netwr,
            INV_MISC_NET type netwr,
            ADDR_CORR_CHARGE type netwr,
    end of ty_zone_type.


TYPES : BEGIN OF ty_output_zone_type,
          zzone type /PWEAVER/Z_ONE,
          total_packs  type /pweaver/efa_ups-PCKG_QTY,
          actual_weight type ntgew_ap,
          rated_weight type ntgew_ap,
          transportation_charge type netwr,
          handling_charge type netwr,
          total_discounts type netwr,
          adjustment_charge type netwr,
          correction_charge type netwr,
          net_charge type netwr,
          line_color  type char4,
        END OF ty_output_zone_type.

data : it_zone_type_ups TYPE STANDARD TABLE OF ty_zone_type,
       wa_zone_type_ups type ty_zone_type.

data : it_output_zone_type_ups TYPE STANDARD TABLE OF ty_output_zone_type,
       wa_output_zone_type_ups type ty_output_zone_type.



**************** For Package Type  **************

  TYPES : BEGIN OF ty_pack_type,
            package_type type char10,
            packages    type /pweaver/efa_ups-PCKG_QTY,
            actualweight type ntgew_ap,
            ratedweight type NTGEW_AP,
            SHIP_INCENT_AMNT type netwr,
            SHIP_NET_AMOUNT type netwr,
            HANDLE_INCEN_AMT type netwr,
            HANDLE_NET_AMNT type netwr,
            CORR_SHIP_INCENT type netwr,
            CORR_SHIP_NET type netwr,
            CORR_HANDLE_DISC type netwr,
            CORR_HANDLE_NET type netwr,
            MISC_INCENT_CHRG type netwr,
            MISC_NET_CHRG type netwr,
            INV_ADD_DISC type  netwr,
            INV_ADD_NET type netwr,
            INV_MISC_DISC type netwr,
            INV_MISC_NET type netwr,
            ADDR_CORR_CHARGE type netwr,
    end of ty_pack_type.


TYPES : BEGIN OF ty_output_pack_type,
          package_type type char10,
          total_packs  type /pweaver/efa_ups-PCKG_QTY,
          actual_weight type ntgew_ap,
          rated_weight type ntgew_ap,
          transportation_charge type netwr,
          handling_charge type netwr,
          total_discounts type netwr,
          adjustment_charge type netwr,
          correction_charge type netwr,
          net_charge type netwr,
          line_color  type char4,
        END OF ty_output_pack_type.

data : it_pack_type_ups TYPE STANDARD TABLE OF ty_pack_type,
       wa_pack_type_ups type ty_pack_type.

data : it_output_pack_type_ups TYPE STANDARD TABLE OF ty_output_pack_type,
       wa_output_pack_type_ups type ty_output_pack_type.

*************** Payment Method  *****************

  TYPES : BEGIN OF ty_payment_type,
            payment_method type /PWEAVER/BILL_OPT_CODE,
            packages    type /pweaver/efa_ups-PCKG_QTY,
            actualweight type ntgew_ap,
            ratedweight type NTGEW_AP,
            SHIP_INCENT_AMNT type netwr,
            SHIP_NET_AMOUNT type netwr,
            HANDLE_INCEN_AMT type netwr,
            HANDLE_NET_AMNT type netwr,
            CORR_SHIP_INCENT type netwr,
            CORR_SHIP_NET type netwr,
            CORR_HANDLE_DISC type netwr,
            CORR_HANDLE_NET type netwr,
            MISC_INCENT_CHRG type netwr,
            MISC_NET_CHRG type netwr,
            INV_ADD_DISC type  netwr,
            INV_ADD_NET type netwr,
            INV_MISC_DISC type netwr,
            INV_MISC_NET type netwr,
            ADDR_CORR_CHARGE type netwr,
    end of ty_payment_type.


TYPES : BEGIN OF ty_output_payment_type,
          payment_method type /PWEAVER/BILL_OPT_CODE,
          total_packs  type /pweaver/efa_ups-PCKG_QTY,
          actual_weight type ntgew_ap,
          rated_weight type ntgew_ap,
          transportation_charge type netwr,
          handling_charge type netwr,
          total_discounts type netwr,
          adjustment_charge type netwr,
          correction_charge type netwr,
          net_charge type netwr,
          line_color  type char4,
        END OF ty_output_payment_type.

data : it_payment_type type STANDARD TABLE OF ty_payment_type,
       wa_payment_type type ty_payment_type.

data : it_output_payment_type TYPE STANDARD TABLE OF ty_output_payment_type,
       wa_output_payment_type type ty_output_payment_type.

*************** Ship Day *****************

  TYPES : BEGIN OF ty_shipday,
            ship_date(10) TYPE c," zpwdate,
            packages    type /pweaver/efa_ups-PCKG_QTY,
            actualweight type ntgew_ap,
            ratedweight type NTGEW_AP,
            SHIP_INCENT_AMNT type netwr,
            SHIP_NET_AMOUNT type netwr,
            HANDLE_INCEN_AMT type netwr,
            HANDLE_NET_AMNT type netwr,
            CORR_SHIP_INCENT type netwr,
            CORR_SHIP_NET type netwr,
            CORR_HANDLE_DISC type netwr,
            CORR_HANDLE_NET type netwr,
            MISC_INCENT_CHRG type netwr,
            MISC_NET_CHRG type netwr,
            INV_ADD_DISC type  netwr,
            INV_ADD_NET type netwr,
            INV_MISC_DISC type netwr,
            INV_MISC_NET type netwr,
            ADDR_CORR_CHARGE type netwr,
    end of ty_shipday.


TYPES : BEGIN OF ty_output_shipday,
          ship_date(10) type c,"zpwdate,
          total_packs  type /pweaver/efa_ups-PCKG_QTY,
          actual_weight type ntgew_ap,
          rated_weight type ntgew_ap,
          transportation_charge type netwr,
          handling_charge type netwr,
          total_discounts type netwr,
          adjustment_charge type netwr,
          correction_charge type netwr,
          net_charge type netwr,
          line_color  type char4,
        END OF ty_output_shipday.

data : it_shipday TYPE STANDARD TABLE OF ty_shipday,
       wa_shipday TYPE ty_shipday,

       it_output_shipday TYPE STANDARD TABLE OF ty_output_shipday,
       wa_output_shipday type ty_output_shipday.

data : it_inv_misc type STANDARD TABLE OF /PWEAVER/EFA_UPS_SUMM,
       wa_inv_misc type /PWEAVER/EFA_UPS_SUMM.

data : v_disc type netwr,
       v_net type netwr,
       v_corr type netwr.

************** For All Details *****************

types : begin of ty_all_details,
          ship_date type string," zpwdate,
          service_type type /pweaver/efa_ups-CHRG_DESC,
          invoice_number type /pweaver/efa_ups-INVOICE_NUMBER,
          zone type /PWEAVER/Z_ONE,
          tracking_number type /PWEAVER/TRCK_NUM,
          container_type type char10,
          package_qty  type /pweaver/efa_ups-PCKG_QTY,
          actual_weight type ntgew_ap,
          billed_weight type ntgew_ap,
          chrg_catg_det_cd type /pweaver/efa_ups-chrg_catg_det_cd,
          transportation_charge type netwr,
          handling_charge type netwr,
          total_discounts type netwr,
          adjustment_charge type netwr,
          correction_charge type netwr,
          net_charge type netwr,
          sender_name type ad_name1,
          sender_company type ad_name1,
          sender_address1 type ad_name1,
          sender_city type ad_city1,
          sender_state type regio,
          sender_postcode type pstlz,
          sender_country type land1,
          receiver_name type ad_name1,
          receiver_company type ad_name1,
          rec_address1 type ad_name1,
          receiver_city type ad_city1,
          receiver_state type regio,
          rec_postcode type pstlz,
          receiver_country type land1,
          line_color  type char4,
        end of ty_all_details.
        types : begin of ty_all_detailsd,
          ship_date type string," zpwdate,
          service_type type /pweaver/efa_ups-CHRG_DESC,
          invoice_no type /pweaver/efa_ups-INVOICE_NUMBER,
          zone type /PWEAVER/Z_ONE,
          tracking_number type /PWEAVER/TRCK_NUM,
          container_type type char10,
          package_qty  type /pweaver/efa_ups-PCKG_QTY,
          actual_weight type ntgew_ap,
          billed_weight type ntgew_ap,
           chrg_catg_det_cd type /pweaver/efa_ups-chrg_catg_det_cd,
          transportation_charge type netwr,
          handling_charge type netwr,
          total_discounts type netwr,
          adjustment_charge type netwr,
          correction_charge type netwr,
          net_charge type netwr,
          sender_name type ad_name1,
          sender_company type ad_name1,
          sender_address1 type ad_name1,
          sender_city type ad_city1,
          sender_state type regio,
          sender_postcode type pstlz,
          sender_country type land1,
          receiver_name type ad_name1,
          receiver_company type ad_name1,
          rec_address1 type ad_name1,
          receiver_city type ad_city1,
          receiver_state type regio,
          rec_postcode type pstlz,
          receiver_country type land1,
          line_color  type char4,
        end of ty_all_detailsd.

data : it_all_details TYPE STANDARD TABLE OF ty_all_details,
       wa_all_details type ty_all_details.
data : it_all_details1 TYPE STANDARD TABLE OF ty_all_details,
       wa_all_details1 type ty_all_details.
data : it_all_detailsd TYPE STANDARD TABLE OF ty_all_detailsd,
       wa_all_detailsd type ty_all_detailsd.
data :  it_all_detailsd_final type standard table of ty_all_detailsd.
data : it_all_track TYPE STANDARD TABLE OF ty_all_details,
       wa_all_track type ty_all_details.
TYPES : begin of ty_TRACK,
        TRACKING_NO TYPE /PWEAVER/EFA_TRC-TRACKING_NO,
        INVOICE_NO TYPE /PWEAVER/EFA_TRC-INVOICE_NO,
        end of ty_TRACK.

DATA : IT_TRACK TYPE STANDARD TABLE OF TY_TRACK,
        WA_TRACK TYPE TY_TRACK.
DATA : IT_FINAL TYPE STANDARD TABLE OF TY_TRACK,
        WA_FINAL TYPE TY_TRACK.
*************************** By Address Corrections ********************

TYPES : begin of ty_addr,
          ship_date type string," zpwdate,
          service_type type /pweaver/efa_ups-CHRG_DESC,
          zone type /PWEAVER/Z_ONE,
          tracking_number type /PWEAVER/TRCK_NUM,
          package_qty  type /pweaver/efa_ups-PCKG_QTY,
          actual_weight type ntgew_ap,
          billed_weight type ntgew_ap,
          transportation_charge type netwr,
          handling_charge type netwr,
          total_discounts type netwr,
          adjustment_charge type netwr,
          correction_charge type netwr,
          address_correction type netwr,
          net_charge type netwr,
          line_color  type char4,
        end of ty_addr.

data : it_addr TYPE  STANDARD TABLE OF ty_addr,
       wa_addr type ty_addr.


  TYPES : BEGIN OF ty_gsr_ups,
            tracking_number type /PWEAVER/TRCK_NUM,
            pickup_date type /pweaver/DATE,
            pod_date TYPE /pweaver/DATE,
            gsr_desc type /pweaver/date,
            pod_sig type /pweaver/efa_trc-pod_signature,
            exp_date type /pweaver/efa_trc-exp_del_date,
          end of ty_gsr_ups.

data : it_gsr_ups type STANDARD TABLE OF ty_gsr_ups,
       wa_gsr_ups TYPE ty_gsr_ups.
 TYPES : BEGIN OF ty_gsr_fedex,
            EXP_GRD_TRCK_ID type /PWEAVER/TRCK_NUM,
            pickup_date type /pweaver/DATE,
            POD_DEL_DATE TYPE /pweaver/DATE,
*            gsr_desc type /pweaver/date,
            POD_SIGN_DESC type /pweaver/efa_trc-pod_signature,
            exp_date type /pweaver/efa_trc-exp_del_date,
          end of ty_gsr_fedex.
 data : it_gsr_fedex type STANDARD TABLE OF ty_gsr_fedex,
       wa_gsr_fedex TYPE ty_gsr_fedex.



data :  is_zpwefa_upstrc  type /pweaver/efa_trc.

data : v_viewer.

data : wa_addr_corr_data type /PWEAVER/EFA_UPS_SUMM,
       it_addr_corr_data TYPE STANDARD TABLE OF /PWEAVER/EFA_UPS_SUMM.

***************************MANIFEST
types :
        begin of x_efa_bill_ups,
       line_color  type char4,

         inv_no          type  /pweaver/INVOICE_NUMBER,
         inv_date        type  sy-datum,
         SHIP_DATE      type string ,"   sy-datum ,


*         ship_method     LIKE lfa1-name1,
          ship_method(40)     type c,
         Track_no        type  /PWEAVER/TRCK_NUM,
         VBELN           TYPE /PWEAVER/MANFEST-VBELN,
         PURCHASE_ORDER      TYPE /PWEAVER/MANFEST-REFERENCE2,
         SALES_ORDER type /pweaver/manfest-SALES_ORDER,
         v_discount      type  netwr,          "FRAKO,                  " discount amount
         v_bill_efa      type  netwr,   "ZPWEFA_UPS-NET_AMNT,    " billed charges
         v_bill_man      type  netwr,  "FRAKO,                  "frieght charges
         v_bill_diff     type netwr,   "v_bill_diff(18) type  c,                      " difference between billed charges and frieght ant for each Tracking number

        end of x_efa_bill_ups,


       begin of x_zpwmanifest_ups,

         DATE_ADDED      type	ERZDT ,
         vbeln  type /pweaver/manfest-vbeln,
         PLANT           type WERKS_D,
         TRACKING_NUMBER type /PWEAVER/TRCK_NUM,
         carrier_code    LIKE vbpa-lifnr ,
         ship_method     LIKE lfa1-name1,
         FREIGHTAMT(18)  type c,
         DISCOUNTAMT(18) type c,
         MASTERTRACKING TYPE  /PWEAVER/TRCK_NUM,
         SALES_ORDER   TYPE /PWEAVER/MANFEST-SALES_ORDER,
         PURCHASE_ORDER type /pweaver/manfest-PURCHASE_ORDER,

        end of x_zpwmanifest_ups,
*        ********************CHNAGES TO INVOICE VS MANIFEST REPORT********************
           begin of x_efa_INVSMAN,
*       line_color  type char4,

*         inv_no          type  /pweaver/INVOICE_NUMBER,
*         inv_date        type  sy-datum,
*         SHIP_DATE      type string ,"   sy-datum ,
*
*
*         ship_method     LIKE lfa1-name1,
         Track_no        type  /PWEAVER/TRCK_NUM,
*         v_discount      type  netwr,          "FRAKO,                  " discount amount
         FRIGHT      type  netwr,   "ZPWEFA_UPS-NET_AMNT,    " billed charges
*         v_bill_man      type  netwr,  "FRAKO,                  "frieght charges
*         v_bill_diff     type netwr,   "v_bill_diff(18) type  c,
                       " difference between billed charges and frieght ant for each Tracking number
         MASTER_TRACK TYPE /PWEAVER/TRCK_NUM,


        end of x_efa_invsman.
*         **********************END OF CHANGE******************************************
*
*
data :
*
       t_efa_bill  type standard table of x_efa_bill_ups,
       w_efa_bill  type x_efa_bill_ups,
*       ***********chnages IN VS MANIFEST****************
           t_efa_INVSMAN  type standard table of X_EFA_INVSMAN,
       w_efa_INVSMAN  type X_EFA_INVSMAN,
*        *************END OF CHANGE *************************
       t_efa_ups   type standard table of /pweaver/efa_UPS,
       t_efa_ups1  type standard table of /pweaver/efa_UPS,
       w_efa_ups   type  /pweaver/efa_UPS,
       w_efa_ups1   type /pweaver/efa_UPS,
       t_efa_track type standard table of /pweaver/efa_trc,
       w_efa_track type /pweaver/efa_trc,
       t_man       type standard table of X_zpwmanifest_ups,
       w_man       type X_zpwmanifest_ups          ,
       t_fieldcat type standard table of lvc_s_fcat,
       w_fieldcat type lvc_s_fcat  .




data: v_tot_bill type  netwr," ZPWEFA_UPS-NET_AMNT,
      lv_bill_efa(18) type c,
      lv_bill_man(18) type c.

data: ls_lfa1 type lfa1,
      ls_carrierconfig type /pweaver/cconfig  ,
      t_carrierconfig type standard table of /pweaver/cconfig           .
data :W_LAYOUT TYPE lvc_s_layo,
     v_repid type string value sy-repid .





types : Begin of x_efa_fed,

       ship_date       type sy-datum,
       ship_method     type /pweaver/SERVICE_TYPE,
       track_num       type /PWEAVER/TRCK_NUM,
       VBELN           TYPE /PWEAVER/MANFEST-VBELN,
       PURCHASE_ORDER     TYPE /PWEAVER/MANFEST-PURCHASE_ORDER,
       transp_chg      type netwr,"ZPWTRAN_CHARG_AMNT,
       handl_chg       type netwr,"ZPWTRK_ID_CHG_AMT1,
       disc_amt        type netwr,"ZPWTRK_ID_CHG_AMT1,
       efa_net_chg     type netwr,"ZPWNET_CHRG_AMNT,
       FREIGHTAMT      type netwr,"ZPWNET_CHRG_AMNT,
       diff            type netwr,"ZPWNET_CHRG_AMNT,
       line_color      type char4,
       End of  x_efa_fed.

 data : t_efa_fed type table of  x_efa_fed.

DATA  w_efa_fed type x_efa_fed..
types : BEGIN OF x_efa_man_fed,

        trk_num        type /PWEAVER/TRCK_NUM,
        FREIGHTAMT     type FRAKO ,
        DISCOUNTAMT    type FRAKO,
        VBELN          TYPE /PWEAVER/MANFEST-VBELN,
        PURCHASE_ORDER TYPE /PWEAVER/MANFEST-PURCHASE_ORDER,

        END   OF x_efa_man_fed.

  data : t_man_fed type table of x_efa_man_fed,
         w_man_fed type x_efa_man_fed,

         t_efa_f type table of /pweaver/efa_fed,
         w_efa_f type /pweaver/efa_fed,
              t_efa_gsr type table of /pweaver/efa_fed,
         w_efa_gsr type /pweaver/efa_fed.


DATA :gs_variant TYPE disvariant,
      date_conc type string.



******************************************************************
