*&---------------------------------------------------------------------*
*& Report  /PWEAVER/EFA_ADMINS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  /PWEAVER/EFA_ADMINS.
*&---------------------------------------------------------------------*
*& Report  /PWEAVER/EFA_ADMINS
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*


* Data Declaration and Internal tables

include /PWEAVER/efa_admin_top.



*---------------------------------------------------------------------*
*INITIALISATION.
initialization.

* Add displayed text string to buttons
*move 'Invoice History' to sscrfields-functxt_01.
*move 'Service Discounts' to sscrfields-functxt_02.
*move 'Zone Discounts' to sscrfields-functxt_03.
*move 'Weight Break Discounts' to sscrfields-functxt_04.
*move 'Vendor Code' to sscrfields-functxt_05.


*  SET PF-STATUS 'ZEFA_ADMIN'.


*** Get all the plants
refresh : it_plant.
select distinct plant from /PWEAVER/CCONFIG into table it_plant.

****Get all the careers
refresh : it_carrier.
select distinct carriertype from /PWEAVER/CCONFIG into table it_carrier.


*---------------------------------------------------------------------*
* EVENT : AT SELECTION-SCREEN                                         *
*---------------------------------------------------------------------*
*AT SELECTION-SCREEN .
at selection-screen on value-request for p_filnam.
*
** For f4 help
 perform get_file_name.

*---------------------------------------------------------------------*
* EVENT : AT SELECTION-SCREEN                                         *
*---------------------------------------------------------------------*
at selection-screen.

*if sscrfields-ucomm = 'FC01'.
*submit zpwefa_inv_history and return.
*elseif sscrfields-ucomm = 'FC02'.
*submit zpwefa_service_discounts and return.
*elseif sscrfields-ucomm = 'FC03'.
*submit zpwefa_zone_discounts and return.
*elseif sscrfields-ucomm = 'FC04'.
*submit zpwefa_discounts and return.
*elseif sscrfields-ucomm = 'FC05'.
*call transaction 'ZPWEFA_VENDOR'.
*endif.

*---------------------------------------------------------------------*
* EVENT : AT SELECTION-SCREEN                                         *
*---------------------------------------------------------------------*
at selection-screen output.




clear : wa_plant,wa_vrm_plant.
loop at it_plant into wa_plant.
  clear : v_tabix,v_number.
  v_tabix = sy-tabix.
 concatenate '0' v_tabix into v_number.
wa_vrm_plant-key = v_number.
wa_vrm_plant-text = wa_plant-werks.
append  wa_vrm_plant to it_vrm_plant.
clear wa_vrm_plant.
endloop.

clear : wa_carrier,wa_vrm_carrier.
*loop at it_carrier into wa_carrier.
*clear : v_tabix,v_number.
*    v_tabix = sy-tabix.
* concatenate '0' v_tabix into v_number.
* wa_vrm_carrier-key = v_number.
*wa_vrm_carrier-text = wa_carrier-carriertype.
*append  wa_vrm_carrier to it_vrm_carrier.
*clear wa_vrm_carrier.
*endloop.

 wa_vrm_carrier-key = 'UPS'.
wa_vrm_carrier-text = 'UPS'.
append  wa_vrm_carrier to it_vrm_carrier.
clear wa_vrm_carrier.

 wa_vrm_carrier-key = 'FEDEX'.
wa_vrm_carrier-text = 'FEDEX'.
append  wa_vrm_carrier to it_vrm_carrier.
clear wa_vrm_carrier.
** wa_vrm_carrier-key = 'FEDEX FRT'.
**wa_vrm_carrier-text = 'FEDEX FRT'.
**append  wa_vrm_carrier to it_vrm_carrier.
**clear wa_vrm_carrier.


call function 'VRM_SET_VALUES'
exporting
id = 'PS_PLANT'
values = it_vrm_plant.

call function 'VRM_SET_VALUES'
exporting
id = 'PS_CARR'
values = it_vrm_carrier.
*refresh : it_vrm_carrier.



*----------------------------------------------------------------------*
* START OF SELECTION                                                  *
*----------------------------------------------------------------------*
start-of-selection.




** Get the data from excel into internal table
  perform get_data_internal_table.
*




*****************************************************************
* Top Of Page Event to print Headers at start of new page        *
*****************************************************************
top-of-page.
** Page Header
*  PERFORM PAGE_HEADER.
*
*END-OF-SELECTION.
*
**Listing the results .
*
*  PERFORM WRITE_REPORT.



*AT LINE-SELECTION.

*  PERFORM out.
*  sy-lsind = sy-lsind - 1.

*---------------------------------------------------------------------*
* EVENT : USER-COMMAND                                                *
*---------------------------------------------------------------------*
at user-command.


* WHEN THE LIST INDEX IS ONE DON'T SHOW THE EXECUTE BUTTON.




*----------------------------------------------------------------------*
*       INCLUDE
*----------------------------------------------------------------------*


include /PWEAVER/efa_admin_form.
