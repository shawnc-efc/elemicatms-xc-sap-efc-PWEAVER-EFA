*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/EFA_ADMIN_TOP Report /PWEAVER/EFA_ADMIN
*&---------------------------------------------------------------------*


tables : /pweaver/cconfig,sscrfields.


***Type-Pools
type-pools: vrm, soi.


* DATA DICLARATION.
data : plant type vrm_id,
       v_number(2) type c,
       v_tabix(2) type c,
       v_carrier(8) type c.
*plant_list TYPE vrm_values,
*plant_value like line of list.


****Internal tables


 types : begin of ty_plant,
  werks type marc-werks,
  end of ty_plant.

 types : begin of ty_carrier,
  carriertype type /pweaver/cconfig-carriertype,
  end of ty_carrier.

 data : it_vrm_plant type vrm_values,
       wa_vrm_plant type vrm_value,
       it_plant type standard table of ty_plant,
       wa_plant type ty_plant.

 data : it_vrm_carrier type vrm_values,
       wa_vrm_carrier type vrm_value,
       it_carrier type standard table of ty_carrier,
       wa_carrier type ty_carrier.

*data : it_data type table of alsmex_tabline initial size 0,
*       is_data type alsmex_tabline.

data : it_data type table of KCDE_CELLS initial size 0,
       is_data type KCDE_CELLS.
*


*********SELECTION SCREEN***************

 selection-screen begin of block b1 with frame title text-001.
 parameters : p_filnam like rlgrap-filename," memory id m01,
              p_date  like sy-datum,
              ps_carr type /pweaver/cconfig-carriertype as listbox visible length 13.
* Add button to application toolbar
*selection-screen function key 1.  "Will have a function code of 'FC01'
*selection-screen function key 2.  "Will have a function code of 'FC02'
*selection-screen function key 3.  "Will have a function code of 'FC03'
*selection-screen function key 4.  "Will have a function code of 'FC03'
*selection-screen function key 5.  "Will have a function code of 'FC03'

 selection-screen end of block b1.
* ps_plant type marc-werks as listbox visible length 10,
 data : ps_plant type c.


  DATA: t_data TYPE soi_generic_table.
