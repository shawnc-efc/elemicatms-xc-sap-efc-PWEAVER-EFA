*&---------------------------------------------------------------------*
*&  Include           /PWEAVER/FREIGHT_AUDIT_F01
*&---------------------------------------------------------------------*

*{   INSERT         &$&$&$&$                                          1

*}   INSERT
form create_and_init_tree.

  data: event type cntl_simple_event,
        events type cntl_simple_events.

* create a simple tree model instance
  create object g_tree
    EXPORTING
      node_selection_mode         = cl_simple_tree_model=>node_sel_mode_single
    EXCEPTIONS
      illegal_node_selection_mode = 1.
  if sy-subrc <> 0.
    message a001.
  endif.

** create a container for the tree control
*  create object g_custom_container
*    EXPORTING      " the container is linked to the custom control with the
*              " name 'TREE_CONTAINER' on the dynpro
*      container_name              = 'TREE_CONTAINER'
*    EXCEPTIONS
*      cntl_error                  = 1
*      cntl_system_error           = 2
*      create_error                = 3
*      lifetime_error              = 4
*      lifetime_dynpro_dynpro_link = 5.
*  if sy-subrc <> 0.
*    message a001.
*  endif.

* create the view (control) of the tree model

  call method g_tree->create_tree_control
    EXPORTING
      parent                       = g_container_1   "g_custom_container
    EXCEPTIONS
      lifetime_error               = 1
      cntl_system_error            = 2
      create_error                 = 3
      failed                       = 4
      tree_control_already_created = 5.
  if sy-subrc <> 0.
    message a001.
  endif.

* define the events which will be passed to the backend
  " node double click
  event-eventid = cl_simple_tree_model=>eventid_node_double_click.
  event-appl_event = 'X'.              " process PAI if event occurs
  append event to events.



  call method g_tree->set_registered_events
    EXPORTING
      events                    = events
    EXCEPTIONS
      illegal_event_combination = 1
      unknown_event             = 2.
  if sy-subrc <> 0.
    message a001.
  endif.

* assign event handlers in the application class to each desired event
  set handler g_application->handle_node_double_click for g_tree.

* add nodes to the tree model
  perform add_nodes.

* expand the root node
  call method g_tree->expand_node
    EXPORTING
      node_key       = 'EFA'                                "#EC NOTEXT
    EXCEPTIONS
      node_not_found = 1.
  if sy-subrc <> 0.
    message a001.
  endif.

endform.                               " CREATE_AND_INIT_TREE



*** INCLUDE simple_tree_model_demoF01
*&---------------------------------------------------------------------*
*&      Form  ADD_NODES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form add_nodes.



***Get the internal table values from report.

  import it_export_output[] from memory id 'ZPWEFA_DIS'.

  import it_cons_export from memory id 'ZPWEFA_CONS'.

  call method g_tree->add_node
    EXPORTING
      node_key = 'EFA'                                      "#EC NOTEXT
      isfolder = 'X'
      text     = 'EFA'
      image    = '@NF@'                                     "#EC NOTEXT
    EXCEPTIONS
      others   = 1.
  if sy-subrc <> 0.
    message a001.
  endif.

  read table it_export_output into wa_export_output with key carrier = 'FEDEX'.
  if sy-subrc = 0.
    call method g_tree->add_node
      EXPORTING
        node_key          = 'FEDEX'                         "#EC NOTEXT
        relative_node_key = 'EFA'
        relationship      = cl_simple_tree_model=>relat_last_child
        isfolder          = 'X'
*        image             = '@3S@'
        text              = 'FedEx'                         "#EC NOTEXT
      EXCEPTIONS
        others            = 1.
    if sy-subrc <> 0.
      message a001.
    endif.


  endif.
  read table it_export_output into wa_export_output with key carrier = 'UPS'.
  if sy-subrc = 0.
    call method g_tree->add_node
      EXPORTING
        node_key          = 'UPS'                           "#EC NOTEXT
        relative_node_key = 'EFA'
        relationship      = cl_simple_tree_model=>relat_last_child
        isfolder          = 'X'
*        image             = '@3S@'
        text              = 'UPS'                           "#EC NOTEXT
      EXCEPTIONS
        others            = 1.
    if sy-subrc <> 0.
      message a001.
    endif.

  endif.

*  DATA: invc_tab TYPE TABLE OF dats WITH HEADER LINE.

  types : begin of ty_invc_tab,
             invoice_date type erzdt,
*             carrier TYPE zcarrier,
              carrier type char20,
          end of ty_invc_tab.

  data : it_invc_tab type table of  ty_invc_tab ,
         invc_tab type ty_invc_tab.
  data : it_invc_ups type table of  ty_invc_tab ,
       invc_ups type ty_invc_tab.




  data: nkey type tm_nodekey,
        n1key type tm_nodekey,
        n2key type tm_nodekey,
        n3key type tm_nodekey,
        n4key type tm_nodekey,
        n5key type tm_nodekey,
        n6key type tm_nodekey,
        nkey_text type string,
        n1key_text type string,
        n2key_text type string,
        n3key_text type string,
        n4key_text type string,
        n5key_text type string,
        n6key_text type string,


******Begin of DECLARATIONS by VAMSI(1) ON 19/08/2011 *******************************

        n12key       type tm_nodekey,
        n12key_text  type string.
**********End of Deckarations by VAMSI (1) **********************
.


  clear: invc_tab,nkey,n1key,n2key.
  refresh it_invc_tab.


***Get the internal table values from report.

*  import it_export_output[] from memory id 'ZPWEFA_DIS'.


  data : w_space(1) type c value  ' ',
        w_newspace(5) type c.

  loop at it_export_output into wa_export_output where gv_cb = 'X'.
* LOOP AT it_invc_tab INTO invc_tab.
    clear : n1key,date_conc.

    n1key = wa_export_output-invoice_number.

    if wa_export_output-carrier eq 'FEDEX' and wa_export_output-invoice_date is not initial.

      concatenate w_space '-' w_space into w_newspace separated by space.
      concatenate w_newspace w_space into w_newspace separated by space.
*      n1key_text = 'Inv Wk:01/28/09'.
       concatenate WA_EXPORT_OUTPUT-INVOICE_DATE+4(2) '/' WA_EXPORT_OUTPUT-INVOICE_DATE+6(2) '/'
                    WA_EXPORT_OUTPUT-INVOICE_DATE+0(4) into date_conc.
      concatenate 'Invoice # :' wa_export_output-invoice_number ' Date # :' date_conc into n1key_text.
*      concatenate 'Invoice # :' wa_export_output-invoice_number w_newspace  '  ' wa_export_output-invoice_date+4(2) '-' wa_export_output-invoice_date+6(2) '-'wa_export_output-invoice_date+0(4)    into n1key_text.

      call method g_tree->add_node
        EXPORTING
          node_key          = n1key                         "#EC NOTEXT
          relative_node_key = 'FEDEX'
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@NF@'        " changed by vamsi"""""""'@3S@'
          text              = n1key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.

*************************************************************************************

************************************************************************************

* Cost By Service.

      n2key_text = 'By Service Type'.
      concatenate n1key '#' 'SERVICE_TYPE' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@AE@'                        "'@36@'
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.


* Cost By ZONE.
      n2key_text = 'By Zone'.
      concatenate n1key '#' 'ZONE' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@36@'
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.


* Cost By PaymentMethod.
      n2key_text = 'By Payment Method'.
      concatenate n1key '#' 'PAYMENT_METHOD' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          image             = '@Q6@'                        "'@36@'
          isfolder          = ''
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.


* Cost By ShipDay.
      n2key_text = 'By Ship Day'.
      concatenate n1key '#' 'SHIP_DAY' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@1U@'                        "'@36@'
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.

*****All Details
*************All Details
      n5key_text = 'By All Details'.
      concatenate n1key '#' 'ALLDET' into n5key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n5key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@83@'                        "'@36@'
          text              = n5key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.


********Address Correction

      n3key_text  = 'By Address Corrections'.
      concatenate n1key '#' 'INCADD' into n3key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n3key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@7M@'                        "'@36@'
          text              = n3key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.
        n4key_text = 'By Guaranteed Service Refunds (GSR''s)'.
        concatenate n1key '#' 'GSR' into n4key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n4key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@AA@'                      "'@36@'
            text              = n4key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.




      n12key_text = 'By Comparing Invoice vs Manifest'.
        CONCATENATE n1key '#' 'MANIFEST' INTO n12key.
        CALL METHOD g_tree->add_node
          EXPORTING
            node_key          = n12key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@46@'       "'@AA@'              "'@36@'
            text              = n12key_text                  "#EC NOTEXT
          EXCEPTIONS
            OTHERS            = 1.
        IF sy-subrc <> 0.
        ENDIF.

****Guaranteed Service Refunds

*      n4key_text = 'By Customer Type Summary Report (CSR''s)'.
*      concatenate n1key '#' 'CSR' into n4key.
*      call method g_tree->add_node
*        EXPORTING
*          node_key          = n4key                         "#EC NOTEXT
*          relative_node_key = n1key
*          relationship      = cl_simple_tree_model=>relat_last_child
*          isfolder          = ''
*          image             = '@36@'
*          text              = n4key_text                    "#EC NOTEXT
*        EXCEPTIONS
*          others            = 1.
*      if sy-subrc <> 0.
*      endif.




* Comparision of Fedex and SAP Data
*      n2key_text = 'Compare FedEx/SAP Data'.
*      concatenate n1key '#' 'COMPARE' into n2key.
*      call method g_tree->add_node
*        EXPORTING
*          node_key          = n2key                         "#EC NOTEXT
*          relative_node_key = n1key
*          relationship      = cl_simple_tree_model=>relat_last_child
*          isfolder          = ''
*          image             = '@36@'
*          text              = n2key_text                    "#EC NOTEXT
*        EXCEPTIONS
*          others            = 1.
*      if sy-subrc <> 0.
*      endif.


*
*      n6key_text = 'By Guaranteed Service Refunds(GSR)'.
*      concatenate n1key '#' 'GSR' into n6key.
*      call method g_tree->add_node
*        EXPORTING
*          node_key          = n6key                         "#EC NOTEXT
*          relative_node_key = n1key
*          relationship      = cl_simple_tree_model=>relat_last_child
*          isfolder          = ''
*          image             = '@AA@'                        "'@36@'
*          text              = n6key_text                    "#EC NOTEXT
*        EXCEPTIONS
*          others            = 1.
*      if sy-subrc <> 0.
*      endif.


****Begin of changes for samsung
*      N6KEY_TEXT = 'Samsung Download'.
*      CONCATENATE N1KEY '#' 'SAMSUNG' INTO N6KEY.
*      CALL METHOD G_TREE->ADD_NODE
*        EXPORTING
*          NODE_KEY          = N6KEY                         "#EC NOTEXT
*          RELATIVE_NODE_KEY = N1KEY
*          RELATIONSHIP      = CL_SIMPLE_TREE_MODEL=>RELAT_LAST_CHILD
*          ISFOLDER          = ''
*          IMAGE             = '@36@'
*          TEXT              = N6KEY_TEXT                    "#EC NOTEXT
*        EXCEPTIONS
*          OTHERS            = 1.
*      IF SY-SUBRC <> 0.
*      ENDIF.
*****End of changes for Samsung

      call method g_tree->expand_node
        EXPORTING
          node_key       = 'FEDEX'                          "#EC NOTEXT
        EXCEPTIONS
          node_not_found = 1.
      if sy-subrc <> 0.
        message a001.
      endif.

    endif.
  endloop.

  data : v_lines type i.

  DESCRIBE TABLE it_cons_export LINES v_lines.

  IF v_lines is initial.

    loop at it_export_output into wa_export_output where gv_cb = 'X'.
* LOOP AT it_invc_tab INTO invc_tab.
      clear: n1key,date_conc.

      n1key = wa_export_output-invoice_number.

      if wa_export_output-carrier eq 'UPS' and wa_export_output-invoice_date is not initial.

        concatenate w_space '-' w_space into w_newspace separated by space.
        concatenate w_newspace w_space into w_newspace separated by space.
*      n1key_text = 'Inv Wk:01/28/09'.

*        call function 'CONVERSION_EXIT_ALPHA_INPUT'
*          exporting
*            input  = wa_export_output-invoice_date+4(2)
*          importing
*            output = wa_export_output-invoice_date+4(2).
*        call function 'CONVERSION_EXIT_ALPHA_INPUT'
*          exporting
*            input  = wa_export_output-invoice_date+6(2)
*          importing
*            output = wa_export_output-invoice_date+6(2).
        concatenate WA_EXPORT_OUTPUT-INVOICE_DATE+4(2) '/' WA_EXPORT_OUTPUT-INVOICE_DATE+6(2) '/'
                    WA_EXPORT_OUTPUT-INVOICE_DATE+0(4) into date_conc.
        concatenate 'Invoice # :' wa_export_output-invoice_number ' Date # :' date_conc into n1key_text.                        "  w_newspace  '  ' wa_export_output-invoice_date+4(2) '-' wa_export_output-invoice_date+6(2)



*      n1key_text = 'Inv Wk:01/28/09'.
*      concatenate 'Inv Wk:' n1key+4(2) '/' n1key+6(2) '/' n1key+0(4) into n1key_text.

        call method g_tree->add_node
          EXPORTING
            node_key          = n1key                       "#EC NOTEXT
            relative_node_key = 'UPS'
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@NF@'                      "'@3S@'
            text              = n1key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.

*************************************************************************************

************************************************************************************

* Cost By Service.

        n2key_text = 'By Service Type'.
        concatenate n1key '#' 'SERVICE_TYPE' into n2key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n2key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@AE@'                      "'@36@'
            text              = n2key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.


** Cost By PackageType.
*      n2key_text = 'By Package Type'.
*      concatenate n1key '#' 'PACKAGE_TYPE' into n2key.
*      call method g_tree->add_node
*        EXPORTING
*          node_key          = n2key                         "#EC NOTEXT
*          relative_node_key = n1key
*          relationship      = cl_simple_tree_model=>relat_last_child
*          isfolder          = ''
*          image             = '@36@'
*          text              = n2key_text                    "#EC NOTEXT
*        EXCEPTIONS
*          others            = 1.
*      if sy-subrc <> 0.
*      endif.


* Cost By ZONE.
        n2key_text = 'By Zone'.
        concatenate n1key '#' 'ZONE' into n2key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n2key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@58@'
            text              = n2key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.

* Cost By Package type.
        n2key_text = 'By Shipment Type'.
        concatenate n1key '#' 'PACKAGE_TYPE' into n2key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n2key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@BD@'
            text              = n2key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.

* Cost By PaymentMethod.
        n2key_text = 'By Payment Method'.
        concatenate n1key '#' 'PAYMENT_METHOD' into n2key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n2key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            image             = '@Q6@'                      "'@36@'
            isfolder          = ''
            text              = n2key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.

* Cost By ShipDay.
        n2key_text = 'By Ship Day'.
        concatenate n1key '#' 'SHIP_DAY' into n2key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n2key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@1U@'                      "'@36@'
            text              = n2key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.

        n5key_text = 'By All Details'.
        concatenate n1key '#' 'ALLDET' into n5key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n5key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@83@'                      "'@36@'
            text              = n5key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.

*         n2key_text = 'By All Details(Inbound)'.
*        concatenate n1key '#' 'INBOUND' into n2key.
*        call method g_tree->add_node
*          EXPORTING
*            node_key          = n2key                       "#EC NOTEXT
*            relative_node_key = n1key
*            relationship      = cl_simple_tree_model=>relat_last_child
*            isfolder          = ''
*            image             = '@83@'
*                                  "'@36@'
*            text              = n2key_text                  "#EC NOTEXT
*          EXCEPTIONS
*            others            = 1.
*        if sy-subrc <> 0.
*        endif.




* CONCATENATE 'Inc address Wk:' n1key+6(2) '/' n1key+4(2) '/' n1key+0(4) INTO n1key_text.
        n3key_text  = 'By Address Corrections'.
        concatenate n1key '#' 'INCADD' into n3key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n3key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@7M@'                      "'@36@'
            text              = n3key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.


        n4key_text = 'By Guaranteed Service Refunds (GSR''s)'.
        concatenate n1key '#' 'GSR' into n4key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n4key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@AA@'                      "'@36@'
            text              = n4key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.




*****************************Begin of Changes by VAMSI(2)for adding New Node "MANIFEST" to TREE for Carreir UPS*********
****COST by comparing invoice vs  MANIFEST bill CHARGES
          n12key_text = 'By Comparing Invoice vs Manifest'.

        CONCATENATE n1key '#' 'MANIFEST' INTO n12key.
        CALL METHOD g_tree->add_node
          EXPORTING
            node_key          = n12key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@46@'       "'@AA@'              "'@36@'
            text              = n12key_text                  "#EC NOTEXT
          EXCEPTIONS
            OTHERS            = 1.
        IF sy-subrc <> 0.
        ENDIF.

         n2key_text = 'Audit multiple invoices '.
        concatenate n1key '#' 'AUDIT' into n2key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n2key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@4R@'                      "'@36@'
            text              = n2key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.
  n2key_text = 'Summary Of Charges'.
        concatenate n1key '#' 'SUMMARY' into n2key.
        call method g_tree->add_node
          EXPORTING
            node_key          = n2key                       "#EC NOTEXT
            relative_node_key = n1key
            relationship      = cl_simple_tree_model=>relat_last_child
            isfolder          = ''
            image             = '@3Z@'                      "'@36@'
            text              = n2key_text                  "#EC NOTEXT
          EXCEPTIONS
            others            = 1.
        if sy-subrc <> 0.
        endif.





** *************   ******     End of Changes by VAMSI(2)  **********************









        call method g_tree->expand_node
          EXPORTING
            node_key       = 'UPS'                          "#EC NOTEXT
          EXCEPTIONS
            node_not_found = 1.
        if sy-subrc <> 0.
          message a001.
        endif.






      endif.

    endloop.

  else.

    LOOP AT it_cons_export INTO wa_cons_export WHERE gv_cb = 'X'.
      n1key = wa_cons_export-account_no.
      if wa_cons_export-date_low is not initial.
        concatenate " 'Account# :'
                    " wa_cons_export-account_no
                     'From'
                     wa_cons_export-date_low+4(2)
                     '/'
                     wa_cons_export-date_low+6(2)
                     '/'
                     wa_cons_export-date_low+0(4)
                     ' to '
                     wa_cons_export-date_high+4(2)
                     '/'
                     wa_cons_export-date_high+6(2)
                     '/'
                     wa_cons_export-date_high+0(4)
                into n1key_text SEPARATED BY space.
      elseif wa_cons_export-invoice_low is not initial.
        concatenate 'From'
                    wa_cons_export-invoice_low
                    'to'
                    wa_cons_export-invoice_high
               into n1key_text SEPARATED BY space.
      endif.


      call method g_tree->add_node
        EXPORTING
          node_key          = n1key                         "#EC NOTEXT
          relative_node_key = 'UPS'
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@NF@'                        "'@3S@'
          text              = n1key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.

*************************************************************************************

************************************************************************************

* Cost By Service.

      n2key_text = 'By Service Type'.
      concatenate n1key '#' 'SERVICE_TYPE' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@AE@'                        "'@36@'
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.


* Cost By ZONE.
      n2key_text = 'By Zone'.
      concatenate n1key '#' 'ZONE' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@58@'
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.

* Cost By Package type.
      n2key_text = 'By Shipment Type'.
      concatenate n1key '#' 'PACKAGE_TYPE' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@BD@'
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.

* Cost By PaymentMethod.
      n2key_text = 'By Payment Method'.
      concatenate n1key '#' 'PAYMENT_METHOD' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          image             = '@Q6@'                        "'@36@'
          isfolder          = ''
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.

* Cost By ShipDay.
      n2key_text = 'By Ship Day'.
      concatenate n1key '#' 'SHIP_DAY' into n2key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n2key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@1U@'                        "'@36@'
          text              = n2key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.

      n5key_text = 'By All Details'.
      concatenate n1key '#' 'ALLDET' into n5key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n5key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@83@'                        "'@36@'
          text              = n5key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.

      n3key_text  = 'By Address Corrections'.
      concatenate n1key '#' 'INCADD' into n3key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n3key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@7M@'                        "'@36@'
          text              = n3key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.


      n4key_text = 'By Guaranteed Service Refunds (GSR''s)'.
      concatenate n1key '#' 'GSR' into n4key.
      call method g_tree->add_node
        EXPORTING
          node_key          = n4key                         "#EC NOTEXT
          relative_node_key = n1key
          relationship      = cl_simple_tree_model=>relat_last_child
          isfolder          = ''
          image             = '@AA@'                        "'@36@'
          text              = n4key_text                    "#EC NOTEXT
        EXCEPTIONS
          others            = 1.
      if sy-subrc <> 0.
      endif.
      call method g_tree->expand_node
        EXPORTING
          node_key       = 'UPS'                            "#EC NOTEXT
        EXCEPTIONS
          node_not_found = 1.
      if sy-subrc <> 0.
        message a001.
      endif.


    ENDLOOP.

  ENDIF.





endform.                               " ADD_NODES

*
******************************************Service_Type*****************************

*&---------------------------------------------------------------------*
*&      Form  SERVICETYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form servicetype.


  clear : approve.
* declare variables used
  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
  if wa_export_output-carrier = 'FEDEX'.

    data: total_amnt type netwr,
          v_amt(10) type c,
          total_savings type netwr,
          total_pkgs type i,
          total_weight type brgew_ap,
          avg_pkg_cst type netwr,
          avg_pnd_cst type netwr.


    data :v_service type /PWEAVER/EFA_FED-service_type,
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt.

    clear : v_service,v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_serv_type,it_final_serv_type,it_fieldcat_serv_type,it_output_serv_type.
    clear : it_serv_type,wa_serv_type,wa_final_serv_type,wa_fieldcat_serv_type.
    select *
              from /PWEAVER/EFA_FED
              into corresponding fields of table it_serv_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.

    if sy-subrc = 0.
      clear : wa_serv_type.
      loop at it_serv_type into wa_serv_type.

        if wa_serv_type-service_type is initial.
          wa_serv_type-service_type = 'miscellanious'.
          endif.

        wa_final_serv_type-service_type  = wa_serv_type-service_type.
        wa_final_serv_type-packages = wa_serv_type-num_pieces.
        wa_final_serv_type-rated_weight = wa_serv_type-rated_wgt_amt.
        wa_final_serv_type-transport_charge = wa_serv_type-tran_charg_amnt.

****Handling charges
        if wa_serv_type-trk_id_chg_des = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des = 'Address Correction' or
      wa_serv_type-trk_id_chg_des = 'Residential' or
      wa_serv_type-trk_id_chg_des = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_serv_type-trk_id_chg_amt.
        endif.

        if wa_serv_type-trk_id_chg_des1 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des1 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des1 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des1 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des1 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des1 = 'Residential' or
      wa_serv_type-trk_id_chg_des1 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des1 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt1.
        endif.

        if wa_serv_type-trk_id_chg_des2 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des2 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des2 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des2 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des2 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des2 = 'Residential' or
      wa_serv_type-trk_id_chg_des2 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des2 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt2.
        endif.

        if wa_serv_type-trk_id_chg_des3 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des3 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des3 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des3 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des3 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des3 = 'Residential' or
      wa_serv_type-trk_id_chg_des3 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des3 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt3.
        endif.

        if wa_serv_type-trk_id_chg_des4 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des4 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des4 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des4 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des4 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des4 = 'Residential' or
      wa_serv_type-trk_id_chg_des4 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des4 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt4.
        endif.

        if wa_serv_type-trk_id_chg_des5 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des5 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des5 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des5 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des5 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des5 = 'Residential' or
      wa_serv_type-trk_id_chg_des5 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des5 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt5.
        endif.

        if wa_serv_type-trk_id_chg_des6 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des6 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des6 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des6 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des6 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des6 = 'Residential' or
      wa_serv_type-trk_id_chg_des6 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des6 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt6.
        endif.

        if wa_serv_type-trk_id_chg_des7 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des7 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des7 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des7 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des7 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des7 = 'Residential' or
      wa_serv_type-trk_id_chg_des7 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des7 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt7.
        endif.

        if wa_serv_type-trk_id_chg_des8 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des8 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des8 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des8 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des8 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des8 = 'Residential' or
      wa_serv_type-trk_id_chg_des8 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des8 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt8.
        endif.


        if wa_serv_type-trk_id_chg_des9 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des9 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des9 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des9 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des9 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des9 = 'Residential' or
      wa_serv_type-trk_id_chg_des9 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des9 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt9.
        endif.


        if wa_serv_type-trk_id_chg_des10 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des10 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des10 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des10 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des10 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des10 = 'Residential' or
      wa_serv_type-trk_id_chg_des10 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des10 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt10.
        endif.


        if wa_serv_type-trk_id_chg_des11 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des11 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des11 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des11 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des11 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des11 = 'Residential' or
      wa_serv_type-trk_id_chg_des11 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des11 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt11.
        endif.


        if wa_serv_type-trk_id_chg_des12 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des12 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des12 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des12 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des12 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des12 = 'Residential' or
      wa_serv_type-trk_id_chg_des12 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des12 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt12.
        endif.


        if wa_serv_type-trk_id_chg_des13 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des13 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des13 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des13 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des13 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des13 = 'Residential' or
      wa_serv_type-trk_id_chg_des13 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des13 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt13.
        endif.


        if wa_serv_type-trk_id_chg_des14 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des14 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des14 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des14 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des14 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des14 = 'Residential' or
      wa_serv_type-trk_id_chg_des14 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des14 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt14.
        endif.


        if wa_serv_type-trk_id_chg_des15 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des15 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des15 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des15 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des15 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des15 = 'Residential' or
      wa_serv_type-trk_id_chg_des15 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des15 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt15.
        endif.


        if wa_serv_type-trk_id_chg_des16 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des16 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des16 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des16 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des16 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des16 = 'Residential' or
      wa_serv_type-trk_id_chg_des16 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des16 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt16.
        endif.


        if wa_serv_type-trk_id_chg_des17 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des17 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des17 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des17 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des17 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des17 = 'Residential' or
      wa_serv_type-trk_id_chg_des17 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des17 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt17.
        endif.


        if wa_serv_type-trk_id_chg_des18 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des18 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des18 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des18 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des18 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des18 = 'Residential' or
      wa_serv_type-trk_id_chg_des18 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des18 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt18.
        endif.


        if wa_serv_type-trk_id_chg_des19 = 'Fuel Surcharge' or
wa_serv_type-trk_id_chg_des19 = 'Weekly Service Chg' or
wa_serv_type-trk_id_chg_des19 = 'Weekday Delivery' or
wa_serv_type-trk_id_chg_des19 = 'Residential Delivery' or
wa_serv_type-trk_id_chg_des19 = 'Address Correction' or
wa_serv_type-trk_id_chg_des19 = 'Residential' or
wa_serv_type-trk_id_chg_des19 = 'DAS Comm' or
wa_serv_type-trk_id_chg_des19 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt19.
        endif.

        if wa_serv_type-trk_id_chg_des20 = 'Fuel Surcharge' or
wa_serv_type-trk_id_chg_des20 = 'Weekly Service Chg' or
wa_serv_type-trk_id_chg_des20 = 'Weekday Delivery' or
wa_serv_type-trk_id_chg_des20 = 'Residential Delivery' or
wa_serv_type-trk_id_chg_des20 = 'Address Correction' or
wa_serv_type-trk_id_chg_des20 = 'Residential' or
wa_serv_type-trk_id_chg_des20 = 'DAS Comm' or
wa_serv_type-trk_id_chg_des20 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt20.
        endif.

        if wa_serv_type-trk_id_chg_des21 = 'Fuel Surcharge' or
wa_serv_type-trk_id_chg_des21 = 'Weekly Service Chg' or
wa_serv_type-trk_id_chg_des21 = 'Weekday Delivery' or
wa_serv_type-trk_id_chg_des21 = 'Residential Delivery' or
wa_serv_type-trk_id_chg_des21 = 'Address Correction' or
wa_serv_type-trk_id_chg_des21 = 'Residential' or
wa_serv_type-trk_id_chg_des21 = 'DAS Comm' or
wa_serv_type-trk_id_chg_des21 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt21.
        endif.

        if wa_serv_type-trk_id_chg_des22 = 'Fuel Surcharge' or
      wa_serv_type-trk_id_chg_des22 = 'Weekly Service Chg' or
      wa_serv_type-trk_id_chg_des22 = 'Weekday Delivery' or
      wa_serv_type-trk_id_chg_des22 = 'Residential Delivery' or
      wa_serv_type-trk_id_chg_des22 = 'Address Correction' or
      wa_serv_type-trk_id_chg_des22 = 'Residential' or
      wa_serv_type-trk_id_chg_des22 = 'DAS Comm' or
      wa_serv_type-trk_id_chg_des22 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt22.
        endif.

        if wa_serv_type-trk_id_chg_des23 = 'Fuel Surcharge' or
wa_serv_type-trk_id_chg_des23 = 'Weekly Service Chg' or
wa_serv_type-trk_id_chg_des23 = 'Weekday Delivery' or
wa_serv_type-trk_id_chg_des23 = 'Residential Delivery' or
wa_serv_type-trk_id_chg_des23 = 'Address Correction' or
wa_serv_type-trk_id_chg_des23 = 'Residential' or
wa_serv_type-trk_id_chg_des23 = 'DAS Comm' or
wa_serv_type-trk_id_chg_des23 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt23.
        endif.

        if wa_serv_type-trk_id_chg_des24 = 'Fuel Surcharge' or
wa_serv_type-trk_id_chg_des24 = 'Weekly Service Chg' or
wa_serv_type-trk_id_chg_des24 = 'Weekday Delivery' or
wa_serv_type-trk_id_chg_des24 = 'Residential Delivery' or
wa_serv_type-trk_id_chg_des24 = 'Address Correction' or
wa_serv_type-trk_id_chg_des24 = 'Residential' or
wa_serv_type-trk_id_chg_des24 = 'DAS Comm' or
wa_serv_type-trk_id_chg_des24 = 'DAS Extended Comm'.

          wa_final_serv_type-handling_charge =  wa_final_serv_type-handling_charge + wa_serv_type-trk_id_chg_amt24.
        endif.





        if wa_serv_type-trk_id_chg_des = 'Discount' or wa_serv_type-trk_id_chg_des = 'Performance Pricing'.
          wa_final_serv_type-discount =  wa_serv_type-trk_id_chg_amt.
        endif.

        if wa_serv_type-trk_id_chg_des1 = 'Discount'or wa_serv_type-trk_id_chg_des1 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt1.
        endif.

        if wa_serv_type-trk_id_chg_des2 = 'Discount' or wa_serv_type-trk_id_chg_des2 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt2.
        endif.

        if wa_serv_type-trk_id_chg_des3 = 'Discount' or wa_serv_type-trk_id_chg_des3 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt3.
        endif.

        if wa_serv_type-trk_id_chg_des4 = 'Discount' or wa_serv_type-trk_id_chg_des4 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt4.
        endif.

        if wa_serv_type-trk_id_chg_des5 = 'Discount' or wa_serv_type-trk_id_chg_des5 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt5.
        endif.

        if wa_serv_type-trk_id_chg_des6 = 'Discount'or wa_serv_type-trk_id_chg_des6 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt6.
        endif.

        if wa_serv_type-trk_id_chg_des7 = 'Discount' or wa_serv_type-trk_id_chg_des7 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt7.
        endif.

        if wa_serv_type-trk_id_chg_des8 = 'Discount' or wa_serv_type-trk_id_chg_des8 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt8.
        endif.

        if wa_serv_type-trk_id_chg_des9 = 'Discount' or wa_serv_type-trk_id_chg_des9 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt9.
        endif.


        if wa_serv_type-trk_id_chg_des10 = 'Discount'or wa_serv_type-trk_id_chg_des10 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt10.
        endif.

        if wa_serv_type-trk_id_chg_des11 = 'Discount' or wa_serv_type-trk_id_chg_des11 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt11.
        endif.

        if wa_serv_type-trk_id_chg_des12 = 'Discount' or wa_serv_type-trk_id_chg_des12 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt12.
        endif.

        if wa_serv_type-trk_id_chg_des13 = 'Discount'or wa_serv_type-trk_id_chg_des13 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt13.
        endif.


        if wa_serv_type-trk_id_chg_des14 = 'Discount' or wa_serv_type-trk_id_chg_des14 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt14.
        endif.

        if wa_serv_type-trk_id_chg_des15 = 'Discount' or wa_serv_type-trk_id_chg_des15 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt15.
        endif.


        if wa_serv_type-trk_id_chg_des16 = 'Discount' or wa_serv_type-trk_id_chg_des16 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt16.
        endif.

        if wa_serv_type-trk_id_chg_des17 = 'Discount' or wa_serv_type-trk_id_chg_des17 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt17.
        endif.

        if wa_serv_type-trk_id_chg_des18 = 'Discount' or wa_serv_type-trk_id_chg_des18 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt18.
        endif.

        if wa_serv_type-trk_id_chg_des19 = 'Discount'or wa_serv_type-trk_id_chg_des19 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt19.
        endif.

        if wa_serv_type-trk_id_chg_des20 = 'Discount'or wa_serv_type-trk_id_chg_des20 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt20.
        endif.

        if wa_serv_type-trk_id_chg_des21 = 'Discount'or wa_serv_type-trk_id_chg_des21 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt21.
        endif.

        if wa_serv_type-trk_id_chg_des22 = 'Discount'or wa_serv_type-trk_id_chg_des22 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt22.
        endif.

        if wa_serv_type-trk_id_chg_des23 = 'Discount'or wa_serv_type-trk_id_chg_des23 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt23.
        endif.

        if wa_serv_type-trk_id_chg_des24 = 'Discount'or wa_serv_type-trk_id_chg_des24 = 'Performance Pricing'.
          wa_final_serv_type-discount =   wa_final_serv_type-discount + wa_serv_type-trk_id_chg_amt24.
        endif.


        wa_final_serv_type-net_charge = wa_serv_type-net_chrg_amnt.

        append wa_final_serv_type to it_final_serv_type.
        clear :wa_final_serv_type,wa_serv_type.
      endloop.


      sort it_final_serv_type by service_type.
      clear wa_final_serv_type.



      loop at it_final_serv_type into wa_final_serv_type.



*        IF NOT WA_FINAL_SERV_TYPE IS INITIAL.
        at end of service_type.
          sum.     " calculates sub totals on service types

          wa_output_serv_type-service_type = wa_final_serv_type-service_type.
          wa_output_serv_type-packages =  wa_final_serv_type-packages.
          wa_output_serv_type-rated_weight = wa_final_serv_type-rated_weight.
          wa_output_serv_type-transport_charge = wa_final_serv_type-transport_charge.
          wa_output_serv_type-handling_charge =  wa_final_serv_type-handling_charge.
          wa_output_serv_type-discount =  wa_final_serv_type-discount.
          wa_output_serv_type-net_charge = wa_final_serv_type-net_charge.


* calculate the grand totals

*v_service = v_service + wa_output_serv_type-service_type.
          v_packages = v_packages + wa_final_serv_type-packages.
          v_rated_weight = v_rated_weight + wa_final_serv_type-rated_weight.

          v_trans_charg = v_trans_charg + wa_final_serv_type-transport_charge.


          v_hand_charg = v_hand_charg + wa_final_serv_type-handling_charge.


          v_discount = v_discount + wa_final_serv_type-discount.

          v_netcharge = v_netcharge +  wa_final_serv_type-net_charge .
          append wa_output_serv_type to it_output_serv_type.
          clear : wa_output_serv_type,wa_final_serv_type.

        endat.

*        ENDIF.
        clear :wa_output_serv_type,wa_final_serv_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_serv_type.

*    wa_output_serv_type-service_type = v_service.
      wa_output_serv_type-packages = v_packages.
      wa_output_serv_type-rated_weight = v_rated_weight.

      wa_output_serv_type-transport_charge = v_trans_charg.
      wa_output_serv_type-handling_charge = v_hand_charg .
      wa_output_serv_type-discount = v_discount .
      wa_output_serv_type-net_charge = v_netcharge.

      wa_output_serv_type-line_color      = 'C500'.
      append wa_output_serv_type to it_output_serv_type.
      clear wa_output_serv_type.



      wa_fieldcat_serv_type-col_pos      = 1.
      wa_fieldcat_serv_type-fieldname    = 'SERVICE_TYPE'.
      wa_fieldcat_serv_type-coltext    = 'Service Type'.
      wa_fieldcat_serv_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      WA_FIELDCAT_SERV_TYPE-OUTPUTLEN = '40'.
      wa_fieldcat_serv_type-lowercase = 'X'.
      append wa_fieldcat_serv_type to it_fieldcat_serv_type.
      clear wa_fieldcat_serv_type.

      wa_fieldcat_serv_type-col_pos      = 2.
      wa_fieldcat_serv_type-fieldname    = 'PACKAGES'.
      wa_fieldcat_serv_type-coltext    = 'Total Packages'.
      wa_fieldcat_serv_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_serv_type to it_fieldcat_serv_type.
      clear wa_fieldcat_serv_type.

      wa_fieldcat_serv_type-col_pos      = 3.
      wa_fieldcat_serv_type-fieldname    = 'RATED_WEIGHT'.
      wa_fieldcat_serv_type-coltext    = 'Rated Weight'.
      wa_fieldcat_serv_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_serv_type to it_fieldcat_serv_type.
      clear wa_fieldcat_serv_type.

      wa_fieldcat_serv_type-col_pos      = 4.
      wa_fieldcat_serv_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_serv_type-coltext    = 'Transport Charge'.
      wa_fieldcat_serv_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_serv_type to it_fieldcat_serv_type.
      clear wa_fieldcat_serv_type.

      wa_fieldcat_serv_type-col_pos      = 5.
      wa_fieldcat_serv_type-fieldname    = 'HANDLING_CHARGE'.
      wa_fieldcat_serv_type-coltext    = 'Handling Charge'.
      wa_fieldcat_serv_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_serv_type to it_fieldcat_serv_type.
      clear wa_fieldcat_serv_type.

      wa_fieldcat_serv_type-col_pos      = 6.
      wa_fieldcat_serv_type-fieldname    = 'DISCOUNT'.
      wa_fieldcat_serv_type-coltext    = 'Discount'.
      wa_fieldcat_serv_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_serv_type to it_fieldcat_serv_type.
      clear wa_fieldcat_serv_type.

      wa_fieldcat_serv_type-col_pos      = 7.
      wa_fieldcat_serv_type-fieldname    = 'NET_CHARGE'.
      wa_fieldcat_serv_type-coltext    = 'Net Charge'.
      wa_fieldcat_serv_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_serv_type to it_fieldcat_serv_type.
      clear wa_fieldcat_serv_type.



      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUTPUT_SERV_TYPE'
*          is_layout                     = gs_layout
           is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'

        changing
          it_outtab                     = it_output_serv_type
          it_fieldcatalog               = it_fieldcat_serv_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.



    endif.
  endif.


*  if wa_export_output-carrier = 'UPS'.

*    DATA: TOTAL_AMNT_UPS TYPE NETWR,
**        v_amt(10) TYPE c,
*         TOTAL_SAVINGS_UPS TYPE NETWR,
*         TOTAL_PKGS_UPS TYPE I,
*         TOTAL_WEIGHT_UPS TYPE BRGEW_AP,
*         AVG_PKG_CST_UPS TYPE NETWR,
*         AVG_PND_CST_UPS TYPE NETWR.
*
*
*    REFRESH: IT_FINAL_SERVICE_TYPE_UPS, IT_FIELDCAT_SERVICE_TYPE_UPS,IT_OUTPUT_SERVICE_TYPE_UPS.
*    CLEAR: WA_FIELDCAT_SERVICE_TYPE_UPS, WA_OUTPUT_SERVICE_TYPE_UPS, WA_FINAL_SERVICE_TYPE_UPS.
*
*    DATA : IT_OUTPUT_UPS_SERV_TYPE TYPE STANDARD TABLE OF TY_ZPWEFA_UPS_SERV_TYPE,
*       WA_OUTPUT_UPS_SERV_TYPE TYPE TY_ZPWEFA_UPS_SERV_TYPE,
*       IT_FINAL_UPS_SERV_TYPE TYPE STANDARD TABLE OF TY_ZPWEFA_UPS_SERV_TYPE,
*       WA_FINAL_UPS_SERV_TYPE TYPE TY_ZPWEFA_UPS_SERV_TYPE,
*       IT_SERV_UPS_TYPE TYPE STANDARD TABLE OF ZPWEFA_UPS,
*       WA_SERV_UPS_TYPE TYPE ZPWEFA_UPS.
*
*
*DATA: IT_FIELDCAT_SERV_UPS_TYPE TYPE TABLE OF LVC_S_FCAT,
*      WA_FIELDCAT_SERV_UPS_TYPE TYPE LVC_S_FCAT.


*    data : v_tabix type sy-tabix,
*          v_next type sy-tabix.
*
*    refresh : it_serv_ups_type,it_output_ups_serv_type,it_fieldcat_serv_ups_type,it_final_ups_serv_type.
*
*
*    select *
*         from zpwefa_ups
*         into corresponding fields of table it_serv_ups_type
*         where invoice_number = v_invoice_number and
*           invoice_date eq wa_export_output-invoice_date.
*
*    if sy-subrc = 0.
*
*      sort it_serv_ups_type by zcounter lead_ship_number chrg_class_code chrg_catg_code.
*
*      data : v_id(4) type c,
*            v_temp_hdl_chrg type zpwefa_ups-basis_value,
*            v_temp_hdl_chrg_2(10) type p,
*            v_netamount type zpwefa_ups-incen_amt,
*             v_billweight type zpwefa_ups-billed_weight,
*            v_handling_charges type zpwefa_ups-basis_value,
*            v_discount_1 type zpwefa_ups-incen_amt.
*
****select unique tracking numbers.
*      refresh : it_ups_track.
*      select distinct trck_num  from zpwefa_ups into table it_ups_track where invoice_number = v_invoice_number
*      and  invoice_date eq wa_export_output-invoice_date.
*
****select unique service types
*      refresh : it_ups_service.
*      select distinct chrg_desc from zpwefa_ups into table it_ups_service where invoice_number = v_invoice_number and
*                                                                             ( chrg_class_code = 'FRT' and  chrg_catg_code = 'SHP') or
*                                                                             ( chrg_class_code = 'FRT' and  chrg_catg_code = 'RTN') or
*                                                                             (  chrg_class_code = 'ACC' and chrg_desc_code  = 'ART' and chrg_catg_code = 'RTN' )
*                                                                               and  invoice_date eq wa_export_output-invoice_date.
*
*      sort it_ups_track by tracking_number.
*      delete it_ups_track where tracking_number = space.
*      loop at it_ups_track into wa_ups_track.
*
*
*        clear wa_ups_service.
*        loop at it_ups_service into wa_ups_service.
*
*
*
*          refresh : it_serv_temp_ups.
*          select * from zpwefa_ups into table it_serv_temp_ups where trck_num = wa_ups_track-tracking_number and
*                                                                   chrg_desc = wa_ups_service-chrg_desc.
*
*
**if wa_ups_track-tracking_number = '1Z1087410246880258'.
**BREAK-POINT.
**endif.
*          if sy-subrc = 0.
*
*
*            clear : v_netamount,v_temp_hdl_chrg,v_temp_hdl_chrg_2,wa_serv_temp_ups,wa_temp_ups_serv_type.
*
*            clear : wa_temp_ups_serv_type.
*
*
*
*            loop at it_serv_temp_ups into wa_serv_temp_ups.
*
*              clear : wa_serv_ups_type.
*              read table it_serv_ups_type into wa_serv_ups_type with key trck_num = wa_ups_track-tracking_number
*                                                                         chrg_desc = wa_ups_service-chrg_desc
*                                                                         chrg_class_code = 'FRT'.
**                                                                       chrg_catg_code = 'SHP'.
*              if sy-subrc = 0 .
*****get the service type
*                if wa_temp_ups_serv_type-service_type is initial.
*                  wa_temp_ups_serv_type-service_type   = wa_serv_ups_type-chrg_desc.
*                endif.
*
*                clear : wa_serv_ups_type.
*                read table it_serv_ups_type into wa_serv_ups_type with key trck_num = wa_ups_track-tracking_number
**                                                                       chrg_Desc = wa_ups_service-chrg_Desc
*                                                                           chrg_class_code = 'FRT'.
*****number of packages
*                if wa_temp_ups_serv_type-packages is initial.
*                  wa_temp_ups_serv_type-packages   = wa_serv_ups_type-pckg_qty.
*                endif.
****Transportation Charge
*                if wa_temp_ups_serv_type-transport_charge is initial.
*                  wa_temp_ups_serv_type-transport_charge = wa_serv_ups_type-basis_value.
*                endif.
*
*              endif.
*
****billed weight
*              clear : v_billweight.
*              select sum( billed_weight ) from zpwefa_ups into v_billweight where trck_num = wa_ups_track-tracking_number.
**              wa_temp_ups_serv_type-rated_weight  = wa_temp_ups_serv_type-rated_weight + wa_serv_ups_type-billed_weight.
*
*
*              wa_temp_ups_serv_type-rated_weight = v_billweight.
*
***handling charges
*              clear : v_handling_charges.
*              select sum( basis_value ) from zpwefa_ups into v_handling_charges where trck_num = wa_ups_track-tracking_number.
*
****Discount
*              clear : v_discount.
*              select sum( incen_amt ) from zpwefa_ups into v_discount_1 where trck_num = wa_ups_track-tracking_number.
*
*
*
*****ACC OR ART
*              clear :wa_serv_temp_ups.
*              read table it_serv_ups_type into wa_serv_ups_type with key trck_num = wa_ups_track-tracking_number
*                                                                          chrg_desc = wa_ups_service-chrg_desc
*                                                                         chrg_class_code = 'ACC'
*                                                                         chrg_desc_code  = 'ART'
*                                                                         chrg_catg_code = 'RTN'.
*              if sy-subrc = 0 .
*
*****get the service type
*                if wa_temp_ups_serv_type-service_type is initial.
*                  wa_temp_ups_serv_type-service_type   = wa_serv_ups_type-chrg_desc.
*                endif.
*****number of packages
*                if wa_temp_ups_serv_type-packages is initial.
*                  wa_temp_ups_serv_type-packages   = wa_serv_ups_type-chrg_unit_qty.
*                endif.
****Transportation Charge
*                if wa_temp_ups_serv_type-transport_charge is initial.
*                  wa_temp_ups_serv_type-transport_charge = wa_serv_ups_type-basis_value.
*                endif.
*
*
*
*              endif.
******get the  handling charges
*
*              v_temp_hdl_chrg  = v_handling_charges - wa_temp_ups_serv_type-transport_charge.
*
***Get the discount amount
*
*              wa_temp_ups_serv_type-discount = v_discount_1.
*
***Get the net amount
*
*              v_netamount = wa_temp_ups_serv_type-transport_charge + v_temp_hdl_chrg - wa_temp_ups_serv_type-discount.
*
**modify it_temp_ups_serv_type from wa_temp_ups_serv_type.
*
*            endloop.
*            wa_final_ups_serv_type-service_type = wa_temp_ups_serv_type-service_type.
*            wa_final_ups_serv_type-packages = wa_temp_ups_serv_type-packages.
*            wa_final_ups_serv_type-rated_weight =  wa_temp_ups_serv_type-rated_weight.
*            wa_final_ups_serv_type-transport_charge = wa_temp_ups_serv_type-transport_charge.
*            wa_final_ups_serv_type-handling_charge = v_temp_hdl_chrg .
*            wa_final_ups_serv_type-discount = wa_temp_ups_serv_type-discount.
*            wa_final_ups_serv_type-net_charge = v_netamount.
*
*            append wa_final_ups_serv_type to it_final_ups_serv_type.
*            clear wa_final_ups_serv_type.
*
*          endif.
*
*        endloop.
*      endloop.



*      sort it_final_ups_serv_type by service_type.
*      clear wa_final_ups_serv_type.
*
*      loop at it_final_ups_serv_type into wa_final_ups_serv_type.
*
*        at end of service_type.
*          sum.     " calculates sub totals on service types
*          wa_output_ups_serv_type-service_type = wa_final_ups_serv_type-service_type.
*          wa_output_ups_serv_type-packages = wa_final_ups_serv_type-packages.
*          wa_output_ups_serv_type-rated_weight = wa_final_ups_serv_type-rated_weight.
*          wa_output_ups_serv_type-transport_charge = wa_final_ups_serv_type-transport_charge.
*          wa_output_ups_serv_type-handling_charge = wa_final_ups_serv_type-handling_charge.
*          wa_output_ups_serv_type-discount = wa_final_ups_serv_type-discount.
*          wa_output_ups_serv_type-net_charge = wa_final_ups_serv_type-net_charge.
**
*          v_packages = v_packages + wa_final_ups_serv_type-packages.
*          v_rated_weight = v_rated_weight + wa_final_ups_serv_type-rated_weight.
*
*          v_trans_charg = v_trans_charg + wa_final_ups_serv_type-transport_charge.
*
*
*          v_hand_charg = v_hand_charg + wa_final_ups_serv_type-handling_charge.
*
*
*          v_discount = v_discount + wa_final_ups_serv_type-discount.
*
*          v_netcharge = v_netcharge +  wa_final_ups_serv_type-net_charge .
*
*          append wa_output_ups_serv_type to it_output_ups_serv_type.
*          clear :wa_output_ups_serv_type.
*
*        endat.
*
*
*        clear wa_final_ups_serv_type.
*
*
*      endloop.
*
*      clear : wa_output_ups_serv_type.
*
**    wa_output_serv_type-service_type = v_service.
*      wa_output_ups_serv_type-packages = v_packages.
*      wa_output_ups_serv_type-rated_weight = v_rated_weight.
*
*      wa_output_ups_serv_type-transport_charge = v_trans_charg.
*      wa_output_ups_serv_type-handling_charge = v_hand_charg .
*      wa_output_ups_serv_type-discount = v_discount .
*      wa_output_ups_serv_type-net_charge = v_netcharge.
*
*      wa_output_ups_serv_type-line_color      = 'C500'.
*      append wa_output_ups_serv_type to it_output_ups_serv_type.
*      clear wa_output_ups_serv_type.
*
*    sort it_final_ups_service by service_type.
*    clear wa_final_ups_service.
*    refresh : it_output_ups_serv_type, it_fieldcat_serv_ups_type.
*
*    loop at it_final_ups_service into wa_final_ups_service.
*
*      at end of service_type.
*        sum.     " calculates sub totals on service types
*        wa_output_ups_serv_type-service_type = wa_final_ups_service-service_type.
*        wa_output_ups_serv_type-packages = wa_final_ups_service-packages.
*        wa_output_ups_serv_type-rated_weight = wa_final_ups_service-rated_weight.
*        wa_output_ups_serv_type-transport_charge = wa_final_ups_service-transport_charge.
*        wa_output_ups_serv_type-handling_charge = wa_final_ups_service-handling_charge.
*        wa_output_ups_serv_type-discount = wa_final_ups_service-discount.
*        wa_output_ups_serv_type-net_charge = wa_final_ups_service-net_charge.
**
*        v_packages = v_packages + wa_final_ups_service-packages.
*        v_rated_weight = v_rated_weight + wa_final_ups_service-rated_weight.
*
*        v_trans_charg = v_trans_charg + wa_final_ups_service-transport_charge.
*
*
*        v_hand_charg = v_hand_charg + wa_final_ups_service-handling_charge.
*
*
*        v_discount = v_discount + wa_final_ups_service-discount.
*
*        v_netcharge = v_netcharge +  wa_final_ups_service-net_charge .
*
*        append wa_output_ups_serv_type to it_output_ups_serv_type.
*        clear :wa_output_ups_serv_type.
*
*      endat.
*
*
*      clear wa_final_ups_service.
*
*
*    endloop.
*
*    clear : wa_output_ups_serv_type.
*
**    wa_output_serv_type-service_type = v_service.
*    wa_output_ups_serv_type-packages = v_packages.
*    wa_output_ups_serv_type-rated_weight = v_rated_weight.
*
*    wa_output_ups_serv_type-transport_charge = v_trans_charg.
*    wa_output_ups_serv_type-handling_charge = v_hand_charg .
*    wa_output_ups_serv_type-discount = v_discount .
*    wa_output_ups_serv_type-net_charge = v_netcharge.
*
*    wa_output_ups_serv_type-line_color      = 'C500'.
*    append wa_output_ups_serv_type to it_output_ups_serv_type.
*    clear wa_output_ups_serv_type.
*
*
*
*    wa_fieldcat_serv_ups_type-col_pos      = 1.
*    wa_fieldcat_serv_ups_type-fieldname    = 'SERVICE_TYPE'.
*    wa_fieldcat_serv_ups_type-coltext    = 'Service Type'.
*    wa_fieldcat_serv_ups_type-tabname      = 'IT_OUPUT_UPS_SERV_TYPE'.
*    append wa_fieldcat_serv_ups_type to it_fieldcat_serv_ups_type.
*    clear wa_fieldcat_serv_ups_type.
*
*    wa_fieldcat_serv_ups_type-col_pos      = 2.
*    wa_fieldcat_serv_ups_type-fieldname    = 'PACKAGES'.
*    wa_fieldcat_serv_ups_type-coltext    = 'Total Packages'.
*    wa_fieldcat_serv_ups_type-tabname      = 'IT_OUPUT_UPS_SERV_TYPE'.
*    append wa_fieldcat_serv_ups_type to it_fieldcat_serv_ups_type.
*    clear wa_fieldcat_serv_ups_type.
*
*    wa_fieldcat_serv_ups_type-col_pos      = 3.
*    wa_fieldcat_serv_ups_type-fieldname    = 'RATED_WEIGHT'.
*    wa_fieldcat_serv_ups_type-coltext    = 'Rated Weight'.
*    wa_fieldcat_serv_ups_type-tabname      = 'IT_OUPUT_UPS_SERV_TYPE'.
*    append wa_fieldcat_serv_ups_type to it_fieldcat_serv_ups_type.
*    clear wa_fieldcat_serv_ups_type.
*
*    wa_fieldcat_serv_ups_type-col_pos      = 4.
*    wa_fieldcat_serv_ups_type-fieldname    = 'TRANSPORT_CHARGE'.
*    wa_fieldcat_serv_ups_type-coltext    = 'Transport Charge'.
*    wa_fieldcat_serv_ups_type-tabname      = 'IT_OUPUT_UPS_SERV_TYPE'.
*    append wa_fieldcat_serv_ups_type to it_fieldcat_serv_ups_type.
*    clear wa_fieldcat_serv_ups_type.
*
*    wa_fieldcat_serv_ups_type-col_pos      = 5.
*    wa_fieldcat_serv_ups_type-fieldname    = 'HANDLING_CHARGE'.
*    wa_fieldcat_serv_ups_type-coltext    = 'Handling Charge'.
*    wa_fieldcat_serv_ups_type-tabname      = 'IT_OUPUT_UPS_SERV_TYPE'.
*    append wa_fieldcat_serv_ups_type to it_fieldcat_serv_ups_type.
*    clear wa_fieldcat_serv_ups_type.
*
*    wa_fieldcat_serv_ups_type-col_pos      = 6.
*    wa_fieldcat_serv_ups_type-fieldname    = 'DISCOUNT'.
*    wa_fieldcat_serv_ups_type-coltext    = 'Discount'.
*    wa_fieldcat_serv_ups_type-tabname      = 'IT_OUPUT_UPS_SERV_TYPE'.
*    append wa_fieldcat_serv_ups_type to it_fieldcat_serv_ups_type.
*    clear wa_fieldcat_serv_ups_type.
*
*    wa_fieldcat_serv_ups_type-col_pos      = 7.
*    wa_fieldcat_serv_ups_type-fieldname    = 'NET_CHARGE'.
*    wa_fieldcat_serv_ups_type-coltext    = 'Net Charge'.
*    wa_fieldcat_serv_ups_type-tabname      = 'IT_OUPUT_UPS_SERV_TYPE'.
*    append wa_fieldcat_serv_ups_type to it_fieldcat_serv_ups_type.
*    clear wa_fieldcat_serv_ups_type.
*
*
*
*    gs_layout-cwidth_opt = 'X'.
*    gs_layout-info_fname = 'LINE_COLOR'.
*    gs_layout-grid_title = space.
*
*
*    call method go_grid->set_table_for_first_display
*      exporting
*        i_structure_name              = 'IT_OUTPUT_UPS_SERV_TYPE'
*        is_layout                     = gs_layout
*      changing
*        it_outtab                     = it_output_ups_serv_type
*        it_fieldcatalog               = it_fieldcat_serv_ups_type
**    IT_SORT                       =
*    exceptions
*      invalid_parameter_combination = 1
*      program_error                 = 2
*      too_many_lines                = 3
*      others                        = 4  .
*    if sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    endif.
*
*




*    endif.
*  endif.


************************************************************      loop at it_serv_ups_type into wa_serv_ups_type.
************************************************************
************************************************************        clear : wa_serv_ups_type.
************************************************************        read table it_serv_ups_type into wa_serv_ups_type index sy-tabix.
************************************************************
************************************************************        if wa_serv_ups_type-chrg_desc = 'Returns 3 UPS Pickup Attempts'.
************************************************************          break-point.
************************************************************        endif.
************************************************************
*************************************************************if WA_SERV_UPS_TYPE-CHRG_DESC = 'Returns Ground'.
*************************************************************  break-point.
*************************************************************endif.
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************
************************************************************        clear : wa_final_ups_serv_type.
************************************************************        v_next = v_tabix - 1.
************************************************************        read table it_final_ups_serv_type into wa_final_ups_serv_type with key tracking_number  = wa_serv_ups_type-lead_ship_number.
*************************************************************                                                                               binary search.
*************************************************************                                                                               chrg_class_code = WA_SERV_UPS_TYPE-chrg_class_code
*************************************************************                                                                               chrg_desc_code =  WA_SERV_UPS_TYPE-chrg_desc_code.
************************************************************
************************************************************
************************************************************
************************************************************        if sy-subrc = 0 and  wa_serv_ups_type-chrg_class_code = 'FSC'.
************************************************************
************************************************************          v_id = sy-tabix.
************************************************************
************************************************************
*************************************************************          IF WA_SERV_UPS_TYPE-CHRG_CLASS_CODE = 'FSC'.
************************************************************          wa_final_ups_serv_type-tracking_number = wa_serv_ups_type-trck_num.
************************************************************          wa_final_ups_serv_type-transport_charge = wa_final_ups_serv_type-transport_charge + wa_serv_ups_type-basis_value.
************************************************************
************************************************************
*************************************************************changes
************************************************************          wa_temp_ups_serv_type-service_type = wa_final_ups_serv_type-service_type.
************************************************************          wa_temp_ups_serv_type-tracking_number = wa_serv_ups_type-trck_num.
************************************************************          wa_temp_ups_serv_type-transport_charge = wa_final_ups_serv_type-transport_charge + wa_serv_ups_type-basis_value.
************************************************************          append  wa_temp_ups_serv_type to it_temp_ups_serv_type.
************************************************************          clear : wa_temp_ups_serv_type.
***************************************************************end of hcnages
************************************************************
************************************************************
************************************************************
************************************************************          modify it_final_ups_serv_type index v_id from wa_final_ups_serv_type transporting  transport_charge.
*************************************************************        delete it_final_ups_Serv_type index sy-tabix.
************************************************************          clear :wa_final_ups_serv_type,wa_serv_ups_type.
************************************************************
************************************************************        elseif ( sy-subrc <> 0 ) or  ( wa_final_ups_serv_type-tracking_number <> wa_serv_ups_type-lead_ship_number )
************************************************************          or  ( sy-subrc = 0  and wa_serv_ups_type-chrg_class_code = 'ACC' and wa_serv_ups_type-chrg_desc_code = 'ART').
************************************************************
*************************************************************          IF  ( WA_SERV_UPS_TYPE-CHRG_CLASS_CODE = 'FRT' )  OR ( WA_SERV_UPS_TYPE-CHRG_CLASS_CODE = 'ACC' AND WA_SERV_UPS_TYPE-CHRG_DESC_CODE = 'ART' ).
************************************************************
************************************************************          if  (  wa_serv_ups_type-chrg_class_code = 'FRT' and ( wa_serv_ups_type-chrg_catg_code = 'SHP' or wa_serv_ups_type-chrg_catg_code = 'RTN') ) or
************************************************************              (  wa_serv_ups_type-chrg_class_code = 'ACC' and wa_serv_ups_type-chrg_desc_code = 'ART' and wa_serv_ups_type-chrg_catg_code = 'RTN').
************************************************************
************************************************************clear wa_final_ups_serv_type.
************************************************************
*************************************************************endif.
************************************************************            wa_final_ups_serv_type-service_type = wa_serv_ups_type-chrg_desc.
************************************************************
************************************************************            wa_final_ups_serv_type-chrg_class_code = wa_serv_ups_type-chrg_class_code.
************************************************************            wa_final_ups_serv_type-chrg_desc_code = wa_serv_ups_type-chrg_desc_code.
************************************************************            wa_final_ups_serv_type-tracking_number = wa_serv_ups_type-lead_ship_number.
*************************************************************            WA_FINAL_UPS_SERV_TYPE-TRANSPORT_CHARGE = WA_SERV_UPS_TYPE-BASIS_VALUE.
************************************************************
************************************************************
************************************************************            append wa_final_ups_serv_type to it_final_ups_serv_type.
************************************************************            clear : wa_final_ups_serv_type,wa_serv_ups_type.
************************************************************
*************************************************************          break-point.
************************************************************          endif.
************************************************************        endif.
************************************************************
*************************************************************      endat.
************************************************************        clear : wa_serv_ups_type.
************************************************************
************************************************************
************************************************************
************************************************************
*************************************************************   ENDIF.
************************************************************        clear : wa_final_ups_serv_type,wa_serv_ups_type.
************************************************************      endloop.
************************************************************
************************************************************
************************************************************
************************************************************      sort it_final_ups_serv_type by service_type.
************************************************************
************************************************************      loop at it_final_ups_serv_type into wa_final_ups_serv_type.
************************************************************        at end of service_type.
************************************************************          sum.
************************************************************          wa_output_ups_serv_type-service_type = wa_final_ups_serv_type-service_type.
************************************************************          wa_output_ups_serv_type-transport_charge = wa_final_ups_serv_type-transport_charge.
************************************************************
************************************************************          append wa_output_ups_serv_type to it_output_ups_serv_type.
************************************************************          clear wa_output_ups_serv_type.
************************************************************        endat.
************************************************************
************************************************************      endloop.
************************************************************
************************************************************
************************************************************
************************************************************      break-point.
************************************************************    endif.
*
*    IF SY-SUBRC = 0.
*      SORT IT_FINAL_UPS_TYPE_UPS BY SERVICE_TYPE.
*      CLEAR WA_FINAL_SERVICE_TYPE_UPS.
*
*
*      LOOP AT IT_FINAL_SERVICE_TYPE_UPS INTO WA_FINAL_SERVICE_TYPE_UPS.
*        WA_FINAL_SERVICE_TYPE_UPS-TOTAL_SHIPMENTS = 1.
*
*        MODIFY IT_FINAL_SERVICE_TYPE_UPS FROM WA_FINAL_SERVICE_TYPE_UPS TRANSPORTING TOTAL_SHIPMENTS.
*
*        AT END OF SERVICE_TYPE.
*          SUM.     " calculates sub totals on service types
*          WA_OUTPUT_SERVICE_TYPE_UPS-V_TOTAL_WEIGHT = WA_FINAL_SERVICE_TYPE_UPS-ACTUALWEIGHT.
*          WA_OUTPUT_SERVICE_TYPE_UPS-V_DUE          = WA_FINAL_SERVICE_TYPE_UPS-TOTAL_BILL_CHARG.
*          WA_FINAL_SERVICE_TYPE_UPS-TOTAL_SHIPMENTS = WA_FINAL_SERVICE_TYPE_UPS-TOTAL_SHIPMENTS.
*          MODIFY IT_FINAL_SERVICE_TYPE_UPS FROM WA_FINAL_SERVICE_TYPE_UPS TRANSPORTING V_TOTAL_WEIGHT
*                                                     V_TOTAL.
*          WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_SHIPMENTS = WA_FINAL_SERVICE_TYPE_UPS-TOTAL_SHIPMENTS.
*          WA_OUTPUT_SERVICE_TYPE_UPS-ZPACKAGES = WA_FINAL_SERVICE_TYPE_UPS-TOTAL_SHIPMENTS.
*          WA_OUTPUT_SERVICE_TYPE_UPS-SERVICE_TYPE = WA_FINAL_SERVICE_TYPE_UPS-SERVICE_TYPE.
*          WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_LETERS = 0.
*          WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_INC_CREDIT = WA_FINAL_SERVICE_TYPE_UPS-TOTAL_INC_CREDIT.
*
*          WA_OUTPUT_SERVICE_TYPE_UPS-PKG_AVG_COST = ( WA_OUTPUT_SERVICE_TYPE_UPS-V_DUE / WA_OUTPUT_SERVICE_TYPE_UPS-ZPACKAGES ).
*          WA_OUTPUT_SERVICE_TYPE_UPS-WEIGHT_AVG_COST = WA_OUTPUT_SERVICE_TYPE_UPS-V_DUE / WA_OUTPUT_SERVICE_TYPE_UPS-V_TOTAL_WEIGHT.
** calculate the grand totals
*          WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_BILL_CHARG = WA_FINAL_SERVICE_TYPE_UPS-TOTAL_BILL_CHARG + WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_BILL_CHARG.
*          TOTAL_AMNT_UPS = WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_BILL_CHARG + TOTAL_AMNT_UPS.
**        total_savings = total_savings + wa_final_service_type-ratedweight.
*          TOTAL_SAVINGS_UPS = TOTAL_SAVINGS_UPS + WA_FINAL_SERVICE_TYPE_UPS-TOTAL_INC_CREDIT.
*          TOTAL_PKGS_UPS    = TOTAL_PKGS_UPS + WA_FINAL_SERVICE_TYPE_UPS-TOTAL_SHIPMENTS.
*          TOTAL_WEIGHT_UPS  = TOTAL_WEIGHT_UPS + WA_OUTPUT_SERVICE_TYPE_UPS-V_TOTAL_WEIGHT.
*          APPEND WA_OUTPUT_SERVICE_TYPE_UPS TO IT_OUTPUT_SERVICE_TYPE_UPS.
*          CLEAR WA_FINAL_SERVICE_TYPE_UPS.
*        ENDAT.
*        CLEAR WA_OUTPUT_SERVICE_TYPE_UPS.
*      ENDLOOP.
*
** calculate the Avg.costs and percentage
*      LOOP AT IT_OUTPUT_SERVICE_TYPE_UPS INTO WA_OUTPUT_SERVICE_TYPE_UPS.
*        WA_OUTPUT_SERVICE_TYPE_UPS-AMOUNT_PERCENT = WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_BILL_CHARG * 100.
*        WA_OUTPUT_SERVICE_TYPE_UPS-AMOUNT_PERCENT = WA_OUTPUT_SERVICE_TYPE_UPS-AMOUNT_PERCENT / TOTAL_AMNT_UPS.
*        MODIFY IT_OUTPUT_SERVICE_TYPE_UPS FROM WA_OUTPUT_SERVICE_TYPE_UPS TRANSPORTING AMOUNT_PERCENT.
*        AT LAST.
*          AVG_PKG_CST_UPS = TOTAL_AMNT_UPS / TOTAL_PKGS_UPS.
*          AVG_PND_CST_UPS = TOTAL_AMNT_UPS / TOTAL_WEIGHT_UPS.
*        ENDAT.
*        CLEAR WA_OUTPUT_SERVICE_TYPE_UPS.
*      ENDLOOP.
** add the grand totals as last record
*      WA_OUTPUT_SERVICE_TYPE_UPS-SERVICE_TYPE = 'Total'.
*      WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_SHIPMENTS = TOTAL_PKGS_UPS.
*      WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_LETERS    = 0.
*      WA_OUTPUT_SERVICE_TYPE_UPS-V_TOTAL_WEIGHT  = TOTAL_WEIGHT_UPS.
*      WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_INC_CREDIT  = TOTAL_SAVINGS_UPS .
*      WA_OUTPUT_SERVICE_TYPE_UPS-TOTAL_BILL_CHARG    = TOTAL_AMNT_UPS.
*      WA_OUTPUT_SERVICE_TYPE_UPS-AMOUNT_PERCENT  = ' '.
*      WA_OUTPUT_SERVICE_TYPE_UPS-PKG_AVG_COST    = AVG_PKG_CST_UPS.
*      WA_OUTPUT_SERVICE_TYPE_UPS-WEIGHT_AVG_COST = AVG_PND_CST_UPS.
*      WA_OUTPUT_SERVICE_TYPE_UPS-LINE_COLOR      = 'C500'.
*      APPEND WA_OUTPUT_SERVICE_TYPE_UPS TO IT_OUTPUT_SERVICE_TYPE_UPS.
*      CLEAR WA_OUTPUT_SERVICE_TYPE_UPS.
*
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 1.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'SERVICE_TYPE'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'Service Type'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 2.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'TOTAL_SHIPMENTS'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'Total Packages'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 3.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'TOTAL_LETERS'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'Total Letters only'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 4.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'V_TOTAL_WEIGHT'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'Total Weight'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 5.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'TOTAL_INC_CREDIT'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'TotalSavings($)'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 6.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'TOTAL_BILL_CHARG'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'TotalNetCost($)'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 7.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'AMOUNT_PERCENT'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'TotalNetCost(%)'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 8.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'PKG_AVG_COST'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'Avg.Cost per Package($)'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COL_POS      = 9.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-FIELDNAME    = 'WEIGHT_AVG_COST'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-COLTEXT    = 'Avg.Cost per Pound($)'.
*      WA_FIELDCAT_SERVICE_TYPE_UPS-TABNAME      = 'IT_OUPUT_SERVICE_TYPE_UPS'.
*      APPEND WA_FIELDCAT_SERVICE_TYPE_UPS TO IT_FIELDCAT_SERVICE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_SERVICE_TYPE_UPS.
*
*
*      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*      GS_LAYOUT-GRID_TITLE = SPACE.
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*        EXPORTING
*          I_STRUCTURE_NAME              = 'IT_OUPUT_SERVICE_TYPE_UPS'
*          IS_LAYOUT                     = GS_LAYOUT
*        CHANGING
*          IT_OUTTAB                     = IT_OUTPUT_SERVICE_TYPE_UPS
*          IT_FIELDCATALOG               = IT_FIELDCAT_SERVICE_TYPE_UPS
**    IT_SORT                       =
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4  .
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
**
*
*    ENDIF.
*  ENDIF.
*  endif.










endform.                    "SERVICETYPE



********************************************************END OF SERVICE TYPE *************************************



********************************************BEGIN OF ZONE ***************************
*---------------------------------------------------------------------*
*&      Form  ZONE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form zone.

  data sumcost type netwr.
  data netpercentage type netwr.
  data avgpackagecharge type netwr.
  data avgpoundcharge type netwr.
  data sumpackages type i.
  data sumweight type brgew_ap.
  data sumavgpkgcost type netwr.
  data sumavgpndcost type netwr.
  data sumdiscount type netwr.
  data count type  i.

  clear : approve.
  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.

  if wa_export_output-carrier = 'FEDEX'.


    data :v_zone type /PWEAVER/EFA_FED-zone_code,
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt.

    clear : v_zone,v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_zone_type,it_final_zone_type,it_fieldcat_zone_type,it_output_zone_type.
    clear : it_zone_type,wa_zone_type,wa_final_zone_type,wa_fieldcat_zone_type.
    select *
              from /PWEAVER/EFA_FED
              into corresponding fields of table it_zone_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.

    if sy-subrc = 0.
      clear : wa_zone_type.
      loop at it_zone_type into wa_zone_type.

if wa_zone_type-zone_code is initial.
  wa_zone_type-zone_code = 'miscellanious'.
  endif.
        wa_final_zone_type-zone  = wa_zone_type-zone_code.
        wa_final_zone_type-packages = wa_zone_type-num_pieces.
        wa_final_zone_type-rated_weight = wa_zone_type-rated_wgt_amt.
*       IF NOT WA_SERV_TYPE-TRAN_CHARG_AMNT IS INITIAL.
        wa_final_zone_type-transport_charge = wa_zone_type-tran_charg_amnt.
*        ENDIF.

****Handling charges
        if wa_zone_type-trk_id_chg_des = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des = 'Address Correction' or
      wa_zone_type-trk_id_chg_des = 'Residential' or
      wa_zone_type-trk_id_chg_des = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_zone_type-trk_id_chg_amt.
        endif.

        if wa_zone_type-trk_id_chg_des1 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des1 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des1 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des1 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des1 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des1 = 'Residential' or
      wa_zone_type-trk_id_chg_des1 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des1 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt1.
        endif.

        if wa_zone_type-trk_id_chg_des2 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des2 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des2 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des2 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des2 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des2 = 'Residential' or
      wa_zone_type-trk_id_chg_des2 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des2 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt2.
        endif.

        if wa_zone_type-trk_id_chg_des3 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des3 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des3 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des3 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des3 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des3 = 'Residential' or
      wa_zone_type-trk_id_chg_des3 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des3 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt3.
        endif.

        if wa_zone_type-trk_id_chg_des4 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des4 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des4 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des4 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des4 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des4 = 'Residential' or
      wa_serv_type-trk_id_chg_des4 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des4 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt4.
        endif.

        if wa_zone_type-trk_id_chg_des5 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des5 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des5 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des5 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des5 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des5 = 'Residential' or
      wa_zone_type-trk_id_chg_des5 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des5 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt5.
        endif.

        if wa_zone_type-trk_id_chg_des6 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des6 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des6 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des6 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des6 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des6 = 'Residential' or
      wa_zone_type-trk_id_chg_des6 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des6 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt6.
        endif.

        if wa_zone_type-trk_id_chg_des7 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des7 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des7 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des7 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des7 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des7 = 'Residential' or
      wa_zone_type-trk_id_chg_des7 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des7 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt7.
        endif.

        if wa_zone_type-trk_id_chg_des8 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des8 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des8 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des8 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des8 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des8 = 'Residential' or
      wa_zone_type-trk_id_chg_des8 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des8 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt8.
        endif.


        if wa_zone_type-trk_id_chg_des9 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des9 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des9 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des9 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des9 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des9 = 'Residential' or
      wa_zone_type-trk_id_chg_des9 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des9 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt9.
        endif.


        if wa_zone_type-trk_id_chg_des10 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des10 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des10 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des10 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des10 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des10 = 'Residential' or
      wa_zone_type-trk_id_chg_des10 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des10 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt10.
        endif.


        if wa_zone_type-trk_id_chg_des11 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des11 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des11 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des11 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des11 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des11 = 'Residential' or
      wa_zone_type-trk_id_chg_des11 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des11 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt11.
        endif.


        if wa_zone_type-trk_id_chg_des12 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des12 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des12 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des12 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des12 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des12 = 'Residential' or
      wa_zone_type-trk_id_chg_des12 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des12 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt12.
        endif.


        if wa_zone_type-trk_id_chg_des13 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des13 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des13 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des13 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des13 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des13 = 'Residential' or
      wa_zone_type-trk_id_chg_des13 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des13 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt13.
        endif.


        if wa_zone_type-trk_id_chg_des14 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des14 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des14 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des14 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des14 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des14 = 'Residential' or
      wa_zone_type-trk_id_chg_des14 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des14 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt14.
        endif.


        if wa_zone_type-trk_id_chg_des15 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des15 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des15 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des15 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des15 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des15 = 'Residential' or
      wa_zone_type-trk_id_chg_des15 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des15 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt15.
        endif.


        if wa_zone_type-trk_id_chg_des16 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des16 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des16 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des16 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des16 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des16 = 'Residential' or
      wa_zone_type-trk_id_chg_des16 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des16 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt16.
        endif.


        if wa_zone_type-trk_id_chg_des17 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des17 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des17 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des17 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des17 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des17 = 'Residential' or
      wa_zone_type-trk_id_chg_des17 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des17 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt17.
        endif.


        if wa_zone_type-trk_id_chg_des18 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des18 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des18 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des18 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des18 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des18 = 'Residential' or
      wa_zone_type-trk_id_chg_des18 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des18 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt18.
        endif.


        if wa_zone_type-trk_id_chg_des19 = 'Fuel Surcharge' or
wa_zone_type-trk_id_chg_des19 = 'Weekly Service Chg' or
wa_zone_type-trk_id_chg_des19 = 'Weekday Delivery' or
wa_zone_type-trk_id_chg_des19 = 'Residential Delivery' or
wa_zone_type-trk_id_chg_des19 = 'Address Correction' or
wa_zone_type-trk_id_chg_des19 = 'Residential' or
wa_zone_type-trk_id_chg_des19 = 'DAS Comm' or
wa_zone_type-trk_id_chg_des19 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt19.
        endif.

        if wa_zone_type-trk_id_chg_des20 = 'Fuel Surcharge' or
wa_zone_type-trk_id_chg_des20 = 'Weekly Service Chg' or
wa_zone_type-trk_id_chg_des20 = 'Weekday Delivery' or
wa_zone_type-trk_id_chg_des20 = 'Residential Delivery' or
wa_zone_type-trk_id_chg_des20 = 'Address Correction' or
wa_zone_type-trk_id_chg_des20 = 'Residential' or
wa_zone_type-trk_id_chg_des20 = 'DAS Comm' or
wa_zone_type-trk_id_chg_des20 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt20.
        endif.

        if wa_zone_type-trk_id_chg_des21 = 'Fuel Surcharge' or
wa_zone_type-trk_id_chg_des21 = 'Weekly Service Chg' or
wa_zone_type-trk_id_chg_des21 = 'Weekday Delivery' or
wa_zone_type-trk_id_chg_des21 = 'Residential Delivery' or
wa_zone_type-trk_id_chg_des21 = 'Address Correction' or
wa_zone_type-trk_id_chg_des21 = 'Residential' or
wa_zone_type-trk_id_chg_des21 = 'DAS Comm' or
wa_zone_type-trk_id_chg_des21 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt21.
        endif.

        if wa_zone_type-trk_id_chg_des22 = 'Fuel Surcharge' or
      wa_zone_type-trk_id_chg_des22 = 'Weekly Service Chg' or
      wa_zone_type-trk_id_chg_des22 = 'Weekday Delivery' or
      wa_zone_type-trk_id_chg_des22 = 'Residential Delivery' or
      wa_zone_type-trk_id_chg_des22 = 'Address Correction' or
      wa_zone_type-trk_id_chg_des22 = 'Residential' or
      wa_zone_type-trk_id_chg_des22 = 'DAS Comm' or
      wa_zone_type-trk_id_chg_des22 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt22.
        endif.

        if wa_zone_type-trk_id_chg_des23 = 'Fuel Surcharge' or
wa_zone_type-trk_id_chg_des23 = 'Weekly Service Chg' or
wa_zone_type-trk_id_chg_des23 = 'Weekday Delivery' or
wa_zone_type-trk_id_chg_des23 = 'Residential Delivery' or
wa_zone_type-trk_id_chg_des23 = 'Address Correction' or
wa_zone_type-trk_id_chg_des23 = 'Residential' or
wa_zone_type-trk_id_chg_des23 = 'DAS Comm' or
wa_zone_type-trk_id_chg_des23 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt23.
        endif.

        if wa_zone_type-trk_id_chg_des24 = 'Fuel Surcharge' or
wa_zone_type-trk_id_chg_des24 = 'Weekly Service Chg' or
wa_zone_type-trk_id_chg_des24 = 'Weekday Delivery' or
wa_zone_type-trk_id_chg_des24 = 'Residential Delivery' or
wa_zone_type-trk_id_chg_des24 = 'Address Correction' or
wa_zone_type-trk_id_chg_des24 = 'Residential' or
wa_zone_type-trk_id_chg_des24 = 'DAS Comm' or
wa_zone_type-trk_id_chg_des24 = 'DAS Extended Comm'.

          wa_final_zone_type-handling_charge =  wa_final_zone_type-handling_charge + wa_zone_type-trk_id_chg_amt24.
        endif.





        if wa_zone_type-trk_id_chg_des = 'Discount'.
          wa_final_zone_type-discount =  wa_zone_type-trk_id_chg_amt.
        endif.

        if wa_zone_type-trk_id_chg_des1 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt1.
        endif.

        if wa_zone_type-trk_id_chg_des2 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt2.
        endif.

        if wa_zone_type-trk_id_chg_des3 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt3.
        endif.

        if wa_zone_type-trk_id_chg_des4 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt4.
        endif.

        if wa_zone_type-trk_id_chg_des5 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt5.
        endif.

        if wa_zone_type-trk_id_chg_des6 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt6.
        endif.

        if wa_zone_type-trk_id_chg_des7 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt7.
        endif.

        if wa_zone_type-trk_id_chg_des8 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt8.
        endif.

        if wa_zone_type-trk_id_chg_des9 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt9.
        endif.


        if wa_zone_type-trk_id_chg_des10 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt10.
        endif.

        if wa_zone_type-trk_id_chg_des11 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt11.
        endif.

        if wa_zone_type-trk_id_chg_des12 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt12.
        endif.

        if wa_zone_type-trk_id_chg_des13 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt13.
        endif.


        if wa_zone_type-trk_id_chg_des14 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt14.
        endif.

        if wa_zone_type-trk_id_chg_des15 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt15.
        endif.


        if wa_zone_type-trk_id_chg_des16 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt16.
        endif.

        if wa_zone_type-trk_id_chg_des17 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt17.
        endif.

        if wa_zone_type-trk_id_chg_des18 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt18.
        endif.

        if wa_zone_type-trk_id_chg_des19 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt19.
        endif.

        if wa_zone_type-trk_id_chg_des20 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt20.
        endif.

        if wa_zone_type-trk_id_chg_des21 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt21.
        endif.

        if wa_zone_type-trk_id_chg_des22 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt22.
        endif.

        if wa_zone_type-trk_id_chg_des23 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt23.
        endif.

        if wa_zone_type-trk_id_chg_des24 = 'Discount'.
          wa_final_zone_type-discount =   wa_final_zone_type-discount + wa_zone_type-trk_id_chg_amt24.
        endif.


        wa_final_zone_type-net_charge = wa_zone_type-net_chrg_amnt.

        append wa_final_zone_type to it_final_zone_type.
        clear :wa_final_zone_type,wa_zone_type.
      endloop.


      sort it_final_zone_type by zone.
      clear wa_final_zone_type.


      loop at it_final_zone_type into wa_final_zone_type.



        if not wa_final_zone_type is initial.
          at end of zone.
            sum.     " calculates sub totals on service types

            wa_output_zone_type-zone = wa_final_zone_type-zone.
            wa_output_zone_type-packages =  wa_final_zone_type-packages.
            wa_output_zone_type-rated_weight = wa_final_zone_type-rated_weight.
            wa_output_zone_type-transport_charge = wa_final_zone_type-transport_charge.
            wa_output_zone_type-handling_charge =  wa_final_zone_type-handling_charge.
            wa_output_zone_type-discount =  wa_final_zone_type-discount.
            wa_output_zone_type-net_charge = wa_final_zone_type-net_charge.
* calculate the grand totals

            v_packages = v_packages + wa_output_zone_type-packages.
            v_rated_weight = v_rated_weight + wa_output_zone_type-rated_weight.
            v_trans_charg = v_trans_charg + wa_output_zone_type-transport_charge.
            v_hand_charg = v_hand_charg + wa_output_zone_type-handling_charge.
            v_discount = v_discount + wa_output_zone_type-discount.
            v_netcharge = v_netcharge +  wa_output_zone_type-net_charge .
            append wa_output_zone_type to it_output_zone_type.
            clear wa_output_zone_type.
          endat.

        endif.
        clear wa_final_zone_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_zone_type.

*    wa_output_serv_type-service_type = v_service.
      wa_output_zone_type-packages = v_packages.
      wa_output_zone_type-rated_weight = v_rated_weight.

      wa_output_zone_type-transport_charge = v_trans_charg.
      wa_output_zone_type-handling_charge = v_hand_charg .
      wa_output_zone_type-discount = v_discount .
      wa_output_zone_type-net_charge = v_netcharge.

      wa_output_zone_type-line_color      = 'C500'.
      append wa_output_zone_type to it_output_zone_type.
      clear wa_output_zone_type.



      wa_fieldcat_zone_type-col_pos      = 1.
      wa_fieldcat_zone_type-fieldname    = 'ZONE'.
      wa_fieldcat_zone_type-coltext    = 'Zone Code'.
      wa_fieldcat_zone_type-tabname      = 'IT_OUPUT_ZONE_TYPE'.
      append wa_fieldcat_zone_type to it_fieldcat_zone_type.
      clear wa_fieldcat_zone_type.

      wa_fieldcat_zone_type-col_pos      = 2.
      wa_fieldcat_zone_type-fieldname    = 'PACKAGES'.
      wa_fieldcat_zone_type-coltext    = 'Total Packages'.
      wa_fieldcat_zone_type-tabname      = 'IT_OUPUT_ZONE_TYPE'.
      append wa_fieldcat_zone_type to it_fieldcat_zone_type.
      clear wa_fieldcat_zone_type.

      wa_fieldcat_zone_type-col_pos      = 3.
      wa_fieldcat_zone_type-fieldname    = 'RATED_WEIGHT'.
      wa_fieldcat_zone_type-coltext    = 'Rated Weight'.
      wa_fieldcat_zone_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_zone_type to it_fieldcat_zone_type.
      clear wa_fieldcat_zone_type.

      wa_fieldcat_zone_type-col_pos      = 4.
      wa_fieldcat_zone_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_zone_type-coltext    = 'Transport Charge'.
      wa_fieldcat_zone_type-tabname      = 'IT_OUPUT_ZONE_TYPE'.
      append wa_fieldcat_zone_type to it_fieldcat_zone_type.
      clear wa_fieldcat_zone_type.

      wa_fieldcat_zone_type-col_pos      = 5.
      wa_fieldcat_zone_type-fieldname    = 'HANDLING_CHARGE'.
      wa_fieldcat_zone_type-coltext    = 'Handling Charge'.
      wa_fieldcat_zone_type-tabname      = 'IT_OUPUT_ZONE_TYPE'.
      append wa_fieldcat_zone_type to it_fieldcat_zone_type.
      clear wa_fieldcat_zone_type.

      wa_fieldcat_zone_type-col_pos      = 6.
      wa_fieldcat_zone_type-fieldname    = 'DISCOUNT'.
      wa_fieldcat_zone_type-coltext    = 'Discount'.
      wa_fieldcat_zone_type-tabname      = 'IT_OUPUT_ZONE_TYPE'.
      append wa_fieldcat_zone_type to it_fieldcat_zone_type.
      clear wa_fieldcat_zone_type.

      wa_fieldcat_zone_type-col_pos      = 7.
      wa_fieldcat_zone_type-fieldname    = 'NET_CHARGE'.
      wa_fieldcat_zone_type-coltext    = 'Net Charge'.
      wa_fieldcat_zone_type-tabname      = 'IT_OUPUT_ZONE_TYPE'.
      append wa_fieldcat_zone_type to it_fieldcat_zone_type.
      clear wa_fieldcat_zone_type.



      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUPUT_ZONE_TYPE'
*          is_layout                     = gs_layout
          is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
        changing
          it_outtab                     = it_output_zone_type
          it_fieldcatalog               = it_fieldcat_zone_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.



    endif.
  endif.

*  if ups = 'X'.
  if wa_export_output-carrier = 'UPS'    .

    sort it_final_ups_zone by zone.
    clear wa_final_ups_zone.
    refresh : it_output_ups_zone_type, it_fieldcat_zone_ups_type.

    loop at it_final_ups_zone into wa_final_ups_zone.

      at end of zone.
        sum.     " calculates sub totals on zone types
        wa_output_ups_zone_type-zone = wa_final_ups_zone-zone.
        wa_output_ups_zone_type-packages = wa_final_ups_zone-packages.
        wa_output_ups_zone_type-rated_weight = wa_final_ups_zone-rated_weight.
        wa_output_ups_zone_type-transport_charge = wa_final_ups_zone-transport_charge.
        wa_output_ups_zone_type-handling_charge = wa_final_ups_zone-handling_charge.
        wa_output_ups_zone_type-discount = wa_final_ups_zone-discount.
        wa_output_ups_zone_type-net_charge = wa_final_ups_zone-net_charge.
*
        v_packages = v_packages + wa_final_ups_zone-packages.
        v_rated_weight = v_rated_weight + wa_final_ups_zone-rated_weight.

        v_trans_charg = v_trans_charg + wa_final_ups_zone-transport_charge.


        v_hand_charg = v_hand_charg + wa_final_ups_zone-handling_charge.


        v_discount = v_discount + wa_final_ups_zone-discount.

        v_netcharge = v_netcharge +  wa_final_ups_zone-net_charge .

        append wa_output_ups_zone_type to it_output_ups_zone_type.
        clear :wa_output_ups_zone_type.

      endat.


      clear wa_final_ups_zone.


    endloop.



* add the grand totals as last record
    clear : wa_output_ups_zone_type.

*    wa_output_serv_type-service_type = v_service.
    wa_output_ups_zone_type-packages = v_packages.
    wa_output_ups_zone_type-rated_weight = v_rated_weight.

    wa_output_ups_zone_type-transport_charge = v_trans_charg.
    wa_output_ups_zone_type-handling_charge = v_hand_charg .
    wa_output_ups_zone_type-discount = v_discount .
    wa_output_ups_zone_type-net_charge = v_netcharge.

    wa_output_ups_zone_type-line_color      = 'C500'.
    append wa_output_ups_zone_type to it_output_ups_zone_type.
    clear wa_output_ups_zone_type.



    wa_fieldcat_zone_ups_type-col_pos      = 1.
    wa_fieldcat_zone_ups_type-fieldname    = 'ZONE'.
    wa_fieldcat_zone_ups_type-coltext    = 'zone'.
    wa_fieldcat_zone_ups_type-tabname      = 'IT_OUPUT_UPS_zone_TYPE'.
    append wa_fieldcat_zone_ups_type to it_fieldcat_zone_ups_type.
    clear wa_fieldcat_zone_ups_type.

    wa_fieldcat_zone_ups_type-col_pos      = 2.
    wa_fieldcat_zone_ups_type-fieldname    = 'PACKAGES'.
    wa_fieldcat_zone_ups_type-coltext    = 'Total Packages'.
    wa_fieldcat_zone_ups_type-tabname      = 'IT_OUPUT_UPS_zone_TYPE'.
    append wa_fieldcat_zone_ups_type to it_fieldcat_zone_ups_type.
    clear wa_fieldcat_zone_ups_type.

    wa_fieldcat_zone_ups_type-col_pos      = 3.
    wa_fieldcat_zone_ups_type-fieldname    = 'RATED_WEIGHT'.
    wa_fieldcat_zone_ups_type-coltext    = 'Rated Weight'.
    wa_fieldcat_zone_ups_type-tabname      = 'IT_OUPUT_UPS_zone_TYPE'.
    append wa_fieldcat_zone_ups_type to it_fieldcat_zone_ups_type.
    clear wa_fieldcat_zone_ups_type.

    wa_fieldcat_zone_ups_type-col_pos      = 4.
    wa_fieldcat_zone_ups_type-fieldname    = 'TRANSPORT_CHARGE'.
    wa_fieldcat_zone_ups_type-coltext    = 'Transport Charge'.
    wa_fieldcat_zone_ups_type-tabname      = 'IT_OUPUT_UPS_zone_TYPE'.
    append wa_fieldcat_zone_ups_type to it_fieldcat_zone_ups_type.
    clear wa_fieldcat_zone_ups_type.

    wa_fieldcat_zone_ups_type-col_pos      = 5.
    wa_fieldcat_zone_ups_type-fieldname    = 'HANDLING_CHARGE'.
    wa_fieldcat_zone_ups_type-coltext    = 'Handling Charge'.
    wa_fieldcat_zone_ups_type-tabname      = 'IT_OUPUT_UPS_zone_TYPE'.
    append wa_fieldcat_zone_ups_type to it_fieldcat_zone_ups_type.
    clear wa_fieldcat_zone_ups_type.

    wa_fieldcat_zone_ups_type-col_pos      = 6.
    wa_fieldcat_zone_ups_type-fieldname    = 'DISCOUNT'.
    wa_fieldcat_zone_ups_type-coltext    = 'Discount'.
    wa_fieldcat_zone_ups_type-tabname      = 'IT_OUPUT_UPS_zone_TYPE'.
    append wa_fieldcat_zone_ups_type to it_fieldcat_zone_ups_type.
    clear wa_fieldcat_zone_ups_type.

    wa_fieldcat_zone_ups_type-col_pos      = 7.
    wa_fieldcat_zone_ups_type-fieldname    = 'NET_CHARGE'.
    wa_fieldcat_zone_ups_type-coltext    = 'Net Charge'.
    wa_fieldcat_zone_ups_type-tabname      = 'IT_OUPUT_UPS_zone_TYPE'.
    append wa_fieldcat_zone_ups_type to it_fieldcat_zone_ups_type.
    clear wa_fieldcat_zone_ups_type.



    gs_layout-cwidth_opt = 'X'.
    gs_layout-info_fname = 'LINE_COLOR'.
    gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
    call method go_grid->set_table_for_first_display
      exporting
        i_structure_name              = 'IT_OUTPUT_UPS_zone_TYPE'
*        is_layout                     = gs_layout
        is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
      changing
        it_outtab                     = it_output_ups_zone_type
        it_fieldcatalog               = it_fieldcat_zone_ups_type
*    IT_SORT                       =
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4  .
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.








*    DATA SUMCOST_UPS TYPE NETWR.
*    DATA NETPERCENTAGE_UPS TYPE NETWR.
*    DATA AVGPACKAGECHARGE_UPS TYPE NETWR.
*    DATA AVGPOUNDCHARGE_UPS TYPE NETWR.
*    DATA SUMPACKAGES_UPS TYPE I.
*    DATA SUMWEIGHT_UPS TYPE BRGEW_AP.
*    DATA SUMAVGPKGCOST_UPS TYPE NETWR.
*    DATA SUMAVGPNDCOST_UPS TYPE NETWR.
*    DATA SUMDISCOUNT_UPS TYPE NETWR.
*    DATA COUNT_UPS TYPE  I.
*
*    CLEAR : APPROVE.
*
*
*    CLEAR: IT_FINAL_ZONE_TYPE_UPS,WA_FINAL_ZONE_TYPE_UPS,SUMCOST_UPS,SUMPACKAGES_UPS,SUMWEIGHT_UPS,COUNT_UPS,WA_FIELDCAT_ZONE_TYPE_UPS.
*    REFRESH:IT_FINAL_ZONE_TYPE_UPS,IT_FIELDCAT_ZONE_TYPE_UPS.
*
*
*
*    SELECT ZONE1 COUNT( DISTINCT TRACKINGNUMBER )  SUM( ACTUALWEIGHT ) SUM( TOTAL_INC_CREDIT ) SUM( TOTAL_BILL_CHARG )
*        INTO (WA_FINAL_ZONE_TYPE_UPS-ZONE1,
*        WA_FINAL_ZONE_TYPE_UPS-ZPACKAGES,
*        WA_FINAL_ZONE_TYPE_UPS-ACTUALWEIGHT,
*        WA_FINAL_ZONE_TYPE_UPS-TOTAL_INC_CREDIT,
*        WA_FINAL_ZONE_TYPE_UPS-TOTAL_BILL_CHARG) FROM ZPWEFA_INVC_UPS  WHERE INVOICE_DATE = WEEK_DT  GROUP BY ZONE1.
*      APPEND WA_FINAL_ZONE_TYPE_UPS TO IT_FINAL_ZONE_TYPE_UPS.
*    ENDSELECT.
*
*    IF NOT IT_FINAL_ZONE_TYPE_UPS[] IS INITIAL.
*
*      LOOP AT IT_FINAL_ZONE_TYPE_UPS INTO WA_FINAL_ZONE_TYPE_UPS.
*        SUMCOST_UPS = SUMCOST_UPS + WA_FINAL_ZONE_TYPE_UPS-TOTAL_BILL_CHARG.
*        SUMPACKAGES_UPS  = SUMPACKAGES_UPS  + WA_FINAL_ZONE_TYPE_UPS-ZPACKAGES.
*        SUMWEIGHT_UPS = SUMWEIGHT_UPS + WA_FINAL_ZONE_TYPE_UPS-ACTUALWEIGHT.
*        SUMDISCOUNT_UPS = SUMDISCOUNT_UPS + WA_FINAL_ZONE_TYPE_UPS-TOTAL_INC_CREDIT.
*        COUNT  = COUNT + 1.
*      ENDLOOP.
*
*
*      LOOP AT IT_FINAL_ZONE_TYPE_UPS INTO WA_FINAL_ZONE_TYPE_UPS.
*        NETPERCENTAGE_UPS = WA_FINAL_ZONE_TYPE_UPS-TOTAL_BILL_CHARG / SUMCOST_UPS * 100.
*        WA_FINAL_ZONE_TYPE_UPS-TOTALNETCOSTPER = NETPERCENTAGE_UPS.
*        IF WA_FINAL_ZONE_TYPE_UPS-ZPACKAGES IS NOT INITIAL.
*          AVGPACKAGECHARGE_UPS = WA_FINAL_ZONE_TYPE_UPS-TOTAL_BILL_CHARG / WA_FINAL_ZONE_TYPE_UPS-ZPACKAGES.
*        ENDIF.
*        WA_FINAL_ZONE_TYPE_UPS-AVGPKGCOST = AVGPACKAGECHARGE_UPS.
*
*
*        IF  WA_FINAL_ZONE_TYPE_UPS-ACTUALWEIGHT IS NOT INITIAL.
*          AVGPOUNDCHARGE_UPS  = WA_FINAL_ZONE_TYPE_UPS-TOTAL_BILL_CHARG / WA_FINAL_ZONE_TYPE_UPS-ACTUALWEIGHT.
*        ENDIF.
*
*
*        WA_FINAL_ZONE_TYPE_UPS-AVGPNDCOST =  AVGPOUNDCHARGE_UPS.
*        WA_FINAL_ZONE_TYPE_UPS-TOTALLETTERS = 0.
*        MODIFY  IT_FINAL_ZONE_TYPE_UPS FROM WA_FINAL_ZONE_TYPE_UPS TRANSPORTING TOTALNETCOSTPER  AVGPKGCOST AVGPNDCOST TOTALLETTERS.
*      ENDLOOP.
*
*      SUMAVGPKGCOST_UPS = SUMCOST_UPS / SUMPACKAGES_UPS.
*      SUMAVGPNDCOST_UPS = SUMCOST_UPS / SUMWEIGHT_UPS.
*
*      CLEAR WA_FINAL_ZONE_TYPE_UPS.
*      WA_FINAL_ZONE_TYPE_UPS-ZPACKAGES = SUMPACKAGES_UPS.
*      WA_FINAL_ZONE_TYPE_UPS-TOTAL_BILL_CHARG = SUMCOST_UPS.
*      WA_FINAL_ZONE_TYPE_UPS-ACTUALWEIGHT = SUMWEIGHT_UPS.
*      WA_FINAL_ZONE_TYPE_UPS-AVGPKGCOST = SUMAVGPKGCOST_UPS.
*      WA_FINAL_ZONE_TYPE_UPS-AVGPNDCOST = SUMAVGPNDCOST_UPS.
*      WA_FINAL_ZONE_TYPE_UPS-TOTAL_INC_CREDIT = SUMDISCOUNT_UPS.
*      WA_FINAL_ZONE_TYPE_UPS-TOTALLETTERS = 0.
*      WA_FINAL_ZONE_TYPE_UPS-LINE_COLOR = 'C500'.
*
*      IF COUNT GE 1.
*        APPEND WA_FINAL_ZONE_TYPE_UPS TO IT_FINAL_ZONE_TYPE_UPS .
*      ENDIF.
*
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'ZONE1'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE_UPS'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Zone'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '10'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'ZPACKAGES'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE_UPS'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Total Packages'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '15'.
**  ZONE_WA_FIELDCAT-DO_SUM = 'X'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'TOTALLETTERS'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE_UPS'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Total Letters Only'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '15'.
**  ZONE_WA_FIELDCAT-DO_SUM = 'X'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'ACTUALWEIGHT'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE_UPS'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Total Weight'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '15'.
**  ZONE_WA_FIELDCAT-DO_SUM = 'X'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'TOTAL_INC_CREDIT'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE_UPS'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Total Savings'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '15'.
**  ZONE_WA_FIELDCAT-DO_SUM = 'X'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'TOTAL_BILL_CHARG'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE_UPS'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Total Net Cost'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '15'.
**  ZONE_WA_FIELDCAT-DO_SUM = 'X'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'TOTALNETCOSTPER'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Total Net Cost %'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-JUST = 'R'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '15'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'AVGPKGCOST'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Avg Cost Per Package'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '23'.
**  ZONE_WA_FIELDCAT-DO_SUM = 'X'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*      WA_FIELDCAT_ZONE_TYPE_UPS-FIELDNAME = 'AVGPNDCOST'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-TABNAME = 'IT_FINAL_ZONE_TYPE'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-COLTEXT = 'Avg Cost Per Pound'.
*      WA_FIELDCAT_ZONE_TYPE_UPS-OUTPUTLEN = '20'.
*      APPEND WA_FIELDCAT_ZONE_TYPE_UPS TO IT_FIELDCAT_ZONE_TYPE_UPS.
*      CLEAR WA_FIELDCAT_ZONE_TYPE_UPS.
*
*
*      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*      GS_LAYOUT-GRID_TITLE = SPACE.
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*         EXPORTING
*           I_STRUCTURE_NAME              = 'IT_FINAL_ZONE_TYPE_UPS'
*           IS_LAYOUT                     = GS_LAYOUT
*         CHANGING
*           IT_OUTTAB                     = IT_FINAL_ZONE_TYPE_UPS
*           IT_FIELDCATALOG               = IT_FIELDCAT_ZONE_TYPE_UPS
**    IT_SORT                       =
*       EXCEPTIONS
*         INVALID_PARAMETER_COMBINATION = 1
*         PROGRAM_ERROR                 = 2
*         TOO_MANY_LINES                = 3
*         OTHERS                        = 4  .
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*
*    ENDIF.
  endif.
endform.                    "ZONE



********************************************END OF ZONE**********************************
******************************************BEGIN OF payment method
*---------------------------------------------------------------------*
*&      Form  PAYMENTMETHOD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form paymentmethod.
  clear : approve.
  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
  if wa_export_output-carrier = 'FEDEX'.


    data :v_payor type /PWEAVER/EFA_FED-payor,
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt.

    clear : v_payor,v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_payor_type,it_final_payor_type,it_fieldcat_payor_type,it_output_payor_type.
    clear : it_payor_type,wa_payor_type,wa_final_payor_type,wa_fieldcat_payor_type.

    select *
              from /PWEAVER/EFA_FED
              into corresponding fields of table it_payor_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.

    if sy-subrc = 0.
      clear : wa_payor_type.
      loop at it_payor_type into wa_payor_type.
*        if wa_payor_type-payor is initial.
*        wa_payor_type-payor = 'miscellanious'.
*        endif.
          if wa_payor_type-payor = 'Recipient' OR wa_payor_type-payor = 'Shipper' OR wa_payor_type-payor =  'Third Party'.
               wa_final_payor_type-payor  = wa_payor_type-payor.
          else. "if wa_payor_type-payor =  '#NAME?'.
.
                wa_final_payor_type-payor  = ''."wa_payor_type-payor..#NAME?

           endif.
        wa_final_payor_type-packages = wa_payor_type-num_pieces.
        wa_final_payor_type-rated_weight = wa_payor_type-rated_wgt_amt.
*       IF NOT WA_SERV_TYPE-TRAN_CHARG_AMNT IS INITIAL.
        wa_final_payor_type-transport_charge = wa_payor_type-tran_charg_amnt.
*        ENDIF.

****Handling charges
        if wa_payor_type-trk_id_chg_des = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des = 'Address Correction' or
      wa_payor_type-trk_id_chg_des = 'Residential' or
      wa_payor_type-trk_id_chg_des = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_payor_type-trk_id_chg_amt.
        endif.

        if wa_payor_type-trk_id_chg_des1 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des1 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des1 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des1 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des1 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des1 = 'Residential' or
      wa_payor_type-trk_id_chg_des1 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des1 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt1.
        endif.

        if wa_payor_type-trk_id_chg_des2 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des2 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des2 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des2 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des2 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des2 = 'Residential' or
      wa_payor_type-trk_id_chg_des2 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des2 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt2.
        endif.

        if wa_payor_type-trk_id_chg_des3 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des3 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des3 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des3 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des3 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des3 = 'Residential' or
      wa_payor_type-trk_id_chg_des3 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des3 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt3.
        endif.

        if wa_payor_type-trk_id_chg_des4 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des4 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des4 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des4 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des4 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des4 = 'Residential' or
      wa_serv_type-trk_id_chg_des4 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des4 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt4.
        endif.

        if wa_payor_type-trk_id_chg_des5 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des5 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des5 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des5 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des5 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des5 = 'Residential' or
      wa_payor_type-trk_id_chg_des5 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des5 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt5.
        endif.

        if wa_payor_type-trk_id_chg_des6 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des6 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des6 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des6 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des6 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des6 = 'Residential' or
      wa_payor_type-trk_id_chg_des6 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des6 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt6.
        endif.

        if wa_payor_type-trk_id_chg_des7 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des7 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des7 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des7 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des7 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des7 = 'Residential' or
      wa_payor_type-trk_id_chg_des7 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des7 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt7.
        endif.

        if wa_payor_type-trk_id_chg_des8 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des8 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des8 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des8 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des8 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des8 = 'Residential' or
      wa_payor_type-trk_id_chg_des8 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des8 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt8.
        endif.


        if wa_payor_type-trk_id_chg_des9 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des9 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des9 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des9 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des9 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des9 = 'Residential' or
      wa_payor_type-trk_id_chg_des9 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des9 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt9.
        endif.


        if wa_payor_type-trk_id_chg_des10 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des10 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des10 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des10 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des10 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des10 = 'Residential' or
      wa_payor_type-trk_id_chg_des10 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des10 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt10.
        endif.

******
        if wa_payor_type-trk_id_chg_des11 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des11 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des11 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des11 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des11 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des11 = 'Residential' or
      wa_payor_type-trk_id_chg_des11 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des11 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt11.
        endif.


        if wa_payor_type-trk_id_chg_des12 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des12 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des12 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des12 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des12 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des12 = 'Residential' or
      wa_payor_type-trk_id_chg_des12 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des12 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt12.
        endif.


        if wa_payor_type-trk_id_chg_des13 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des13 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des13 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des13 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des13 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des13 = 'Residential' or
      wa_payor_type-trk_id_chg_des13 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des13 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt13.
        endif.


        if wa_payor_type-trk_id_chg_des14 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des14 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des14 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des14 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des14 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des14 = 'Residential' or
      wa_payor_type-trk_id_chg_des14 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des14 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt14.
        endif.


        if wa_payor_type-trk_id_chg_des15 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des15 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des15 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des15 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des15 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des15 = 'Residential' or
      wa_payor_type-trk_id_chg_des15 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des15 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt15.
        endif.


        if wa_payor_type-trk_id_chg_des16 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des16 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des16 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des16 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des16 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des16 = 'Residential' or
      wa_payor_type-trk_id_chg_des16 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des16 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt16.
        endif.


        if wa_payor_type-trk_id_chg_des17 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des17 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des17 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des17 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des17 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des17 = 'Residential' or
      wa_payor_type-trk_id_chg_des17 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des17 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt17.
        endif.


        if wa_payor_type-trk_id_chg_des18 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des18 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des18 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des18 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des18 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des18 = 'Residential' or
      wa_payor_type-trk_id_chg_des18 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des18 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt18.
        endif.


        if wa_payor_type-trk_id_chg_des19 = 'Fuel Surcharge' or
wa_payor_type-trk_id_chg_des19 = 'Weekly Service Chg' or
wa_payor_type-trk_id_chg_des19 = 'Weekday Delivery' or
wa_payor_type-trk_id_chg_des19 = 'Residential Delivery' or
wa_payor_type-trk_id_chg_des19 = 'Address Correction' or
wa_payor_type-trk_id_chg_des19 = 'Residential' or
wa_payor_type-trk_id_chg_des19 = 'DAS Comm' or
wa_payor_type-trk_id_chg_des19 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt19.
        endif.

        if wa_payor_type-trk_id_chg_des20 = 'Fuel Surcharge' or
wa_payor_type-trk_id_chg_des20 = 'Weekly Service Chg' or
wa_payor_type-trk_id_chg_des20 = 'Weekday Delivery' or
wa_payor_type-trk_id_chg_des20 = 'Residential Delivery' or
wa_payor_type-trk_id_chg_des20 = 'Address Correction' or
wa_payor_type-trk_id_chg_des20 = 'Residential' or
wa_payor_type-trk_id_chg_des20 = 'DAS Comm' or
wa_payor_type-trk_id_chg_des20 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt20.
        endif.

        if wa_payor_type-trk_id_chg_des21 = 'Fuel Surcharge' or
wa_payor_type-trk_id_chg_des21 = 'Weekly Service Chg' or
wa_payor_type-trk_id_chg_des21 = 'Weekday Delivery' or
wa_payor_type-trk_id_chg_des21 = 'Residential Delivery' or
wa_payor_type-trk_id_chg_des21 = 'Address Correction' or
wa_payor_type-trk_id_chg_des21 = 'Residential' or
wa_payor_type-trk_id_chg_des21 = 'DAS Comm' or
wa_payor_type-trk_id_chg_des21 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt21.
        endif.

        if wa_payor_type-trk_id_chg_des22 = 'Fuel Surcharge' or
      wa_payor_type-trk_id_chg_des22 = 'Weekly Service Chg' or
      wa_payor_type-trk_id_chg_des22 = 'Weekday Delivery' or
      wa_payor_type-trk_id_chg_des22 = 'Residential Delivery' or
      wa_payor_type-trk_id_chg_des22 = 'Address Correction' or
      wa_payor_type-trk_id_chg_des22 = 'Residential' or
      wa_payor_type-trk_id_chg_des22 = 'DAS Comm' or
      wa_payor_type-trk_id_chg_des22 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt22.
        endif.

        if wa_payor_type-trk_id_chg_des23 = 'Fuel Surcharge' or
wa_payor_type-trk_id_chg_des23 = 'Weekly Service Chg' or
wa_payor_type-trk_id_chg_des23 = 'Weekday Delivery' or
wa_payor_type-trk_id_chg_des23 = 'Residential Delivery' or
wa_payor_type-trk_id_chg_des23 = 'Address Correction' or
wa_payor_type-trk_id_chg_des23 = 'Residential' or
wa_payor_type-trk_id_chg_des23 = 'DAS Comm' or
wa_payor_type-trk_id_chg_des23 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt23.
        endif.

        if wa_payor_type-trk_id_chg_des24 = 'Fuel Surcharge' or
wa_payor_type-trk_id_chg_des24 = 'Weekly Service Chg' or
wa_payor_type-trk_id_chg_des24 = 'Weekday Delivery' or
wa_payor_type-trk_id_chg_des24 = 'Residential Delivery' or
wa_payor_type-trk_id_chg_des24 = 'Address Correction' or
wa_payor_type-trk_id_chg_des24 = 'Residential' or
wa_payor_type-trk_id_chg_des24 = 'DAS Comm' or
wa_payor_type-trk_id_chg_des24 = 'DAS Extended Comm'.

          wa_final_payor_type-handling_charge =  wa_final_payor_type-handling_charge + wa_payor_type-trk_id_chg_amt24.
        endif.





        if wa_payor_type-trk_id_chg_des = 'Discount' or wa_payor_type-trk_id_chg_des = 'Performance Pricing'.
          wa_final_payor_type-discount =  wa_payor_type-trk_id_chg_amt.
        endif.

        if wa_payor_type-trk_id_chg_des1 = 'Discount' or wa_payor_type-trk_id_chg_des1 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt1.
        endif.

        if wa_payor_type-trk_id_chg_des2 = 'Discount' or wa_payor_type-trk_id_chg_des2 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt2.
        endif.

        if wa_payor_type-trk_id_chg_des3 = 'Discount' or wa_payor_type-trk_id_chg_des3 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt3.
        endif.

        if wa_payor_type-trk_id_chg_des4 = 'Discount' or wa_payor_type-trk_id_chg_des4 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt4.
        endif.

        if wa_payor_type-trk_id_chg_des5 = 'Discount' or wa_payor_type-trk_id_chg_des5 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt5.
        endif.

        if wa_payor_type-trk_id_chg_des6 = 'Discount' or wa_payor_type-trk_id_chg_des6 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt6.
        endif.

        if wa_payor_type-trk_id_chg_des7 = 'Discount' or wa_payor_type-trk_id_chg_des7 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt7.
        endif.

        if wa_payor_type-trk_id_chg_des8 = 'Discount' or wa_payor_type-trk_id_chg_des8 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt8.
        endif.

        if wa_payor_type-trk_id_chg_des9 = 'Discount' or wa_payor_type-trk_id_chg_des9 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt9.
        endif.


        if wa_payor_type-trk_id_chg_des10 = 'Discount' or wa_payor_type-trk_id_chg_des10 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt10.
        endif.

        if wa_payor_type-trk_id_chg_des11 = 'Discount' or wa_payor_type-trk_id_chg_des11 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt11.
        endif.

        if wa_payor_type-trk_id_chg_des12 = 'Discount' or wa_payor_type-trk_id_chg_des12 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt12.
        endif.

        if wa_payor_type-trk_id_chg_des13 = 'Discount' or wa_payor_type-trk_id_chg_des13 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt13.
        endif.


        if wa_payor_type-trk_id_chg_des14 = 'Discount' or wa_payor_type-trk_id_chg_des14 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt14.
        endif.

        if wa_payor_type-trk_id_chg_des15 = 'Discount' or wa_payor_type-trk_id_chg_des15 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt15.
        endif.


        if wa_payor_type-trk_id_chg_des16 = 'Discount' or wa_payor_type-trk_id_chg_des16 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt16.
        endif.

        if wa_payor_type-trk_id_chg_des17 = 'Discount' or wa_payor_type-trk_id_chg_des17 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt17.
        endif.

        if wa_payor_type-trk_id_chg_des18 = 'Discount' or wa_payor_type-trk_id_chg_des18 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt18.
        endif.

        if wa_payor_type-trk_id_chg_des19 = 'Discount' or wa_payor_type-trk_id_chg_des19 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt19.
        endif.

        if wa_payor_type-trk_id_chg_des20 = 'Discount' or wa_payor_type-trk_id_chg_des20 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt20.
        endif.

        if wa_payor_type-trk_id_chg_des21 = 'Discount' or wa_payor_type-trk_id_chg_des21 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt21.
        endif.

        if wa_payor_type-trk_id_chg_des22 = 'Discount' or wa_payor_type-trk_id_chg_des22 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt22.
        endif.

        if wa_payor_type-trk_id_chg_des23 = 'Discount' or wa_payor_type-trk_id_chg_des23 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt23.
        endif.

        if wa_payor_type-trk_id_chg_des24 = 'Discount' or wa_payor_type-trk_id_chg_des24 = 'Performance Pricing'.
          wa_final_payor_type-discount =   wa_final_payor_type-discount + wa_payor_type-trk_id_chg_amt24.
        endif.


        wa_final_payor_type-net_charge = wa_payor_type-net_chrg_amnt.

        append wa_final_payor_type to it_final_payor_type.
        clear :wa_final_payor_type,wa_payor_type.
      endloop.


      sort it_final_payor_type by payor.
      clear wa_final_payor_type.


      loop at it_final_payor_type into wa_final_payor_type.



*        if not wa_final_payor_type is initial  .
*          if wa_final_payor_type = 'Recipient' OR wa_final_payor_type = 'Shipper' OR wa_final_payor_type =  'Third Party'.
          at end of payor.
            sum.     " calculates sub totals on service types

            wa_output_payor_type-payor = wa_final_payor_type-payor.
            wa_output_payor_type-packages =  wa_final_payor_type-packages.
            wa_output_payor_type-rated_weight = wa_final_payor_type-rated_weight.
            wa_output_payor_type-transport_charge = wa_final_payor_type-transport_charge.
            wa_output_payor_type-handling_charge =  wa_final_payor_type-handling_charge.
            wa_output_payor_type-discount =  wa_final_payor_type-discount.
            wa_output_payor_type-net_charge = wa_final_payor_type-net_charge.
* calculate the grand totals

            v_packages = v_packages + wa_output_payor_type-packages.
            v_rated_weight = v_rated_weight + wa_output_payor_type-rated_weight.
            v_trans_charg = v_trans_charg + wa_output_payor_type-transport_charge.
            v_hand_charg = v_hand_charg + wa_output_payor_type-handling_charge.
            v_discount = v_discount + wa_output_payor_type-discount.
            v_netcharge = v_netcharge +  wa_output_payor_type-net_charge .
            append wa_output_payor_type to it_output_payor_type.
            clear wa_output_payor_type.

          endat.


*
*        ELSE.
*
*          at end of payor.
*            sum.     " calculates sub totals on service types
*
*            wa_output_payor_type-payor = 'Inv misc'." wa_final_payor_type-payor.
*            wa_output_payor_type-packages =  wa_final_payor_type-packages.
*            wa_output_payor_type-rated_weight = wa_final_payor_type-rated_weight.
*            wa_output_payor_type-transport_charge = wa_final_payor_type-transport_charge.
*            wa_output_payor_type-handling_charge =  wa_final_payor_type-handling_charge.
*            wa_output_payor_type-discount =  wa_final_payor_type-discount.
*            wa_output_payor_type-net_charge = wa_final_payor_type-net_charge.
** calculate the grand totals
*
*            v_packages = v_packages + wa_output_payor_type-packages.
*            v_rated_weight = v_rated_weight + wa_output_payor_type-rated_weight.
*            v_trans_charg = v_trans_charg + wa_output_payor_type-transport_charge.
*            v_hand_charg = v_hand_charg + wa_output_payor_type-handling_charge.
*            v_discount = v_discount + wa_output_payor_type-discount.
*            v_netcharge = v_netcharge +  wa_output_payor_type-net_charge .
*            append wa_output_payor_type to it_output_payor_type.
*            clear wa_output_payor_type.
*          endat.
*
*       endif.



*        ENDIF.


        clear wa_final_payor_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_payor_type.

*    wa_output_serv_type-service_type = v_service.
      wa_output_payor_type-packages = v_packages.
      wa_output_payor_type-rated_weight = v_rated_weight.

      wa_output_payor_type-transport_charge = v_trans_charg.
      wa_output_payor_type-handling_charge = v_hand_charg .
      wa_output_payor_type-discount = v_discount .
      wa_output_payor_type-net_charge = v_netcharge.

      wa_output_payor_type-line_color      = 'C500'.
      append wa_output_payor_type to it_output_payor_type.
      clear wa_output_payor_type.



      wa_fieldcat_payor_type-col_pos      = 1.
      wa_fieldcat_payor_type-fieldname    = 'PAYOR'.
      wa_fieldcat_payor_type-coltext    = 'Payment Method'.
      wa_fieldcat_payor_type-tabname      = 'IT_OUPUT_PAYOR_TYPE'.
      append wa_fieldcat_payor_type to it_fieldcat_payor_type.
      clear wa_fieldcat_payor_type.

      wa_fieldcat_payor_type-col_pos      = 2.
      wa_fieldcat_payor_type-fieldname    = 'PACKAGES'.
      wa_fieldcat_payor_type-coltext    = 'Total Packages'.
      wa_fieldcat_payor_type-tabname      = 'IT_OUPUT_PAYOR_TYPE'.
      append wa_fieldcat_payor_type to it_fieldcat_payor_type.
      clear wa_fieldcat_payor_type.

      wa_fieldcat_payor_type-col_pos      = 3.
      wa_fieldcat_payor_type-fieldname    = 'RATED_WEIGHT'.
      wa_fieldcat_payor_type-coltext    = 'Rated Weight'.
      wa_fieldcat_payor_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_payor_type to it_fieldcat_payor_type.
      clear wa_fieldcat_payor_type.

      wa_fieldcat_payor_type-col_pos      = 4.
      wa_fieldcat_payor_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_payor_type-coltext    = 'Transport Charge'.
      wa_fieldcat_payor_type-tabname      = 'IT_OUPUT_PAYOR_TYPE'.
      append wa_fieldcat_payor_type to it_fieldcat_payor_type.
      clear wa_fieldcat_payor_type.

      wa_fieldcat_payor_type-col_pos      = 5.
      wa_fieldcat_payor_type-fieldname    = 'HANDLING_CHARGE'.
      wa_fieldcat_payor_type-coltext    = 'Handling Charge'.
      wa_fieldcat_payor_type-tabname      = 'IT_OUPUT_PAYOR_TYPE'.
      append wa_fieldcat_payor_type to it_fieldcat_payor_type.
      clear wa_fieldcat_payor_type.

      wa_fieldcat_payor_type-col_pos      = 6.
      wa_fieldcat_payor_type-fieldname    = 'DISCOUNT'.
      wa_fieldcat_payor_type-coltext    = 'Discount'.
      wa_fieldcat_payor_type-tabname      = 'IT_OUPUT_PAYOR_TYPE'.
      append wa_fieldcat_payor_type to it_fieldcat_payor_type.
      clear wa_fieldcat_payor_type.

      wa_fieldcat_payor_type-col_pos      = 7.
      wa_fieldcat_payor_type-fieldname    = 'NET_CHARGE'.
      wa_fieldcat_payor_type-coltext    = 'Net Charge'.
      wa_fieldcat_payor_type-tabname      = 'IT_OUPUT_PAYOR_TYPE'.
      append wa_fieldcat_payor_type to it_fieldcat_payor_type.
      clear wa_fieldcat_payor_type.



      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUPUT_PAYOR_TYPE'
*          is_layout                     = gs_layout
          is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
        changing
          it_outtab                     = it_output_payor_type
          it_fieldcatalog               = it_fieldcat_payor_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.



    endif.
  endif.
*  if ups = 'X'.
  if wa_export_output-carrier = 'UPS'.

    sort it_final_ups_payor by payor.
    clear wa_final_ups_payor.

    refresh : it_output_ups_payor_type,it_fieldcat_payor_ups_type.

    loop at it_final_ups_payor into wa_final_ups_payor.

      at end of payor.
        sum.     " calculates sub totals on zone types
        wa_output_ups_payor_type-payor = wa_final_ups_payor-payor.
        wa_output_ups_payor_type-packages = wa_final_ups_payor-packages.
        wa_output_ups_payor_type-rated_weight = wa_final_ups_payor-rated_weight.
        wa_output_ups_payor_type-transport_charge = wa_final_ups_payor-transport_charge.
        wa_output_ups_payor_type-handling_charge = wa_final_ups_payor-handling_charge.
        wa_output_ups_payor_type-discount = wa_final_ups_payor-discount.
        wa_output_ups_payor_type-net_charge = wa_final_ups_payor-net_charge.
*
        v_packages = v_packages + wa_final_ups_payor-packages.
        v_rated_weight = v_rated_weight + wa_final_ups_payor-rated_weight.

        v_trans_charg = v_trans_charg + wa_final_ups_payor-transport_charge.

        v_hand_charg = v_hand_charg + wa_final_ups_payor-handling_charge.


        v_discount = v_discount + wa_final_ups_payor-discount.

        v_netcharge = v_netcharge +  wa_final_ups_payor-net_charge .

        append wa_output_ups_payor_type to it_output_ups_payor_type.
        clear :wa_output_ups_payor_type.

      endat.


      clear wa_final_ups_payor.


    endloop.



* add the grand totals as last record
    clear : wa_output_ups_payor_type.

*    wa_output_serv_type-service_type = v_service.
    wa_output_ups_payor_type-packages = v_packages.
    wa_output_ups_payor_type-rated_weight = v_rated_weight.

    wa_output_ups_payor_type-transport_charge = v_trans_charg.
    wa_output_ups_payor_type-handling_charge = v_hand_charg .
    wa_output_ups_payor_type-discount = v_discount .
    wa_output_ups_payor_type-net_charge = v_netcharge.

    wa_output_ups_payor_type-line_color      = 'C500'.
    append wa_output_ups_payor_type to it_output_ups_payor_type.
    clear wa_output_ups_payor_type.



    wa_fieldcat_payor_ups_type-col_pos      = 1.
    wa_fieldcat_payor_ups_type-fieldname    = 'PAYOR'.
    wa_fieldcat_payor_ups_type-coltext    = 'Payment Method'.
    wa_fieldcat_payor_ups_type-tabname      = 'IT_OUPUT_UPS_payor_TYPE'.
    append wa_fieldcat_payor_ups_type to it_fieldcat_payor_ups_type.
    clear wa_fieldcat_payor_ups_type.

    wa_fieldcat_payor_ups_type-col_pos      = 2.
    wa_fieldcat_payor_ups_type-fieldname    = 'PACKAGES'.
    wa_fieldcat_payor_ups_type-coltext    = 'Total Packages'.
    wa_fieldcat_payor_ups_type-tabname      = 'IT_OUPUT_UPS_payor_TYPE'.
    append wa_fieldcat_payor_ups_type to it_fieldcat_payor_ups_type.
    clear wa_fieldcat_payor_ups_type.

    wa_fieldcat_payor_ups_type-col_pos      = 3.
    wa_fieldcat_payor_ups_type-fieldname    = 'RATED_WEIGHT'.
    wa_fieldcat_payor_ups_type-coltext    = 'Rated Weight'.
    wa_fieldcat_payor_ups_type-tabname      = 'IT_OUPUT_UPS_payor_TYPE'.
    append wa_fieldcat_payor_ups_type to it_fieldcat_payor_ups_type.
    clear wa_fieldcat_payor_ups_type.

    wa_fieldcat_payor_ups_type-col_pos      = 4.
    wa_fieldcat_payor_ups_type-fieldname    = 'TRANSPORT_CHARGE'.
    wa_fieldcat_payor_ups_type-coltext    = 'Transport Charge'.
    wa_fieldcat_payor_ups_type-tabname      = 'IT_OUPUT_UPS_payor_TYPE'.
    append wa_fieldcat_payor_ups_type to it_fieldcat_payor_ups_type.
    clear wa_fieldcat_payor_ups_type.

    wa_fieldcat_payor_ups_type-col_pos      = 5.
    wa_fieldcat_payor_ups_type-fieldname    = 'HANDLING_CHARGE'.
    wa_fieldcat_payor_ups_type-coltext    = 'Handling Charge'.
    wa_fieldcat_payor_ups_type-tabname      = 'IT_OUPUT_UPS_payor_TYPE'.
    append wa_fieldcat_payor_ups_type to it_fieldcat_payor_ups_type.
    clear wa_fieldcat_payor_ups_type.

    wa_fieldcat_payor_ups_type-col_pos      = 6.
    wa_fieldcat_payor_ups_type-fieldname    = 'DISCOUNT'.
    wa_fieldcat_payor_ups_type-coltext    = 'Discount'.
    wa_fieldcat_payor_ups_type-tabname      = 'IT_OUPUT_UPS_payor_TYPE'.
    append wa_fieldcat_payor_ups_type to it_fieldcat_payor_ups_type.
    clear wa_fieldcat_payor_ups_type.

    wa_fieldcat_payor_ups_type-col_pos      = 7.
    wa_fieldcat_payor_ups_type-fieldname    = 'NET_CHARGE'.
    wa_fieldcat_payor_ups_type-coltext    = 'Net Charge'.
    wa_fieldcat_payor_ups_type-tabname      = 'IT_OUPUT_UPS_payor_TYPE'.
    append wa_fieldcat_payor_ups_type to it_fieldcat_payor_ups_type.
    clear wa_fieldcat_payor_ups_type.



    gs_layout-cwidth_opt = 'X'.
    gs_layout-info_fname = 'LINE_COLOR'.
    gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
    call method go_grid->set_table_for_first_display
      exporting
        i_structure_name              = 'IT_OUTPUT_UPS_payor_TYPE'
*        is_layout                     = gs_layout
        is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
      changing
        it_outtab                     = it_output_ups_payor_type
        it_fieldcatalog               = it_fieldcat_payor_ups_type
*    IT_SORT                       =
    exceptions
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      others                        = 4  .
    if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.
*    DATA: TOTAL_AMNT_UPS     TYPE   NETWR,
*          TOTAL_SAVINGS_UPS  TYPE   NETWR,
*          TOTAL_PKGS_UPS     TYPE   I,
*          TOTAL_WEIGHT_UPS   TYPE   BRGEW_AP,
*          AVG_PKG_CST_UPS    TYPE   NETWR,
*          AVG_PND_CST_UPS    TYPE   NETWR.
*
*
*    REFRESH: IT_FINAL_PAYMENT_METHOD_UPS, IT_OUTPUT_PAYMENT_METHOD_UPS, IT_FIELDCAT_PAYMENT_METHOD_UPS.
*    CLEAR :  WA_FINAL_PAYMENT_METHOD_UPS, WA_OUTPUT_PAYMENT_METHOD_UPS, WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*
*
**frm_date = to_date - 7.
*
*    SELECT  PAYMENT_METHOD
*            ACTUALWEIGHT
*            TOTAL_INC_CREDIT
*            TOTAL_BILL_CHARG
*            FROM ZPWEFA_INVC_UPS
*            INTO CORRESPONDING FIELDS OF TABLE IT_FINAL_PAYMENT_METHOD_UPS
*            WHERE  INVOICE_DATE EQ WEEK_DT AND CARRIER EQ ZCARRIERTYPE.
*
*
*    IF SY-SUBRC = 0.
*
*      SORT IT_FINAL_PAYMENT_METHOD_UPS BY PAYMENT_METHOD.
*
*      CLEAR WA_FINAL_PAYMENT_METHOD_UPS.
*
*      LOOP AT IT_FINAL_PAYMENT_METHOD_UPS INTO WA_FINAL_PAYMENT_METHOD_UPS.
*
*        WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_SHIPMENTS = 1.
*
*        MODIFY IT_FINAL_PAYMENT_METHOD_UPS FROM WA_FINAL_PAYMENT_METHOD_UPS TRANSPORTING TOTAL_SHIPMENTS.
*
*        AT END OF PAYMENT_METHOD.
*
*          SUM.                                                       " calculates sub totals on service types
*
*          WA_OUTPUT_PAYMENT_METHOD_UPS-V_TOTAL_WEIGHT    =   WA_FINAL_PAYMENT_METHOD_UPS-ACTUALWEIGHT.
*          WA_OUTPUT_PAYMENT_METHOD_UPS-V_DUE             =   WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_BILL_CHARG.
*          WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_SHIPMENTS    =   WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_SHIPMENTS.
*
*          MODIFY IT_FINAL_PAYMENT_METHOD_UPS FROM WA_FINAL_PAYMENT_METHOD_UPS TRANSPORTING V_TOTAL_WEIGHT
*                                                     V_TOTAL.
*
*          WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_SHIPMENTS    =   WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_SHIPMENTS.
*          WA_OUTPUT_PAYMENT_METHOD_UPS-ZPACKAGES          =   WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_SHIPMENTS.
*          WA_OUTPUT_PAYMENT_METHOD_UPS-PAYMENT_METHOD     =   WA_FINAL_PAYMENT_METHOD_UPS-PAYMENT_METHOD.
*          WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_LETERS       =   0.
*          WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_INC_CREDIT            =   WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_INC_CREDIT.
*          WA_OUTPUT_PAYMENT_METHOD_UPS-PKG_AVG_COST       = ( WA_OUTPUT_PAYMENT_METHOD_UPS-V_DUE / WA_OUTPUT_PAYMENT_METHOD_UPS-ZPACKAGES ).
*          WA_OUTPUT_PAYMENT_METHOD_UPS-WEIGHT_AVG_COST    =   WA_OUTPUT_PAYMENT_METHOD_UPS-V_DUE / WA_OUTPUT_PAYMENT_METHOD_UPS-V_TOTAL_WEIGHT.
*
*
** calculate the grand totals
*
*          WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_BILL_CHARG   =   WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_BILL_CHARG + WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_BILL_CHARG.
*          TOTAL_AMNT_UPS         =   WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_BILL_CHARG + TOTAL_AMNT_UPS.
*          TOTAL_SAVINGS_UPS      =   TOTAL_SAVINGS_UPS + WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_INC_CREDIT.
*          TOTAL_PKGS_UPS         =   TOTAL_PKGS_UPS + WA_FINAL_PAYMENT_METHOD_UPS-TOTAL_SHIPMENTS.
*          TOTAL_WEIGHT_UPS       =   TOTAL_WEIGHT_UPS + WA_OUTPUT_PAYMENT_METHOD_UPS-V_TOTAL_WEIGHT.
*
*          APPEND WA_OUTPUT_PAYMENT_METHOD_UPS TO IT_OUTPUT_PAYMENT_METHOD_UPS.
*          CLEAR WA_OUTPUT_PAYMENT_METHOD_UPS.
*
*        ENDAT.
*
*        CLEAR WA_FINAL_PAYMENT_METHOD_UPS.
*
*      ENDLOOP.
** calculate the Avg.costs and percentage
*
*      LOOP AT IT_OUTPUT_PAYMENT_METHOD_UPS INTO WA_OUTPUT_PAYMENT_METHOD_UPS .
*
*        WA_OUTPUT_PAYMENT_METHOD_UPS-AMOUNT_PERCENT = WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_BILL_CHARG * 100 .
*        WA_OUTPUT_PAYMENT_METHOD_UPS-AMOUNT_PERCENT = WA_OUTPUT_PAYMENT_METHOD_UPS-AMOUNT_PERCENT / TOTAL_AMNT_UPS.
*
*        MODIFY IT_OUTPUT_PAYMENT_METHOD_UPS FROM WA_OUTPUT_PAYMENT_METHOD_UPS TRANSPORTING AMOUNT_PERCENT.
*
*        AT LAST.
*
*          AVG_PKG_CST_UPS = TOTAL_AMNT_UPS / TOTAL_PKGS_UPS.
*          AVG_PND_CST_UPS = TOTAL_AMNT_UPS / TOTAL_WEIGHT_UPS.
*
*        ENDAT.
*
*        CLEAR WA_OUTPUT_PAYMENT_METHOD_UPS.
*
*      ENDLOOP.
** add the grand totals as last record
*
*      WA_OUTPUT_PAYMENT_METHOD_UPS-PAYMENT_METHOD  =  'Total'.
*      WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_SHIPMENTS  =  TOTAL_PKGS_UPS.
*      WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_LETERS     =  0.
*      WA_OUTPUT_PAYMENT_METHOD_UPS-V_TOTAL_WEIGHT   =  TOTAL_WEIGHT_UPS.
*      WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_INC_CREDIT   =  TOTAL_SAVINGS_UPS .
*      WA_OUTPUT_PAYMENT_METHOD_UPS-TOTAL_BILL_CHARG          =  TOTAL_AMNT_UPS.
*      WA_OUTPUT_PAYMENT_METHOD_UPS-AMOUNT_PERCENT   =  ' '.
*      WA_OUTPUT_PAYMENT_METHOD_UPS-PKG_AVG_COST     =  AVG_PKG_CST_UPS.
*      WA_OUTPUT_PAYMENT_METHOD_UPS-WEIGHT_AVG_COST  =  AVG_PND_CST_UPS.
*      WA_OUTPUT_PAYMENT_METHOD_UPS-LINE_COLOR       =  'C500'.
*      APPEND WA_OUTPUT_PAYMENT_METHOD_UPS TO IT_OUTPUT_PAYMENT_METHOD_UPS.
*      CLEAR WA_OUTPUT_PAYMENT_METHOD_UPS.
*
*
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 1.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'PAYMENT_METHOD'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'Payment Method'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 2.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'TOTAL_SHIPMENTS'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'Total Packages'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 3.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'TOTAL_LETERS'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'Total Letters only'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 4.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'V_TOTAL_WEIGHT'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'Total Weight'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 5.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'TOTAL_INC_CREDIT'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'TotalSavings($)'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 6.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'TOTAL_BILL_CHARG'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'TotalNetCost($)'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 7.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'AMOUNT_PERCENT_UPS'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'TotalNetCost%'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 8.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'PKG_AVG_COST'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'Avg.Cost per Package($)'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD_UPS.
*
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COL_POS      = 9.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-FIELDNAME    = 'WEIGHT_AVG_COST'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-COLTEXT      = 'Avg.Cost per Pound($)'.
*      WA_FIELDCAT_PAYMENT_METHOD_UPS-TABNAME      = 'IT_OUTPUT_PAYMENT_METHOD_UPS'.
*      APPEND WA_FIELDCAT_PAYMENT_METHOD_UPS TO IT_FIELDCAT_PAYMENT_METHOD_UPS.
*      CLEAR WA_FIELDCAT_PAYMENT_METHOD.
*
*
*      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*      GS_LAYOUT-GRID_TITLE = SPACE.
*
*
*
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*        EXPORTING
*          I_STRUCTURE_NAME              = 'IT_OUTPUT_PAYMENT_METHOD_UPS'
**    IS_VARIANT                    =
**    I_SAVE                        =
**    I_DEFAULT                     = 'X'
*          IS_LAYOUT                     = GS_LAYOUT
*        CHANGING
*          IT_OUTTAB                     = IT_OUTPUT_PAYMENT_METHOD_UPS
*          IT_FIELDCATALOG               = IT_FIELDCAT_PAYMENT_METHOD_UPS
**    IT_SORT                       =
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4  .
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*
*
*
*    ENDIF.
  endif.

endform.                    "PAYMENTMETHOD


*****************************************END OF PAYMENT METHOD***********

*************************BEGIN OF SHIPMENT DATE


*&---------------------------------------------------------------------*
*&      Form  shipday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form shipday.

  clear : approve.


  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.

  if wa_export_output-carrier = 'FEDEX'.


    data :v_shipdate type /PWEAVER/EFA_FED-shipment_date,
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt.

    clear : v_shipdate,v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_shipdate_type,it_final_shipdate_type,it_fieldcat_shipdate_type,it_output_shipdate_type.
    clear : it_shipdate_type,wa_shipdate_type,wa_final_shipdate_type,wa_fieldcat_shipdate_type.

    select *
              from /PWEAVER/EFA_FED
              into corresponding fields of table it_shipdate_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.

    if sy-subrc = 0.
      clear : wa_shipdate_type.
      loop at it_shipdate_type into wa_shipdate_type.

        wa_final_shipdate_type-shipdate  = wa_shipdate_type-shipment_date.
        wa_final_shipdate_type-packages = wa_shipdate_type-num_pieces.
        wa_final_shipdate_type-rated_weight = wa_shipdate_type-rated_wgt_amt.
*       IF NOT WA_SERV_TYPE-TRAN_CHARG_AMNT IS INITIAL.
        wa_final_shipdate_type-transport_charge = wa_shipdate_type-tran_charg_amnt.
*        ENDIF.

****Handling charges
        if wa_shipdate_type-trk_id_chg_des = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des = 'Residential' or
      wa_shipdate_type-trk_id_chg_des = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_shipdate_type-trk_id_chg_amt.
        endif.

        if wa_shipdate_type-trk_id_chg_des1 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des1 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des1 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des1 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des1 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des1 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des1 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des1 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt1.
        endif.

        if wa_shipdate_type-trk_id_chg_des2 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des2 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des2 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des2 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des2 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des2 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des2 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des2 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt2.
        endif.

        if wa_shipdate_type-trk_id_chg_des3 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des3 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des3 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des3 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des3 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des3 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des3 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des3 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt3.
        endif.

        if wa_shipdate_type-trk_id_chg_des4 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des4 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des4 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des4 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des4 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des4 = 'Residential' or
      wa_serv_type-trk_id_chg_des4 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des4 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt4.
        endif.

        if wa_shipdate_type-trk_id_chg_des5 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des5 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des5 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des5 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des5 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des5 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des5 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des5 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt5.
        endif.

        if wa_shipdate_type-trk_id_chg_des6 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des6 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des6 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des6 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des6 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des6 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des6 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des6 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt6.
        endif.

        if wa_shipdate_type-trk_id_chg_des7 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des7 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des7 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des7 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des7 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des7 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des7 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des7 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt7.
        endif.

        if wa_shipdate_type-trk_id_chg_des8 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des8 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des8 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des8 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des8 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des8 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des8 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des8 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt8.
        endif.


        if wa_shipdate_type-trk_id_chg_des9 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des9 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des9 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des9 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des9 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des9 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des9 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des9 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt9.
        endif.


        if wa_shipdate_type-trk_id_chg_des10 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des10 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des10 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des10 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des10 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des10 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des10 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des10 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt10.
        endif.

******
        if wa_shipdate_type-trk_id_chg_des11 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des11 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des11 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des11 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des11 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des11 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des11 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des11 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt11.
        endif.


        if wa_shipdate_type-trk_id_chg_des12 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des12 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des12 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des12 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des12 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des12 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des12 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des12 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt12.
        endif.


        if wa_shipdate_type-trk_id_chg_des13 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des13 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des13 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des13 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des13 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des13 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des13 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des13 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt13.
        endif.


        if wa_shipdate_type-trk_id_chg_des14 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des14 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des14 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des14 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des14 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des14 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des14 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des14 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt14.
        endif.


        if wa_shipdate_type-trk_id_chg_des15 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des15 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des15 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des15 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des15 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des15 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des15 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des15 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt15.
        endif.


        if wa_shipdate_type-trk_id_chg_des16 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des16 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des16 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des16 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des16 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des16 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des16 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des16 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt16.
        endif.


        if wa_shipdate_type-trk_id_chg_des17 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des17 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des17 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des17 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des17 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des17 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des17 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des17 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt17.
        endif.


        if wa_shipdate_type-trk_id_chg_des18 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des18 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des18 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des18 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des18 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des18 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des18 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des18 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt18.
        endif.


        if wa_shipdate_type-trk_id_chg_des19 = 'Fuel Surcharge' or
wa_shipdate_type-trk_id_chg_des19 = 'Weekly Service Chg' or
wa_shipdate_type-trk_id_chg_des19 = 'Weekday Delivery' or
wa_shipdate_type-trk_id_chg_des19 = 'Residential Delivery' or
wa_shipdate_type-trk_id_chg_des19 = 'Address Correction' or
wa_shipdate_type-trk_id_chg_des19 = 'Residential' or
wa_shipdate_type-trk_id_chg_des19 = 'DAS Comm' or
wa_shipdate_type-trk_id_chg_des19 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt19.
        endif.

        if wa_shipdate_type-trk_id_chg_des20 = 'Fuel Surcharge' or
wa_shipdate_type-trk_id_chg_des20 = 'Weekly Service Chg' or
wa_shipdate_type-trk_id_chg_des20 = 'Weekday Delivery' or
wa_shipdate_type-trk_id_chg_des20 = 'Residential Delivery' or
wa_shipdate_type-trk_id_chg_des20 = 'Address Correction' or
wa_shipdate_type-trk_id_chg_des20 = 'Residential' or
wa_shipdate_type-trk_id_chg_des20 = 'DAS Comm' or
wa_shipdate_type-trk_id_chg_des20 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt20.
        endif.

        if wa_shipdate_type-trk_id_chg_des21 = 'Fuel Surcharge' or
wa_shipdate_type-trk_id_chg_des21 = 'Weekly Service Chg' or
wa_shipdate_type-trk_id_chg_des21 = 'Weekday Delivery' or
wa_shipdate_type-trk_id_chg_des21 = 'Residential Delivery' or
wa_shipdate_type-trk_id_chg_des21 = 'Address Correction' or
wa_shipdate_type-trk_id_chg_des21 = 'Residential' or
wa_shipdate_type-trk_id_chg_des21 = 'DAS Comm' or
wa_shipdate_type-trk_id_chg_des21 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt21.
        endif.

        if wa_shipdate_type-trk_id_chg_des22 = 'Fuel Surcharge' or
      wa_shipdate_type-trk_id_chg_des22 = 'Weekly Service Chg' or
      wa_shipdate_type-trk_id_chg_des22 = 'Weekday Delivery' or
      wa_shipdate_type-trk_id_chg_des22 = 'Residential Delivery' or
      wa_shipdate_type-trk_id_chg_des22 = 'Address Correction' or
      wa_shipdate_type-trk_id_chg_des22 = 'Residential' or
      wa_shipdate_type-trk_id_chg_des22 = 'DAS Comm' or
      wa_shipdate_type-trk_id_chg_des22 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt22.
        endif.

        if wa_shipdate_type-trk_id_chg_des23 = 'Fuel Surcharge' or
wa_shipdate_type-trk_id_chg_des23 = 'Weekly Service Chg' or
wa_shipdate_type-trk_id_chg_des23 = 'Weekday Delivery' or
wa_shipdate_type-trk_id_chg_des23 = 'Residential Delivery' or
wa_shipdate_type-trk_id_chg_des23 = 'Address Correction' or
wa_shipdate_type-trk_id_chg_des23 = 'Residential' or
wa_shipdate_type-trk_id_chg_des23 = 'DAS Comm' or
wa_shipdate_type-trk_id_chg_des23 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt23.
        endif.

        if wa_shipdate_type-trk_id_chg_des24 = 'Fuel Surcharge' or
wa_shipdate_type-trk_id_chg_des24 = 'Weekly Service Chg' or
wa_shipdate_type-trk_id_chg_des24 = 'Weekday Delivery' or
wa_shipdate_type-trk_id_chg_des24 = 'Residential Delivery' or
wa_shipdate_type-trk_id_chg_des24 = 'Address Correction' or
wa_shipdate_type-trk_id_chg_des24 = 'Residential' or
wa_shipdate_type-trk_id_chg_des24 = 'DAS Comm' or
wa_shipdate_type-trk_id_chg_des24 = 'DAS Extended Comm'.

          wa_final_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge + wa_shipdate_type-trk_id_chg_amt24.
        endif.





        if wa_shipdate_type-trk_id_chg_des = 'Discount' or wa_shipdate_type-trk_id_chg_des = 'Performance Pricing'.
          wa_final_shipdate_type-discount =  wa_shipdate_type-trk_id_chg_amt.
        endif.

        if wa_shipdate_type-trk_id_chg_des1 = 'Discount' or wa_shipdate_type-trk_id_chg_des1 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt1.
        endif.

        if wa_shipdate_type-trk_id_chg_des2 = 'Discount' or  wa_shipdate_type-trk_id_chg_des2 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt2.
        endif.

        if wa_shipdate_type-trk_id_chg_des3 = 'Discount' or  wa_shipdate_type-trk_id_chg_des3 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt3.
        endif.

        if wa_shipdate_type-trk_id_chg_des4 = 'Discount' or  wa_shipdate_type-trk_id_chg_des4 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt4.
        endif.

        if wa_shipdate_type-trk_id_chg_des5 = 'Discount'or  wa_shipdate_type-trk_id_chg_des5 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt5.
        endif.

        if wa_shipdate_type-trk_id_chg_des6 = 'Discount' or  wa_shipdate_type-trk_id_chg_des6 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt6.
        endif.

        if wa_shipdate_type-trk_id_chg_des7 = 'Discount' or  wa_shipdate_type-trk_id_chg_des7 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt7.
        endif.

        if wa_shipdate_type-trk_id_chg_des8 = 'Discount' or  wa_shipdate_type-trk_id_chg_des8 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt8.
        endif.

        if wa_shipdate_type-trk_id_chg_des9 = 'Discount' or  wa_shipdate_type-trk_id_chg_des9 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt9.
        endif.


        if wa_shipdate_type-trk_id_chg_des10 = 'Discount' or  wa_shipdate_type-trk_id_chg_des10 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt10.
        endif.

        if wa_shipdate_type-trk_id_chg_des11 = 'Discount' or  wa_shipdate_type-trk_id_chg_des11 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt11.
        endif.

        if wa_shipdate_type-trk_id_chg_des12 = 'Discount' or  wa_shipdate_type-trk_id_chg_des12 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt12.
        endif.

        if wa_shipdate_type-trk_id_chg_des13 = 'Discount' or  wa_shipdate_type-trk_id_chg_des13 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt13.
        endif.


        if wa_shipdate_type-trk_id_chg_des14 = 'Discount' or  wa_shipdate_type-trk_id_chg_des14 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt14.
        endif.

        if wa_shipdate_type-trk_id_chg_des15 = 'Discount' or  wa_shipdate_type-trk_id_chg_des15 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt15.
        endif.


        if wa_shipdate_type-trk_id_chg_des16 = 'Discount' or  wa_shipdate_type-trk_id_chg_des16 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt16.
        endif.

        if wa_shipdate_type-trk_id_chg_des17 = 'Discount' or  wa_shipdate_type-trk_id_chg_des17 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt17.
        endif.

        if wa_shipdate_type-trk_id_chg_des18 = 'Discount' or  wa_shipdate_type-trk_id_chg_des18 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt18.
        endif.

        if wa_shipdate_type-trk_id_chg_des19 = 'Discount' or  wa_shipdate_type-trk_id_chg_des19 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt19.
        endif.

        if wa_shipdate_type-trk_id_chg_des20 = 'Discount' or  wa_shipdate_type-trk_id_chg_des20 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt20.
        endif.

        if wa_shipdate_type-trk_id_chg_des21 = 'Discount' or  wa_shipdate_type-trk_id_chg_des21 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt21.
        endif.

        if wa_shipdate_type-trk_id_chg_des22 = 'Discount' or  wa_shipdate_type-trk_id_chg_des22 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt22.
        endif.

        if wa_shipdate_type-trk_id_chg_des23 = 'Discount' or  wa_shipdate_type-trk_id_chg_des23 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt23.
        endif.

        if wa_shipdate_type-trk_id_chg_des24 = 'Discount' or  wa_shipdate_type-trk_id_chg_des24 = 'Performance Pricing'.
          wa_final_shipdate_type-discount =   wa_final_shipdate_type-discount + wa_shipdate_type-trk_id_chg_amt24.
        endif.


        wa_final_shipdate_type-net_charge = wa_shipdate_type-net_chrg_amnt.

        append wa_final_shipdate_type to it_final_shipdate_type.
        clear :wa_final_shipdate_type,wa_shipdate_type.
      endloop.


      sort it_final_shipdate_type by shipdate.
      clear wa_final_shipdate_type.


      loop at it_final_shipdate_type into wa_final_shipdate_type.



        if not wa_final_shipdate_type is initial.
          at end of shipdate.
            sum.     " calculates sub totals on service types
            data : v_date(8) type c.
            clear : v_date.
            concatenate wa_final_shipdate_type+0(4) wa_final_shipdate_type+4(2) wa_final_shipdate_type+6(2) into v_date.
*            CONCATENATE WA_FINAL_SHIPDATE_TYPE-SHIPDATE+4(2)'-' WA_FINAL_SHIPDATE_TYPE-SHIPDATE+6(2) '-' WA_FINAL_SHIPDATE_TYPE-SHIPDATE+0(4)  INTO WA_FINAL_SHIPDATE_TYPE-SHIPDATE.
*            replace '/' with '.' in WA_FINAL_SHIPDATE_TYPE-SHIPDATE.
            wa_output_shipdate_type-shipdate = v_date.
*            WA_OUTPUT_SHIPDATE_TYPE-SHIPDATE = WA_FINAL_SHIPDATE_TYPE-SHIPDATE.
            wa_output_shipdate_type-packages =  wa_final_shipdate_type-packages.
            wa_output_shipdate_type-rated_weight = wa_final_shipdate_type-rated_weight.
            wa_output_shipdate_type-transport_charge = wa_final_shipdate_type-transport_charge.
            wa_output_shipdate_type-handling_charge =  wa_final_shipdate_type-handling_charge.
            wa_output_shipdate_type-discount =  wa_final_shipdate_type-discount.
            wa_output_shipdate_type-net_charge = wa_final_shipdate_type-net_charge.
* calculate the grand totals

            v_packages = v_packages + wa_output_shipdate_type-packages.
            v_rated_weight = v_rated_weight + wa_output_shipdate_type-rated_weight.
            v_trans_charg = v_trans_charg + wa_output_shipdate_type-transport_charge.
            v_hand_charg = v_hand_charg + wa_output_shipdate_type-handling_charge.
            v_discount = v_discount + wa_output_shipdate_type-discount.
            v_netcharge = v_netcharge +  wa_output_shipdate_type-net_charge .
            append wa_output_shipdate_type to it_output_shipdate_type.
            clear wa_output_shipdate_type.
          endat.

        endif.
        clear wa_final_shipdate_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_shipdate_type.

*    wa_output_serv_type-service_type = v_service.
      wa_output_shipdate_type-packages = v_packages.
      wa_output_shipdate_type-rated_weight = v_rated_weight.

      wa_output_shipdate_type-transport_charge = v_trans_charg.
      wa_output_shipdate_type-handling_charge = v_hand_charg .
      wa_output_shipdate_type-discount = v_discount .
      wa_output_shipdate_type-net_charge = v_netcharge.

      wa_output_shipdate_type-line_color      = 'C500'.
      append wa_output_shipdate_type to it_output_shipdate_type.
      clear wa_output_shipdate_type.



      wa_fieldcat_shipdate_type-col_pos      = 1.
      wa_fieldcat_shipdate_type-fieldname    = 'SHIPDATE'.
      wa_fieldcat_shipdate_type-coltext    = 'Shipment Date'.
      wa_fieldcat_shipdate_type-tabname      = 'IT_OUPUT_SHIPDATE_TYPE'.
      append wa_fieldcat_shipdate_type to it_fieldcat_shipdate_type.
      clear wa_fieldcat_shipdate_type.

      wa_fieldcat_shipdate_type-col_pos      = 2.
      wa_fieldcat_shipdate_type-fieldname    = 'PACKAGES'.
      wa_fieldcat_shipdate_type-coltext    = 'Total Packages'.
      wa_fieldcat_shipdate_type-tabname      = 'IT_OUPUT_SHIPDATE_TYPE'.
      append wa_fieldcat_shipdate_type to it_fieldcat_shipdate_type.
      clear wa_fieldcat_shipdate_type.

      wa_fieldcat_shipdate_type-col_pos      = 3.
      wa_fieldcat_shipdate_type-fieldname    = 'RATED_WEIGHT'.
      wa_fieldcat_shipdate_type-coltext    = 'Rated Weight'.
      wa_fieldcat_shipdate_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_shipdate_type to it_fieldcat_shipdate_type.
      clear wa_fieldcat_shipdate_type.

      wa_fieldcat_shipdate_type-col_pos      = 4.
      wa_fieldcat_shipdate_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_shipdate_type-coltext    = 'Transport Charge'.
      wa_fieldcat_shipdate_type-tabname      = 'IT_OUPUT_SHIPDATE_TYPE'.
      append wa_fieldcat_shipdate_type to it_fieldcat_shipdate_type.
      clear wa_fieldcat_shipdate_type.

      wa_fieldcat_shipdate_type-col_pos      = 5.
      wa_fieldcat_shipdate_type-fieldname    = 'HANDLING_CHARGE'.
      wa_fieldcat_shipdate_type-coltext    = 'Handling Charge'.
      wa_fieldcat_shipdate_type-tabname      = 'IT_OUPUT_SHIPDATE_TYPE'.
      append wa_fieldcat_shipdate_type to it_fieldcat_shipdate_type.
      clear wa_fieldcat_shipdate_type.

      wa_fieldcat_shipdate_type-col_pos      = 6.
      wa_fieldcat_shipdate_type-fieldname    = 'DISCOUNT'.
      wa_fieldcat_shipdate_type-coltext    = 'Discount'.
      wa_fieldcat_shipdate_type-tabname      = 'IT_OUPUT_SHIPDATE_TYPE'.
      append wa_fieldcat_shipdate_type to it_fieldcat_shipdate_type.
      clear wa_fieldcat_shipdate_type.

      wa_fieldcat_shipdate_type-col_pos      = 7.
      wa_fieldcat_shipdate_type-fieldname    = 'NET_CHARGE'.
      wa_fieldcat_shipdate_type-coltext    = 'Net Charge'.
      wa_fieldcat_shipdate_type-tabname      = 'IT_OUPUT_SHIPDATE_TYPE'.
      append wa_fieldcat_shipdate_type to it_fieldcat_shipdate_type.
      clear wa_fieldcat_shipdate_type.



      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUPUT_SHIPDATE_TYPE'
*          is_layout                     = gs_layout
          is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
        changing
          it_outtab                     = it_output_shipdate_type
          it_fieldcatalog               = it_fieldcat_shipdate_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.



    endif.
  endif.
  if ups = 'X'.
* declare variables used
*    DATA: "v_mm(2), v_dd(2), v_yyyy(4),
*          "count TYPE i,
*          TOTAL_AMNT_UPS TYPE NETWR,
*          TOTAL_SAVINGS_UPS TYPE NETWR,
*          TOTAL_PKGS_UPS TYPE I,
*          TOTAL_WEIGHT_UPS TYPE BRGEW_AP,
*          AVG_PKG_CST_UPS TYPE NETWR,
*          AVG_PND_CST_UPS TYPE NETWR.
*
*
** DATA noc TYPE string.
*
*    CLEAR: WA_FINAL_SHIP_DAY_UPS,WA_OUTPUT_SHIP_DAY_UPS,TOTAL_AMNT_UPS,TOTAL_SAVINGS_UPS,TOTAL_PKGS_UPS,TOTAL_WEIGHT_UPS.
*    REFRESH: IT_FINAL_SHIP_DAY_UPS,IT_OUTPUT_SHIP_DAY_UPS,IT_FIELDCAT_SHIP_DAY_UPS.
*
**  gs_layout-grid_title = 'Customer Package'.
*
*    SELECT  PICKUP_DATE
*            ACTUALWEIGHT
*            TOTAL_INC_CREDIT
*            TOTAL_BILL_CHARG
*              FROM ZPWEFA_INVC_UPS
*              INTO CORRESPONDING FIELDS OF TABLE IT_FINAL_SHIP_DAY_UPS
*              WHERE INVOICE_DATE EQ WEEK_DT .
*
*
*    IF SY-SUBRC = 0.
*      SORT IT_FINAL_SHIP_DAY_UPS BY PICKUP_DATE.
*      CLEAR WA_FINAL_SHIP_DAY_UPS.
*      LOOP AT IT_FINAL_SHIP_DAY_UPS INTO WA_FINAL_SHIP_DAY_UPS .
*        WA_FINAL_SHIP_DAY_UPS-TOTAL_SHIPMENTS = 1.
*        MODIFY IT_FINAL_SHIP_DAY_UPS FROM WA_FINAL_SHIP_DAY_UPS TRANSPORTING TOTAL_SHIPMENTS.
*
*        AT END OF PICKUP_DATE.
*          SUM.     " calculates sub totals on service types
*          WA_OUTPUT_SHIP_DAY_UPS-V_TOTAL_WEIGHT = WA_FINAL_SHIP_DAY_UPS-ACTUALWEIGHT.
*          WA_OUTPUT_SHIP_DAY_UPS-V_DUE          = WA_FINAL_SHIP_DAY_UPS-TOTAL_BILL_CHARG.
*          WA_FINAL_SHIP_DAY_UPS-TOTAL_SHIPMENTS = WA_FINAL_SHIP_DAY_UPS-TOTAL_SHIPMENTS.
*          WA_OUTPUT_SHIP_DAY_UPS-TOTAL_SHIPMENTS = WA_FINAL_SHIP_DAY_UPS-TOTAL_SHIPMENTS.
*          WA_OUTPUT_SHIP_DAY_UPS-TOTAL_SHIPMENTS1 = WA_FINAL_SHIP_DAY_UPS-TOTAL_SHIPMENTS.
*          WA_OUTPUT_SHIP_DAY_UPS-PICKUP_DATE = WA_FINAL_SHIP_DAY_UPS-PICKUP_DATE.
*          WA_OUTPUT_SHIP_DAY_UPS-TOTAL_LETERS = 0.
*          WA_OUTPUT_SHIP_DAY_UPS-TOTAL_INC_CREDIT = WA_FINAL_SHIP_DAY_UPS-TOTAL_INC_CREDIT.
*          WA_OUTPUT_SHIP_DAY_UPS-PKG_AVG_COST = ( WA_OUTPUT_SHIP_DAY_UPS-V_DUE / WA_OUTPUT_SHIP_DAY_UPS-TOTAL_SHIPMENTS1 ).
*          WA_OUTPUT_SHIP_DAY_UPS-WEIGHT_AVG_COST = WA_OUTPUT_SHIP_DAY_UPS-V_DUE / WA_OUTPUT_SHIP_DAY_UPS-V_TOTAL_WEIGHT.
** calculate the grand totals
*          WA_OUTPUT_SHIP_DAY_UPS-TOTAL_BILL_CHARG = WA_FINAL_SHIP_DAY_UPS-TOTAL_BILL_CHARG + WA_OUTPUT_SHIP_DAY_UPS-TOTAL_BILL_CHARG.
*          TOTAL_AMNT_UPS = WA_OUTPUT_SHIP_DAY_UPS-TOTAL_BILL_CHARG + TOTAL_AMNT_UPS.
*          TOTAL_SAVINGS_UPS = TOTAL_SAVINGS_UPS + WA_FINAL_SHIP_DAY_UPS-TOTAL_INC_CREDIT.
*          TOTAL_PKGS_UPS    = TOTAL_PKGS_UPS + WA_FINAL_SHIP_DAY_UPS-TOTAL_SHIPMENTS.
*          TOTAL_WEIGHT_UPS  = TOTAL_WEIGHT_UPS + WA_OUTPUT_SHIP_DAY_UPS-V_TOTAL_WEIGHT.
*          APPEND WA_OUTPUT_SHIP_DAY_UPS TO IT_OUTPUT_SHIP_DAY_UPS.
*          CLEAR WA_FINAL_SHIP_DAY_UPS.
*        ENDAT.
*        CLEAR WA_OUTPUT_SHIP_DAY_UPS.
*      ENDLOOP.
** calculate the Avg.costs and percentage
*      LOOP AT IT_OUTPUT_SHIP_DAY_UPS INTO WA_OUTPUT_SHIP_DAY_UPS.
*        WA_OUTPUT_SHIP_DAY_UPS-AMOUNT_PERCENT = WA_OUTPUT_SHIP_DAY_UPS-TOTAL_BILL_CHARG * 100 .
*        WA_OUTPUT_SHIP_DAY_UPS-AMOUNT_PERCENT = WA_OUTPUT_SHIP_DAY_UPS-AMOUNT_PERCENT / TOTAL_AMNT_UPS.
*        MODIFY IT_OUTPUT_SHIP_DAY_UPS FROM WA_OUTPUT_SHIP_DAY_UPS TRANSPORTING AMOUNT_PERCENT.
*        AT LAST.
*          AVG_PKG_CST_UPS = TOTAL_AMNT_UPS / TOTAL_PKGS_UPS.
*          AVG_PND_CST_UPS = TOTAL_AMNT_UPS / TOTAL_WEIGHT_UPS.
*        ENDAT.
*        CLEAR WA_OUTPUT_SHIP_DAY_UPS.
*      ENDLOOP.
** add the grand totals as last record
**Data : wa_output_ship_day_ups-pickup_Date(8) type c.
*
*      WA_OUTPUT_SHIP_DAY_UPS-PICKUP_DATE = 'Total'.
*      WA_OUTPUT_SHIP_DAY_UPS-TOTAL_SHIPMENTS = TOTAL_PKGS_UPS.
*      WA_OUTPUT_SHIP_DAY_UPS-TOTAL_LETERS    = 0.
*      WA_OUTPUT_SHIP_DAY_UPS-V_TOTAL_WEIGHT  = TOTAL_WEIGHT_UPS.
*      WA_OUTPUT_SHIP_DAY_UPS-TOTAL_INC_CREDIT  = TOTAL_SAVINGS_UPS .
*      WA_OUTPUT_SHIP_DAY_UPS-TOTAL_BILL_CHARG          = TOTAL_AMNT_UPS.
*      WA_OUTPUT_SHIP_DAY_UPS-AMOUNT_PERCENT  = ' '.
*      WA_OUTPUT_SHIP_DAY_UPS-PKG_AVG_COST    = AVG_PKG_CST_UPS.
*      WA_OUTPUT_SHIP_DAY_UPS-WEIGHT_AVG_COST = AVG_PND_CST_UPS.
*      WA_OUTPUT_SHIP_DAY_UPS-LINE_COLOR      = 'C500'.
*      APPEND WA_OUTPUT_SHIP_DAY_UPS TO IT_OUTPUT_SHIP_DAY_UPS.
*      CLEAR WA_OUTPUT_SHIP_DAY_UPS.
*
*
** prepare field cats:
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 1.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'PICKUP_DATE'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'Pickup Date'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 2.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'TOTAL_SHIPMENTS'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'Total Packages'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 3.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'TOTAL_LETERS'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'Total Letters only'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 4.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'V_TOTAL_WEIGHT'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'Total Weight'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 5.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'TOTAL_INC_CREDIT'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'TotalSavings($)'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 6.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'TOTAL_BILL_CHARG'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'TotalNetCost($)'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 7.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'AMOUNT_PERCENT'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'TotalNetCost%'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 8.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'PKG_AVG_COST'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'Avg.Cost per Package($)'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*
*      WA_FIELDCAT_SHIP_DAY_UPS-COL_POS      = 9.
*      WA_FIELDCAT_SHIP_DAY_UPS-FIELDNAME    = 'WEIGHT_AVG_COST'.
*      WA_FIELDCAT_SHIP_DAY_UPS-COLTEXT    = 'Avg.Cost per Pound($)'.
*      WA_FIELDCAT_SHIP_DAY_UPS-TABNAME      = 'IT_OUTPUT_SHIP_DAY'.
*      APPEND WA_FIELDCAT_SHIP_DAY_UPS TO IT_FIELDCAT_SHIP_DAY_UPS.
*      CLEAR WA_FIELDCAT_SHIP_DAY_UPS.
*      GS_LAYOUT-GRID_TITLE = SPACE.
*
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*          EXPORTING
*            I_STRUCTURE_NAME              = 'IT_OUTPUT_SHIP_DAY_UPS'
**    IS_VARIANT                    =
**    I_SAVE                        =
**    I_DEFAULT                     = 'X'
*            IS_LAYOUT                     = GS_LAYOUT
*          CHANGING
*            IT_OUTTAB                     = IT_OUTPUT_SHIP_DAY_UPS
*            IT_FIELDCATALOG               = IT_FIELDCAT_SHIP_DAY_UPS
**    IT_SORT                       =
*        EXCEPTIONS
*          INVALID_PARAMETER_COMBINATION = 1
*          PROGRAM_ERROR                 = 2
*          TOO_MANY_LINES                = 3
*          OTHERS                        = 4  .
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*
*
*    ENDIF.
  endif.

endform.                    "shipday



********************END OF SHIPMENT DATE


***************Begin of ALLDETAILS****************

*---------------------------------------------------------------------*
*&      Form  Alldetails
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form alldetails.



  clear : approve.
  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
* declare variables used
  if wa_export_output-carrier = 'FEDEX'.

    data: total_amnt type netwr,
          v_amt(10) type c,
          total_savings type netwr,
          total_pkgs type i,
          total_weight type brgew_ap,
          avg_pkg_cst type netwr,
          avg_pnd_cst type netwr.


    data :
*V_ALLDETICE TYPE ZPWEFA_FEDEX-ALLDETICE_TYPE,
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt,
    v_fuel_surcharge type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_perform_price  type /PWEAVER/EFA_FED-tran_charg_amnt.

    clear : v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_alldet_type,it_final_alldet_type,it_fieldcat_alldet_type,it_output_alldet_type.
    clear : it_alldet_type,wa_alldet_type,wa_final_alldet_type,wa_fieldcat_alldet_type.
    select *
              from /pweaver/efa_fed
              into corresponding fields of table it_alldet_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.

    if sy-subrc = 0.
      clear : wa_alldet_type.
      loop at it_alldet_type into wa_alldet_type.
        data: day(2) type c,
      month(2) type c,
      year(4) type c.




        wa_final_alldet_type-shipment_date  = wa_alldet_type-shipment_date.
        wa_final_alldet_type-service_type  = wa_alldet_type-service_type.
        wa_final_alldet_type-exp_grd_trck_id = wa_alldet_type-exp_grd_trck_id.
        wa_final_alldet_type-org_cust_ref = wa_alldet_type-org_cust_ref.
        wa_final_alldet_type-org_ref_3 = wa_alldet_type-org_ref_3.
*        WA_FINAL_ALLDET_TYPE-POD = WA_ALLDET_TYPE-EXP_GRD_TRCK_ID.

        if wa_alldet_type-pod_del_date = '0'.

  wa_alldet_type-pod_del_date = ''.
*endif.
else.

*CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*  EXPORTING
*    INPUT         = wa_alldet_type-pod_del_date
* IMPORTING
*  OUTPUT        = wa_alldet_type-pod_del_date.
*
*
*day = wa_alldet_type-pod_del_date+0(2).
*month = wa_alldet_type-pod_del_date+2(2).
*year = wa_alldet_type-pod_del_date+4(4).
*
*
*clear wa_alldet_type-pod_del_date.
*concatenate month '/'day'/'year into wa_alldet_type-pod_del_date.


endif.
        wa_final_alldet_type-pod_del_date = wa_alldet_type-pod_del_date.



  data:  hour(2) type c,
               min(2) type c,
               sec(2) type c.


CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    INPUT         = wa_alldet_type-pod_del_time
 IMPORTING
  OUTPUT        = wa_alldet_type-pod_del_time.

         if wa_alldet_type-pod_del_time = '0'.
           wa_alldet_type-pod_del_time = '00:00:00'.
           else.

        hour = wa_alldet_type-pod_del_time+4(2).
        min = wa_alldet_type-pod_del_time+6(2).
        sec = wa_alldet_type-pod_del_time+8(2).


         concatenate hour':'min':'sec into wa_alldet_type-pod_del_time.

endif.

        wa_final_alldet_type-pod_del_time = wa_alldet_type-pod_del_time.
        wa_final_alldet_type-pod_sign_desc = wa_alldet_type-pod_sign_desc.
        wa_final_alldet_type-zone_code = wa_alldet_type-zone_code.
        wa_final_alldet_type-num_pieces = wa_alldet_type-num_pieces.
        wa_final_alldet_type-rated_weight = wa_alldet_type-rated_wgt_amt.
        wa_final_alldet_type-shipper_name = wa_alldet_type-shipper_name.


        wa_final_alldet_type-transport_charge = wa_alldet_type-tran_charg_amnt.


****Handling charges
        if
      wa_alldet_type-trk_id_chg_des = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des = 'Residential' or
      wa_alldet_type-trk_id_chg_des = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des = 'Call Tag'.    .

          wa_final_alldet_type-handling_charge =  wa_alldet_type-trk_id_chg_amt.
        endif.

        if
      wa_alldet_type-trk_id_chg_des1 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des1 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des1 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des1 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des1 = 'Residential' or
      wa_alldet_type-trk_id_chg_des1 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des1 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des1 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des1 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des1 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des1 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des1 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des1 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des1 = 'Call Tag'..    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt1.
        endif.

        if
      wa_alldet_type-trk_id_chg_des2 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des2 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des2 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des2 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des2 = 'Residential' or
      wa_alldet_type-trk_id_chg_des2 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des2 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des2 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des2 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des2 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des2 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des2 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des2 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des2 = 'Call Tag'..    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt2.
        endif.

        if
      wa_alldet_type-trk_id_chg_des3 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des3 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des3 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des3 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des3 = 'Residential' or
      wa_alldet_type-trk_id_chg_des3 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des3 = 'DAS Extended Comm' or
     wa_alldet_type-trk_id_chg_des3 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des3 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des3 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des3 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des3 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des3 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des3 = 'Call Tag'.    .

          .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt3.
        endif.

        if
      wa_alldet_type-trk_id_chg_des4 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des4 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des4 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des4 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des4 = 'Residential' or
      wa_serv_type-trk_id_chg_des4 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des4 = 'DAS Extended Comm' or
        wa_alldet_type-trk_id_chg_des4 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des4 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des4 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des4 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des4 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des4 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des4 = 'Call Tag'.    .
   .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt4.
        endif.

        if
      wa_alldet_type-trk_id_chg_des5 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des5 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des5 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des5 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des5 = 'Residential' or
      wa_alldet_type-trk_id_chg_des5 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des5 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des5 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des5 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des5 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des5 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des5 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des5 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des5 = 'Call Tag'.    .
    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt5.
        endif.

        if
      wa_alldet_type-trk_id_chg_des6 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des6 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des6 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des6 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des6 = 'Residential' or
      wa_alldet_type-trk_id_chg_des6 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des6 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des6 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des6 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des6 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des6 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des6 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des6 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des6 = 'Call Tag'.       .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt6.
        endif.

        if
      wa_alldet_type-trk_id_chg_des7 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des7 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des7 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des7 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des7 = 'Residential' or
      wa_alldet_type-trk_id_chg_des7 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des7 = 'DAS Extended Comm' or
       wa_alldet_type-trk_id_chg_des7 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des7 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des7 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des7 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des7 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des7 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des7 = 'Call Tag'.    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt7.
        endif.

        if
      wa_alldet_type-trk_id_chg_des8 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des8 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des8 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des8 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des8 = 'Residential' or
      wa_alldet_type-trk_id_chg_des8 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des8 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des8 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des8 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des8 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des8 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des8 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des8 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des8 = 'Call Tag'.    .    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt8.
        endif.


        if
      wa_alldet_type-trk_id_chg_des9 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des9 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des9 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des9 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des9 = 'Residential' or
      wa_alldet_type-trk_id_chg_des9 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des9 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des9 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des9 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des9 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des9 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des9 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des9 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des9 = 'Call Tag'.    .    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt9.
        endif.


        if
      wa_alldet_type-trk_id_chg_des10 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des10 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des10 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des10 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des10 = 'Residential' or
      wa_alldet_type-trk_id_chg_des10 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des10 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des10 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des10 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des10 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des10 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des10 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des10 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des10 = 'Call Tag'.    .
    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt10.
        endif.


        if
      wa_alldet_type-trk_id_chg_des11 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des11 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des11 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des11 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des11 = 'Residential' or
      wa_alldet_type-trk_id_chg_des11 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des11 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des11 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des11 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des11 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des11 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des11 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des11 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des11 = 'Call Tag'..    .
    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt11.
        endif.


        if
      wa_alldet_type-trk_id_chg_des12 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des12 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des12 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des12 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des12 = 'Residential' or
      wa_alldet_type-trk_id_chg_des12 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des12 = 'DAS Extended Comm' or
       wa_alldet_type-trk_id_chg_des12 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des12 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des12 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des12 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des12 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des12 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des12 = 'Call Tag'..    .
   .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt12.
        endif.


        if
      wa_alldet_type-trk_id_chg_des13 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des13 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des13 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des13 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des13 = 'Residential' or
      wa_alldet_type-trk_id_chg_des13 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des13 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des13 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des13 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des13 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des13 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des13 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des13 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des13 = 'Call Tag'..    .
    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt13.
        endif.


        if
      wa_alldet_type-trk_id_chg_des14 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des14 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des14 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des14 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des14 = 'Residential' or
      wa_alldet_type-trk_id_chg_des14 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des14 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des14 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des14 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des14 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des14 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des14 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des14 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des14 = 'Call Tag'..    .
    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt14.
        endif.


        if
      wa_alldet_type-trk_id_chg_des15 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des15 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des15 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des15 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des15 = 'Residential' or
      wa_alldet_type-trk_id_chg_des15 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des15 = 'DAS Extended Comm' or
       wa_alldet_type-trk_id_chg_des15 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des15 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des15 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des15 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des15 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des15 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des15 = 'Call Tag'..    .   .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt15.
        endif.


        if
      wa_alldet_type-trk_id_chg_des16 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des16 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des16 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des16 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des16 = 'Residential' or
      wa_alldet_type-trk_id_chg_des16 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des16 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des16 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des16 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des16 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des16 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des16 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des16 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des16 = 'Call Tag'..    .    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt16.
        endif.


        if
      wa_alldet_type-trk_id_chg_des17 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des17 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des17 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des17 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des17 = 'Residential' or
      wa_alldet_type-trk_id_chg_des17 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des17 = 'DAS Extended Comm'or
      wa_alldet_type-trk_id_chg_des17 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des17 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des17 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des17 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des17 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des17 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des17 = 'Call Tag'..    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt17.
        endif.


        if
      wa_alldet_type-trk_id_chg_des18 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des18 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des18 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des18 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des18 = 'Residential' or
      wa_alldet_type-trk_id_chg_des18 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des18 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des18 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des18 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des18 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des18 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des18 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des18 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des18 = 'Call Tag'..    .    .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt18.
        endif.


        if
wa_alldet_type-trk_id_chg_des19 = 'Weekly Service Chg' or
wa_alldet_type-trk_id_chg_des19 = 'Weekday Delivery' or
wa_alldet_type-trk_id_chg_des19 = 'Residential Delivery' or
wa_alldet_type-trk_id_chg_des19 = 'Address Correction' or
wa_alldet_type-trk_id_chg_des19 = 'Residential' or
wa_alldet_type-trk_id_chg_des19 = 'DAS Comm' or
wa_alldet_type-trk_id_chg_des19 = 'DAS Extended Comm' or
 wa_alldet_type-trk_id_chg_des19 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des19 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des19 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des19 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des19 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des19 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des19 = 'Call Tag'..               .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt19.
        endif.

        if
wa_alldet_type-trk_id_chg_des20 = 'Weekly Service Chg' or
wa_alldet_type-trk_id_chg_des20 = 'Weekday Delivery' or
wa_alldet_type-trk_id_chg_des20 = 'Residential Delivery' or
wa_alldet_type-trk_id_chg_des20 = 'Address Correction' or
wa_alldet_type-trk_id_chg_des20 = 'Residential' or
wa_alldet_type-trk_id_chg_des20 = 'DAS Comm' or
wa_alldet_type-trk_id_chg_des20 = 'DAS Extended Comm' or
wa_alldet_type-trk_id_chg_des20 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des20 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des20 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des20 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des20 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des20 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des20 = 'Call Tag'..          .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt20.
        endif.

        if
wa_alldet_type-trk_id_chg_des21 = 'Weekly Service Chg' or
wa_alldet_type-trk_id_chg_des21 = 'Weekday Delivery' or
wa_alldet_type-trk_id_chg_des21 = 'Residential Delivery' or
wa_alldet_type-trk_id_chg_des21 = 'Address Correction' or
wa_alldet_type-trk_id_chg_des21 = 'Residential' or
wa_alldet_type-trk_id_chg_des21 = 'DAS Comm' or
wa_alldet_type-trk_id_chg_des21 = 'DAS Extended Comm' or
 wa_alldet_type-trk_id_chg_des21 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des21 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des21 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des21 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des21 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des21 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des21 = 'Call Tag'..         .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt21.
        endif.

        if
      wa_alldet_type-trk_id_chg_des22 = 'Weekly Service Chg' or
      wa_alldet_type-trk_id_chg_des22 = 'Weekday Delivery' or
      wa_alldet_type-trk_id_chg_des22 = 'Residential Delivery' or
      wa_alldet_type-trk_id_chg_des22 = 'Address Correction' or
      wa_alldet_type-trk_id_chg_des22 = 'Residential' or
      wa_alldet_type-trk_id_chg_des22 = 'DAS Comm' or
      wa_alldet_type-trk_id_chg_des22 = 'DAS Extended Comm' or
      wa_alldet_type-trk_id_chg_des22 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des22 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des22 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des22 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des22 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des22 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des22 = 'Call Tag'..          .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt22.
        endif.

        if
wa_alldet_type-trk_id_chg_des23 = 'Weekly Service Chg' or
wa_alldet_type-trk_id_chg_des23 = 'Weekday Delivery' or
wa_alldet_type-trk_id_chg_des23 = 'Residential Delivery' or
wa_alldet_type-trk_id_chg_des23 = 'Address Correction' or
wa_alldet_type-trk_id_chg_des23 = 'Residential' or
wa_alldet_type-trk_id_chg_des23 = 'DAS Comm' or
wa_alldet_type-trk_id_chg_des23 = 'DAS Extended Comm' or
 wa_alldet_type-trk_id_chg_des23 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des23 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des23 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des23 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des23 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des23 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des23 = 'Call Tag'..         .

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt23.
        endif.

        if wa_alldet_type-trk_id_chg_des24 = 'Weekly Service Chg' or
wa_alldet_type-trk_id_chg_des24 = 'Weekday Delivery' or
wa_alldet_type-trk_id_chg_des24 = 'Residential Delivery' or
wa_alldet_type-trk_id_chg_des24 = 'Address Correction' or
wa_alldet_type-trk_id_chg_des24 = 'Residential' or
wa_alldet_type-trk_id_chg_des24 = 'DAS Comm' or
wa_alldet_type-trk_id_chg_des24 = 'DAS Extended Comm' or
 wa_alldet_type-trk_id_chg_des24 = 'DAS Extended Resi' or
      wa_alldet_type-trk_id_chg_des24 = 'DAS Resi'          or
      wa_alldet_type-trk_id_chg_des24 =  'Direct Signature' or
      wa_alldet_type-trk_id_chg_des24 = 'DAS Hawaii Comm' or
      wa_alldet_type-trk_id_chg_des24 = 'AHS - Dimensions' or
      wa_alldet_type-trk_id_chg_des24 = 'AHS - Weight' or
      wa_alldet_type-trk_id_chg_des24 = 'Call Tag'.

          wa_final_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge + wa_alldet_type-trk_id_chg_amt24.
        endif.

**************Fuel Surcharge
*****Fuel Surcharge
        if wa_alldet_type-trk_id_chg_des = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_alldet_type-trk_id_chg_amt.
        endif.

        if wa_alldet_type-trk_id_chg_des1 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt1.
        endif.

        if wa_alldet_type-trk_id_chg_des2 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt2.
        endif.

        if wa_alldet_type-trk_id_chg_des3 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt3.
        endif.

        if wa_alldet_type-trk_id_chg_des4 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt4.
        endif.

        if wa_alldet_type-trk_id_chg_des5 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt5.
        endif.

        if wa_alldet_type-trk_id_chg_des6 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt6.
        endif.

        if wa_alldet_type-trk_id_chg_des7 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt7.
        endif.

        if wa_alldet_type-trk_id_chg_des8 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt8.
        endif.


        if wa_alldet_type-trk_id_chg_des9 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt9.
        endif.


        if wa_alldet_type-trk_id_chg_des10 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt10.
        endif.


        if wa_alldet_type-trk_id_chg_des11 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt11.
        endif.


        if wa_alldet_type-trk_id_chg_des12 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt12.
        endif.


        if wa_alldet_type-trk_id_chg_des13 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt13.
        endif.


        if wa_alldet_type-trk_id_chg_des14 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt14.
        endif.


        if wa_alldet_type-trk_id_chg_des15 = 'Fuel Surcharge' .
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt15.
        endif.


        if wa_alldet_type-trk_id_chg_des16 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt16.
        endif.


        if wa_alldet_type-trk_id_chg_des17 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt17.
        endif.


        if wa_alldet_type-trk_id_chg_des18 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt18.
        endif.


        if wa_alldet_type-trk_id_chg_des19 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt19.
        endif.

        if wa_alldet_type-trk_id_chg_des20 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt20.
        endif.

        if wa_alldet_type-trk_id_chg_des21 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt21.
        endif.

        if wa_alldet_type-trk_id_chg_des22 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt22.
        endif.

        if wa_alldet_type-trk_id_chg_des23 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt23.
        endif.

        if wa_alldet_type-trk_id_chg_des24 = 'Fuel Surcharge'.
          wa_final_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge + wa_alldet_type-trk_id_chg_amt24.
        endif.



***************Earned Discount
        if wa_alldet_type-trk_id_chg_des = 'Discount'.
          wa_final_alldet_type-discount =  wa_alldet_type-trk_id_chg_amt.
        endif.
        if wa_alldet_type-trk_id_chg_des1 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt1.
        endif.

        if wa_alldet_type-trk_id_chg_des2 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt2.
        endif.

        if wa_alldet_type-trk_id_chg_des3 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt3.
        endif.

        if wa_alldet_type-trk_id_chg_des4 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt4.
        endif.

        if wa_alldet_type-trk_id_chg_des5 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt5.
        endif.

        if wa_alldet_type-trk_id_chg_des6 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt6.
        endif.

        if wa_alldet_type-trk_id_chg_des7 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt7.
        endif.

        if wa_alldet_type-trk_id_chg_des8 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt8.
        endif.

        if wa_alldet_type-trk_id_chg_des9 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt9.
        endif.


        if wa_alldet_type-trk_id_chg_des10 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt10.
        endif.

        if wa_alldet_type-trk_id_chg_des11 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt11.
        endif.

        if wa_alldet_type-trk_id_chg_des12 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt12.
        endif.

        if wa_alldet_type-trk_id_chg_des13 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt13.
        endif.


        if wa_alldet_type-trk_id_chg_des14 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt14.
        endif.

        if wa_alldet_type-trk_id_chg_des15 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt15.
        endif.


        if wa_alldet_type-trk_id_chg_des16 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt16.
        endif.

        if wa_alldet_type-trk_id_chg_des17 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt17.
        endif.

        if wa_alldet_type-trk_id_chg_des18 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt18.
        endif.

        if wa_alldet_type-trk_id_chg_des19 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt19.
        endif.

        if wa_alldet_type-trk_id_chg_des20 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt20.
        endif.

        if wa_alldet_type-trk_id_chg_des21 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt21.
        endif.

        if wa_alldet_type-trk_id_chg_des22 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt22.
        endif.

        if wa_alldet_type-trk_id_chg_des23 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt23.
        endif.

        if wa_alldet_type-trk_id_chg_des24 = 'Discount'.
          wa_final_alldet_type-discount =   wa_final_alldet_type-discount + wa_alldet_type-trk_id_chg_amt24.
        endif.


****Performance Discount


        if wa_alldet_type-trk_id_chg_des = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =  wa_alldet_type-trk_id_chg_amt.
        endif.

        if wa_alldet_type-trk_id_chg_des1 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt1.
        endif.

        if wa_alldet_type-trk_id_chg_des2 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt2.
        endif.

        if wa_alldet_type-trk_id_chg_des3 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt3.
        endif.

        if wa_alldet_type-trk_id_chg_des4 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt4.
        endif.

        if wa_alldet_type-trk_id_chg_des5 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt5.
        endif.

        if wa_alldet_type-trk_id_chg_des6 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt6.
        endif.

        if wa_alldet_type-trk_id_chg_des7 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt7.
        endif.

        if wa_alldet_type-trk_id_chg_des8 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt8.
        endif.

        if wa_alldet_type-trk_id_chg_des9 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt9.
        endif.


        if wa_alldet_type-trk_id_chg_des10 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt10.
        endif.

        if wa_alldet_type-trk_id_chg_des11 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt11.
        endif.

        if wa_alldet_type-trk_id_chg_des12 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt12.
        endif.

        if wa_alldet_type-trk_id_chg_des13 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt13.
        endif.


        if wa_alldet_type-trk_id_chg_des14 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt14.
        endif.

        if wa_alldet_type-trk_id_chg_des15 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt15.
        endif.


        if wa_alldet_type-trk_id_chg_des16 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt16.
        endif.

        if wa_alldet_type-trk_id_chg_des17 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt17.
        endif.

        if wa_alldet_type-trk_id_chg_des18 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt18.
        endif.

        if wa_alldet_type-trk_id_chg_des19 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt19.
        endif.

        if wa_alldet_type-trk_id_chg_des20 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt20.
        endif.

        if wa_alldet_type-trk_id_chg_des21 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt21.
        endif.

        if wa_alldet_type-trk_id_chg_des22 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt22.
        endif.

        if wa_alldet_type-trk_id_chg_des23 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt23.
        endif.

        if wa_alldet_type-trk_id_chg_des24 = 'Performance Pricing'.
          wa_final_alldet_type-perform_price =   wa_final_alldet_type-perform_price + wa_alldet_type-trk_id_chg_amt24.
        endif.


        wa_final_alldet_type-net_charge = wa_alldet_type-net_chrg_amnt.

        append wa_final_alldet_type to it_final_alldet_type.
        clear :wa_final_alldet_type,wa_alldet_type.
      endloop.


      sort it_final_alldet_type by exp_grd_trck_id.
      clear wa_final_alldet_type.


      loop at it_final_alldet_type into wa_final_alldet_type.



        if not wa_final_alldet_type is initial.
*          AT END OF ALLDETICE_TYPE.
*            SUM.     " calculates sub totals on ALLDETice types
          data : v_date(8) type c,
           v_pod(8) type c.
          clear : v_date.
          concatenate wa_final_alldet_type-shipment_date+0(4) wa_final_alldet_type-shipment_date+4(2) wa_final_alldet_type-shipment_date+6(2) into v_date.

*          CONCATENATE WA_FINAL_ALLDET_TYPE-SHIPMENT_DATE+4(2)'-' WA_FINAL_ALLDET_TYPE-SHIPMENT_DATE+6(2) '-' WA_FINAL_ALLDET_TYPE-SHIPMENT_DATE+0(4)  INTO WA_FINAL_ALLDET_TYPE-SHIPMENT_DATE.

*          WA_OUTPUT_ALLDET_TYPE-SHIPMENT_DATE = WA_FINAL_ALLDET_TYPE-SHIPMENT_DATE.
          wa_output_alldet_type-shipment_date = v_date.
          wa_output_alldet_type-service_type = wa_final_alldet_type-service_type.
          wa_output_alldet_type-exp_grd_trck_id = wa_final_alldet_type-exp_grd_trck_id.
          wa_output_alldet_type-num_pieces =  wa_final_alldet_type-num_pieces.
          wa_output_alldet_type-rated_weight = wa_final_alldet_type-rated_weight.
          wa_output_alldet_type-org_cust_ref =  wa_final_alldet_type-org_cust_ref.
          wa_output_alldet_type-org_ref_3 =  wa_final_alldet_type-org_ref_3.
*          WA_OUTPUT_ALLDET_TYPE-POD =  WA_FINAL_ALLDET_TYPE-POD.
          wa_output_alldet_type-pod =  'Proof of Delivery'.




          clear : v_pod.
*          concatenate wa_final_alldet_type-pod_del_date+0(4) wa_final_alldet_type-pod_del_date+4(2) wa_final_alldet_type-pod_del_date+6(2) into v_pod.
*          CONCATENATE WA_FINAL_ALLDET_TYPE-POD_DEL_DATE+4(2)'-' WA_FINAL_ALLDET_TYPE-POD_DEL_DATE+6(2) '-' WA_FINAL_ALLDET_TYPE-POD_DEL_DATE+0(4)  INTO WA_FINAL_ALLDET_TYPE-POD_DEL_DATE.

*          WA_OUTPUT_ALLDET_TYPE-POD_DEL_DATE =  WA_FINAL_ALLDET_TYPE-POD_DEL_DATE.
          wa_output_alldet_type-pod_del_date =  wa_final_alldet_type-pod_del_date.
          wa_output_alldet_type-pod_del_time =  wa_final_alldet_type-pod_del_time.
          wa_output_alldet_type-pod_sign_desc =  wa_final_alldet_type-pod_sign_desc.
          wa_output_alldet_type-shipper_name =  wa_final_alldet_type-shipper_name.
          wa_output_alldet_type-fuel_surcharge =  wa_final_alldet_type-fuel_surcharge.
          wa_output_alldet_type-perform_price =  wa_final_alldet_type-perform_price.

          wa_output_alldet_type-zone_code =  wa_final_alldet_type-zone_code.
          wa_output_alldet_type-rated_weight = wa_final_alldet_type-rated_weight.
          wa_output_alldet_type-transport_charge = wa_final_alldet_type-transport_charge.
          wa_output_alldet_type-handling_charge =  wa_final_alldet_type-handling_charge.
          wa_output_alldet_type-discount =  wa_final_alldet_type-discount.
          wa_output_alldet_type-net_charge = wa_final_alldet_type-net_charge.


* calculate the grand totals

*v_ALLDETice = v_ALLDETice + wa_output_ALLDET_type-ALLDETice_type.
          V_PACKAGES = V_PACKAGES + WA_OUTPUT_ALLDET_TYPE-NUM_PIECES.
          v_rated_weight = v_rated_weight + wa_output_alldet_type-rated_weight.

          v_trans_charg = v_trans_charg + wa_output_alldet_type-transport_charge.


          v_hand_charg = v_hand_charg + wa_output_alldet_type-handling_charge.
          v_fuel_surcharge = v_fuel_surcharge + wa_output_alldet_type-fuel_surcharge.
          v_perform_price = v_perform_price + wa_output_alldet_type-perform_price.

          v_discount = v_discount + wa_output_alldet_type-discount.

          v_netcharge = v_netcharge +  wa_output_alldet_type-net_charge .
          append wa_output_alldet_type to it_output_alldet_type.
          clear wa_output_alldet_type.
*          ENDAT.

        endif.
        clear wa_final_alldet_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_alldet_type.

*    wa_output_ALLDET_type-ALLDETice_type = v_ALLDETice.
      WA_OUTPUT_ALLDET_TYPE-NUM_PIECES = V_PACKAGES.
      WA_OUTPUT_ALLDET_TYPE-RATED_WEIGHT = V_RATED_WEIGHT.

      wa_output_alldet_type-transport_charge = v_trans_charg.
      wa_output_alldet_type-handling_charge = v_hand_charg .
      wa_output_alldet_type-fuel_surcharge = v_fuel_surcharge .
      wa_output_alldet_type-discount = v_discount .
      wa_output_alldet_type-perform_price = v_perform_price .
      wa_output_alldet_type-net_charge = v_netcharge.

      wa_output_alldet_type-line_color      = 'C500'.
      append wa_output_alldet_type to it_output_alldet_type.
      clear wa_output_alldet_type.



      wa_fieldcat_alldet_type-col_pos      = 1.
      wa_fieldcat_alldet_type-fieldname    = 'SHIPMENT_DATE'.
      wa_fieldcat_alldet_type-coltext    = 'Shipment Date'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 2.
      wa_fieldcat_alldet_type-fieldname    = 'SERVICE_TYPE'.
      wa_fieldcat_alldet_type-coltext    = 'Service Type'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      WA_FIELDCAT_ALLDET_TYPE-OUTPUTLEN = '40'.
      wa_fieldcat_alldet_type-lowercase = 'X'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 3.
      wa_fieldcat_alldet_type-fieldname    = 'EXP_GRD_TRCK_ID'.
      wa_fieldcat_alldet_type-coltext    = 'Tracking Number'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      wa_fieldcat_alldet_type-hotspot   = 'x'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 4.
      wa_fieldcat_alldet_type-fieldname    = 'ORG_CUST_REF'.
      wa_fieldcat_alldet_type-coltext    = 'Reference #'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 5.
      wa_fieldcat_alldet_type-fieldname    = 'ORG_REF_3'.
      wa_fieldcat_alldet_type-coltext    = 'PO #'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 6.
      wa_fieldcat_alldet_type-fieldname    = 'POD'.
      wa_fieldcat_alldet_type-coltext    = 'Proof of Delivery'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      wa_fieldcat_alldet_type-hotspot   = 'x'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 7.
      wa_fieldcat_alldet_type-fieldname    = 'POD_DEL_DATE'.
      wa_fieldcat_alldet_type-coltext    = 'POD Date'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 8.
      wa_fieldcat_alldet_type-fieldname    = 'POD_DEL_TIME'.
      wa_fieldcat_alldet_type-coltext    = 'POD Time'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 9.
      wa_fieldcat_alldet_type-fieldname    = 'POD_SIGN_DESC'.
      wa_fieldcat_alldet_type-coltext    = 'POD Signature'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.


      wa_fieldcat_alldet_type-col_pos      = 10.
      wa_fieldcat_alldet_type-fieldname    = 'ZONE_CODE'.
      wa_fieldcat_alldet_type-coltext    = 'Zone'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.

      wa_fieldcat_alldet_type-col_pos      = 11.
      wa_fieldcat_alldet_type-fieldname    = 'NUM_PIECES'.
      wa_fieldcat_alldet_type-coltext    = 'Packages'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
      wa_fieldcat_alldet_type-col_pos      = 12.
      wa_fieldcat_alldet_type-fieldname    = 'RATED_WEIGHT'.
      wa_fieldcat_alldet_type-coltext    = 'Rated Weight'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
      wa_fieldcat_alldet_type-col_pos      = 13.
      wa_fieldcat_alldet_type-fieldname    = 'SHIPPER_NAME'.
      wa_fieldcat_alldet_type-coltext    = 'Shipper Name'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
      wa_fieldcat_alldet_type-col_pos      = 14.
      wa_fieldcat_alldet_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_alldet_type-coltext    = 'Transport Charge'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
      wa_fieldcat_alldet_type-col_pos      = 15.
      wa_fieldcat_alldet_type-fieldname    = 'FUEL_SURCHARGE'.
      wa_fieldcat_alldet_type-coltext    = 'Fuel Surcharge'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
      wa_fieldcat_alldet_type-col_pos      = 16.
      wa_fieldcat_alldet_type-fieldname    = 'HANDLING_CHARGE'.
      wa_fieldcat_alldet_type-coltext    = 'Handling Charge'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
      wa_fieldcat_alldet_type-col_pos      = 17.
      wa_fieldcat_alldet_type-fieldname    = 'DISCOUNT'.
      wa_fieldcat_alldet_type-coltext    = 'Discount'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
      wa_fieldcat_alldet_type-col_pos      = 18.
      wa_fieldcat_alldet_type-fieldname    = 'PERFORM_PRICE'.
      wa_fieldcat_alldet_type-coltext    = 'Performance Pricing'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
      wa_fieldcat_alldet_type-col_pos      = 19.
      wa_fieldcat_alldet_type-fieldname    = 'NET_CHARGE'.
      wa_fieldcat_alldet_type-coltext    = 'Net Charge'.
      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
      clear wa_fieldcat_alldet_type.
*      wa_fieldcat_alldet_type-col_pos      = 20.
*      wa_fieldcat_alldet_type-fieldname    = 'CREDIT_AMOUNT'.
*      wa_fieldcat_alldet_type-coltext    = 'Credit Amount'.
*      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
*      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
*      clear wa_fieldcat_alldet_type.
*      wa_fieldcat_alldet_type-col_pos      = 21.
*      wa_fieldcat_alldet_type-fieldname    = 'ERROR_CODE'.
*      wa_fieldcat_alldet_type-coltext    = 'Error Code'.
*      wa_fieldcat_alldet_type-tabname      = 'IT_OUPUT_ALLDET_TYPE'.
*      append wa_fieldcat_alldet_type to it_fieldcat_alldet_type.
*      clear wa_fieldcat_alldet_type.


      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUPUT_ALLDET_TYPE'
*          is_layout                     = gs_layout
          is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
        changing
          it_outtab                     = it_output_alldet_type
          it_fieldcatalog               = it_fieldcat_alldet_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.

      set handler g_application->handle_hotspot_click for go_grid .

    endif.
  endif.


  if ups = 'X'.

*    CLEAR : APPROVE.
*    REFRESH: IT_FINAL_ALL_DETAILS_UPS,IT_FIELDCAT_ALL_DETAILS_UPS.
*
*    SELECT PICKUP_DATE
*           STCOMPANY
*           STNAME
*           STADDR1
*           STCITY
*           STSTATE
*           STZIP
*           STCOUNTRY
*      TRACKINGNUMBER
**    pod_signature
**    pod_date
*       ACTUALWEIGHT
**    ratedweight
*      TOTAL_BILL_CHARG SERVICE_TYPE PAYMENT_METHOD
*     ADDR_CORRECTION ADDR_CORR_CHRG  ERRORCODE TOTAL_INC_CREDIT
**    total_charge
*      CARRIER
*              FROM ZPWEFA_INVC_UPS INTO CORRESPONDING FIELDS OF TABLE IT_FINAL_ALL_DETAILS_UPS
*              WHERE INVOICE_DATE EQ WEEK_DT AND CARRIER EQ ZCARRIERTYPE.
*
*    IF SY-SUBRC = 0.
*
**    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
**      EXPORTING
**        i_structure_name       = 'ZPWEFAINVOICE'
**      CHANGING
**        ct_fieldcat            = it_fieldcat8
**      EXCEPTIONS
**        inconsistent_interface = 1
**        program_error          = 2
**        OTHERS                 = 3.
**    IF sy-subrc <> 0.
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**    ENDIF.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 1.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'PICKUP_DATE'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Pickup Date'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 2.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STCOMPANY'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Company'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 3.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STNAME'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Name'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 4.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STADDR1'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Address'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
**    wa_fieldcat8-col_pos      = 5.
**    wa_fieldcat8-fieldname    = 'Consignee CITY'.
**    wa_fieldcat8-coltext    = 'Ship To City'.
**    wa_fieldcat8-tabname      = 'IT_FINALA_ALL_DETAILS'.
**    APPEND wa_fieldcat8 TO it_fieldcat8.
**    CLEAR wa_fieldcat8.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 5.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STSTATE'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To State'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 6.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STZIP'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Zip'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 7.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STCOUNTRY'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Country'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 8.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'TRACKINGNUMBER'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Tracking No'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-HOTSPOT    = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
**    wa_fieldcat_all_details-col_pos      = 9.
**    wa_fieldcat_all_details-fieldname    = 'POD_SIGNATURE'.
**    wa_fieldcat_all_details-coltext    = 'POD Signature'.
**    wa_fieldcat_all_details-tabname      = 'IT_FINAL_ALL_DETAILS'.
**    APPEND wa_fieldcat_all_details TO it_fieldcat_all_details.
**    CLEAR wa_fieldcat_all_details.
**
**    wa_fieldcat_all_details-col_pos      = 10.
**    wa_fieldcat_all_details-fieldname    = 'POD_DATE'.
**    wa_fieldcat_all_details-coltext    = 'POD Date'.
**    wa_fieldcat_all_details-tabname      = 'IT_FINAL_ALL_DETAILS'.
**    APPEND wa_fieldcat_all_details TO it_fieldcat_all_details.
**    CLEAR wa_fieldcat_all_details.
*
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 09.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'ACTUALWEIGHT'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Weight'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
**    wa_fieldcat_all_details_ups-col_pos      = 10.
**    wa_fieldcat_all_details_ups-fieldname    = 'RATEDWEIGHT'.
**    wa_fieldcat_all_details_ups-coltext    = 'Billed Weight'.
**    wa_fieldcat_all_details_ups-do_sum       = 'X'.
**    wa_fieldcat_all_details_ups-tabname      = 'IT_FINAL_ALL_DETAILS_UPS'.
**    APPEND wa_fieldcat_all_details_ups TO it_fieldcat_all_details_ups.
**    CLEAR wa_fieldcat_all_details_ups.
*
**    wa_fieldcat8-col_pos      = 14.
**    wa_fieldcat8-fieldname    = 'SUMCHARGE'.
**    wa_fieldcat8-coltext    = 'Original Charge'.
**    wa_fieldcat8-do_sum       = 'X'.
**    wa_fieldcat8-tabname      = 'IT_FINAL_ALL_DETAILS'.
**    APPEND wa_fieldcat8 TO it_fieldcat8.
**    CLEAR wa_fieldcat8.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 11.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'SERVICE_TYPE'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Service Type'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 12.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'PAYMENT_METHOD'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Payment Method'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 13.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'ADDR_CORRECTION'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Address Correction'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 14.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'ADDR_CORR_CHRG'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Address Correction Charge'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 15.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'ERRORCODE'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Error Description'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 16.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'TOTAL_INC_CREDIT'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Credit Amount'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 17.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'TOTAL_BILL_CHARG'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Total Net Due'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*      GS_LAYOUT-GRID_TITLE = SPACE.
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*     EXPORTING
*       I_STRUCTURE_NAME              = 'IT_FINAL_ALL_DETAILS_UPS'
**    IS_VARIANT                    =
**    I_SAVE                        =
**    I_DEFAULT                     = 'X'
*       IS_LAYOUT                     = GS_LAYOUT
*     CHANGING
*       IT_OUTTAB                     = IT_FINAL_ALL_DETAILS_UPS
*       IT_FIELDCATALOG               = IT_FIELDCAT_ALL_DETAILS_UPS
**    IT_SORT                       =
*   EXCEPTIONS
*     INVALID_PARAMETER_COMBINATION = 1
*     PROGRAM_ERROR                 = 2
*     TOO_MANY_LINES                = 3
*     OTHERS                        = 4  .
*      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*
*      SET HANDLER G_APPLICATION->HANDLE_HOTSPOT_CLICK FOR GO_GRID .
*
*
*
*    ENDIF.

  endif.
endform.                    "Alldetails


**************end of ALLDETAILS

*********Charge type summary report
*&---------------------------------------------------------------------*
*&      Form  GSR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form csr.

  clear : approve.
  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
  if wa_export_output-carrier = 'FEDEX'.
    data: total_amnt type netwr,
             v_amt(10) type c,
             total_savings type netwr,
             total_pkgs type i,
             total_weight type brgew_ap,
             avg_pkg_cst type netwr,
             avg_pnd_cst type netwr.


    data :  v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt.

    clear : v_packages,v_rated_weight,v_trans_charg,v_netcharge.
    refresh : it_csr_type,it_final_csr_type,it_fieldcat_csr_type,it_output_csr_type.
    clear : it_csr_type,wa_csr_type,wa_final_csr_type,wa_fieldcat_csr_type.
    select *
              from /PWEAVER/EFA_FED
              into corresponding fields of table it_csr_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.

    if sy-subrc = 0.
      clear : wa_csr_type.
      loop at it_csr_type into wa_csr_type.


*        ENDIF.

******Get the address correction

IF WA_CSR_TYPE-TRK_ID_CHG_DES = 'Address Correction'.
if sy-subrc = 0.
        WA_FINAL_CSR_TYPE-charg_desc = wa_csr_type-TRK_ID_CHG_DES.
        WA_FINAL_CSR_TYPE-PACKAGES = WA_CSR_TYPE-NUM_PIECES.
        WA_FINAL_CSR_TYPE-RATED_WEIGHT = WA_CSR_TYPE-RATED_WGT_AMT.
        WA_FINAL_CSR_TYPE-TRANSPORT_CHARGE = WA_CSR_TYPE-TRAN_CHARG_AMNT.

        WA_FINAL_CSR_TYPE-NET_CHRG_AMNT = WA_CSR_TYPE-NET_CHRG_AMNT.
endif.



        IF WA_CSR_TYPE-TRK_ID_CHG_DES1 = 'Address Correction'.
        WA_FINAL_CSR_TYPE-charg_desc = wa_csr_type-TRK_ID_CHG_DES1.
        WA_FINAL_CSR_TYPE-PACKAGES = WA_CSR_TYPE-NUM_PIECES.
        WA_FINAL_CSR_TYPE-RATED_WEIGHT = WA_CSR_TYPE-RATED_WGT_AMT.
        WA_FINAL_CSR_TYPE-TRANSPORT_CHARGE = WA_CSR_TYPE-TRAN_CHARG_AMNT.
        WA_FINAL_CSR_TYPE-NET_CHRG_AMNT = WA_CSR_TYPE-NET_CHRG_AMNT.
        ENDIF.

        if wa_csr_type-trk_id_chg_des = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt.
        endif.


        if wa_csr_type-trk_id_chg_des1 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des1.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt1.
        endif.



        if wa_csr_type-trk_id_chg_des2 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des2.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt2.
        endif.

        if wa_csr_type-trk_id_chg_des3 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des3.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt3.
        endif.

        if wa_csr_type-trk_id_chg_des4 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des4.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt4.
        endif.

        if wa_csr_type-trk_id_chg_des5 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des5.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt5.
        endif.

        if wa_csr_type-trk_id_chg_des6 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des6.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt6.
        endif.

        if wa_csr_type-trk_id_chg_des7 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des7.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt7.
        endif.

        if wa_csr_type-trk_id_chg_des8 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des8.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt8.
        endif.


        if wa_csr_type-trk_id_chg_des9 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des9.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt9.
        endif.


        if wa_csr_type-trk_id_chg_des10 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des10.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt10.
        endif.


        if wa_csr_type-trk_id_chg_des11 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des11.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt11.
        endif.


        if wa_csr_type-trk_id_chg_des12 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des12.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt12.
        endif.


        if wa_csr_type-trk_id_chg_des13 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des13.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt13.
        endif.


        if wa_csr_type-trk_id_chg_des14 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des14.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt14.
        endif.


        if wa_csr_type-trk_id_chg_des15 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des15.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt15.
        endif.


        if wa_csr_type-trk_id_chg_des16 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des16.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt16.
        endif.


        if wa_csr_type-trk_id_chg_des17 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des17.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt17.
        endif.


        if wa_csr_type-trk_id_chg_des18 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des18.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt18.
        endif.


        if wa_csr_type-trk_id_chg_des19 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des19.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt19.
        endif.

        if wa_csr_type-trk_id_chg_des20 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des20.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt20.
        endif.

        if wa_csr_type-trk_id_chg_des21 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des21.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt21.
        endif.

        if wa_csr_type-trk_id_chg_des22 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des22.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt22.
        endif.

        if wa_csr_type-trk_id_chg_des23 = 'Fuel Surcharge'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des23.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt23.
        endif.

        if wa_csr_type-trk_id_chg_des24 = 'Fuel Surcharge' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des24.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt24.
        endif.

        append wa_final_csr_type to it_final_csr_type.
        clear wa_final_csr_type.

**********Second

        if wa_csr_type-trk_id_chg_des = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt.
        endif.


        if wa_csr_type-trk_id_chg_des1 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des1.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt1.
        endif.



        if wa_csr_type-trk_id_chg_des2 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des2.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt2.
        endif.

        if wa_csr_type-trk_id_chg_des3 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des3.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt3.
        endif.

        if wa_csr_type-trk_id_chg_des4 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des4.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt4.
        endif.

        if wa_csr_type-trk_id_chg_des5 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des5.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt5.
        endif.

        if wa_csr_type-trk_id_chg_des6 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des6.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt6.
        endif.

        if wa_csr_type-trk_id_chg_des7 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des7.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt7.
        endif.

        if wa_csr_type-trk_id_chg_des8 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des8.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt8.
        endif.


        if wa_csr_type-trk_id_chg_des9 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des9.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt9.
        endif.


        if wa_csr_type-trk_id_chg_des10 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des10.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt10.
        endif.


        if wa_csr_type-trk_id_chg_des11 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des11.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt11.
        endif.


        if wa_csr_type-trk_id_chg_des12 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des12.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt12.
        endif.


        if wa_csr_type-trk_id_chg_des13 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des13.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt13.
        endif.


        if wa_csr_type-trk_id_chg_des14 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des14.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt14.
        endif.


        if wa_csr_type-trk_id_chg_des15 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des15.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt15.
        endif.


        if wa_csr_type-trk_id_chg_des16 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des16.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt16.
        endif.


        if wa_csr_type-trk_id_chg_des17 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des17.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt17.
        endif.


        if wa_csr_type-trk_id_chg_des18 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des18.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt18.
        endif.


        if wa_csr_type-trk_id_chg_des19 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des19.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt19.
        endif.

        if wa_csr_type-trk_id_chg_des20 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des20.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt20.
        endif.

        if wa_csr_type-trk_id_chg_des21 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des21.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt21.
        endif.

        if wa_csr_type-trk_id_chg_des22 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des22.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt22.
        endif.

        if wa_csr_type-trk_id_chg_des23 = 'Address Correction'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des23.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt23.
        endif.

        if wa_csr_type-trk_id_chg_des24 = 'Address Correction' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des24.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt24.
        endif.

        append wa_final_csr_type to it_final_csr_type.
        clear wa_final_csr_type.


***** Third one

        if wa_csr_type-trk_id_chg_des = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt.
        endif.


        if wa_csr_type-trk_id_chg_des1 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des1.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt1.
        endif.



        if wa_csr_type-trk_id_chg_des2 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des2.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt2.
        endif.

        if wa_csr_type-trk_id_chg_des3 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des3.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt3.
        endif.

        if wa_csr_type-trk_id_chg_des4 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des4.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt4.
        endif.

        if wa_csr_type-trk_id_chg_des5 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des5.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt5.
        endif.

        if wa_csr_type-trk_id_chg_des6 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des6.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt6.
        endif.

        if wa_csr_type-trk_id_chg_des7 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des7.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt7.
        endif.

        if wa_csr_type-trk_id_chg_des8 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des8.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt8.
        endif.


        if wa_csr_type-trk_id_chg_des9 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des9.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt9.
        endif.


        if wa_csr_type-trk_id_chg_des10 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des10.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt10.
        endif.


        if wa_csr_type-trk_id_chg_des11 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des11.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt11.
        endif.


        if wa_csr_type-trk_id_chg_des12 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des12.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt12.
        endif.


        if wa_csr_type-trk_id_chg_des13 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des13.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt13.
        endif.


        if wa_csr_type-trk_id_chg_des14 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des14.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt14.
        endif.


        if wa_csr_type-trk_id_chg_des15 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des15.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt15.
        endif.


        if wa_csr_type-trk_id_chg_des16 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des16.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt16.
        endif.


        if wa_csr_type-trk_id_chg_des17 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des17.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt17.
        endif.


        if wa_csr_type-trk_id_chg_des18 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des18.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt18.
        endif.


        if wa_csr_type-trk_id_chg_des19 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des19.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt19.
        endif.

        if wa_csr_type-trk_id_chg_des20 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des20.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt20.
        endif.

        if wa_csr_type-trk_id_chg_des21 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des21.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt21.
        endif.

        if wa_csr_type-trk_id_chg_des22 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des22.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt22.
        endif.

        if wa_csr_type-trk_id_chg_des23 = 'Weekly Service Chg'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des23.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt23.
        endif.

        if wa_csr_type-trk_id_chg_des24 = 'Weekly Service Chg' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des24.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt24.
        endif.

        append wa_final_csr_type to it_final_csr_type.
        clear wa_final_csr_type.

** **Fourth one

        if wa_csr_type-trk_id_chg_des = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt.
        endif.


        if wa_csr_type-trk_id_chg_des1 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des1.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt1.
        endif.



        if wa_csr_type-trk_id_chg_des2 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des2.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt2.
        endif.

        if wa_csr_type-trk_id_chg_des3 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des3.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt3.
        endif.

        if wa_csr_type-trk_id_chg_des4 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des4.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt4.
        endif.

        if wa_csr_type-trk_id_chg_des5 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des5.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt5.
        endif.

        if wa_csr_type-trk_id_chg_des6 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des6.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt6.
        endif.

        if wa_csr_type-trk_id_chg_des7 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des7.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt7.
        endif.

        if wa_csr_type-trk_id_chg_des8 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des8.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt8.
        endif.


        if wa_csr_type-trk_id_chg_des9 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des9.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt9.
        endif.


        if wa_csr_type-trk_id_chg_des10 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des10.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt10.
        endif.


        if wa_csr_type-trk_id_chg_des11 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des11.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt11.
        endif.


        if wa_csr_type-trk_id_chg_des12 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des12.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt12.
        endif.


        if wa_csr_type-trk_id_chg_des13 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des13.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt13.
        endif.


        if wa_csr_type-trk_id_chg_des14 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des14.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt14.
        endif.


        if wa_csr_type-trk_id_chg_des15 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des15.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt15.
        endif.


        if wa_csr_type-trk_id_chg_des16 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des16.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt16.
        endif.


        if wa_csr_type-trk_id_chg_des17 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des17.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt17.
        endif.


        if wa_csr_type-trk_id_chg_des18 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des18.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt18.
        endif.


        if wa_csr_type-trk_id_chg_des19 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des19.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt19.
        endif.

        if wa_csr_type-trk_id_chg_des20 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des20.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt20.
        endif.

        if wa_csr_type-trk_id_chg_des21 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des21.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt21.
        endif.

        if wa_csr_type-trk_id_chg_des22 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des22.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt22.
        endif.

        if wa_csr_type-trk_id_chg_des23 = 'Residential Delivery'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des23.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt23.
        endif.

        if wa_csr_type-trk_id_chg_des24 = 'Residential Delivery' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des24.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt24.
        endif.

        append wa_final_csr_type to it_final_csr_type.
        clear wa_final_csr_type.


*****Fifth One

        if wa_csr_type-trk_id_chg_des = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt.
        endif.


        if wa_csr_type-trk_id_chg_des1 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des1.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt1.
        endif.



        if wa_csr_type-trk_id_chg_des2 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des2.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt2.
        endif.

        if wa_csr_type-trk_id_chg_des3 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des3.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt3.
        endif.

        if wa_csr_type-trk_id_chg_des4 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des4.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt4.
        endif.

        if wa_csr_type-trk_id_chg_des5 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des5.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt5.
        endif.

        if wa_csr_type-trk_id_chg_des6 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des6.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt6.
        endif.

        if wa_csr_type-trk_id_chg_des7 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des7.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt7.
        endif.

        if wa_csr_type-trk_id_chg_des8 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des8.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt8.
        endif.


        if wa_csr_type-trk_id_chg_des9 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des9.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt9.
        endif.


        if wa_csr_type-trk_id_chg_des10 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des10.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt10.
        endif.


        if wa_csr_type-trk_id_chg_des11 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des11.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt11.
        endif.


        if wa_csr_type-trk_id_chg_des12 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des12.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt12.
        endif.


        if wa_csr_type-trk_id_chg_des13 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des13.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt13.
        endif.


        if wa_csr_type-trk_id_chg_des14 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des14.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt14.
        endif.


        if wa_csr_type-trk_id_chg_des15 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des15.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt15.
        endif.


        if wa_csr_type-trk_id_chg_des16 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des16.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt16.
        endif.


        if wa_csr_type-trk_id_chg_des17 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des17.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt17.
        endif.


        if wa_csr_type-trk_id_chg_des18 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des18.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt18.
        endif.


        if wa_csr_type-trk_id_chg_des19 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des19.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt19.
        endif.

        if wa_csr_type-trk_id_chg_des20 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des20.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt20.
        endif.

        if wa_csr_type-trk_id_chg_des21 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des21.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt21.
        endif.

        if wa_csr_type-trk_id_chg_des22 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des22.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt22.
        endif.

        if wa_csr_type-trk_id_chg_des23 = 'Residential'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des23.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt23.
        endif.

        if wa_csr_type-trk_id_chg_des24 = 'Residential' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des24.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt24.
        endif.

        append wa_final_csr_type to it_final_csr_type.
        clear wa_final_csr_type.


*****Sixth One

        if wa_csr_type-trk_id_chg_des = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt.
        endif.


        if wa_csr_type-trk_id_chg_des1 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt1.
        endif.



        if wa_csr_type-trk_id_chg_des2 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des2.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt2.
        endif.

        if wa_csr_type-trk_id_chg_des3 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des3.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt3.
        endif.

        if wa_csr_type-trk_id_chg_des4 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des4.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt4.
        endif.

        if wa_csr_type-trk_id_chg_des5 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des5.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt5.
        endif.

        if wa_csr_type-trk_id_chg_des6 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des6.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt6.
        endif.

        if wa_csr_type-trk_id_chg_des7 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des7.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt7.
        endif.

        if wa_csr_type-trk_id_chg_des8 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des8.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt8.
        endif.


        if wa_csr_type-trk_id_chg_des9 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des9.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt9.
        endif.


        if wa_csr_type-trk_id_chg_des10 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des10.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt10.
        endif.


        if wa_csr_type-trk_id_chg_des11 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des11.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt11.
        endif.


        if wa_csr_type-trk_id_chg_des12 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des12.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt12.
        endif.


        if wa_csr_type-trk_id_chg_des13 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des13.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt13.
        endif.


        if wa_csr_type-trk_id_chg_des14 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des14.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt14.
        endif.


        if wa_csr_type-trk_id_chg_des15 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des15.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt15.
        endif.


        if wa_csr_type-trk_id_chg_des16 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des16.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt16.
        endif.


        if wa_csr_type-trk_id_chg_des17 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des17.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt17.
        endif.


        if wa_csr_type-trk_id_chg_des18 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des18.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt18.
        endif.


        if wa_csr_type-trk_id_chg_des19 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des19.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt19.
        endif.

        if wa_csr_type-trk_id_chg_des20 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des20.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt20.
        endif.

        if wa_csr_type-trk_id_chg_des21 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des21.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt21.
        endif.

        if wa_csr_type-trk_id_chg_des22 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des22.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt22.
        endif.

        if wa_csr_type-trk_id_chg_des23 = 'DAS Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des23.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt23.
        endif.

        if wa_csr_type-trk_id_chg_des24 = 'DAS Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des24.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt24.
        endif.

        append wa_final_csr_type to it_final_csr_type.
        clear wa_final_csr_type.

******SEventh one

        if wa_csr_type-trk_id_chg_des = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt.
        endif.


        if wa_csr_type-trk_id_chg_des1 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des1.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt1.
        endif.



        if wa_csr_type-trk_id_chg_des2 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des2.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt2.
        endif.

        if wa_csr_type-trk_id_chg_des3 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des3.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt3.
        endif.

        if wa_csr_type-trk_id_chg_des4 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des4.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt4.
        endif.

        if wa_csr_type-trk_id_chg_des5 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des5.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt5.
        endif.

        if wa_csr_type-trk_id_chg_des6 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des6.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt6.
        endif.

        if wa_csr_type-trk_id_chg_des7 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des7.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt7.
        endif.

        if wa_csr_type-trk_id_chg_des8 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des8.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt8.
        endif.


        if wa_csr_type-trk_id_chg_des9 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des9.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt9.
        endif.


        if wa_csr_type-trk_id_chg_des10 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des10.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt10.
        endif.


        if wa_csr_type-trk_id_chg_des11 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des11.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt11.
        endif.


        if wa_csr_type-trk_id_chg_des12 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des12.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt12.
        endif.


        if wa_csr_type-trk_id_chg_des13 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des13.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt13.
        endif.


        if wa_csr_type-trk_id_chg_des14 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des14.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt14.
        endif.


        if wa_csr_type-trk_id_chg_des15 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des15.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt15.
        endif.


        if wa_csr_type-trk_id_chg_des16 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des16.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt16.
        endif.


        if wa_csr_type-trk_id_chg_des17 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des17.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt17.
        endif.


        if wa_csr_type-trk_id_chg_des18 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des18.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt18.
        endif.


        if wa_csr_type-trk_id_chg_des19 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des19.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt19.
        endif.

        if wa_csr_type-trk_id_chg_des20 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des20.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt20.
        endif.

        if wa_csr_type-trk_id_chg_des21 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des21.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt21.
        endif.

        if wa_csr_type-trk_id_chg_des22 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des22.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt22.
        endif.

        if wa_csr_type-trk_id_chg_des23 = 'DAS Extended Comm'.
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des23.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt23.
        endif.

        if wa_csr_type-trk_id_chg_des24 = 'DAS Extended Comm' .
          wa_final_csr_type-charg_desc = wa_csr_type-trk_id_chg_des24.
          wa_final_csr_type-packages = wa_csr_type-num_pieces.
          wa_final_csr_type-rated_weight = wa_csr_type-rated_wgt_amt.
          wa_final_csr_type-transport_charge = wa_csr_type-tran_charg_amnt.
          wa_final_csr_type-handling_charge =  wa_final_csr_type-handling_charge + wa_csr_type-trk_id_chg_amt24.
        endif.

        append wa_final_csr_type to it_final_csr_type.
        clear :wa_final_csr_type,wa_csr_type.



endif.
      endloop.

      sort it_final_csr_type by charg_desc.
      clear wa_final_csr_type.


      loop at it_final_csr_type into wa_final_csr_type.



        if not wa_final_csr_type is initial.

          at end of charg_desc.
            sum.     " calculates sub totals on CSRice types

            wa_output_csr_type-charg_desc = wa_final_csr_type-charg_desc.
            wa_output_csr_type-packages =  wa_final_csr_type-packages.
            wa_output_csr_type-rated_weight = wa_final_csr_type-rated_weight.
            wa_output_csr_type-transport_charge = wa_final_csr_type-transport_charge.
            wa_output_csr_type-handling_charge = wa_final_csr_type-handling_charge.




* calculate the grand totals

*v_CSRice = v_CSRice + wa_output_CSR_type-CSRice_type.
*            V_PACKAGES = V_PACKAGES + WA_OUTPUT_CSR_TYPE-PACKAGES.
*            V_RATED_WEIGHT = V_RATED_WEIGHT + WA_OUTPUT_CSR_TYPE-RATED_WEIGHT.
*
*            V_TRANS_CHARG = V_TRANS_CHARG + WA_OUTPUT_CSR_TYPE-TRANSPORT_CHARGE.


            v_hand_charg = v_hand_charg + wa_output_csr_type-handling_charge.
            v_packages = v_packages + wa_output_csr_type-packages.
            v_rated_weight = v_rated_weight + wa_final_csr_type-rated_weight.
            v_trans_charg = v_trans_charg + wa_final_csr_type-transport_charge.


            append wa_output_csr_type to it_output_csr_type.
            clear wa_output_csr_type.
          endat.

        endif.
        clear wa_final_csr_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_csr_type.


      wa_output_csr_type-handling_charge = v_hand_charg .
      wa_output_csr_type-packages =  v_packages .
      wa_output_csr_type-rated_weight = v_rated_weight .
      wa_output_csr_type-transport_charge = v_trans_charg.

      wa_output_csr_type-line_color      = 'C500'.
      append wa_output_csr_type to it_output_csr_type.
      clear wa_output_csr_type.



      wa_fieldcat_csr_type-col_pos      = 1.
      wa_fieldcat_csr_type-fieldname    = 'CHARG_DESC'.
      wa_fieldcat_csr_type-coltext    = 'Charge Description'.
      wa_fieldcat_csr_type-tabname      = 'IT_OUPUT_CSR_TYPE'.
      append wa_fieldcat_csr_type to it_fieldcat_csr_type.
      clear wa_fieldcat_csr_type.

      wa_fieldcat_csr_type-col_pos      = 2.
      wa_fieldcat_csr_type-fieldname    = 'PACKAGES'.
      wa_fieldcat_csr_type-coltext    = 'Total Packages'.
      wa_fieldcat_csr_type-tabname      = 'IT_OUPUT_CSR_TYPE'.
      append wa_fieldcat_csr_type to it_fieldcat_csr_type.
      clear wa_fieldcat_csr_type.

      wa_fieldcat_csr_type-col_pos      = 3.
      wa_fieldcat_csr_type-fieldname    = 'RATED_WEIGHT'.
      wa_fieldcat_csr_type-coltext    = 'Rated Weight'.
      wa_fieldcat_csr_type-tabname      = 'IT_OUPUT_CSR_TYPE'.
      append wa_fieldcat_csr_type to it_fieldcat_csr_type.
      clear wa_fieldcat_csr_type.

      wa_fieldcat_csr_type-col_pos      = 4.
      wa_fieldcat_csr_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_csr_type-coltext    = 'Transport Charge'.
      wa_fieldcat_csr_type-tabname      = 'IT_OUPUT_CSR_TYPE'.
      append wa_fieldcat_csr_type to it_fieldcat_csr_type.
      clear wa_fieldcat_csr_type.

      wa_fieldcat_csr_type-col_pos      = 5.
      wa_fieldcat_csr_type-fieldname    = 'HANDLING_CHARGE'.
      wa_fieldcat_csr_type-coltext    = 'Charge Amount'.
      wa_fieldcat_csr_type-tabname      = 'IT_OUPUT_CSR_TYPE'.
      append wa_fieldcat_csr_type to it_fieldcat_csr_type.
      clear wa_fieldcat_csr_type.




      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUPUT_CSR_TYPE'
*          is_layout                     = gs_layout
          is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
        changing
          it_outtab                     = it_output_csr_type
          it_fieldcatalog               = it_fieldcat_csr_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.



    endif.
  endif.

  if ups = 'X'.


*    REFRESH: IT_FINAL_GSR_UPS, IT_FIELDCAT_GSR_UPS,IT_OUTPUT_GSR_UPS.
*    CLEAR: WA_FIELDCAT_GSR_UPS, WA_OUTPUT_GSR_UPS, WA_FINAL_GSR_UPS, GS_LAYOUT.
*
*
*    SELECT  * FROM ZPWEFA_INVC_UPS
*              INTO CORRESPONDING FIELDS OF TABLE IT_OUTPUT_GSR_UPS
*              WHERE INVOICE_DATE EQ WEEK_DT AND TOTAL_INC_CREDIT NE 0 AND CARRIER EQ ZCARRIERTYPE .
*
*    IF SY-SUBRC EQ 0.
*
*      SORT IT_OUTPUT_GSR_UPS BY SERVICE_TYPE.
*      CLEAR WA_FINAL_GSR_UPS.
*
*
*      WA_FIELDCAT_GSR_UPS-COL_POS      = 1.
*      WA_FIELDCAT_GSR_UPS-FIELDNAME    = 'INVOICE_NO'.
*      WA_FIELDCAT_GSR_UPS-COLTEXT    = 'Invoice#'.
*      WA_FIELDCAT_GSR_UPS-TABNAME      = 'IT_OUTPUT_GSR_UPS'.
*      APPEND WA_FIELDCAT_GSR_UPS TO IT_FIELDCAT_GSR_UPS.
*      CLEAR WA_FIELDCAT_GSR_UPS.
*
*      WA_FIELDCAT_GSR_UPS-COL_POS      = 2.
*      WA_FIELDCAT_GSR_UPS-FIELDNAME    = 'SERVICE_TYPE'.
*      WA_FIELDCAT_GSR_UPS-COLTEXT    = 'Service Type'.
*      WA_FIELDCAT_GSR_UPS-TABNAME      = 'IT_OUTPUT_GSR_UPS'.
*      APPEND WA_FIELDCAT_GSR_UPS TO IT_FIELDCAT_GSR_UPS.
*      CLEAR WA_FIELDCAT_GSR_UPS.
*
*      WA_FIELDCAT_GSR_UPS-COL_POS      = 3.
*      WA_FIELDCAT_GSR_UPS-FIELDNAME    = 'PICKUP_DATE'.
*      WA_FIELDCAT_GSR_UPS-COLTEXT    = 'Pickup Date'.
*      WA_FIELDCAT_GSR_UPS-TABNAME      = 'IT_OUTPUT_GSR_UPS'.
*      APPEND WA_FIELDCAT_GSR_UPS TO IT_FIELDCAT_GSR_UPS.
*      CLEAR WA_FIELDCAT_GSR_UPS.
*
*      WA_FIELDCAT_GSR_UPS-COL_POS      = 4.
*      WA_FIELDCAT_GSR_UPS-FIELDNAME    = 'TRACKINGNUMBER'.
*      WA_FIELDCAT_GSR_UPS-COLTEXT    = 'Tracking No.'.
*      WA_FIELDCAT_GSR_UPS-TABNAME      = 'IT_OUTPUT_GSR_UPS'.
*      WA_FIELDCAT_GSR_UPS-HOTSPOT    =  'X'.
*      APPEND WA_FIELDCAT_GSR_UPS TO IT_FIELDCAT_GSR_UPS.
*      CLEAR WA_FIELDCAT_GSR_UPS.
*
**  wa_fieldcat_gsr-col_pos      = 5.
**  wa_fieldcat_gsr-fieldname    = 'POD'.
**  wa_fieldcat_gsr-coltext    = 'POD'.
**  wa_fieldcat_gsr-tabname      = 'IT_OUTPUT_GSR'.
**  APPEND wa_fieldcat_gsr TO it_fieldcat_gsr.
**  CLEAR wa_fieldcat_gsr.
**
**  wa_fieldcat_gsr-col_pos      = 6.
**  wa_fieldcat_gsr-fieldname    = 'PACKAGESIGNATURE'.
**  wa_fieldcat_gsr-coltext    = 'POD Signature'.
**  wa_fieldcat_gsr-tabname      = 'IT_OUTPUT_GSR'.
**  APPEND wa_fieldcat_gsr TO it_fieldcat_gsr.
**  CLEAR wa_fieldcat_gsr.
*
*      WA_FIELDCAT_GSR_UPS-COL_POS      = 5.
*      WA_FIELDCAT_GSR_UPS-FIELDNAME    = 'SUMCHARGE'.
*      WA_FIELDCAT_GSR_UPS-COLTEXT    = 'Original Amount'.
*      WA_FIELDCAT_GSR_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_GSR_UPS-TABNAME      = 'IT_OUTPUT_GSR_UPS'.
*      APPEND WA_FIELDCAT_GSR_UPS TO IT_FIELDCAT_GSR_UPS.
*      CLEAR WA_FIELDCAT_GSR_UPS.
*
*      WA_FIELDCAT_GSR_UPS-COL_POS      = 6.
*      WA_FIELDCAT_GSR_UPS-FIELDNAME    = 'TOTAL_INC_CREDIT'.
*      WA_FIELDCAT_GSR_UPS-COLTEXT    = 'Credit Amount'.
*      WA_FIELDCAT_GSR_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_GSR_UPS-TABNAME      = 'IT_OUTPUT_GSR_UPS'.
*      APPEND WA_FIELDCAT_GSR_UPS TO IT_FIELDCAT_GSR_UPS.
*      CLEAR WA_FIELDCAT_GSR_UPS.
*
*      WA_FIELDCAT_GSR_UPS-COL_POS      = 7.
*      WA_FIELDCAT_GSR_UPS-FIELDNAME    = 'TOTAL_BILL_CHARG'.
*      WA_FIELDCAT_GSR_UPS-COLTEXT    = 'Net Due'.
*      WA_FIELDCAT_GSR_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_GSR_UPS-TABNAME      = 'IT_OUTPUT_GSR_UPS'.
*      APPEND WA_FIELDCAT_GSR_UPS TO IT_FIELDCAT_GSR_UPS.
*      CLEAR WA_FIELDCAT_GSR_UPS.
*
*      WA_FIELDCAT_GSR_UPS-COL_POS      = 8.
*      WA_FIELDCAT_GSR_UPS-FIELDNAME    = 'ERRORCODE'.
*      WA_FIELDCAT_GSR_UPS-COLTEXT    = 'Error Code Description'.
*      WA_FIELDCAT_GSR_UPS-TABNAME      = 'IT_OUTPUT_GSR_UPS'.
*      APPEND WA_FIELDCAT_GSR_UPS TO IT_FIELDCAT_GSR_UPS.
*      CLEAR WA_FIELDCAT_GSR_UPS.
*
*
*      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*      GS_LAYOUT-GRID_TITLE = SPACE.
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*     EXPORTING
*       I_STRUCTURE_NAME              = 'IT_OUTPUT_GSR_UPS'
**    IS_VARIANT                    =
**    I_SAVE                        =
**    I_DEFAULT                     = 'X'
*       IS_LAYOUT                     = GS_LAYOUT
*     CHANGING
*       IT_OUTTAB                     = IT_OUTPUT_GSR_UPS
*       IT_FIELDCATALOG               = IT_FIELDCAT_GSR_UPS
**    IT_SORT                       =
*    EXCEPTIONS
*     INVALID_PARAMETER_COMBINATION = 1
*     PROGRAM_ERROR                 = 2
*     TOO_MANY_LINES                = 3
*     OTHERS                        = 4  .
*      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*      SET HANDLER G_APPLICATION->HANDLE_HOTSPOT_CLICK FOR GO_GRID .
*
*
*
*
*    ENDIF.

  endif.

endform.                    "GSR









*******End of Charge type summary report



*********Address Correction **********


*&---------------------------------------------------------------------*
*&      Form  INCORRETADDR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form incorretaddr.
*data :v1(2),v2(2),v3(4) type c.
*data :  c type wa_addcorr_type-shipment_date,
*date(11) type c.
*c = wa_addcorr_type-shipment_date.
  clear : approve.
  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
  if wa_export_output-carrier = 'FEDEX'.

    data : t_hype_link type lvc_t_hype,
           fs_hype_link type lvc_s_hype.

    data :v_addr_corr type /PWEAVER/EFA_FED-net_chrg_amnt,
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt.

    clear : v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_addcorr_type,it_final_addcorr_type,it_fieldcat_addcorr_type,it_output_addcorr_type.
    clear : it_addcorr_type,wa_addcorr_type,wa_final_addcorr_type,wa_fieldcat_addcorr_type.

    select *
              from /PWEAVER/EFA_FED
              into corresponding fields of table it_addcorr_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.

    if sy-subrc = 0.
      clear : wa_addcorr_type.
      loop at it_addcorr_type into wa_addcorr_type.

*concatenate wa_final_addcorr_type-shipment_date+0(4) wa_final_addcorr_type-shipment_date+4(2)  wa_final_addcorr_type-shipment_date+6(2)  into wa_final_addcorr_type-shipment_date.
        wa_final_addcorr_type-shipment_date  = wa_addcorr_type-shipment_date.
        wa_final_addcorr_type-service_type  = wa_addcorr_type-service_type.
        wa_final_addcorr_type-exp_grd_trck_id  = wa_addcorr_type-exp_grd_trck_id.
        wa_final_addcorr_type-org_cust_ref  = wa_addcorr_type-org_cust_ref.
        wa_final_addcorr_type-org_ref_3  = wa_addcorr_type-org_ref_3.
*        WA_FINAL_ADDCORR_TYPE-POD  = WA_ADDCORR_TYPE-POD.
        wa_final_addcorr_type-pod_del_date  = wa_addcorr_type-pod_del_date.
        wa_final_addcorr_type-pod_del_time  = wa_addcorr_type-pod_del_time.
        wa_final_addcorr_type-pod_sign_desc  = wa_addcorr_type-pod_sign_desc.
        wa_final_addcorr_type-transport_charge = wa_addcorr_type-tran_charg_amnt.
        wa_final_addcorr_type-net_charge  = wa_addcorr_type-net_chrg_amnt.

        wa_final_addcorr_type-recipeint_name = wa_addcorr_type-recipeint_name.
        wa_final_addcorr_type-recipeint_comp = wa_addcorr_type-recipeint_comp.
        wa_final_addcorr_type-recp_addr_line1 = wa_addcorr_type-recp_addr_line1.
        wa_final_addcorr_type-recp_addr_line2 = wa_addcorr_type-recp_addr_line2.
        wa_final_addcorr_type-recipient_city = wa_addcorr_type-recipient_city.
        wa_final_addcorr_type-recipient_state = wa_addcorr_type-recipient_state.
        wa_final_addcorr_type-rec_zip_code = wa_addcorr_type-rec_zip_code.
        wa_final_addcorr_type-rec_country = wa_addcorr_type-rec_country.


*        ENDIF.

****Handling charges

        if wa_addcorr_type-trk_id_chg_des = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =  wa_addcorr_type-trk_id_chg_amt.
        endif.

        if wa_addcorr_type-trk_id_chg_des1 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt1.
        endif.

        if wa_addcorr_type-trk_id_chg_des2 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt2.
        endif.

        if wa_addcorr_type-trk_id_chg_des3 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt3.
        endif.

        if wa_addcorr_type-trk_id_chg_des4 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt4.
        endif.

        if wa_addcorr_type-trk_id_chg_des5 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt5.
        endif.

        if wa_addcorr_type-trk_id_chg_des6 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt6.
        endif.

        if wa_addcorr_type-trk_id_chg_des7 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt7.
        endif.

        if wa_addcorr_type-trk_id_chg_des8 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt8.
        endif.

        if wa_addcorr_type-trk_id_chg_des9 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt9.
        endif.


        if wa_addcorr_type-trk_id_chg_des10 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt10.
        endif.

        if wa_addcorr_type-trk_id_chg_des11 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt11.
        endif.

        if wa_addcorr_type-trk_id_chg_des12 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt12.
        endif.

        if wa_addcorr_type-trk_id_chg_des13 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt13.
        endif.


        if wa_addcorr_type-trk_id_chg_des14 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt14.
        endif.

        if wa_addcorr_type-trk_id_chg_des15 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt15.
        endif.


        if wa_addcorr_type-trk_id_chg_des16 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt16.
        endif.

        if wa_addcorr_type-trk_id_chg_des17 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt17.
        endif.

        if wa_addcorr_type-trk_id_chg_des18 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt18.
        endif.

        if wa_addcorr_type-trk_id_chg_des19 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt19.
        endif.

        if wa_addcorr_type-trk_id_chg_des20 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt20.
        endif.

        if wa_addcorr_type-trk_id_chg_des21 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt21.
        endif.

        if wa_addcorr_type-trk_id_chg_des22 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt22.
        endif.

        if wa_addcorr_type-trk_id_chg_des23 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt23.
        endif.

        if wa_addcorr_type-trk_id_chg_des24 = 'Address Correction'.
          wa_final_addcorr_type-addr_charge =   wa_final_addcorr_type-addr_charge + wa_addcorr_type-trk_id_chg_amt24.
        endif.




        append wa_final_addcorr_type to it_final_addcorr_type.
        clear :wa_final_addcorr_type,wa_addcorr_type.
      endloop.


      sort it_final_addcorr_type by service_type.
      clear wa_final_addcorr_type.


      loop at it_final_addcorr_type into wa_final_addcorr_type.


*      DATA: day TYPE C,
*        mon TYPE C,
*        year TYPE C,
*      v_date(10) type c.
*      split  wa_final_addcorr_type-pod_del_date at '.' into day mon year .
*
**      day = wa_final_addcorr_type-shipment_date+0(2).
**mon = wa_final_addcorr_type-shipment_date+3(2).
**year = wa_final_addcorr_type-shipment_date+6(4).
*
*
*concatenate  day'/'mon'/'year  into wa_final_addcorr_type-shipment_date.

        if wa_final_addcorr_type-addr_charge <> '0.00'.


data: day(2) type c,
      month(2) type c,
      year(4) type c.
*******************Address correction
**          concatenate wa_final_addcorr_type-shipment_date+0(4)'/' wa_final_addcorr_type-shipment_date+4(2) '/' wa_final_addcorr_type-shipment_date+6(2)  into wa_final_addcorr_type-shipment_date.
**           concatenate wa_final_addcorr_type-shipment_date+0(2)'/' wa_final_addcorr_type-shipment_date+2(2) '/' wa_final_addcorr_type-shipment_date+4(4)  into  wa_final_addcorr_type-shipment_date.
*           concatenate wa_final_addcorr_type-shipment_date+0(4) wa_final_addcorr_type-shipment_date+4(2)  wa_final_addcorr_type-shipment_date+6(2)  into wa_final_addcorr_type-shipment_date.
**
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
    INPUT         = wa_final_addcorr_type-shipment_date
 IMPORTING
  OUTPUT        = wa_final_addcorr_type-shipment_date.
day = wa_final_addcorr_type-shipment_date+6(2).

month = wa_final_addcorr_type-shipment_date+4(2).
year = wa_final_addcorr_type-shipment_date+0(4).

clear wa_final_addcorr_type-shipment_date.
concatenate month '.'day'.'year into wa_final_addcorr_type-shipment_date.
*          .

replace all occurrences of '.' in  wa_final_addcorr_type-shipment_date with '/'.

*   split  wa_final_addcorr_type-pod_del_date at '.' into v1 v2 v3 .
*concatenate  v1'/' v2 '/' v3  into wa_final_addcorr_type-pod_del_date.
*concatenate wa_final_addcorr_type-shipment_date+0(4) wa_final_addcorr_type-shipment_date+4(2) wa_final_addcorr_type-shipment_date+6(2) into wa_final_addcorr_type-shipment_date.
    wa_output_addcorr_type-shipment_date                   = wa_final_addcorr_type-shipment_date.

*          wa_output_addcorr_type-shipment_date                   = wa_final_addcorr_type-shipment_date.
          wa_output_addcorr_type-service_type      =  wa_final_addcorr_type-service_type.
          wa_output_addcorr_type-exp_grd_trck_id      = wa_final_addcorr_type-exp_grd_trck_id.
          wa_output_addcorr_type-org_cust_ref       = wa_final_addcorr_type-org_cust_ref.
          wa_output_addcorr_type-org_ref_3        = wa_final_addcorr_type-org_ref_3.
          wa_output_addcorr_type-pod        = 'Proof of Delivery'.
          data : v_address(8) type c.
          clear : v_address.
*          concatenate wa_final_addcorr_type-pod_del_date+0(4) wa_final_addcorr_type-pod_del_date+4(2) wa_final_addcorr_type-pod_del_date+6(2) into wa_final_addcorr_type-shipment_date.
*split  wa_final_addcorr_type-pod_del_date at '.' into v1 v2 v3 .
*concatenate  v1'/' v2 '/' v3  into wa_final_addcorr_type-pod_del_date.
*          CONCATENATE WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE+4(2)'-' WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE+6(2) '-' WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE+0(4)  INTO WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE.

if WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE = '0'.

  WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE = ''.

else.

CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
    INPUT         = WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE
 IMPORTING
  OUTPUT        = WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE.


day = WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE+6(2).
month = WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE+4(2).
year = WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE+0(4).


clear WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE.
concatenate month '.'day'.'year into WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE.
replace all occurrences of '.' in  wa_final_addcorr_type-POD_DEL_DATE with '/'.
 WA_OUTPUT_ADDCORR_TYPE-POD_DEL_DATE        = WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE.
endif.
 WA_OUTPUT_ADDCORR_TYPE-POD_DEL_DATE        = WA_FINAL_ADDCORR_TYPE-POD_DEL_DATE.


*          wa_output_addcorr_type-pod_del_date        = v_address.
        data:  hour(2) type c,
               min(2) type c,
               sec(2) type c,
               time type t.

*OVERLAY wa_final_addcorr_type-pod_del_time WITH time.
  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    INPUT         = wa_final_addcorr_type-pod_del_time
 IMPORTING
  OUTPUT        = wa_final_addcorr_type-pod_del_time.

         if wa_final_addcorr_type-pod_del_time = '0'.
           wa_final_addcorr_type-pod_del_time = '00:00:00'.
           else.

        hour = wa_final_addcorr_type-pod_del_time+2(2).
        min = wa_final_addcorr_type-pod_del_time+4(2).
        sec = wa_final_addcorr_type-pod_del_time+6(2).


         concatenate hour':'min':'sec':' into wa_final_addcorr_type-pod_del_time.

         endif.

          wa_output_addcorr_type-pod_del_time        = wa_final_addcorr_type-pod_del_time.
          wa_output_addcorr_type-pod_sign_desc        = wa_final_addcorr_type-pod_sign_desc.
          wa_output_addcorr_type-transport_charge        = wa_final_addcorr_type-transport_charge.
          wa_output_addcorr_type-net_charge        = wa_final_addcorr_type-net_charge.
          wa_output_addcorr_type-recipeint_name        = wa_final_addcorr_type-recipeint_name.
          wa_output_addcorr_type-recipeint_comp        = wa_final_addcorr_type-recipeint_comp.
          wa_output_addcorr_type-recp_addr_line1        = wa_final_addcorr_type-recp_addr_line1.
          wa_output_addcorr_type-recp_addr_line2       = wa_final_addcorr_type-recp_addr_line2.
          wa_output_addcorr_type-recipient_city        = wa_final_addcorr_type-recipient_city.
          wa_output_addcorr_type-recipient_state        = wa_final_addcorr_type-recipient_state.
          wa_output_addcorr_type-rec_zip_code       = wa_final_addcorr_type-rec_zip_code.
          wa_output_addcorr_type-rec_country       = wa_final_addcorr_type-rec_country.
          wa_output_addcorr_type-addr_charge =  wa_final_addcorr_type-addr_charge.


* calculate the grand totals
          v_trans_charg = v_trans_charg + wa_output_addcorr_type-transport_charge.
          v_addr_corr = v_hand_charg + wa_output_addcorr_type-addr_charge.

          v_netcharge = v_netcharge +  wa_output_addcorr_type-net_charge .
          append wa_output_addcorr_type to it_output_addcorr_type.
          clear wa_output_addcorr_type.
*          ENDAT.
        endif.
        clear wa_final_addcorr_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_addcorr_type.

*    wa_output_serv_type-service_type = v_service.
*if v_trans_charg is not initial or v_addr_corr is not initial or v_netcharge is not INITIAL.


      wa_output_addcorr_type-transport_charge = v_trans_charg.
      wa_output_addcorr_type-addr_charge =  v_addr_corr .
      wa_output_addcorr_type-net_charge = v_netcharge.

      wa_output_addcorr_type-line_color      = 'C500'.
      append wa_output_addcorr_type to it_output_addcorr_type.
      clear wa_output_addcorr_type.
*      endif.

      wa_fieldcat_addcorr_type-col_pos      = 1.
      wa_fieldcat_addcorr_type-fieldname    = 'SHIPMENT_DATE'.
      wa_fieldcat_addcorr_type-coltext    = 'Shipment Date'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_ADDCORR_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 2.
      wa_fieldcat_addcorr_type-fieldname    = 'SERVICE_TYPE'.
      wa_fieldcat_addcorr_type-coltext    = 'Service Type'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_ADDCORR_TYPE'.
      wA_fieldcat_ADDCORR_TYPE-OUTPUTLEN = '35'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 3.
      wa_fieldcat_addcorr_type-fieldname    = 'EXP_GRD_TRCK_ID'.
      wa_fieldcat_addcorr_type-coltext    = 'Tracking Number'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      wa_fieldcat_addcorr_type-hotspot   = 'x'.
      wA_fieldcat_ADDCORR_TYPE-OUTPUTLEN = '35'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 4.
      wa_fieldcat_addcorr_type-fieldname    = 'ORG_CUST_REF'.
      wa_fieldcat_addcorr_type-coltext    = 'Reference #'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 5.
      wa_fieldcat_addcorr_type-fieldname    = 'ORG_REF_3'.
      wa_fieldcat_addcorr_type-coltext    = 'PO #'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 6.
      wa_fieldcat_addcorr_type-fieldname    = 'POD '.
      wa_fieldcat_addcorr_type-coltext    = 'Proof of Delivery'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      wa_fieldcat_addcorr_type-hotspot   = 'x'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 7.
      wa_fieldcat_addcorr_type-fieldname    = 'POD_DEL_DATE'.
      wa_fieldcat_addcorr_type-coltext    = 'POD Date'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 8.
      wa_fieldcat_addcorr_type-fieldname    = 'POD_DEL_TIME'.
      wa_fieldcat_addcorr_type-coltext    = 'POD Time'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 9.
      wa_fieldcat_addcorr_type-fieldname    = 'POD_SIGN_DESC'.
      wa_fieldcat_addcorr_type-coltext    = 'POD Signature'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.



      wa_fieldcat_addcorr_type-col_pos      = 10.
      wa_fieldcat_addcorr_type-fieldname    = 'RECIPEINT_NAME'.
      wa_fieldcat_addcorr_type-coltext    = 'Recipeint Name'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.
      wa_fieldcat_addcorr_type-col_pos      = 11.
      wa_fieldcat_addcorr_type-fieldname    = 'RECIPEINT_COMP'.
      wa_fieldcat_addcorr_type-coltext    = 'Recipeint Company'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 12.
      wa_fieldcat_addcorr_type-fieldname    = 'RECP_ADDR_LINE1'.
      wa_fieldcat_addcorr_type-coltext    = 'Recipeint Address'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 13.
      wa_fieldcat_addcorr_type-fieldname    = 'RECP_ADDR_LINE2'.
      wa_fieldcat_addcorr_type-coltext    = 'Recipeint Address'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 14.
      wa_fieldcat_addcorr_type-fieldname    = 'RECIPIENT_CITY '.
      wa_fieldcat_addcorr_type-coltext    = 'Recipeint City'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 15.
      wa_fieldcat_addcorr_type-fieldname    = 'RECIPIENT_STATE'.
      wa_fieldcat_addcorr_type-coltext    = 'Recipeint State'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 16.
      wa_fieldcat_addcorr_type-fieldname    = 'REC_ZIP_CODE'.
      wa_fieldcat_addcorr_type-coltext    = 'Recipient Zipcode'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 17.
      wa_fieldcat_addcorr_type-fieldname    = 'REC_COUNTRY'.
      wa_fieldcat_addcorr_type-coltext    = 'Recipient Country'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.


      wa_fieldcat_addcorr_type-col_pos      = 18.
      wa_fieldcat_addcorr_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_addcorr_type-coltext    = 'Transportation Charge'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_ADDCORR_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 19.
      wa_fieldcat_addcorr_type-fieldname    = 'ADDR_CHARGE'.
      wa_fieldcat_addcorr_type-coltext    = 'Address Correction Amt'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_ADDCORR_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.

      wa_fieldcat_addcorr_type-col_pos      = 20.
      wa_fieldcat_addcorr_type-fieldname    = 'NET_CHARGE'.
      wa_fieldcat_addcorr_type-coltext    = 'Net Charge'.
      wa_fieldcat_addcorr_type-tabname      = 'IT_OUPUT_ADDCORR_TYPE'.
      append wa_fieldcat_addcorr_type to it_fieldcat_addcorr_type.
      clear wa_fieldcat_addcorr_type.



      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUPUT_ADDCORR_TYPE'
          is_layout                     = gs_layout
        changing
          it_outtab                     = it_output_addcorr_type
          it_fieldcatalog               = it_fieldcat_addcorr_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.

      set handler g_application->handle_hotspot_click for go_grid .

    endif.
  endif.

endform.                    "INCORRETADDR



*******End of address Correction

*********** Comparision between FEDEX and SAP Data
form compare_fedex_sap.



  clear : approve.
  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
* declare variables used
  if wa_export_output-carrier = 'FEDEX'.

    data: total_amnt type netwr,
          v_amt(10) type c,
          total_savings type netwr,
          total_pkgs type i,
          total_weight type brgew_ap,
          avg_pkg_cst type netwr,
          avg_pnd_cst type netwr.


    data :
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt,
    v_fuel_surcharge type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_perform_price  type /PWEAVER/EFA_FED-tran_charg_amnt.

    clear : v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_comp_type,it_final_comp_type,it_fieldcat_comp_type,it_output_comp_type.
    clear : it_comp_type,wa_comp_type,wa_final_comp_type,wa_fieldcat_comp_type.
    select *
              from /PWEAVER/EFA_FED
              into corresponding fields of table it_comp_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.


*****Get the data from  manifest table


    if not it_comp_type[] is not initial.
      select *
                from /PWEAVER/MANFEST
                into corresponding fields of table it_sap_comp_type
                for all entries in it_comp_type where tracking_number = it_comp_type-exp_grd_trck_id.
    endif.

    if sy-subrc = 0.
      clear : wa_comp_type.
      loop at it_comp_type into wa_comp_type.

        wa_final_comp_type-shipment_date  = wa_comp_type-shipment_date.
        wa_final_comp_type-service_type  = wa_comp_type-service_type.
        wa_final_comp_type-exp_grd_trck_id = wa_comp_type-exp_grd_trck_id.
        wa_final_comp_type-org_cust_ref = wa_comp_type-org_cust_ref.
        wa_final_comp_type-org_ref_3 = wa_comp_type-org_ref_3.
        wa_final_comp_type-pod = 'Proof of Delivery'.
        wa_final_comp_type-pod_del_date = wa_comp_type-pod_del_date.
        wa_final_comp_type-pod_del_time = wa_comp_type-pod_del_time.
        wa_final_comp_type-pod_sign_desc = wa_comp_type-pod_sign_desc.
        wa_final_comp_type-zone_code = wa_comp_type-zone_code.
        wa_final_comp_type-num_pieces = wa_comp_type-num_pieces.
        wa_final_comp_type-rated_weight = wa_comp_type-rated_wgt_amt.
        wa_final_comp_type-shipper_name = wa_comp_type-shipper_name.


        wa_final_comp_type-transport_charge = wa_comp_type-tran_charg_amnt.


****Handling charges
        if   wa_comp_type-trk_id_chg_des = 'Weekly COMPice Chg' or
       wa_comp_type-trk_id_chg_des = 'Weekday Delivery' or
       wa_comp_type-trk_id_chg_des = 'Residential Delivery' or
       wa_comp_type-trk_id_chg_des = 'Address Correction' or
       wa_comp_type-trk_id_chg_des = 'Residential' or
       wa_comp_type-trk_id_chg_des = 'DAS Comm' or
       wa_comp_type-trk_id_chg_des = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_comp_type-trk_id_chg_amt.
        endif.

        if  wa_comp_type-trk_id_chg_des1 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des1 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des1 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des1 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des1 = 'Residential' or
      wa_comp_type-trk_id_chg_des1 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des1 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt1.
        endif.

        if  wa_comp_type-trk_id_chg_des2 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des2 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des2 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des2 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des2 = 'Residential' or
      wa_comp_type-trk_id_chg_des2 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des2 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt2.
        endif.

        if wa_comp_type-trk_id_chg_des3 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des3 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des3 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des3 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des3 = 'Residential' or
      wa_comp_type-trk_id_chg_des3 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des3 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt3.
        endif.

        if  wa_comp_type-trk_id_chg_des4 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des4 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des4 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des4 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des4 = 'Residential' or
      wa_comp_type-trk_id_chg_des4 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des4 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt4.
        endif.

        if wa_comp_type-trk_id_chg_des5 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des5 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des5 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des5 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des5 = 'Residential' or
      wa_comp_type-trk_id_chg_des5 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des5 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt5.
        endif.

        if wa_comp_type-trk_id_chg_des6 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des6 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des6 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des6 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des6 = 'Residential' or
      wa_comp_type-trk_id_chg_des6 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des6 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt6.
        endif.

        if wa_comp_type-trk_id_chg_des7 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des7 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des7 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des7 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des7 = 'Residential' or
      wa_comp_type-trk_id_chg_des7 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des7 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt7.
        endif.

        if wa_comp_type-trk_id_chg_des8 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des8 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des8 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des8 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des8 = 'Residential' or
      wa_comp_type-trk_id_chg_des8 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des8 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt8.
        endif.


        if wa_comp_type-trk_id_chg_des9 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des9 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des9 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des9 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des9 = 'Residential' or
      wa_comp_type-trk_id_chg_des9 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des9 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt9.
        endif.


        if wa_comp_type-trk_id_chg_des10 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des10 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des10 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des10 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des10 = 'Residential' or
      wa_comp_type-trk_id_chg_des10 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des10 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt10.
        endif.


        if wa_comp_type-trk_id_chg_des11 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des11 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des11 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des11 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des11 = 'Residential' or
      wa_comp_type-trk_id_chg_des11 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des11 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt11.
        endif.


        if wa_comp_type-trk_id_chg_des12 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des12 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des12 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des12 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des12 = 'Residential' or
      wa_comp_type-trk_id_chg_des12 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des12 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt12.
        endif.


        if wa_comp_type-trk_id_chg_des13 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des13 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des13 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des13 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des13 = 'Residential' or
      wa_comp_type-trk_id_chg_des13 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des13 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt13.
        endif.


        if     wa_comp_type-trk_id_chg_des14 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des14 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des14 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des14 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des14 = 'Residential' or
      wa_comp_type-trk_id_chg_des14 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des14 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt14.
        endif.


        if wa_comp_type-trk_id_chg_des15 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des15 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des15 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des15 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des15 = 'Residential' or
      wa_comp_type-trk_id_chg_des15 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des15 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt15.
        endif.


        if wa_comp_type-trk_id_chg_des16 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des16 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des16 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des16 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des16 = 'Residential' or
      wa_comp_type-trk_id_chg_des16 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des16 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt16.
        endif.


        if wa_comp_type-trk_id_chg_des17 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des17 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des17 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des17 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des17 = 'Residential' or
      wa_comp_type-trk_id_chg_des17 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des17 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt17.
        endif.


        if wa_comp_type-trk_id_chg_des18 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des18 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des18 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des18 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des18 = 'Residential' or
      wa_comp_type-trk_id_chg_des18 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des18 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt18.
        endif.


        if wa_comp_type-trk_id_chg_des19 = 'Weekly COMPice Chg' or
wa_comp_type-trk_id_chg_des19 = 'Weekday Delivery' or
wa_comp_type-trk_id_chg_des19 = 'Residential Delivery' or
wa_comp_type-trk_id_chg_des19 = 'Address Correction' or
wa_comp_type-trk_id_chg_des19 = 'Residential' or
wa_comp_type-trk_id_chg_des19 = 'DAS Comm' or
wa_comp_type-trk_id_chg_des19 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt19.
        endif.

        if wa_comp_type-trk_id_chg_des20 = 'Weekly COMPice Chg' or
wa_comp_type-trk_id_chg_des20 = 'Weekday Delivery' or
wa_comp_type-trk_id_chg_des20 = 'Residential Delivery' or
wa_comp_type-trk_id_chg_des20 = 'Address Correction' or
wa_comp_type-trk_id_chg_des20 = 'Residential' or
wa_comp_type-trk_id_chg_des20 = 'DAS Comm' or
wa_comp_type-trk_id_chg_des20 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt20.
        endif.

        if  wa_comp_type-trk_id_chg_des21 = 'Weekly COMPice Chg' or
wa_comp_type-trk_id_chg_des21 = 'Weekday Delivery' or
wa_comp_type-trk_id_chg_des21 = 'Residential Delivery' or
wa_comp_type-trk_id_chg_des21 = 'Address Correction' or
wa_comp_type-trk_id_chg_des21 = 'Residential' or
wa_comp_type-trk_id_chg_des21 = 'DAS Comm' or
wa_comp_type-trk_id_chg_des21 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt21.
        endif.

        if wa_comp_type-trk_id_chg_des22 = 'Weekly COMPice Chg' or
      wa_comp_type-trk_id_chg_des22 = 'Weekday Delivery' or
      wa_comp_type-trk_id_chg_des22 = 'Residential Delivery' or
      wa_comp_type-trk_id_chg_des22 = 'Address Correction' or
      wa_comp_type-trk_id_chg_des22 = 'Residential' or
      wa_comp_type-trk_id_chg_des22 = 'DAS Comm' or
      wa_comp_type-trk_id_chg_des22 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt22.
        endif.

        if wa_comp_type-trk_id_chg_des23 = 'Weekly COMPice Chg' or
wa_comp_type-trk_id_chg_des23 = 'Weekday Delivery' or
wa_comp_type-trk_id_chg_des23 = 'Residential Delivery' or
wa_comp_type-trk_id_chg_des23 = 'Address Correction' or
wa_comp_type-trk_id_chg_des23 = 'Residential' or
wa_comp_type-trk_id_chg_des23 = 'DAS Comm' or
wa_comp_type-trk_id_chg_des23 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt23.
        endif.

        if wa_comp_type-trk_id_chg_des24 = 'Weekly COMPice Chg' or
wa_comp_type-trk_id_chg_des24 = 'Weekday Delivery' or
wa_comp_type-trk_id_chg_des24 = 'Residential Delivery' or
wa_comp_type-trk_id_chg_des24 = 'Address Correction' or
wa_comp_type-trk_id_chg_des24 = 'Residential' or
wa_comp_type-trk_id_chg_des24 = 'DAS Comm' or
wa_comp_type-trk_id_chg_des24 = 'DAS Extended Comm'.

          wa_final_comp_type-handling_charge =  wa_final_comp_type-handling_charge + wa_comp_type-trk_id_chg_amt24.
        endif.

**************Fuel Surcharge
*****Fuel Surcharge
        if wa_comp_type-trk_id_chg_des = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_comp_type-trk_id_chg_amt.
        endif.

        if wa_comp_type-trk_id_chg_des1 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt1.
        endif.

        if wa_comp_type-trk_id_chg_des2 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt2.
        endif.

        if wa_comp_type-trk_id_chg_des3 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt3.
        endif.

        if wa_comp_type-trk_id_chg_des4 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt4.
        endif.

        if wa_comp_type-trk_id_chg_des5 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt5.
        endif.

        if wa_comp_type-trk_id_chg_des6 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt6.
        endif.

        if wa_comp_type-trk_id_chg_des7 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt7.
        endif.

        if wa_comp_type-trk_id_chg_des8 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt8.
        endif.


        if wa_comp_type-trk_id_chg_des9 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt9.
        endif.


        if wa_comp_type-trk_id_chg_des10 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt10.
        endif.


        if wa_comp_type-trk_id_chg_des11 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt11.
        endif.


        if wa_comp_type-trk_id_chg_des12 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt12.
        endif.


        if wa_comp_type-trk_id_chg_des13 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt13.
        endif.


        if wa_comp_type-trk_id_chg_des14 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt14.
        endif.


        if wa_comp_type-trk_id_chg_des15 = 'Fuel Surcharge' .
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt15.
        endif.


        if wa_comp_type-trk_id_chg_des16 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt16.
        endif.


        if wa_comp_type-trk_id_chg_des17 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt17.
        endif.


        if wa_comp_type-trk_id_chg_des18 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt18.
        endif.


        if wa_comp_type-trk_id_chg_des19 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt19.
        endif.

        if wa_comp_type-trk_id_chg_des20 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt20.
        endif.

        if wa_comp_type-trk_id_chg_des21 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt21.
        endif.

        if wa_comp_type-trk_id_chg_des22 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt22.
        endif.

        if wa_comp_type-trk_id_chg_des23 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt23.
        endif.

        if wa_comp_type-trk_id_chg_des24 = 'Fuel Surcharge'.
          wa_final_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge + wa_comp_type-trk_id_chg_amt24.
        endif.



***************Earned Discount
        if wa_comp_type-trk_id_chg_des = 'Earned Discount'.
          wa_final_comp_type-discount =  wa_comp_type-trk_id_chg_amt.
        endif.

        if wa_comp_type-trk_id_chg_des1 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt1.
        endif.

        if wa_comp_type-trk_id_chg_des2 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt2.
        endif.

        if wa_comp_type-trk_id_chg_des3 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt3.
        endif.

        if wa_comp_type-trk_id_chg_des4 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt4.
        endif.

        if wa_comp_type-trk_id_chg_des5 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt5.
        endif.

        if wa_comp_type-trk_id_chg_des6 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt6.
        endif.

        if wa_comp_type-trk_id_chg_des7 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt7.
        endif.

        if wa_comp_type-trk_id_chg_des8 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt8.
        endif.

        if wa_comp_type-trk_id_chg_des9 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt9.
        endif.


        if wa_comp_type-trk_id_chg_des10 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt10.
        endif.

        if wa_comp_type-trk_id_chg_des11 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt11.
        endif.

        if wa_comp_type-trk_id_chg_des12 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt12.
        endif.

        if wa_comp_type-trk_id_chg_des13 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt13.
        endif.


        if wa_comp_type-trk_id_chg_des14 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt14.
        endif.

        if wa_comp_type-trk_id_chg_des15 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt15.
        endif.


        if wa_comp_type-trk_id_chg_des16 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt16.
        endif.

        if wa_comp_type-trk_id_chg_des17 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt17.
        endif.

        if wa_comp_type-trk_id_chg_des18 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt18.
        endif.

        if wa_comp_type-trk_id_chg_des19 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt19.
        endif.

        if wa_comp_type-trk_id_chg_des20 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt20.
        endif.

        if wa_comp_type-trk_id_chg_des21 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt21.
        endif.

        if wa_comp_type-trk_id_chg_des22 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt22.
        endif.

        if wa_comp_type-trk_id_chg_des23 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt23.
        endif.

        if wa_comp_type-trk_id_chg_des24 = 'Earned Discount'.
          wa_final_comp_type-discount =   wa_final_comp_type-discount + wa_comp_type-trk_id_chg_amt24.
        endif.


****Performance Discount


        if wa_comp_type-trk_id_chg_des = 'Performance Pricing'.
          wa_final_comp_type-perform_price =  wa_comp_type-trk_id_chg_amt.
        endif.

        if wa_comp_type-trk_id_chg_des1 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt1.
        endif.

        if wa_comp_type-trk_id_chg_des2 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt2.
        endif.

        if wa_comp_type-trk_id_chg_des3 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt3.
        endif.

        if wa_comp_type-trk_id_chg_des4 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt4.
        endif.

        if wa_comp_type-trk_id_chg_des5 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt5.
        endif.

        if wa_comp_type-trk_id_chg_des6 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt6.
        endif.

        if wa_comp_type-trk_id_chg_des7 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt7.
        endif.

        if wa_comp_type-trk_id_chg_des8 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt8.
        endif.

        if wa_comp_type-trk_id_chg_des9 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt9.
        endif.


        if wa_comp_type-trk_id_chg_des10 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt10.
        endif.

        if wa_comp_type-trk_id_chg_des11 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt11.
        endif.

        if wa_comp_type-trk_id_chg_des12 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt12.
        endif.

        if wa_comp_type-trk_id_chg_des13 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt13.
        endif.


        if wa_comp_type-trk_id_chg_des14 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt14.
        endif.

        if wa_comp_type-trk_id_chg_des15 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt15.
        endif.


        if wa_comp_type-trk_id_chg_des16 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt16.
        endif.

        if wa_comp_type-trk_id_chg_des17 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt17.
        endif.

        if wa_comp_type-trk_id_chg_des18 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt18.
        endif.

        if wa_comp_type-trk_id_chg_des19 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt19.
        endif.

        if wa_comp_type-trk_id_chg_des20 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt20.
        endif.

        if wa_comp_type-trk_id_chg_des21 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt21.
        endif.

        if wa_comp_type-trk_id_chg_des22 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt22.
        endif.

        if wa_comp_type-trk_id_chg_des23 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt23.
        endif.

        if wa_comp_type-trk_id_chg_des24 = 'Performance Pricing'.
          wa_final_comp_type-perform_price =   wa_final_comp_type-perform_price + wa_comp_type-trk_id_chg_amt24.
        endif.


        wa_final_comp_type-net_charge = wa_comp_type-net_chrg_amnt.

        read table it_sap_comp_type  into wa_sap_comp_type with key tracking_number = wa_comp_type-exp_grd_trck_id.
        if sy-subrc = 0 .
          wa_final_comp_type-date_added = wa_sap_comp_type-date_added.
          wa_final_comp_type-time_added = wa_sap_comp_type-time_added.
          wa_final_comp_type-plant  = wa_sap_comp_type-plant.
          wa_final_comp_type-vbeln  = wa_sap_comp_type-vbeln.
          wa_final_comp_type-pkgcount = wa_sap_comp_type-pkgcount.
          wa_final_comp_type-totalpkg  = wa_sap_comp_type-totalpkg.
          wa_final_comp_type-handling_unit  = wa_sap_comp_type-handling_unit.
          wa_final_comp_type-tracking_number  = wa_sap_comp_type-tracking_number.
          wa_final_comp_type-package_weight  = wa_sap_comp_type-package_weight.
          wa_final_comp_type-carrier_code  = wa_sap_comp_type-carrier_code.
          wa_final_comp_type-carriertype  = wa_sap_comp_type-carriertype.
          wa_final_comp_type-freightamt  = wa_sap_comp_type-freightamt.
          wa_final_comp_type-discountamt  = wa_sap_comp_type-discountamt.
          wa_final_comp_type-insurance  = wa_sap_comp_type-insurance.
          wa_final_comp_type-canc_dt  = wa_sap_comp_type-canc_dt.
          wa_final_comp_type-canc_tim  = wa_sap_comp_type-canc_tim.
          wa_final_comp_type-company  = wa_sap_comp_type-company.
          wa_final_comp_type-contact  = wa_sap_comp_type-contact.
          wa_final_comp_type-address1  = wa_sap_comp_type-address1.
          wa_final_comp_type-address2  = wa_sap_comp_type-address2.
          wa_final_comp_type-city  = wa_sap_comp_type-city.
          wa_final_comp_type-region  = wa_sap_comp_type-region.
          wa_final_comp_type-postalcode  = wa_sap_comp_type-postalcode.
          wa_final_comp_type-country  = wa_sap_comp_type-country.
          wa_final_comp_type-phone  = wa_sap_comp_type-phone.
          wa_final_comp_type-line_color = 'C400'.
        else.
          if wa_sap_comp_type-tracking_number <> wa_comp_type-exp_grd_trck_id.

            wa_final_comp_type-comments = 'Tracking no. not found in SAP'.
            wa_final_comp_type-line_color = 'C600'.
          elseif wa_sap_comp_type-package_weight <> wa_comp_type-rated_wgt_amt.
            wa_final_comp_type-comments = 'Error in Weight'.
            wa_final_comp_type-line_color = 'C600'.
          endif.
        endif.



        append wa_final_comp_type to it_final_comp_type.
        clear :wa_final_comp_type,wa_sap_comp_type,wa_comp_type.
      endloop.


      sort it_final_comp_type by exp_grd_trck_id.
      clear wa_final_comp_type.


      loop at it_final_comp_type into wa_final_comp_type.



        if not wa_final_comp_type is initial.
*          AT END OF COMPICE_TYPE.
*            SUM.     " calculates sub totals on COMPice types


          data : v_ship_date(8) type c,
                v_pod_date(8) type c.
          clear : v_ship_date.
          concatenate wa_final_comp_type-shipment_date+0(4) wa_final_comp_type-shipment_date+4(2) wa_final_comp_type-shipment_date+6(2) into v_ship_date.

*          CONCATENATE WA_FINAL_COMP_TYPE-SHIPMENT_DATE+0(4) '/' WA_FINAL_COMP_TYPE-SHIPMENT_DATE+4(2) '/' WA_FINAL_COMP_TYPE-SHIPMENT_DATE+6(2) INTO WA_FINAL_COMP_TYPE-SHIPMENT_DATE.

*          WA_OUTPUT_COMP_TYPE-SHIPMENT_DATE = WA_FINAL_COMP_TYPE-SHIPMENT_DATE.
          wa_output_comp_type-shipment_date = v_ship_date.
          wa_output_comp_type-service_type = wa_final_comp_type-service_type.
          wa_output_comp_type-exp_grd_trck_id = wa_final_comp_type-exp_grd_trck_id.
          wa_output_comp_type-num_pieces =  wa_final_comp_type-num_pieces.
          wa_output_comp_type-org_cust_ref =  wa_final_comp_type-org_cust_ref.
          wa_output_comp_type-org_ref_3 =  wa_final_comp_type-org_ref_3.
          wa_output_comp_type-pod =  wa_final_comp_type-pod.

          clear : v_pod_date.
          concatenate wa_final_comp_type-pod_del_date+0(4) wa_final_comp_type-pod_del_date+4(2) wa_final_comp_type-pod_del_date+6(2) into v_pod_date.


*          CONCATENATE WA_FINAL_COMP_TYPE-POD_DEL_DATE+0(4) '-' WA_FINAL_COMP_TYPE-POD_DEL_DATE+4(2) '-' WA_FINAL_COMP_TYPE-POD_DEL_DATE+6(2) INTO WA_FINAL_COMP_TYPE-POD_DEL_DATE.

          wa_output_comp_type-pod_del_date =  v_pod_date.
          wa_output_comp_type-pod_del_date =  wa_final_comp_type-pod_del_date.
          wa_output_comp_type-pod_del_time =  wa_final_comp_type-pod_del_time.
          wa_output_comp_type-pod_sign_desc =  wa_final_comp_type-pod_sign_desc.
          wa_output_comp_type-shipper_name =  wa_final_comp_type-shipper_name.
          wa_output_comp_type-fuel_surcharge =  wa_final_comp_type-fuel_surcharge.
          wa_output_comp_type-perform_price =  wa_final_comp_type-perform_price.

          wa_output_comp_type-zone_code =  wa_final_comp_type-zone_code.
          wa_output_comp_type-rated_weight = wa_final_comp_type-rated_weight.
          wa_output_comp_type-transport_charge = wa_final_comp_type-transport_charge.
          wa_output_comp_type-handling_charge =  wa_final_comp_type-handling_charge.
          wa_output_comp_type-discount =  wa_final_comp_type-discount.
          wa_output_comp_type-net_charge = wa_final_comp_type-net_charge.


*******Get the sap data
          data : v_sap_date.
          clear : v_sap_date.
          concatenate wa_final_comp_type-date_added+0(4) wa_final_comp_type-date_added+4(2) wa_final_comp_type-date_added+6(2) into v_sap_date.


*          CONCATENATE WA_FINAL_COMP_TYPE-DATE_ADDED+0(4) '-' WA_FINAL_COMP_TYPE-DATE_ADDED+4(2) '-' WA_FINAL_COMP_TYPE-DATE_ADDED+6(2) INTO WA_FINAL_COMP_TYPE-DATE_ADDED.
          wa_output_comp_type-date_added = v_sap_date.
          wa_output_comp_type-date_added = wa_final_comp_type-date_added.
          wa_output_comp_type-time_added = wa_final_comp_type-time_added.
          wa_output_comp_type-plant = wa_final_comp_type-plant.
          wa_output_comp_type-vbeln = wa_final_comp_type-vbeln.
          wa_output_comp_type-pkgcount = wa_final_comp_type-pkgcount.
          wa_output_comp_type-totalpkg = wa_final_comp_type-totalpkg.
          wa_output_comp_type-handling_unit = wa_final_comp_type-handling_unit.
          wa_output_comp_type-tracking_number = wa_final_comp_type-tracking_number.
          wa_output_comp_type-package_weight = wa_final_comp_type-package_weight.
          wa_output_comp_type-carrier_code = wa_final_comp_type-carrier_code.
          wa_output_comp_type-carriertype = wa_final_comp_type-carriertype.
          wa_output_comp_type-freightamt = wa_final_comp_type-freightamt.
          wa_output_comp_type-discountamt = wa_final_comp_type-discountamt.
          wa_output_comp_type-insurance = wa_final_comp_type-insurance.
          wa_output_comp_type-canc_dt = wa_final_comp_type-canc_dt.
          wa_output_comp_type-canc_tim = wa_final_comp_type-canc_tim.
          wa_output_comp_type-company = wa_final_comp_type-company.
          wa_output_comp_type-contact = wa_final_comp_type-contact.
          wa_output_comp_type-address1 = wa_final_comp_type-address1.
          wa_output_comp_type-address2 = wa_final_comp_type-address2.
          wa_output_comp_type-city = wa_final_comp_type-city.
          wa_output_comp_type-region = wa_final_comp_type-region.
          wa_output_comp_type-postalcode = wa_final_comp_type-postalcode.
          wa_output_comp_type-country = wa_final_comp_type-country.
          wa_output_comp_type-phone = wa_final_comp_type-phone.
          wa_output_comp_type-comments = wa_final_comp_type-comments.
          wa_output_comp_type-line_color = wa_final_comp_type-line_color.
*
*
*****If the rated wieght and package weight is of different then show it in different color.
*IF WA_OUTPUT_COMP_TYPE-RATED_WEIGHT <> WA_OUTPUT_COMP_TYPE-PACKAGE_WEIGHT.
*WA_OUTPUT_COMP_TYPE-RATED_WEIGHT-LINE_COLOR = 'C700'.
*endif.



* calculate the grand totals

*v_COMPice = v_COMPice + wa_output_COMP_type-COMPice_type.
*          V_PACKAGES = V_PACKAGES + WA_OUTPUT_COMP_TYPE-PACKAGES.
          v_rated_weight = v_rated_weight + wa_output_comp_type-rated_weight.

          v_trans_charg = v_trans_charg + wa_output_comp_type-transport_charge.


          v_hand_charg = v_hand_charg + wa_output_comp_type-handling_charge.
          v_fuel_surcharge = v_fuel_surcharge + wa_output_comp_type-fuel_surcharge.
          v_perform_price = v_perform_price + wa_output_comp_type-perform_price.

          v_discount = v_discount + wa_output_comp_type-discount.

          v_netcharge = v_netcharge +  wa_output_comp_type-net_charge .
          append wa_output_comp_type to it_output_comp_type.
          clear wa_output_comp_type.
*          ENDAT.

        endif.
        clear wa_final_comp_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_comp_type.

*    wa_output_COMP_type-COMPice_type = v_COMPice.
*      WA_OUTPUT_COMP_TYPE-PACKAGES = V_PACKAGES.
*      WA_OUTPUT_COMP_TYPE-RATED_WEIGHT = V_RATED_WEIGHT.

      wa_output_comp_type-transport_charge = v_trans_charg.
      wa_output_comp_type-handling_charge = v_hand_charg .
      wa_output_comp_type-fuel_surcharge = v_fuel_surcharge .
      wa_output_comp_type-discount = v_discount .
      wa_output_comp_type-perform_price = v_perform_price .
      wa_output_comp_type-net_charge = v_netcharge.

      wa_output_comp_type-line_color      = 'C500'.
      append wa_output_comp_type to it_output_comp_type.
      clear wa_output_comp_type.



      wa_fieldcat_comp_type-col_pos      = 1.
      wa_fieldcat_comp_type-fieldname    = 'SHIPMENT_DATE'.
      wa_fieldcat_comp_type-coltext    = 'Shipment Date'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 2.
      wa_fieldcat_comp_type-fieldname    = 'SERVICE_TYPE'.
      wa_fieldcat_comp_type-coltext    = 'Service Type'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 3.
      wa_fieldcat_comp_type-fieldname    = 'EXP_GRD_TRCK_ID'.
      wa_fieldcat_comp_type-coltext    = 'Tracking Number'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      wa_fieldcat_comp_type-hotspot   = 'X'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 4.
      wa_fieldcat_comp_type-fieldname    = 'ORG_CUST_REF'.
      wa_fieldcat_comp_type-coltext    = 'Reference #'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 5.
      wa_fieldcat_comp_type-fieldname    = 'ORG_REF_3'.
      wa_fieldcat_comp_type-coltext    = 'PO #'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 6.
      wa_fieldcat_comp_type-fieldname    = 'POD'.
      wa_fieldcat_comp_type-coltext    = 'Proof of Delivery'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      wa_fieldcat_comp_type-hotspot   = 'X'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 7.
      wa_fieldcat_comp_type-fieldname    = 'POD_DEL_DATE'.
      wa_fieldcat_comp_type-coltext    = 'POD Date'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 8.
      wa_fieldcat_comp_type-fieldname    = 'POD_DEL_TIME'.
      wa_fieldcat_comp_type-coltext    = 'POD Time'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 9.
      wa_fieldcat_comp_type-fieldname    = 'POD_SIGN_DESC'.
      wa_fieldcat_comp_type-coltext    = 'POD Signature'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.


      wa_fieldcat_comp_type-col_pos      = 10.
      wa_fieldcat_comp_type-fieldname    = 'ZONE_CODE'.
      wa_fieldcat_comp_type-coltext    = 'Zone'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 11.
      wa_fieldcat_comp_type-fieldname    = 'NUM_PIECES'.
      wa_fieldcat_comp_type-coltext    = 'Packages'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 12.
      wa_fieldcat_comp_type-fieldname    = 'RATED_WGT_AMT'.
      wa_fieldcat_comp_type-coltext    = 'Rated Weight'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 13.
      wa_fieldcat_comp_type-fieldname    = 'SHIPPER_NAME'.
      wa_fieldcat_comp_type-coltext    = 'Shipper Name'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 14.
      wa_fieldcat_comp_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_comp_type-coltext    = 'Transport Charge'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 15.
      wa_fieldcat_comp_type-fieldname    = 'FUEL_SURCHARGE'.
      wa_fieldcat_comp_type-coltext    = 'Fuel Surcharge'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 16.
      wa_fieldcat_comp_type-fieldname    = 'HANDLING_CHARGE'.
      wa_fieldcat_comp_type-coltext    = 'Handling Charge'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 17.
      wa_fieldcat_comp_type-fieldname    = 'DISCOUNT'.
      wa_fieldcat_comp_type-coltext    = 'Discount'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 18.
      wa_fieldcat_comp_type-fieldname    = 'PERFORM_PRICE'.
      wa_fieldcat_comp_type-coltext    = 'Performance Pricing'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 19.
      wa_fieldcat_comp_type-fieldname    = 'NET_CHARGE'.
      wa_fieldcat_comp_type-coltext    = 'Net Charge'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.
      wa_fieldcat_comp_type-col_pos      = 20.
      wa_fieldcat_comp_type-fieldname    = 'CREDIT_AMOUNT'.
      wa_fieldcat_comp_type-coltext    = 'Credit Amount'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 21.
      wa_fieldcat_comp_type-fieldname    = 'ERROR_CODE'.
      wa_fieldcat_comp_type-coltext    = 'Error Code'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

******* SAP DATA FIEDS
      wa_fieldcat_comp_type-col_pos      = 23.
      wa_fieldcat_comp_type-fieldname    = 'DATE_ADDED'.
      wa_fieldcat_comp_type-coltext    = 'SAP Creation Date'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 24.
      wa_fieldcat_comp_type-fieldname    = 'TIME_ADDED'.
      wa_fieldcat_comp_type-coltext    = 'SAP Creation Time'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 25.
      wa_fieldcat_comp_type-fieldname    = 'PLANT'.
      wa_fieldcat_comp_type-coltext    = 'SAP Plant'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 26.
      wa_fieldcat_comp_type-fieldname    = 'VBELN'.
      wa_fieldcat_comp_type-coltext    = 'SAP Order'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 27.
      wa_fieldcat_comp_type-fieldname    = 'PKGCOUNT'.
      wa_fieldcat_comp_type-coltext    = 'SAP Package Count'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 28.
      wa_fieldcat_comp_type-fieldname    = 'TOTALPKG'.
      wa_fieldcat_comp_type-coltext    = 'SAP Total Packages'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.


      wa_fieldcat_comp_type-col_pos      = 29.
      wa_fieldcat_comp_type-fieldname    = 'HANDLING_UNIT'.
      wa_fieldcat_comp_type-coltext    = 'SAP Handling Unit'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 30.
      wa_fieldcat_comp_type-fieldname    = 'TRACKING_NUMBER'.
      wa_fieldcat_comp_type-coltext    = 'SAP Tracking Number'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      wa_fieldcat_comp_type-hotspot   = 'X'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.


      wa_fieldcat_comp_type-col_pos      = 31.
      wa_fieldcat_comp_type-fieldname    = 'PACKAGE_WEIGHT'.
      wa_fieldcat_comp_type-coltext    = 'SAP Package Weight'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 32.
      wa_fieldcat_comp_type-fieldname    = 'CARRIER_CODE'.
      wa_fieldcat_comp_type-coltext    = 'SAP Carrier Code'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.


      wa_fieldcat_comp_type-col_pos      = 33.
      wa_fieldcat_comp_type-fieldname    = 'CARRIERTYPE'.
      wa_fieldcat_comp_type-coltext    = 'SAP Carrier Type'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 34.
      wa_fieldcat_comp_type-fieldname    = 'FREIGHTAMT'.
      wa_fieldcat_comp_type-coltext    = 'SAP Freight Amount'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 35.
      wa_fieldcat_comp_type-fieldname    = 'DISCOUNTAMT'.
      wa_fieldcat_comp_type-coltext    = 'SAP Discount Amount'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.



      wa_fieldcat_comp_type-col_pos      = 36.
      wa_fieldcat_comp_type-fieldname    = 'INSURANCE'.
      wa_fieldcat_comp_type-coltext    = 'SAP Insurnace Amount'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 37.
      wa_fieldcat_comp_type-fieldname    = 'CANC_DT'.
      wa_fieldcat_comp_type-coltext    = 'SAP Cancel Date'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 37.
      wa_fieldcat_comp_type-fieldname    = 'CANC_TIM'.
      wa_fieldcat_comp_type-coltext    = 'SAP Cancel Time'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 38.
      wa_fieldcat_comp_type-fieldname    = 'COMPANY'.
      wa_fieldcat_comp_type-coltext    = 'SAP Company'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 39.
      wa_fieldcat_comp_type-fieldname    = 'CONTACT'.
      wa_fieldcat_comp_type-coltext    = 'SAP Contact'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.


      wa_fieldcat_comp_type-col_pos      = 40.
      wa_fieldcat_comp_type-fieldname    = 'ADDRESS1'.
      wa_fieldcat_comp_type-coltext    = 'SAP Address'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.


      wa_fieldcat_comp_type-col_pos      = 41.
      wa_fieldcat_comp_type-fieldname    = 'ADDRESS2'.
      wa_fieldcat_comp_type-coltext    = 'SAP Address'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 42.
      wa_fieldcat_comp_type-fieldname    = 'CITY'.
      wa_fieldcat_comp_type-coltext    = 'SAP City'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 43.
      wa_fieldcat_comp_type-fieldname    = 'REGION'.
      wa_fieldcat_comp_type-coltext    = 'SAP Region'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 44.
      wa_fieldcat_comp_type-fieldname    = 'POSTALCODE'.
      wa_fieldcat_comp_type-coltext    = 'SAP Postal Code'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 45.
      wa_fieldcat_comp_type-fieldname    = 'COUNTRY'.
      wa_fieldcat_comp_type-coltext    = 'SAP Country'.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 46.
      wa_fieldcat_comp_type-fieldname    = 'PHONE '.
      wa_fieldcat_comp_type-coltext    = 'SAP PHONE '.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.

      wa_fieldcat_comp_type-col_pos      = 47.
      wa_fieldcat_comp_type-fieldname    = 'COMMENTS '.
      wa_fieldcat_comp_type-coltext    = 'Comments '.
      wa_fieldcat_comp_type-tabname      = 'IT_OUPUT_COMP_TYPE'.
      append wa_fieldcat_comp_type to it_fieldcat_comp_type.
      clear wa_fieldcat_comp_type.



      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.


      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUPUT_COMP_TYPE'
          is_layout                     = gs_layout
        changing
          it_outtab                     = it_output_comp_type
          it_fieldcatalog               = it_fieldcat_comp_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.

      set handler g_application->handle_hotspot_click for go_grid .

    endif.
  endif.

*  CALL METHOD go_grid->set_table_for_first_display
*    EXPORTING
*      i_structure_name              = 'IT_OUTPUT2'
*      is_layout                     = gs_layout
*    CHANGING
*      it_outtab                     = it_output2
*      it_fieldcatalog               = it_fieldcat2
**    IT_SORT                       =
*  EXCEPTIONS
*    invalid_parameter_combination = 1
*    program_error                 = 2
*    too_many_lines                = 3
*    OTHERS                        = 4  .
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*
*
*
*  if retval <> cl_gfw=>ok.
*    call method cl_gfw=>show_msg
*      EXPORTING
*        msgno = retval.
*  else.
*
*    if retval = cl_gfw=>ok.
*
** set font size
*  CALL METHOD BUNDLE_DISPLAY->IF_CUSTOMIZING~SET
*    EXPORTING ATTR_ID = CL_CU_DISPLAY_CONTEXT=>CO_TF_SIZE
*              VALUE   = 14.
*
** add title context to bundle_drawing
*  CALL METHOD BUNDLE_DRAWING->IF_CUSTOMIZING~SET
*    EXPORTING ATTR_ID = CL_CU_DRAWING_AREA=>CO_TITLE_CONTEXT
*              VALUE   = BUNDLE_DISPLAY.
*
** set the title
*  CALL METHOD BUNDLE_DRAWING->IF_CUSTOMIZING~SET
*    EXPORTING ATTR_ID = CL_CU_DRAWING_AREA=>CO_TITLE
*              VALUE   ='Cost By : Service Type'.
*
*
*
*    endif.
*
*
*    if retval <> cl_gfw=>ok.
*      call method cl_gfw=>show_msg
*        EXPORTING
*          msgno = retval.
*    endif.
*  endif. "//fill_dc ok


  if ups = 'X'.

*    CLEAR : APPROVE.
*    REFRESH: IT_FINAL_ALL_DETAILS_UPS,IT_FIELDCAT_ALL_DETAILS_UPS.
*
*    SELECT PICKUP_DATE
*           STCOMPANY
*           STNAME
*           STADDR1
*           STCITY
*           STSTATE
*           STZIP
*           STCOUNTRY
*      TRACKINGNUMBER
**    pod_signature
**    pod_date
*       ACTUALWEIGHT
**    ratedweight
*      TOTAL_BILL_CHARG SERVICE_TYPE PAYMENT_METHOD
*     ADDR_CORRECTION ADDR_CORR_CHRG  ERRORCODE TOTAL_INC_CREDIT
**    total_charge
*      CARRIER
*              FROM ZPWEFA_INVC_UPS INTO CORRESPONDING FIELDS OF TABLE IT_FINAL_ALL_DETAILS_UPS
*              WHERE INVOICE_DATE EQ WEEK_DT AND CARRIER EQ ZCARRIERTYPE.
*
*    IF SY-SUBRC = 0.
*
**    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
**      EXPORTING
**        i_structure_name       = 'ZPWEFAINVOICE'
**      CHANGING
**        ct_fieldcat            = it_fieldcat8
**      EXCEPTIONS
**        inconsistent_interface = 1
**        program_error          = 2
**        OTHERS                 = 3.
**    IF sy-subrc <> 0.
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
**    ENDIF.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 1.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'PICKUP_DATE'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Pickup Date'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 2.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STCOMPANY'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Company'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 3.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STNAME'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Name'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 4.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STADDR1'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Address'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
**    wa_fieldcat8-col_pos      = 5.
**    wa_fieldcat8-fieldname    = 'Consignee CITY'.
**    wa_fieldcat8-coltext    = 'Ship To City'.
**    wa_fieldcat8-tabname      = 'IT_FINALA_ALL_DETAILS'.
**    APPEND wa_fieldcat8 TO it_fieldcat8.
**    CLEAR wa_fieldcat8.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 5.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STSTATE'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To State'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 6.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STZIP'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Zip'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 7.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'STCOUNTRY'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Ship To Country'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 8.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'TRACKINGNUMBER'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Tracking No'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-HOTSPOT    = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
**    wa_fieldcat_all_details-col_pos      = 9.
**    wa_fieldcat_all_details-fieldname    = 'POD_SIGNATURE'.
**    wa_fieldcat_all_details-coltext    = 'POD Signature'.
**    wa_fieldcat_all_details-tabname      = 'IT_FINAL_ALL_DETAILS'.
**    APPEND wa_fieldcat_all_details TO it_fieldcat_all_details.
**    CLEAR wa_fieldcat_all_details.
**
**    wa_fieldcat_all_details-col_pos      = 10.
**    wa_fieldcat_all_details-fieldname    = 'POD_DATE'.
**    wa_fieldcat_all_details-coltext    = 'POD Date'.
**    wa_fieldcat_all_details-tabname      = 'IT_FINAL_ALL_DETAILS'.
**    APPEND wa_fieldcat_all_details TO it_fieldcat_all_details.
**    CLEAR wa_fieldcat_all_details.
*
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 09.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'ACTUALWEIGHT'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Weight'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
**    wa_fieldcat_all_details_ups-col_pos      = 10.
**    wa_fieldcat_all_details_ups-fieldname    = 'RATEDWEIGHT'.
**    wa_fieldcat_all_details_ups-coltext    = 'Billed Weight'.
**    wa_fieldcat_all_details_ups-do_sum       = 'X'.
**    wa_fieldcat_all_details_ups-tabname      = 'IT_FINAL_ALL_DETAILS_UPS'.
**    APPEND wa_fieldcat_all_details_ups TO it_fieldcat_all_details_ups.
**    CLEAR wa_fieldcat_all_details_ups.
*
**    wa_fieldcat8-col_pos      = 14.
**    wa_fieldcat8-fieldname    = 'SUMCHARGE'.
**    wa_fieldcat8-coltext    = 'Original Charge'.
**    wa_fieldcat8-do_sum       = 'X'.
**    wa_fieldcat8-tabname      = 'IT_FINAL_ALL_DETAILS'.
**    APPEND wa_fieldcat8 TO it_fieldcat8.
**    CLEAR wa_fieldcat8.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 11.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'SERVICE_TYPE'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Service Type'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 12.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'PAYMENT_METHOD'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Payment Method'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 13.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'ADDR_CORRECTION'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Address Correction'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 14.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'ADDR_CORR_CHRG'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Address Correction Charge'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 15.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'ERRORCODE'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Error Description'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 16.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'TOTAL_INC_CREDIT'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Credit Amount'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*      WA_FIELDCAT_ALL_DETAILS_UPS-COL_POS      = 17.
*      WA_FIELDCAT_ALL_DETAILS_UPS-FIELDNAME    = 'TOTAL_BILL_CHARG'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-COLTEXT    = 'Total Net Due'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-DO_SUM       = 'X'.
*      WA_FIELDCAT_ALL_DETAILS_UPS-TABNAME      = 'IT_FINAL_ALL_DETAILS_UPS'.
*      APPEND WA_FIELDCAT_ALL_DETAILS_UPS TO IT_FIELDCAT_ALL_DETAILS_UPS.
*      CLEAR WA_FIELDCAT_ALL_DETAILS_UPS.
*
*
*      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*      GS_LAYOUT-GRID_TITLE = SPACE.
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*     EXPORTING
*       I_STRUCTURE_NAME              = 'IT_FINAL_ALL_DETAILS_UPS'
**    IS_VARIANT                    =
**    I_SAVE                        =
**    I_DEFAULT                     = 'X'
*       IS_LAYOUT                     = GS_LAYOUT
*     CHANGING
*       IT_OUTTAB                     = IT_FINAL_ALL_DETAILS_UPS
*       IT_FIELDCATALOG               = IT_FIELDCAT_ALL_DETAILS_UPS
**    IT_SORT                       =
*   EXCEPTIONS
*     INVALID_PARAMETER_COMBINATION = 1
*     PROGRAM_ERROR                 = 2
*     TOO_MANY_LINES                = 3
*     OTHERS                        = 4  .
*      IF SY-SUBRC <> 0.
*        MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                 WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*
*      SET HANDLER G_APPLICATION->HANDLE_HOTSPOT_CLICK FOR GO_GRID .
*
*
*
*    ENDIF.

  endif.
endform.                    "Alldetails

***********end of comparision between Fedex and SAP DATA



*&---------------------------------------------------------------------*
*&      Form  carrier_remittance_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form carrier_remittance_report .
  approve = 'X'.


  data: val type netwr, val1 type i,val2 type string,
*       val_str TYPE zcsshpmani-freight_amt,
        val_str type amount,
        val3 type i.
  data: column_texts type table of gprtxt with header line,
        column_texts1 type table of gprtxt with header line,
        values type table of gprval with header line.
  data:  noc type string.




  data: "v_mm(2), v_dd(2), v_yyyy(4),
         "count TYPE i,
         total_amnt type netwr,
         total_sum type netwr,
         total_savings type netwr,
         total_pkgs type i,
         total_weight type brgew_ap,
         avg_pkg_cst type netwr,
         avg_pnd_cst type netwr.

  data : ls_node(30) type c.
  data : v_numeric(12) type c.
  v_numeric = '.0123456789-'.

  import wa_node to ls_node from memory id 'NODE_MEMID'.



*  IF LS_NODE+8(3) = 'FDX' OR LS_NODE+8(3) = 'UPS'.
  set handler g_application->handle_user_command for go_grid.
  set handler g_application->handle_toolbar for go_grid.



  clear : wa_export_output.
  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
  if wa_export_output-carrier = 'FEDEX'.



    data :v_service type /PWEAVER/EFA_FED-service_type,
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_performance type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_credit type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt,
    v_transport type /PWEAVER/EFA_FED-net_chrg_amnt,
    v_invoice_amount type /PWEAVER/EFA_FED-net_chrg_amnt,
*    V_HANDLING TYPE ZPWEFA_FEDEX-NET_CHRG_AMNT.
 v_handling type /PWEAVER/EFA_FED-num_pieces.
    clear : v_service,v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_remit_type,it_final_remit_type,it_fieldcat_remit_type,it_output_remit_type.
    clear : it_remit_type,wa_remit_type,wa_final_remit_type,wa_fieldcat_remit_type.
    select *
              from /PWEAVER/EFA_FED
              into table it_remit_type
              where invoice_number = v_invoice_number and
                invoice_date eq wa_export_output-invoice_date.

    if sy-subrc = 0.
      clear : wa_remit_type.
      loop at it_remit_type into wa_remit_type.

        wa_final_remit_type-bill_to_acc_num = wa_remit_type-bill_to_acc_num.
        wa_final_remit_type-invoice_number = wa_remit_type-invoice_number.
        wa_final_remit_type-invoice_date = wa_remit_type-invoice_date.
        if wa_remit_type-service_type = 'Ground'.
          wa_final_remit_type-service_type  = 'FedEx Ground'.
        elseif   wa_remit_type-service_type <> 'Ground'.

          wa_final_remit_type-service_type  = 'FedEx Express'.
        endif.
*        WA_FINAL_REMIT_TYPE-SERVICE_TYPE  = 'GROUND'.
        wa_final_remit_type-packages = wa_remit_type-num_pieces.


        if wa_remit_type-trk_id_chg_des = 'Earned Discount'.
          wa_final_remit_type-discount =  wa_remit_type-trk_id_chg_amt.
        endif.

        if wa_remit_type-trk_id_chg_des1 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt1.
        endif.

        if wa_remit_type-trk_id_chg_des2 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt2.
        endif.

        if wa_remit_type-trk_id_chg_des3 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt3.
        endif.

        if wa_remit_type-trk_id_chg_des4 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt4.
        endif.

        if wa_remit_type-trk_id_chg_des5 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt5.
        endif.

        if wa_remit_type-trk_id_chg_des6 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt6.
        endif.

        if wa_remit_type-trk_id_chg_des7 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt7.
        endif.

        if wa_remit_type-trk_id_chg_des8 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt8.
        endif.

        if wa_remit_type-trk_id_chg_des9 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt9.
        endif.


        if wa_remit_type-trk_id_chg_des10 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt10.
        endif.

        if wa_remit_type-trk_id_chg_des11 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt11.
        endif.

        if wa_remit_type-trk_id_chg_des12 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt12.
        endif.

        if wa_remit_type-trk_id_chg_des13 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt13.
        endif.


        if wa_remit_type-trk_id_chg_des14 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt14.
        endif.

        if wa_remit_type-trk_id_chg_des15 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt15.
        endif.


        if wa_remit_type-trk_id_chg_des16 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt16.
        endif.

        if wa_remit_type-trk_id_chg_des17 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt17.
        endif.

        if wa_remit_type-trk_id_chg_des18 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt18.
        endif.

        if wa_remit_type-trk_id_chg_des19 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt19.
        endif.

        if wa_remit_type-trk_id_chg_des20 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt20.
        endif.

        if wa_remit_type-trk_id_chg_des21 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt21.
        endif.

        if wa_remit_type-trk_id_chg_des22 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt22.
        endif.

        if wa_remit_type-trk_id_chg_des23 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt23.
        endif.

        if wa_remit_type-trk_id_chg_des24 = 'Earned Discount'.
          wa_final_remit_type-discount =   wa_final_remit_type-discount + wa_remit_type-trk_id_chg_amt24.
        endif.


*****Performance Discount

        if wa_remit_type-trk_id_chg_des = 'Performance Pricing'.
          wa_final_remit_type-performance =  wa_remit_type-trk_id_chg_amt.
        endif.

        if wa_remit_type-trk_id_chg_des1 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt1.
        endif.

        if wa_remit_type-trk_id_chg_des2 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt2.
        endif.

        if wa_remit_type-trk_id_chg_des3 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt3.
        endif.

        if wa_remit_type-trk_id_chg_des4 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt4.
        endif.

        if wa_remit_type-trk_id_chg_des5 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt5.
        endif.

        if wa_remit_type-trk_id_chg_des6 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt6.
        endif.

        if wa_remit_type-trk_id_chg_des7 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt7.
        endif.

        if wa_remit_type-trk_id_chg_des8 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt8.
        endif.

        if wa_remit_type-trk_id_chg_des9 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt9.
        endif.


        if wa_remit_type-trk_id_chg_des10 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt10.
        endif.

        if wa_remit_type-trk_id_chg_des11 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt11.
        endif.

        if wa_remit_type-trk_id_chg_des12 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt12.
        endif.

        if wa_remit_type-trk_id_chg_des13 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt13.
        endif.


        if wa_remit_type-trk_id_chg_des14 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt14.
        endif.

        if wa_remit_type-trk_id_chg_des15 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt15.
        endif.


        if wa_remit_type-trk_id_chg_des16 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt16.
        endif.

        if wa_remit_type-trk_id_chg_des17 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt17.
        endif.

        if wa_remit_type-trk_id_chg_des18 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt18.
        endif.

        if wa_remit_type-trk_id_chg_des19 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt19.
        endif.

        if wa_remit_type-trk_id_chg_des20 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt20.
        endif.

        if wa_remit_type-trk_id_chg_des21 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt21.
        endif.

        if wa_remit_type-trk_id_chg_des22 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt22.
        endif.

        if wa_remit_type-trk_id_chg_des23 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt23.
        endif.

        if wa_remit_type-trk_id_chg_des24 = 'Performance Pricing'.
          wa_final_remit_type-performance =   wa_final_remit_type-performance + wa_remit_type-trk_id_chg_amt24.
        endif.




*********handling and Transport Charges
        wa_final_remit_type-transport_charge = wa_remit_type-tran_charg_amnt.
*        ENDIF.

****Handling charges
*        WA_FINAL_REMIT_TYPE-TRANSPORT_CHARGE = WA_REMIT_TYPE-TRAN_CHARG_AMNT.
*        ENDIF.







****Handling charges
        if wa_remit_type-trk_id_chg_des = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des = 'Address Correction' or
      wa_remit_type-trk_id_chg_des = 'Residential' or
      wa_remit_type-trk_id_chg_des = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des = 'DAS Extended Comm'.


*if not WA_REMIT_TYPE-TRK_ID_CHG_AMT is initial.
          wa_final_remit_type-handling_charge =  wa_remit_type-trk_id_chg_amt.
*endif.

        endif.

        if wa_remit_type-trk_id_chg_des1 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des1 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des1 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des1 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des1 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des1 = 'Residential' or
      wa_remit_type-trk_id_chg_des1 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des1 = 'DAS Extended Comm'.

*if not WA_REMIT_TYPE-TRK_ID_CHG_AMT1 is initial.
          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt1.
*endif.
        endif.

        if wa_remit_type-trk_id_chg_des2 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des2 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des2 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des2 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des2 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des2 = 'Residential' or
      wa_remit_type-trk_id_chg_des2 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des2 = 'DAS Extended Comm'.

*if not WA_REMIT_TYPE-TRK_ID_CHG_AMT2 is initial.
          wa_final_remit_type-handling_charge    =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt2.
*endif.
        endif.

        if wa_remit_type-trk_id_chg_des3 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des3 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des3 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des3 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des3 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des3 = 'Residential' or
      wa_remit_type-trk_id_chg_des3 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des3 = 'DAS Extended Comm'.

*if not WA_REMIT_TYPE-TRK_ID_CHG_AMT3 is initial.
          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt3.
*endif.
        endif.

        if wa_remit_type-trk_id_chg_des4 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des4 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des4 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des4 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des4 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des4 = 'Residential' or
      wa_serv_type-trk_id_chg_des4 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des4 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt4.
        endif.

        if wa_remit_type-trk_id_chg_des5 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des5 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des5 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des5 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des5 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des5 = 'Residential' or
      wa_remit_type-trk_id_chg_des5 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des5 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt5.
        endif.

        if wa_remit_type-trk_id_chg_des6 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des6 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des6 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des6 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des6 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des6 = 'Residential' or
      wa_remit_type-trk_id_chg_des6 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des6 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt6.
        endif.

        if wa_remit_type-trk_id_chg_des7 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des7 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des7 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des7 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des7 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des7 = 'Residential' or
      wa_remit_type-trk_id_chg_des7 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des7 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt7.
        endif.

        if wa_remit_type-trk_id_chg_des8 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des8 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des8 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des8 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des8 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des8 = 'Residential' or
      wa_remit_type-trk_id_chg_des8 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des8 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt8.
        endif.


        if wa_remit_type-trk_id_chg_des9 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des9 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des9 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des9 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des9 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des9 = 'Residential' or
      wa_remit_type-trk_id_chg_des9 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des9 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt9.
        endif.


        if wa_remit_type-trk_id_chg_des10 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des10 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des10 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des10 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des10 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des10 = 'Residential' or
      wa_remit_type-trk_id_chg_des10 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des10 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt10.
        endif.


        if wa_remit_type-trk_id_chg_des11 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des11 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des11 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des11 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des11 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des11 = 'Residential' or
      wa_remit_type-trk_id_chg_des11 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des11 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt11.
        endif.


        if wa_remit_type-trk_id_chg_des12 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des12 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des12 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des12 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des12 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des12 = 'Residential' or
      wa_remit_type-trk_id_chg_des12 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des12 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt12.
        endif.


        if wa_remit_type-trk_id_chg_des13 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des13 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des13 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des13 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des13 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des13 = 'Residential' or
      wa_remit_type-trk_id_chg_des13 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des13 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt13.
        endif.


        if wa_remit_type-trk_id_chg_des14 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des14 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des14 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des14 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des14 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des14 = 'Residential' or
      wa_remit_type-trk_id_chg_des14 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des14 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt14.
        endif.


        if wa_remit_type-trk_id_chg_des15 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des15 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des15 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des15 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des15 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des15 = 'Residential' or
      wa_remit_type-trk_id_chg_des15 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des15 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt15.
        endif.


        if wa_remit_type-trk_id_chg_des16 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des16 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des16 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des16 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des16 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des16 = 'Residential' or
      wa_remit_type-trk_id_chg_des16 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des16 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt16.
        endif.


        if wa_remit_type-trk_id_chg_des17 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des17 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des17 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des17 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des17 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des17 = 'Residential' or
      wa_remit_type-trk_id_chg_des17 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des17 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt17.
        endif.


        if wa_remit_type-trk_id_chg_des18 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des18 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des18 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des18 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des18 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des18 = 'Residential' or
      wa_remit_type-trk_id_chg_des18 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des18 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt18.
        endif.


        if wa_remit_type-trk_id_chg_des19 = 'Fuel Surcharge' or
wa_remit_type-trk_id_chg_des19 = 'Weekly Service Chg' or
wa_remit_type-trk_id_chg_des19 = 'Weekday Delivery' or
wa_remit_type-trk_id_chg_des19 = 'Residential Delivery' or
wa_remit_type-trk_id_chg_des19 = 'Address Correction' or
wa_remit_type-trk_id_chg_des19 = 'Residential' or
wa_remit_type-trk_id_chg_des19 = 'DAS Comm' or
wa_remit_type-trk_id_chg_des19 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt19.
        endif.

        if wa_remit_type-trk_id_chg_des20 = 'Fuel Surcharge' or
wa_remit_type-trk_id_chg_des20 = 'Weekly Service Chg' or
wa_remit_type-trk_id_chg_des20 = 'Weekday Delivery' or
wa_remit_type-trk_id_chg_des20 = 'Residential Delivery' or
wa_remit_type-trk_id_chg_des20 = 'Address Correction' or
wa_remit_type-trk_id_chg_des20 = 'Residential' or
wa_remit_type-trk_id_chg_des20 = 'DAS Comm' or
wa_remit_type-trk_id_chg_des20 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt20.
        endif.

        if wa_remit_type-trk_id_chg_des21 = 'Fuel Surcharge' or
wa_remit_type-trk_id_chg_des21 = 'Weekly Service Chg' or
wa_remit_type-trk_id_chg_des21 = 'Weekday Delivery' or
wa_remit_type-trk_id_chg_des21 = 'Residential Delivery' or
wa_remit_type-trk_id_chg_des21 = 'Address Correction' or
wa_remit_type-trk_id_chg_des21 = 'Residential' or
wa_remit_type-trk_id_chg_des21 = 'DAS Comm' or
wa_remit_type-trk_id_chg_des21 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt21.
        endif.

        if wa_remit_type-trk_id_chg_des22 = 'Fuel Surcharge' or
      wa_remit_type-trk_id_chg_des22 = 'Weekly Service Chg' or
      wa_remit_type-trk_id_chg_des22 = 'Weekday Delivery' or
      wa_remit_type-trk_id_chg_des22 = 'Residential Delivery' or
      wa_remit_type-trk_id_chg_des22 = 'Address Correction' or
      wa_remit_type-trk_id_chg_des22 = 'Residential' or
      wa_remit_type-trk_id_chg_des22 = 'DAS Comm' or
      wa_remit_type-trk_id_chg_des22 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt22.
        endif.

        if wa_remit_type-trk_id_chg_des23 = 'Fuel Surcharge' or
wa_remit_type-trk_id_chg_des23 = 'Weekly Service Chg' or
wa_remit_type-trk_id_chg_des23 = 'Weekday Delivery' or
wa_remit_type-trk_id_chg_des23 = 'Residential Delivery' or
wa_remit_type-trk_id_chg_des23 = 'Address Correction' or
wa_remit_type-trk_id_chg_des23 = 'Residential' or
wa_remit_type-trk_id_chg_des23 = 'DAS Comm' or
wa_remit_type-trk_id_chg_des23 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt23.
        endif.

        if wa_remit_type-trk_id_chg_des24 = 'Fuel Surcharge' or
wa_remit_type-trk_id_chg_des24 = 'Weekly Service Chg' or
wa_remit_type-trk_id_chg_des24 = 'Weekday Delivery' or
wa_remit_type-trk_id_chg_des24 = 'Residential Delivery' or
wa_remit_type-trk_id_chg_des24 = 'Address Correction' or
wa_remit_type-trk_id_chg_des24 = 'Residential' or
wa_remit_type-trk_id_chg_des24 = 'DAS Comm' or
wa_remit_type-trk_id_chg_des24 = 'DAS Extended Comm'.

          wa_final_remit_type-handling_charge =  wa_final_remit_type-handling_charge + wa_remit_type-trk_id_chg_amt24.
        endif.





*       WA_FINAL_REMIT_TYPE-CREDIT_AMOUNT = WA_REMIT_TYPE-CREDIT_AMOUNT.
        wa_final_remit_type-credit_amount = '0.00'.
        wa_final_remit_type-net_charge = wa_remit_type-net_chrg_amnt.
        wa_final_remit_type-invoice_amount = wa_remit_type-net_chrg_amnt.

        append wa_final_remit_type to it_final_remit_type.
        clear :wa_final_remit_type,wa_remit_type.
      endloop.


      sort it_final_remit_type by service_type handling_charge.
      clear wa_final_remit_type.



      loop at it_final_remit_type into wa_final_remit_type.

*        IF NOT WA_FINAL_REMIT_TYPE IS INITIAL.
*        WA_OUTPUT_REMIT_TYPE-BILL_TO_ACC_NUM = WA_FINAL_REMIT_TYPE-BILL_TO_ACC_NUM.
        wa_output_remit_type-bill_to_acc_num = '400000011'.
        wa_output_remit_type-invoice_number = wa_final_remit_type-invoice_number.
        wa_output_remit_type-invoice_date = wa_final_remit_type-invoice_date.
        concatenate    wa_output_remit_type-invoice_date+0(4) '-' wa_output_remit_type-invoice_date+4(2) '-' wa_output_remit_type-invoice_date+6(2) into wa_output_remit_type-invoice_date.
        at end of service_type.
          sum.     " calculates sub totals on service types
          wa_output_remit_type-service_type = wa_final_remit_type-service_type.
          wa_output_remit_type-handling_charge =   wa_final_remit_type-handling_charge.
          wa_output_remit_type-packages =   wa_final_remit_type-packages.
          wa_output_remit_type-invoice_amount =   wa_final_remit_type-invoice_amount.
          wa_output_remit_type-discount =   wa_final_remit_type-discount.
          wa_output_remit_type-performance =   wa_final_remit_type-performance.

          wa_output_remit_type-transport_charge =   wa_final_remit_type-transport_charge.
          wa_output_remit_type-credit_amount =   wa_final_remit_type-credit_amount.
          wa_output_remit_type-net_charge =  wa_final_remit_type-net_charge.
* calculate the grand totals

*v_service = v_service + wa_output_serv_type-service_type.
          v_packages = v_packages + wa_output_remit_type-packages.
          v_handling = v_handling  + wa_output_remit_type-handling_charge.
          v_discount = v_discount + wa_output_remit_type-discount.
          v_performance = v_performance  + wa_output_remit_type-performance.
          v_transport = v_transport  + wa_output_remit_type-transport_charge.
          v_invoice_amount = v_invoice_amount + wa_output_remit_type-invoice_amount.
*            V_HANDLING = V_HANDLING  + WA_OUTPUT_REMIT_TYPE-HANDLING_CHARGE.
          v_credit = v_credit + wa_output_remit_type-credit_amount.
          v_netcharge = v_netcharge +  wa_output_remit_type-net_charge .
*            collect WA_OUTPUT_REMIT_TYPE inTO IT_OUTPUT_REMIT_TYPE.
          append wa_output_remit_type to it_output_remit_type.
          clear : wa_output_remit_type,wa_final_remit_type.
        endat.


*       ENDIF.
        clear wa_final_remit_type.
      endloop.


* add the grand totals as last record
      clear : wa_output_remit_type.

*    wa_output_serv_type-service_type = v_service.
      wa_output_remit_type-packages = v_packages.

      wa_output_remit_type-discount = v_discount .
      wa_output_remit_type-performance = v_performance .
      wa_output_remit_type-credit_amount = v_credit .
      wa_output_remit_type-net_charge = v_netcharge.
      wa_output_remit_type-transport_charge = v_transport .
      wa_output_remit_type-invoice_amount = v_invoice_amount.
      wa_output_remit_type-handling_charge = v_handling.


      wa_output_remit_type-line_color      = 'C500'.
      append wa_output_remit_type to it_output_remit_type.
      clear wa_output_remit_type.



      wa_fieldcat_remit_type-col_pos      = 1.
      wa_fieldcat_remit_type-fieldname    = 'BILL_TO_ACC_NUM'.
      wa_fieldcat_remit_type-coltext    = 'Account Number'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.


      wa_fieldcat_remit_type-col_pos      = 2.
      wa_fieldcat_remit_type-fieldname    = 'INVOICE_NUMBER'.
      wa_fieldcat_remit_type-coltext    = 'Invoice Number'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.

      wa_fieldcat_remit_type-col_pos      = 3.
      wa_fieldcat_remit_type-fieldname    = 'INVOICE_DATE'.
      wa_fieldcat_remit_type-coltext    = 'Invoice Date'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.

      wa_fieldcat_remit_type-col_pos      = 4.
      wa_fieldcat_remit_type-fieldname    = 'SERVICE_TYPE'.
      wa_fieldcat_remit_type-coltext    = 'Service Type'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.



      wa_fieldcat_remit_type-col_pos      = 5.
      wa_fieldcat_remit_type-fieldname    = 'PACKAGES'.
      wa_fieldcat_remit_type-coltext    = 'Total Packages'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.

      wa_fieldcat_remit_type-col_pos      = 6.
      wa_fieldcat_remit_type-fieldname    = 'TRANSPORT_CHARGE'.
      wa_fieldcat_remit_type-coltext    = 'Transport Charges'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.

      wa_fieldcat_remit_type-col_pos      = 7.
      wa_fieldcat_remit_type-fieldname    = 'HANDLING_CHARGE'.
      wa_fieldcat_remit_type-coltext    = 'Handling Charges'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.

      wa_fieldcat_remit_type-col_pos      = 8.
      wa_fieldcat_remit_type-fieldname    = 'DISCOUNT'.
      wa_fieldcat_remit_type-coltext    = 'Earned Discount'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.

      wa_fieldcat_remit_type-col_pos      = 9.
      wa_fieldcat_remit_type-fieldname    = 'PERFORMANCE'.
      wa_fieldcat_remit_type-coltext    = 'Performance Pricing'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.


      wa_fieldcat_remit_type-col_pos      = 10.
      wa_fieldcat_remit_type-fieldname    = 'INVOICE_AMOUNT'.
      wa_fieldcat_remit_type-coltext    = 'Invoice Amount'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.


      wa_fieldcat_remit_type-col_pos      = 11.
      wa_fieldcat_remit_type-fieldname    = 'CREDIT_AMOUNT'.
      wa_fieldcat_remit_type-coltext    = 'Credit Amount'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.

      wa_fieldcat_remit_type-col_pos      = 12.
      wa_fieldcat_remit_type-fieldname    = 'NET_CHARGE'.
      wa_fieldcat_remit_type-coltext    = 'Net Charge'.
      wa_fieldcat_remit_type-tabname      = 'IT_OUPUT_REMIT_TYPE'.
      append wa_fieldcat_remit_type to it_fieldcat_remit_type.
      clear wa_fieldcat_remit_type.



      gs_layout-cwidth_opt = 'X'.
      gs_layout-info_fname = 'LINE_COLOR'.
      gs_layout-grid_title = space.


      call method go_grid->set_table_for_first_display
        exporting
          i_structure_name              = 'IT_OUPUT_REMIT_TYPE'
          is_layout                     = gs_layout
        changing
          it_outtab                     = it_output_remit_type
          it_fieldcatalog               = it_fieldcat_remit_type
*    IT_SORT                       =
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4  .
      if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      endif.



    endif.
  endif.
  if ups = 'X'.

*    ZCARRIERTYPE = 'UPS'.
*    SELECT  *
*              FROM ZPWEFA_INVC_UPS
*              INTO CORRESPONDING FIELDS OF TABLE IT_CARRIER_REMITT_FINAL_UPS
*              WHERE INVOICE_DATE EQ WEEK_DT AND CARRIER EQ ZCARRIERTYPE.
*
*    IF SY-SUBRC = 0.
*      SORT IT_CARRIER_REMITT_FINAL_UPS BY INVOICE_NO.
*      CLEAR WA_CARRIER_REMITT_FINAL_UPS.
*
*      LOOP AT IT_CARRIER_REMITT_FINAL_UPS INTO WA_CARRIER_REMITT_FINAL_UPS.
*
*        WA_CARRIER_REMITT_FINAL_UPS-TOTAL_SHIPMENTS = 1.
*
*        MODIFY IT_CARRIER_REMITT_FINAL_UPS FROM WA_CARRIER_REMITT_FINAL_UPS TRANSPORTING TOTAL_SHIPMENTS.
*
*        AT END OF INVOICE_NO.
*          SUM.     " calculates sub totals on invoice
*
*
*
**          wa_carrier_output-netdue          = wa_carrier_final-netdue.
*          WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_SHIPMENTS = WA_CARRIER_REMITT_FINAL_UPS-TOTAL_SHIPMENTS.
*          WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_AMOUNT = WA_CARRIER_REMITT_FINAL_UPS-TOTAL_SHIPMENTS.
**Testing purpose
**wa_carrier_remittance_final-discount = '1000.00'.
*
*          WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_INC_CREDIT = WA_CARRIER_REMITT_FINAL_UPS-TOTAL_INC_CREDIT.
*
*
*****Testing
*          WA_CARRIER_REMITT_FINAL_UPS-ACCTNO = '57926838'.
*          IF NOT WA_CARRIER_REMITT_FINAL_UPS-ACCTNO IS INITIAL.
*            WA_CARRIER_REMITT_OUTPUT_UPS-ACCTNO = WA_CARRIER_REMITT_FINAL_UPS-ACCTNO.
*
*          ENDIF.
*          WA_CARRIER_REMITT_OUTPUT_UPS-INVOICE_DATE = WEEK_DT.
*          WA_CARRIER_REMITT_OUTPUT_UPS-SERVICE_TYPE = 'Ground/Express Shipping'.
*          WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_NON_SHIPMENTS = 0.
*          WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_BILL_CHARG = WA_CARRIER_REMITT_FINAL_UPS-TOTAL_BILL_CHARG.
*          WA_CARRIER_REMITT_OUTPUT_UPS-NETDUE = WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_BILL_CHARG - WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_INC_CREDIT.
** calculate the grand totals
*          TOTAL_AMNT = TOTAL_AMNT + WA_CARRIER_REMITT_OUTPUT_UPS-NETDUE.
*          TOTAL_SAVINGS = TOTAL_SAVINGS + WA_CARRIER_REMITT_FINAL_UPS-TOTAL_INC_CREDIT.
*          TOTAL_PKGS    = TOTAL_PKGS + WA_CARRIER_REMITT_FINAL_UPS-TOTAL_SHIPMENTS.
*          TOTAL_SUM = TOTAL_SUM + WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_BILL_CHARG.
*          APPEND  WA_CARRIER_REMITT_OUTPUT_UPS TO  IT_CARRIER_REMITT_OUTPUT_UPS.
*          CLEAR WA_CARRIER_REMITT_FINAL_UPS.
*        ENDAT.
*        CLEAR WA_CARRIER_REMITT_OUTPUT_UPS.
*      ENDLOOP.
*
*
** add the grand totals as last record
*      WA_CARRIER_REMITT_OUTPUT_UPS-ACCTNO = 'Total'.
*      WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_SHIPMENTS = TOTAL_PKGS.
*      WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_AMOUNT  = TOTAL_PKGS.
*      WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_INC_CREDIT = TOTAL_SAVINGS .
*      WA_CARRIER_REMITT_OUTPUT_UPS-NETDUE          = TOTAL_AMNT.
*      WA_CARRIER_REMITT_OUTPUT_UPS-TOTAL_BILL_CHARG       = TOTAL_SUM.
*
*      WA_CARRIER_REMITT_OUTPUT_UPS-LINE_COLOR      = 'C500'.
*      APPEND WA_CARRIER_REMITT_OUTPUT_UPS TO IT_CARRIER_REMITT_OUTPUT_UPS.
*      CLEAR WA_CARRIER_REMITT_OUTPUT_UPS.
*
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 1.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'ACCTNO'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Carrier A/C #'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 2.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'INVOICE_NO'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'SAP A/P Doc#'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 3.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'INVOICE_DATE'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Invoice Date'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 4.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'SERVICE_TYPE'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Invoice Type'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 5.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'TOTAL_AMOUNT'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Total Count'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 6.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'TOTAL_SHIPMENTS'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Package Count'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 7.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'TOTAL_NON_SHIPMENTS'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Non Package Count'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 8.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'TOTAL_BILL_CHARG'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Original Invoice Amount'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 9.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'TOTAL_INC_CREDIT'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Credit Amount'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COL_POS      = 10.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-FIELDNAME    = 'NETDUE'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-COLTEXT    = 'Net Due Amount'.
*      WA_CARRIER_REMITT_FIELDCAT_UPS-TABNAME      = 'IT_CARRIER_REMITT_OUTPUT_UPS'.
*      APPEND WA_CARRIER_REMITT_FIELDCAT_UPS TO IT_CARRIER_REMITT_FIELDCAT_UPS.
*      CLEAR WA_CARRIER_REMITT_FIELDCAT_UPS.
*
*
*      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*      GS_LAYOUT-GRID_TITLE = '                             Carrier Remittance Report                              '.
*
*
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*        EXPORTING
*               I_STRUCTURE_NAME              = 'IT_CARRIER_REMITT_OUTPUT_UPS'
*          IS_LAYOUT                     = GS_LAYOUT
*        CHANGING
*          IT_OUTTAB                     = IT_CARRIER_REMITT_OUTPUT_UPS
*          IT_FIELDCATALOG               = IT_CARRIER_REMITT_FIELDCAT_UPS
**    IT_SORT                       =
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4.
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*    ENDIF.
*
*
*
*    FREE MEMORY ID 'NODE_MEMID'.

  endif.
*
*  ENDIF.
endform.                    " carrier_remittance_report

*&---------------------------------------------------------------------*
*&      Form  GSR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form gsr .
  clear : approve.
  clear : wa_export_output.
*  read table it_export_output into wa_export_output with key invoice_number = v_invoice_number.
*  if wa_export_output-carrier = 'FEDEX'.

    data : t_hype_link type lvc_t_hype,
           fs_hype_link type lvc_s_hype.

    data :v_gsr type /PWEAVER/EFA_FED-net_chrg_amnt,
    v_packages type /PWEAVER/EFA_FED-num_pieces,
    v_rated_weight type /PWEAVER/EFA_FED-rated_wgt_amt,
    v_trans_charg  type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_hand_charg type /PWEAVER/EFA_FED-tran_charg_amnt,
     v_discount type /PWEAVER/EFA_FED-tran_charg_amnt,
    v_netcharge type /PWEAVER/EFA_FED-net_chrg_amnt.

    clear : v_packages,v_rated_weight,v_trans_charg,v_hand_charg,v_discount,v_netcharge.
    refresh : it_gsr_type,it_final_gsr_type,it_fieldcat_gsr_type,it_output_gsr_type.
    clear : it_gsr_type,wa_gsr_type,wa_final_gsr_type,wa_fieldcat_gsr_type.

*break-point.
*if v_invoice_number = '713032737'.
*    select *
*              from zpwefa_fedex
*              into corresponding fields of table it_gsr_type
*              where invoice_number = '726155334'.
**WHERE INVOICE_NUMBER = V_INVOICE_NUMBER AND
**                INVOICE_DATE EQ WA_EXPORT_OUTPUT-INVOICE_DATE.
*endif.
  data : v_podate type /PWEAVER/MANFEST-pod_date,
         v_expdate type /PWEAVER/MANFEST-pod_date,
*         v_diff_days type VTBBEWE-ATAGE,
         v_podsig type /PWEAVER/MANFEST-pod_signature.

    select *
              from /PWEAVER/EFA_FED
              into corresponding fields of table it_gsr_type
              where invoice_number = v_invoice_number and grsflag = 'X'.
    if sy-subrc = 0.
      clear : wa_gsr_type.
*
      loop at it_gsr_type into wa_gsr_type.
        wa_gsr_fedex-EXP_GRD_TRCK_ID = wa_gsr_type-EXP_GRD_TRCK_ID.
        wa_gsr_fedex-pickup_date = wa_gsr_type-SHIPMENT_DATE.
        select single POD_DATE POD_SIGNATURE EXP_DEL_DATE from /PWEAVER/MANFEST INTO (V_PODATE , V_PODSIG , V_EXPDATE)  WHERE TRACKING_NUMBER = WA_GSR_TYPE-EXP_GRD_TRCK_ID.
        wa_gsr_fedex-POD_DEL_DATE = V_PODATE.
        wa_gsr_fedex-POD_SIGN_DESC = V_PODSIG.
        wa_gsr_fedex-exp_date = V_EXPDATE.
        APPEND WA_GSR_FEDEX TO IT_GSR_FEDEX.
        CLEAR : wa_gsr_fedex,
                v_podate,
                v_podsig,
                v_expdate.
        ENDLOOP.
        ENDIF.
        refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'EXP_GRD_TRCK_ID'.
  wa_fieldcat-coltext      = 'Tracking Number'.
  wa_fieldcat-tabname      = 'IT_GSR_FEDEX'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'PICKUP_DATE'.
  wa_fieldcat-coltext      = 'Pick Up Date'.
  wa_fieldcat-tabname      = 'IT_GSR_FEDEX'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'POD_DEL_DATE'.
  wa_fieldcat-coltext      = 'Pod Date'.
  wa_fieldcat-tabname      = 'IT_GSR_FEDEX'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.
    wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'POD_SIGN_DESC'.
  wa_fieldcat-coltext      = 'Pod Sig'.
  wa_fieldcat-tabname      = 'IT_GSR_FEDEX'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'EXP_DATE'.
  wa_fieldcat-coltext      = 'Expected Date'.
  wa_fieldcat-tabname      = 'IT_GSR_FEDEX'.
*      wa_fieldcat-hotspot      = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By GSR '.


  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_GSR_FEDEX'
      is_layout                     = gs_layout
    changing
      it_outtab                     = it_GSR_FEDEX
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
*
*
*        wa_final_gsr_type-shipment_date  = wa_gsr_type-shipment_date.
*        wa_final_gsr_type-service_type  = wa_gsr_type-service_type.
*        wa_final_gsr_type-exp_grd_trck_id  = wa_gsr_type-exp_grd_trck_id.
*        wa_final_gsr_type-org_cust_ref  = wa_gsr_type-org_cust_ref.
*        wa_final_gsr_type-org_ref_3  = wa_gsr_type-org_ref_3.
**        WA_FINAL_GSR_TYPE-POD  = WA_GSR_TYPE-POD.
*        wa_final_gsr_type-pod_del_date  = wa_gsr_type-pod_del_date.
*        wa_final_gsr_type-pod_del_time  = wa_gsr_type-pod_del_time.
*        wa_final_gsr_type-pod_sign_desc  = wa_gsr_type-pod_sign_desc.
*        wa_final_gsr_type-transport_charge = wa_gsr_type-tran_charg_amnt.
*
*
**        ENDIF.
*
*
*
*
*        append wa_final_gsr_type to it_final_gsr_type.
*        clear wa_final_gsr_type.
*      endloop.


*      sort it_final_gsr_type by service_type.
*      clear wa_final_gsr_type.


*      loop at it_final_gsr_type into wa_final_gsr_type.
*
*
******check with zpwefa_gsr table
*
**        SELECT SINGLE * FROM ZPWEFA_GSR INTO ZPWEFA_GSR WHERE EXP_GRD_TRCK_ID = WA_FINAL_GSR_TYPE-EXP_GRD_TRCK_ID.
*
**IF SY-SUBRC = 0.
*
**         IF ZPWEFA_GSR-DELIVERY_DATE > WA_FINAL_GSR_TYPE-POD_DEL_DATE.
*******************Address correction
*        concatenate wa_final_gsr_type-shipment_date+4(2)'-' wa_final_gsr_type-shipment_date+6(2) '-' wa_final_gsr_type-shipment_date+0(4)  into wa_final_gsr_type-shipment_date.
*
*        wa_output_gsr_type-shipment_date                   = wa_final_gsr_type-shipment_date.
*        wa_output_gsr_type-service_type      =  wa_final_gsr_type-service_type.
*        wa_output_gsr_type-exp_grd_trck_id      = wa_final_gsr_type-exp_grd_trck_id.
*        wa_output_gsr_type-org_cust_ref       = wa_final_gsr_type-org_cust_ref.
*        wa_output_gsr_type-org_ref_3        = wa_final_gsr_type-org_ref_3.
*
*        concatenate wa_final_gsr_type-pod_del_date+4(2)'-' wa_final_gsr_type-pod_del_date+6(2) '-' wa_final_gsr_type-pod_del_date+0(4)  into wa_final_gsr_type-pod_del_date.
*
*        wa_output_gsr_type-pod_del_date        = wa_final_gsr_type-pod_del_date.
*        wa_output_gsr_type-pod_del_time        = wa_final_gsr_type-pod_del_time.
*        wa_output_gsr_type-pod_sign_desc        = wa_final_gsr_type-pod_sign_desc.
*        wa_output_gsr_type-transport_charge        = wa_final_gsr_type-transport_charge.
*        v_trans_charg = v_trans_charg + wa_output_gsr_type-transport_charge.
*
*        append wa_output_gsr_type to it_output_gsr_type.
*        clear wa_output_gsr_type.
*        clear wa_final_gsr_type.
**          ENDIF.
**        ENDIF.
*      endloop.


* add the grand totals as last record
*      clear : wa_output_gsr_type.
*
**    wa_output_serv_type-service_type = v_service.
*      if not it_output_gsr_type[] is initial.
*
*        wa_output_gsr_type-transport_charge = v_trans_charg.
*
*
*        wa_output_gsr_type-line_color      = 'C500'.
*        append wa_output_gsr_type to it_output_gsr_type.
*        clear wa_output_gsr_type.
*
*      endif.

*      wa_fieldcat_gsr_type-col_pos      = 1.
*      wa_fieldcat_gsr_type-fieldname    = 'SHIPMENT_DATE'.
*      wa_fieldcat_gsr_type-coltext    = 'Shipment Date'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_GSR_TYPE'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*      wa_fieldcat_gsr_type-col_pos      = 2.
*      wa_fieldcat_gsr_type-fieldname    = 'SERVICE_TYPE'.
*      wa_fieldcat_gsr_type-coltext    = 'Service Type'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_GSR_TYPE'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*      wa_fieldcat_gsr_type-col_pos      = 3.
*      wa_fieldcat_gsr_type-fieldname    = 'EXP_GRD_TRCK_ID'.
*      wa_fieldcat_gsr_type-coltext    = 'Tracking Number'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
*      wa_fieldcat_gsr_type-hotspot   = 'x'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*      wa_fieldcat_gsr_type-col_pos      = 4.
*      wa_fieldcat_gsr_type-fieldname    = 'ORG_CUST_REF'.
*      wa_fieldcat_gsr_type-coltext    = 'Reference #'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*      wa_fieldcat_gsr_type-col_pos      = 5.
*      wa_fieldcat_gsr_type-fieldname    = 'ORG_REF_3'.
*      wa_fieldcat_gsr_type-coltext    = 'PO #'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*      wa_fieldcat_gsr_type-col_pos      = 6.
*      wa_fieldcat_gsr_type-fieldname    = 'POD '.
*      wa_fieldcat_gsr_type-coltext    = 'Proof of Delivery'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
*      wa_fieldcat_gsr_type-hotspot   = 'x'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*      wa_fieldcat_gsr_type-col_pos      = 7.
*      wa_fieldcat_gsr_type-fieldname    = 'POD_DEL_DATE'.
*      wa_fieldcat_gsr_type-coltext    = 'POD Date'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*      wa_fieldcat_gsr_type-col_pos      = 8.
*      wa_fieldcat_gsr_type-fieldname    = 'POD_DEL_TIME'.
*      wa_fieldcat_gsr_type-coltext    = 'POD Time'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*      wa_fieldcat_gsr_type-col_pos      = 9.
*      wa_fieldcat_gsr_type-fieldname    = 'POD_SIGN_DESC'.
*      wa_fieldcat_gsr_type-coltext    = 'POD Signature'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_SERV_TYPE'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*
*      wa_fieldcat_gsr_type-col_pos      = 10.
*      wa_fieldcat_gsr_type-fieldname    = 'TRANSPORT_CHARGE'.
*      wa_fieldcat_gsr_type-coltext    = 'Transportation Charge'.
*      wa_fieldcat_gsr_type-tabname      = 'IT_OUPUT_GSR_TYPE'.
*      append wa_fieldcat_gsr_type to it_fieldcat_gsr_type.
*      clear wa_fieldcat_gsr_type.
*
*
*      gs_layout-cwidth_opt = 'X'.
*      gs_layout-info_fname = 'LINE_COLOR'.
*      gs_layout-grid_title = space.
*
*
*      call method go_grid->set_table_for_first_display
*        exporting
*          i_structure_name              = 'IT_OUPUT_GSR_TYPE'
*          is_layout                     = gs_layout
*        changing
*          it_outtab                     = it_output_gsr_type
*          it_fieldcatalog               = it_fieldcat_gsr_type
**    IT_SORT                       =
*      exceptions
*        invalid_parameter_combination = 1
*        program_error                 = 2
*        too_many_lines                = 3
*        others                        = 4  .
*      if sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      endif.
*
*      set handler g_application->handle_hotspot_click for go_grid .

**    endif.
*  endif.

endform.                    " GSR
*&---------------------------------------------------------------------*
*&      Form  samsung_download
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
*FORM SAMSUNG_DOWNLOAD .
*
*  CLEAR : APPROVE.
*  CLEAR : WA_EXPORT_OUTPUT.
*  READ TABLE IT_EXPORT_OUTPUT INTO WA_EXPORT_OUTPUT WITH KEY INVOICE_NUMBER = V_INVOICE_NUMBER.
*
*  IF WA_EXPORT_OUTPUT-CARRIER = 'FEDEX'.
*
*    DATA : V_SHIPMENT(40) TYPE C.
*
*    REFRESH : IT_SAM_FEDEX_TYPE,IT_SAM_FEDEX_TYPE,IT_FIELDCAT_SAMSUNG_TYPE.
*    CLEAR : IT_SAM_FEDEX_TYPE,IT_SAM_FEDEX_TYPE,IT_FIELDCAT_SAMSUNG_TYPE,WA_FIELDCAT_SAMSUNG_TYPE,WA_SAM_FEDEX_TYPE,WA_SAM_FEDEX_TYPE.
*    SELECT *
*              FROM ZPWEFA_FEDEX
*              INTO CORRESPONDING FIELDS OF TABLE IT_SAM_FEDEX_TYPE
*              WHERE INVOICE_NUMBER = V_INVOICE_NUMBER AND
*                INVOICE_DATE EQ WA_EXPORT_OUTPUT-INVOICE_DATE.
*
*    IF SY-SUBRC = 0.
*
*
*      LOOP AT IT_SAM_FEDEX_TYPE INTO WA_SAM_FEDEX_TYPE.
*
*        WA_OUTPUT_SAM_FEDEX_TYPE-SENDER_IDENTIFIER = 'DJUI'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-RECEIVER_IDENTIFIER = '31BA'.
*        CONCATENATE WA_SAM_FEDEX_TYPE-BILL_TO_ACC_NUM '-' WA_SAM_FEDEX_TYPE-EXP_GRD_TRCK_ID INTO V_SHIPMENT.
*
*        WA_OUTPUT_SAM_FEDEX_TYPE-INVOICE_NUMBER = V_SHIPMENT.
*
*        DATA : V_DATE(8) TYPE C.
*        CLEAR : V_DATE.
*        CONCATENATE WA_SAM_FEDEX_TYPE-INVOICE_DATE+0(4) WA_SAM_FEDEX_TYPE-INVOICE_DATE+4(2) WA_SAM_FEDEX_TYPE-INVOICE_DATE+6(2) INTO V_DATE.
*
**        WA_OUTPUT_SAM_FEDEX_TYPE-BILLING_DATE = WA_SAM_FEDEX_TYPE-INVOICE_DATE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-BILLING_DATE = V_DATE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-DIVISION = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-BUSINESS_TYPE = 'LD'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-ORDER_TYPE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TRANSPORTATION_TYPE = 'M1'.
*        DATA : V_SHIP_DATE(8) TYPE C.
*        CLEAR : V_SHIP_DATE.
*        CONCATENATE WA_SAM_FEDEX_TYPE-SHIPMENT_DATE+0(4) WA_SAM_FEDEX_TYPE-SHIPMENT_DATE+4(2) WA_SAM_FEDEX_TYPE-SHIPMENT_DATE+6(2) INTO V_SHIP_DATE.
*
*        WA_OUTPUT_SAM_FEDEX_TYPE-ACTUAL_DATE = V_SHIP_DATE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REFERENCE_TYPE = '6DT'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REFERENCE_MESSAGE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REFERENCE_DOCUMENT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REFERENCE_DATE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-RELATED_DOCUMENT_1 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-RELATED_DOCUMENT_2 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SERVICE_TYPE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-INCOTERMS_CODE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-INCOTERMS_PLACE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-PLANT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-WH_LOCATION = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CARRIER = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-FORWARDER = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-FROM_PLACE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-COUNTRY_FROM = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-FROM_PORT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-COUNTRY_PORT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TO_PLACE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-COUNTRY_PLACE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TO_PORT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-COUNTRY_OF_PORT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-GENERAL_INFO_1 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-GENERAL_INFO_2 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-GENERAL_INFO_3 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SEQUENCE_NUM = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CARGO_MEASUREMENT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-MESASURE_AMOUNT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-MESASURE_UNIT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SEQUENCE_AMOUNT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TRUCK_CLASSIFICATIN = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TRUCK_NUMBER = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TRUCK_TYPE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TRUCK_ROUND = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CONSOLIDATION_NUMBER = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CONSOLIDATION_COUNT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-MULTI_STOP = SPACE.
*
**********
*
*
*************
*        WA_OUTPUT_SAM_FEDEX_TYPE-FR_ISSUE_DATE  = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-RATE_EXCHANGE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-BASIS_CURRENCY = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-DESTINATION_CURRENCY = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-RATE_EXCHANGE_DATE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REMARK_1 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REMARK_2 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REMARK_3 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SEQUENCE_NUMBER = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REFERENCE_DOC_CODE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REFERENCE_DOC_NUMBER = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REFERENCE_DOC_DATE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-MATERIAL = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-PLANT_1 = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-PACK_QUANTITY = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-PACK_QUAN_CODE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-QUANTITY = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-QUANTITY_CODE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-VOLUME = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-VOLUME_CODE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-GROSS_WEIGHT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-GROSS_WEIGHT_CODE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-NET_WEIGHT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-NET_WEIGHT_CODE = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CHARGABLE_WEIGHT = SPACE.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CHARGABLE_WEIGHT_CODE = SPACE.
*
*
****Freigth Charges
*        WA_OUTPUT_SAM_FEDEX_TYPE-SEQUENCE = '1'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-LOGISTIC_CODE = '104'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SUB_CODE = '100'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_SAM_FEDEX_TYPE-TRAN_CHARG_AMNT.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TAX_AMOUNT = '0'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REQUEST_AMOUNT =  WA_SAM_FEDEX_TYPE-TRAN_CHARG_AMNT.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CURRENCY = 'USD'.
*
*        APPEND WA_OUTPUT_SAM_FEDEX_TYPE TO IT_OUTPUT_SAM_FEDEX_TYPE.
*        CLEAR : WA_OUTPUT_SAM_FEDEX_TYPE.
*
*********end of Freight charges
********begin of fuel surchages
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES1 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT1.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES2 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT2.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES3 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT3.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES4 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT4.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES5 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT5.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES6 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT6.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES7 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT7.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES8 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT8.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES9 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT9.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES10 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT10.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES11 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT11.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES12 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT12.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES13 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT13.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES14 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT14.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES15 = 'Fuel Surcharge' .
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT15.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES16 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT16.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES17 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT17.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES18 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT18.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES19 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT19.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES20 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT20.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES21 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT21.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES22 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT22.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES23 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT23.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES24 = 'Fuel Surcharge'.
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT24.
*        ENDIF.
*
*
****Freigth Charges
*        WA_OUTPUT_SAM_FEDEX_TYPE-SEQUENCE = '2'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-LOGISTIC_CODE = '104'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SUB_CODE = '300'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TAX_AMOUNT = '0'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REQUEST_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CURRENCY = 'USD'.
*
*        APPEND WA_OUTPUT_SAM_FEDEX_TYPE TO IT_OUTPUT_SAM_FEDEX_TYPE.
*        CLEAR : WA_OUTPUT_SAM_FEDEX_TYPE.
********end of fuel surchages
*
********other charges
*        IF   WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES = 'Weekly Service Chg' OR
*              WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES = 'Weekday Delivery' OR
*              WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES = 'Residential Delivery' OR
*              WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES = 'Address Correction' OR
*              WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES = 'Residential' OR
*              WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES = 'DAS Comm' OR
*              WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT.
*        ENDIF.
*
*        IF  WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES1 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES1 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES1 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES1 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES1 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES1 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES1 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT1.
*        ENDIF.
*
*        IF  WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES2 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES2 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES2 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES2 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES2 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES2 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES2 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT2.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES3 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES3 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES3 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES3 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES3 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES3 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES3 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT3.
*        ENDIF.
*
*        IF  WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES4 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES4 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES4 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES4 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES4 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES4 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES4 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT4.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES5 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES5 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES5 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES5 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES5 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES5 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES5 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT5.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES6 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES6 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES6 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES6 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES6 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES6 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES6 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT6.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES7 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES7 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES7 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES7 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES7 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES7 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES7 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT7.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES8 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES8 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES8 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES8 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES8 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES8 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES8 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT8.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES9 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES9 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES9 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES9 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES9 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES9 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES9 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT9.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES10 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES10 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES10 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES10 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES10 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES10 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES10 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT10.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES11 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES11 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES11 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES11 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES11 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES11 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES11 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT11.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES12 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES12 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES12 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES12 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES12 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES12 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES12 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT12.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES13 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES13 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES13 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES13 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES13 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES13 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES13 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT13.
*        ENDIF.
*
*
*        IF     WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES14 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES14 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES14 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES14 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES14 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES14 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES14 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT14.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES15 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES15 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES15 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES15 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES15 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES15 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES15 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT15.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES16 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES16 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES16 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES16 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES16 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES16 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES16 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT16.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES17 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES17 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES17 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES17 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES17 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES17 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES17 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT17.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES18 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES18 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES18 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES18 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES18 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES18 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES18 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT18.
*        ENDIF.
*
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES19 = 'Weekly Service Chg' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES19 = 'Weekday Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES19 = 'Residential Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES19 = 'Address Correction' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES19 = 'Residential' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES19 = 'DAS Comm' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES19 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT19.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES20 = 'Weekly Service Chg' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES20 = 'Weekday Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES20 = 'Residential Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES20 = 'Address Correction' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES20 = 'Residential' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES20 = 'DAS Comm' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES20 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT20.
*        ENDIF.
*
*        IF  WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES21 = 'Weekly Service Chg' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES21 = 'Weekday Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES21 = 'Residential Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES21 = 'Address Correction' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES21 = 'Residential' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES21 = 'DAS Comm' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES21 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT21.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES22 = 'Weekly Service Chg' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES22 = 'Weekday Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES22 = 'Residential Delivery' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES22 = 'Address Correction' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES22 = 'Residential' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES22 = 'DAS Comm' OR
*      WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES22 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT22.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES23 = 'Weekly Service Chg' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES23 = 'Weekday Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES23 = 'Residential Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES23 = 'Address Correction' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES23 = 'Residential' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES23 = 'DAS Comm' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES23 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT23.
*        ENDIF.
*
*        IF WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES24 = 'Weekly Service Chg' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES24 = 'Weekday Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES24 = 'Residential Delivery' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES24 = 'Address Correction' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES24 = 'Residential' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES24 = 'DAS Comm' OR
*WA_SAM_FEDEX_TYPE-TRK_ID_CHG_DES24 = 'DAS Extended Comm'.
*
*          WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT + WA_SAM_FEDEX_TYPE-TRK_ID_CHG_AMT24.
*        ENDIF.
*
*
***********Begin of Handling charges
*
*
*        WA_OUTPUT_SAM_FEDEX_TYPE-SEQUENCE = '3'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-LOGISTIC_CODE = '104'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SUB_CODE = '400'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT.
*        WA_OUTPUT_SAM_FEDEX_TYPE-TAX_AMOUNT = '0'.
*        WA_OUTPUT_SAM_FEDEX_TYPE-REQUEST_AMOUNT =  WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT.
*        WA_OUTPUT_SAM_FEDEX_TYPE-CURRENCY = 'USD'.
*
*        IF WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT IS NOT INITIAL OR WA_OUTPUT_SAM_FEDEX_TYPE-SUPPLY_AMOUNT > 0.
*          IF  WA_OUTPUT_SAM_FEDEX_TYPE-REQUEST_AMOUNT IS NOT INITIAL OR WA_OUTPUT_SAM_FEDEX_TYPE-REQUEST_AMOUNT > 0.
*
*            APPEND WA_OUTPUT_SAM_FEDEX_TYPE TO IT_OUTPUT_SAM_FEDEX_TYPE.
*            CLEAR : WA_OUTPUT_SAM_FEDEX_TYPE.
*
*          ENDIF.
*        ENDIF.
*
*      ENDLOOP.
*
*      IF NOT IT_OUTPUT_SAM_FEDEX_TYPE[] IS INITIAL.
*
*
*
*      ENDIF.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 1.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'SENDER_IDENTIFIER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Sender Identifier'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 2.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'RECEIVER_IDENTIFIER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Receiver Identifier'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 3.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'INVOICE_NUMBER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Invoice Number (Shipment ID)'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 4.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'BILLING_DATE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'BillingDate'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 5.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'DIVISION'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Division'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
********
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 6.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'BUSINESS_TYPE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Business Type'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 7.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'ORDER_TYPE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Order Type'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 8.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'TRANSPORTATION_TYPE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Transportation Type'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 9.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'ACTUAL_DATE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Actual Date'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 10.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REFERENCE_TYPE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Type'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 11.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REFERENCE_MESSAGE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Message Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 12.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REFERENCE_DOCUMENT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Document Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 13.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REFERENCE_DATE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Date'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 14.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'RELATED_DOCUMENT_1'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Document Number1'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 15.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'RELATED_DOCUMENT_2'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Document Number2'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 16.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'SERVICE_TYPE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Service Type'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 17.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'INCOTERMS_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Incoterms Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 18.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'INCOTERMS_PLACE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Incoterms Place'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 19.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'PLANT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Plant'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 20.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'WH_LOCATION'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'WH Location'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 21.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'CARRIER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Carrier'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 22.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'FORWARDER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Forwarder'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 23.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'FROM_PLACE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'From Place'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 24.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'COUNTRY_FROM'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Coountry of FromPlace'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 25.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'FROM_PORT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'From Port'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 26.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'COUNTRY_PORT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Country of FromPort'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 27.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'TO_PLACE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'To Place'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 28.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'COUNTRY_PLACE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Country of ToPlace'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 29.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'TO_PORT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'To Port'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 30.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'COUNTRY_OF_PORT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Country of ToPort'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 31.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'GENERAL_INFO_1'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'General Information'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 32.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'GENERAL_INFO_2'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'General Information'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 33.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'GENERAL_INFO_3'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'General Information'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 34.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'SEQUENCE_NUM'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Sequence Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 35.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'CARGO_MEASUREMENT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Cargo Measurement Type'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 36.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'MESASURE_AMOUNT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Measurement (Amount)'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 37.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'MEASURE_UNIT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Measurement Unit'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 38.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'SEQUENCE_NUMBER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Sequence Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 39.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'TRUCK_CLASSIFICATIN'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Truck/CN TR Classification'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 40.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'TRUCK_NUMBER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Truck/CN TR Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 41.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'TRUCK_TYPE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Truck/CN Type'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 42.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'TRUCK_ROUND'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Truck Round'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 43.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'CONSOLIDATION_NUMBER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Consolidation Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 44.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'CONSOLIDATION_COUNT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Consolidation Count'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 45.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'MULTI_STOP'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Multi Stop Indicator'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 46.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'SEQUENCE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Sequence Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 47.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'LOGISTIC_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Logistics Cost Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 48.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'SUB_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Sub Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 49.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'SUPPLY_AMOUNT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Supply Amount'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 50.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'TAX_AMOUNT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Tax Amount'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
**********
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 51.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REQUEST_AMOUNT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Request Amount'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 52.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'CURRENCY'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Currency'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 53.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'FR_ISSUE_DATE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'FR Issue Date'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 54.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'RATE_EXCHANGE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Rate Of Exchange'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 55.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'BASIS_CURRENCY'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Basis Currency'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 56.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'DESTINATION_CURRENCY'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Destination on Currency'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 57.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'RATE_EXCHANGE_DATE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Rate Of Exchange Date'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 58.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REMARK_1'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Remark'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 59.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REMARK_2'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Remark'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 60.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REMARK_3'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Remark'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 61.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'SEQUENCE_NUMBER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Sequence Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 62.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REFERENCE_DOC_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Document Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 63.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REFERENCE_DOC_NUMBER'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Document Number'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 64.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'REFERENCE_DOC_DATE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Reference Document Date'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 65.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'MATERIAL'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Material'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 66.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'PLANT_1'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Plant'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 67.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'PACK_QUANTITY'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Pack Quantity'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 68.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'PACK_QUAN_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Pack Quantity Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 69.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'QUANTITY'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Quantity'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 70.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'QUANTITY_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Quantity Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 71.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'VOLUME'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Volume'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 72.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'VOLUME_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Volume Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 73.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'GROSS_WEIGHT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Gross Weight'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 74.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'GROSS_WEIGHT_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Gross Weight Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 75.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'NET_WEIGHT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Net Weight'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 76.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'NET_WEIGHT_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Net Weight Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 77.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'CHARGABLE_WEIGHT'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Chargable Weight'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*      WA_FIELDCAT_SAMSUNG_TYPE-COL_POS      = 78.
*      WA_FIELDCAT_SAMSUNG_TYPE-FIELDNAME    = 'CHARGABLE_WEIGHT_CODE'.
*      WA_FIELDCAT_SAMSUNG_TYPE-COLTEXT    = 'Chargable Weight Code'.
*      WA_FIELDCAT_SAMSUNG_TYPE-TABNAME      = 'IT_OUTPUT_SAM_FEDEX_TYPE'.
*      APPEND WA_FIELDCAT_SAMSUNG_TYPE TO IT_FIELDCAT_SAMSUNG_TYPE.
*      CLEAR WA_FIELDCAT_SAMSUNG_TYPE.
*
*
*
*      GS_LAYOUT-CWIDTH_OPT = 'X'.
*      GS_LAYOUT-INFO_FNAME = 'LINE_COLOR'.
*      GS_LAYOUT-GRID_TITLE = SPACE.
*
*
*      CALL METHOD GO_GRID->SET_TABLE_FOR_FIRST_DISPLAY
*        EXPORTING
*          I_STRUCTURE_NAME              = 'IT_OUTPUT_SAM_FEDEX_TYPE'
*          IS_LAYOUT                     = GS_LAYOUT
*        CHANGING
*          IT_OUTTAB                     = IT_OUTPUT_SAM_FEDEX_TYPE
*          IT_FIELDCATALOG               = IT_FIELDCAT_SAMSUNG_TYPE
**    IT_SORT                       =
*      EXCEPTIONS
*        INVALID_PARAMETER_COMBINATION = 1
*        PROGRAM_ERROR                 = 2
*        TOO_MANY_LINES                = 3
*        OTHERS                        = 4  .
*      IF SY-SUBRC <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*      SET HANDLER G_APPLICATION->HANDLE_HOTSPOT_CLICK FOR GO_GRID .
*
*
*
*    ENDIF.
*  ENDIF.
*ENDFORM.                    " samsung_download
*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FETCH_DATA .
*if it_final_ups is INITIAL.
  data : v_tabix type sy-tabix,
        v_next type sy-tabix.

  refresh : it_ups_data, it_ups_track, it_ups_service, it_final_ups_service, it_final_ups_zone.
  select *
       from /PWEAVER/EFA_UPS
       into corresponding fields of table it_ups_data
       where invoice_number = v_invoice_number and
         invoice_date eq wa_export_output-invoice_date.

  if sy-subrc = 0.

    sort it_ups_data by counter lead_ship_number chrg_class_code chrg_catg_code.

    data : v_id(4) type c,
          v_temp_hdl_chrg type /PWEAVER/EFA_UPS-basis_value,
          v_temp_hdl_chrg_2(10) type p,
          v_netamount type /PWEAVER/EFA_UPS-incen_amt,
           v_billweight type /PWEAVER/EFA_UPS-billed_weight,
          v_handling_charges type /PWEAVER/EFA_UPS-basis_value,
          v_discount_1 type /PWEAVER/EFA_UPS-incen_amt.

***select unique tracking numbers.
    refresh : it_ups_track.
    select distinct trck_num  from /PWEAVER/EFA_UPS into table it_ups_track where invoice_number = v_invoice_number
    and  invoice_date eq wa_export_output-invoice_date.

***select unique service types
    refresh : it_ups_service.
    select distinct chrg_desc from /PWEAVER/EFA_UPS into table it_ups_service where invoice_number = v_invoice_number and
                                                                           ( chrg_class_code = 'FRT' and  chrg_catg_code = 'SHP') or
                                                                           ( chrg_class_code = 'FRT' and  chrg_catg_code = 'RTN') or
                                                                           (  chrg_class_code = 'ACC' and chrg_desc_code  = 'ART' and chrg_catg_code = 'RTN' )
                                                                             and  invoice_date eq wa_export_output-invoice_date.

    sort it_ups_track by tracking_number.
    delete it_ups_track where tracking_number = space.


    loop at it_ups_track into wa_ups_track.


      clear wa_ups_service.
      loop at it_ups_service into wa_ups_service.



        refresh : it_ups_data_temp.
    select * from /PWEAVER/EFA_UPS into table it_ups_data_temp where trck_num = wa_ups_track-tracking_number and
                                                                 chrg_desc = wa_ups_service-chrg_desc.
        if sy-subrc = 0.


          clear : v_netamount,v_temp_hdl_chrg,v_temp_hdl_chrg_2,wa_ups_data_t_temp.

          clear : wa_ups_data_temp.



          loop at it_ups_data_temp into wa_ups_data_t_temp.

            clear : wa_ups_data.
            read table it_ups_data into wa_ups_data with key trck_num = wa_ups_track-tracking_number
                                                                       chrg_desc = wa_ups_service-chrg_desc
                                                                       chrg_class_code = 'FRT'.
            if sy-subrc = 0 .
****get the service type
              if wa_ups_data_temp-service_type is initial.
                wa_ups_data_temp-service_type   = wa_ups_data-chrg_desc.
              endif.

              if wa_ups_data_temp-zone is initial.
                wa_ups_data_temp-zone   = wa_ups_data-z_one.
              endif.

              if wa_ups_data_temp-payor is initial.
                wa_ups_data_temp-payor   = wa_ups_data-bill_opt_code.
              endif.

              clear : wa_ups_data.
              read table it_ups_data into wa_ups_data with key trck_num = wa_ups_track-tracking_number
                                                                         chrg_class_code = 'FRT'.
****number of packages
              if wa_ups_data_temp-packages is initial.
                wa_ups_data_temp-packages   = wa_ups_data-pckg_qty.
              endif.
***Transportation Charge
              if wa_ups_data_temp-transport_charge is initial.
                wa_ups_data_temp-transport_charge = wa_ups_data-basis_value.
              endif.

            endif.

***billed weight
            clear : v_billweight.
            select sum( billed_weight ) from /PWEAVER/EFA_UPS into v_billweight where trck_num = wa_ups_track-tracking_number.

            wa_ups_data_temp-rated_weight = v_billweight.

**handling charges
            clear : v_handling_charges.
            select sum( basis_value ) from /PWEAVER/EFA_UPS into v_handling_charges where trck_num = wa_ups_track-tracking_number.

***Discount
            clear : v_discount_1.
            select sum( incen_amt ) from /PWEAVER/EFA_UPS into v_discount_1 where trck_num = wa_ups_track-tracking_number.



****ACC OR ART
            clear :wa_ups_data_t_temp.
            read table it_ups_data into wa_ups_data with key trck_num = wa_ups_track-tracking_number
                                                                        chrg_desc = wa_ups_service-chrg_desc
                                                                       chrg_class_code = 'ACC'
                                                                       chrg_desc_code  = 'ART'
                                                                       chrg_catg_code = 'RTN'.
            if sy-subrc = 0 .

****get the service type
              if wa_ups_data_temp-service_type is initial.
                wa_ups_data_temp-service_type   = wa_ups_data-chrg_desc.
              endif.

              if wa_ups_data_temp-zone is initial.
                wa_ups_data_temp-zone   = wa_ups_data-z_one.
              endif.

              if wa_ups_data_temp-payor is initial.
                wa_ups_data_temp-payor   = wa_ups_data-bill_opt_code.
              endif.
****number of packages
              if wa_ups_data_temp-packages is initial.
                wa_ups_data_temp-packages   = wa_ups_data-chrg_unit_qty.
              endif.
***Transportation Charge
              if wa_ups_data_temp-transport_charge is initial.
                wa_ups_data_temp-transport_charge = wa_ups_data-basis_value.
              endif.



            endif.
*****get the  handling charges

            v_temp_hdl_chrg  = v_handling_charges - wa_ups_data_temp-transport_charge.

**Get the discount amount

            wa_ups_data_temp-discount = v_discount_1.

**Get the net amount

            v_netamount = wa_ups_data_temp-transport_charge + v_temp_hdl_chrg - wa_ups_data_temp-discount.


          endloop.
          wa_final_ups_service-service_type = wa_ups_data_temp-service_type.
          wa_final_ups_service-packages = wa_ups_data_temp-packages.
          wa_final_ups_service-rated_weight =  wa_ups_data_temp-rated_weight.
          wa_final_ups_service-transport_charge = wa_ups_data_temp-transport_charge.
          wa_final_ups_service-handling_charge = v_temp_hdl_chrg .
          wa_final_ups_service-discount = wa_ups_data_temp-discount.
          wa_final_ups_service-net_charge = v_netamount.

          append wa_final_ups_service to it_final_ups_service.
          clear wa_final_ups_service.


          wa_final_ups_zone-zone = wa_ups_data_temp-zone.
          wa_final_ups_zone-packages = wa_ups_data_temp-packages.
          wa_final_ups_zone-rated_weight =  wa_ups_data_temp-rated_weight.
          wa_final_ups_zone-transport_charge = wa_ups_data_temp-transport_charge.
          wa_final_ups_zone-handling_charge = v_temp_hdl_chrg .
          wa_final_ups_zone-discount = wa_ups_data_temp-discount.
          wa_final_ups_zone-net_charge = v_netamount.

          append wa_final_ups_zone to it_final_ups_zone.
          clear wa_final_ups_zone.


          wa_final_ups_payor-payor = wa_ups_data_temp-payor.
          wa_final_ups_payor-packages = wa_ups_data_temp-packages.
          wa_final_ups_payor-rated_weight =  wa_ups_data_temp-rated_weight.
          wa_final_ups_payor-transport_charge = wa_ups_data_temp-transport_charge.
          wa_final_ups_payor-handling_charge = v_temp_hdl_chrg .
          wa_final_ups_payor-discount = wa_ups_data_temp-discount.
          wa_final_ups_payor-net_charge = v_netamount.

          append wa_final_ups_payor to it_final_ups_payor.
          clear wa_final_ups_payor.

        endif.

      endloop.
    endloop.

  endif.

*endif.

ENDFORM.                    " FETCH_DATA
*&---------------------------------------------------------------------*
*&      Form  SERVICE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SERVICE_TYPE .




*  REFRESH : it_service_type, it_ups_summ.
*  CLEAR   : wa_service_type, wa_ups_summ.
*
*  select * from /PWEAVER/EFA_UPS_summ INTO table it_ups_summ where INVOICE_NUMBER = v_invoice_number and
*                                                             chrg_catg_code = 'SHP'.
*
*    LOOP AT it_ups_summ into wa_ups_summ.
*
*      wa_service_type-servicetype  =  wa_ups_summ-service_type.
*      wa_service_type-packages     =  wa_ups_summ-package_qty.
*      wa_service_type-ratedweight  =  wa_ups_summ-billed_weight.
*      wa_service_type-SHIP_INCENT_AMNT = wa_ups_summ-SHIP_INCENT_AMNT.
*      wa_service_type-SHIP_NET_AMOUNT  = wa_ups_summ-SHIP_NET_AMOUNT.
*      wa_service_type-HANDLE_INCEN_AMT = wa_ups_summ-HANDLE_INCEN_AMT.
*      wa_service_type-HANDLE_NET_AMNT  = wa_ups_summ-HANDLE_NET_AMNT.
*      wa_service_type-CORR_SHIP_INCENT = wa_ups_summ-CORR_SHIP_INCENT.
*      wa_service_type-CORR_SHIP_NET    = wa_ups_summ-CORR_SHIP_NET.
*      wa_service_type-CORR_HANDLE_DISC = wa_ups_summ-CORR_HANDLE_DISC.
*      wa_service_type-CORR_HANDLE_NET  =  wa_ups_summ-CORR_HANDLE_NET.
*      wa_service_type-MISC_INCENT_CHRG = wa_ups_summ-MISC_INCENT_CHRG.
*      wa_service_type-MISC_NET_CHRG    = wa_ups_summ-MISC_NET_CHRG.
*      wa_service_type-INV_ADD_DISC     = wa_ups_summ-INV_ADD_DISC .
*      wa_service_type-INV_ADD_NET      = wa_ups_summ-INV_ADD_NET .
*      wa_service_type-INV_MISC_DISC    = wa_ups_summ-INV_MISC_DISC .
*      wa_service_type-INV_MISC_NET     = wa_ups_summ-INV_MISC_NET.
*
*      append wa_service_type to it_service_type.
*      clear wa_service_type.
*
*
*
*
*    ENDLOOP.
*
*    sort it_service_type by servicetype.
  refresh it_output_service_type.
  clear : wa_output_service_type.

  LOOP AT it_service_type into wa_service_type.

    At NEW servicetype.
      sum.

      wa_output_service_type-service_type = wa_service_type-servicetype.
      wa_output_service_type-total_packs  = wa_service_type-packages.
      wa_output_service_type-actual_weight = wa_service_type-actualweight.
      wa_output_service_type-rated_weight = wa_service_type-ratedweight.
      wa_output_service_type-transportation_charge = wa_service_type-ship_net_amount + wa_service_type-SHIP_INCENT_AMNT.
      wa_output_service_type-handling_charge = wa_service_type-handle_net_amnt + wa_service_type-HANDLE_INCEN_AMT.
      wa_output_service_type-total_discounts = wa_service_type-ship_incent_amnt +
                                               wa_service_type-handle_incen_amt +
                                               wa_service_type-MISC_INCENT_CHRG.
      wa_output_service_type-correction_charge = wa_service_type-corr_ship_net   +
                                                 wa_service_type-CORR_HANDLE_NET ." +
*                                                  wa_service_type-ADDR_CORR_CHARGE.
      wa_output_service_type-adjustment_charge = wa_service_type-MISC_INCENT_CHRG + wa_service_type-misc_net_chrg.
      wa_output_service_type-net_charge  = wa_output_service_type-transportation_charge +
                                           wa_output_service_type-handling_charge       -
                                           wa_output_service_type-total_discounts       +
                                           wa_output_service_type-correction_charge     +
                                           wa_output_service_type-adjustment_charge.

*       IF wa_output_service_type-service_type is INITIAL.
*           wa_output_service_type-service_type = 'Invoice Misc'.
*           wa_output_service_type-total_discounts = wa_output_service_type-total_discounts + wa_service_type-corr_ship_incent.
*       ENDIF.

      append wa_output_service_type to it_output_service_type.
      clear wa_output_service_type.
    endat.

  ENDLOOP.

  if v_disc is not INITIAL or v_net is not INITIAL.

    wa_output_service_type-service_type    =  'Miscellaneous and Other Adjustments'.
    wa_output_service_type-total_discounts =  v_disc.
    wa_output_service_type-correction_charge = v_corr.
    wa_output_service_type-net_charge      =  v_net.
    wa_output_service_type-line_color      =  'C600'.
    append wa_output_service_type to it_output_service_type.
    clear wa_output_service_type.
  endif.

  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'SERVICE_TYPE'.
  wa_fieldcat-coltext      = 'Service Type'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'TOTAL_PACKS'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'RATED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 9.
  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
  wa_fieldcat-coltext      = 'Adjustment_Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Service Type'.


  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_OUTPUT_SERVICE_TYPE'
      is_layout                     = gs_layout
*      i_save                        = 'X'
*      i_default                     = 'A'
    changing
      it_outtab                     = it_output_service_type
      it_fieldcatalog               = it_fieldcat
*      it_sort                        =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

ENDFORM.                    " SERVICE_TYPE
*&---------------------------------------------------------------------*
*&      Form  FETCH_DATA_UPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FETCH_DATA_UPS .

  REFRESH : it_service_type, it_zone_type_ups, it_pack_type_ups, it_payment_type  , it_shipday , it_ups_summ, it_all_details, it_gsr_ups.
  CLEAR   : wa_service_type, wa_zone_type_ups, wa_pack_type_ups, wa_payment_type , wa_shipday, wa_ups_summ, wa_all_details, wa_gsr_ups.
  CLEAR   : v_disc, v_net, v_corr.

*  select * from /PWEAVER/EFA_UPS_summ into table it_inv_misc where invoice_number = v_invoice_number and
*                                                                  other_adj = 'X'.


*  select * from /PWEAVER/EFA_UPS_summ INTO table it_ups_summ where INVOICE_NUMBER = v_invoice_number and
*                                                             ( chrg_catg_code = 'SHP' OR chrg_catg_code = 'RTN' ).

  LOOP AT it_ups_summ into wa_ups_summ.

    wa_service_type-servicetype      =  wa_ups_summ-service_type.
    wa_service_type-packages         =  wa_ups_summ-package_qty.
    wa_service_type-actualweight     =  wa_ups_summ-entered_weight.
    wa_service_type-ratedweight      =  wa_ups_summ-billed_weight.
    wa_service_type-SHIP_INCENT_AMNT = wa_ups_summ-SHIP_INCENT_AMNT.
    wa_service_type-SHIP_NET_AMOUNT  = wa_ups_summ-SHIP_NET_AMOUNT.
    wa_service_type-HANDLE_INCEN_AMT = wa_ups_summ-HANDLE_INCEN_AMT.
    wa_service_type-HANDLE_NET_AMNT  = wa_ups_summ-HANDLE_NET_AMNT.
    wa_service_type-CORR_SHIP_INCENT = wa_ups_summ-CORR_SHIP_INCENT.
    wa_service_type-CORR_SHIP_NET    = wa_ups_summ-CORR_SHIP_NET.
    wa_service_type-CORR_HANDLE_DISC = wa_ups_summ-CORR_HANDLE_DISC.
    wa_service_type-CORR_HANDLE_NET  =  wa_ups_summ-CORR_HANDLE_NET.
    wa_service_type-MISC_INCENT_CHRG = wa_ups_summ-MISC_INCENT_CHRG.
    wa_service_type-MISC_NET_CHRG    = wa_ups_summ-MISC_NET_CHRG.
    wa_service_type-INV_ADD_DISC     = wa_ups_summ-INV_ADD_DISC .
    wa_service_type-INV_ADD_NET      = wa_ups_summ-INV_ADD_NET .
    wa_service_type-INV_MISC_DISC    = wa_ups_summ-INV_MISC_DISC .
    wa_service_type-INV_MISC_NET     = wa_ups_summ-INV_MISC_NET.
*      wa_service_type-ADDR_CORR_CHARGE = wa_ups_summ-ADDR_CORR_CHARGE.

    append wa_service_type to it_service_type.
    clear wa_service_type.


    wa_zone_type_ups-zzone            = wa_ups_summ-zzone.
    wa_zone_type_ups-packages         =  wa_ups_summ-package_qty.
    wa_zone_type_ups-actualweight     = wa_ups_summ-entered_weight.
    wa_zone_type_ups-ratedweight      =  wa_ups_summ-billed_weight.
    wa_zone_type_ups-SHIP_INCENT_AMNT = wa_ups_summ-SHIP_INCENT_AMNT.
    wa_zone_type_ups-SHIP_NET_AMOUNT  = wa_ups_summ-SHIP_NET_AMOUNT.
    wa_zone_type_ups-HANDLE_INCEN_AMT = wa_ups_summ-HANDLE_INCEN_AMT.
    wa_zone_type_ups-HANDLE_NET_AMNT  = wa_ups_summ-HANDLE_NET_AMNT.
    wa_zone_type_ups-CORR_SHIP_INCENT = wa_ups_summ-CORR_SHIP_INCENT.
    wa_zone_type_ups-CORR_SHIP_NET    = wa_ups_summ-CORR_SHIP_NET.
    wa_zone_type_ups-CORR_HANDLE_DISC = wa_ups_summ-CORR_HANDLE_DISC.
    wa_zone_type_ups-CORR_HANDLE_NET  =  wa_ups_summ-CORR_HANDLE_NET.
    wa_zone_type_ups-MISC_INCENT_CHRG = wa_ups_summ-MISC_INCENT_CHRG.
    wa_zone_type_ups-MISC_NET_CHRG    = wa_ups_summ-MISC_NET_CHRG.
    wa_zone_type_ups-INV_ADD_DISC     = wa_ups_summ-INV_ADD_DISC .
    wa_zone_type_ups-INV_ADD_NET      = wa_ups_summ-INV_ADD_NET .
    wa_zone_type_ups-INV_MISC_DISC    = wa_ups_summ-INV_MISC_DISC .
    wa_zone_type_ups-INV_MISC_NET     = wa_ups_summ-INV_MISC_NET.

    append wa_zone_type_ups to it_zone_type_ups.
    clear wa_zone_type_ups.

    wa_pack_type_ups-package_type            = wa_ups_summ-container_type.
    wa_pack_type_ups-packages         =  wa_ups_summ-package_qty.
    wa_pack_type_ups-actualweight     = wa_ups_summ-entered_weight.
    wa_pack_type_ups-ratedweight      =  wa_ups_summ-billed_weight.
    wa_pack_type_ups-SHIP_INCENT_AMNT = wa_ups_summ-SHIP_INCENT_AMNT.
    wa_pack_type_ups-SHIP_NET_AMOUNT  = wa_ups_summ-SHIP_NET_AMOUNT.
    wa_pack_type_ups-HANDLE_INCEN_AMT = wa_ups_summ-HANDLE_INCEN_AMT.
    wa_pack_type_ups-HANDLE_NET_AMNT  = wa_ups_summ-HANDLE_NET_AMNT.
    wa_pack_type_ups-CORR_SHIP_INCENT = wa_ups_summ-CORR_SHIP_INCENT.
    wa_pack_type_ups-CORR_SHIP_NET    = wa_ups_summ-CORR_SHIP_NET.
    wa_pack_type_ups-CORR_HANDLE_DISC = wa_ups_summ-CORR_HANDLE_DISC.
    wa_pack_type_ups-CORR_HANDLE_NET  =  wa_ups_summ-CORR_HANDLE_NET.
    wa_pack_type_ups-MISC_INCENT_CHRG = wa_ups_summ-MISC_INCENT_CHRG.
    wa_pack_type_ups-MISC_NET_CHRG    = wa_ups_summ-MISC_NET_CHRG.
    wa_pack_type_ups-INV_ADD_DISC     = wa_ups_summ-INV_ADD_DISC .
    wa_pack_type_ups-INV_ADD_NET      = wa_ups_summ-INV_ADD_NET .
    wa_pack_type_ups-INV_MISC_DISC    = wa_ups_summ-INV_MISC_DISC .
    wa_pack_type_ups-INV_MISC_NET     = wa_ups_summ-INV_MISC_NET.

    append wa_pack_type_ups to it_pack_type_ups.
    clear wa_pack_type_ups.

    wa_payment_type-PAYMENT_METHOD   = wa_ups_summ-payment_method.
    wa_payment_type-packages         =  wa_ups_summ-package_qty.
    wa_payment_type-actualweight     = wa_ups_summ-entered_weight.
    wa_payment_type-ratedweight      =  wa_ups_summ-billed_weight.
    wa_payment_type-SHIP_INCENT_AMNT = wa_ups_summ-SHIP_INCENT_AMNT.
    wa_payment_type-SHIP_NET_AMOUNT  = wa_ups_summ-SHIP_NET_AMOUNT.
    wa_payment_type-HANDLE_INCEN_AMT = wa_ups_summ-HANDLE_INCEN_AMT.
    wa_payment_type-HANDLE_NET_AMNT  = wa_ups_summ-HANDLE_NET_AMNT.
    wa_payment_type-CORR_SHIP_INCENT = wa_ups_summ-CORR_SHIP_INCENT.
    wa_payment_type-CORR_SHIP_NET    = wa_ups_summ-CORR_SHIP_NET.
    wa_payment_type-CORR_HANDLE_DISC = wa_ups_summ-CORR_HANDLE_DISC.
    wa_payment_type-CORR_HANDLE_NET  =  wa_ups_summ-CORR_HANDLE_NET.
    wa_payment_type-MISC_INCENT_CHRG = wa_ups_summ-MISC_INCENT_CHRG.
    wa_payment_type-MISC_NET_CHRG    = wa_ups_summ-MISC_NET_CHRG.
    wa_payment_type-INV_ADD_DISC     = wa_ups_summ-INV_ADD_DISC .
    wa_payment_type-INV_ADD_NET      = wa_ups_summ-INV_ADD_NET .
    wa_payment_type-INV_MISC_DISC    = wa_ups_summ-INV_MISC_DISC .
    wa_payment_type-INV_MISC_NET     = wa_ups_summ-INV_MISC_NET.

    append wa_payment_type to it_payment_type.
    clear wa_payment_type.

    wa_shipday-ship_date        =  wa_ups_summ-ship_date.
    wa_shipday-packages         =  wa_ups_summ-package_qty.
    wa_shipday-actualweight     =  wa_ups_summ-entered_weight.
    wa_shipday-ratedweight      =  wa_ups_summ-billed_weight.
    wa_shipday-SHIP_INCENT_AMNT = wa_ups_summ-SHIP_INCENT_AMNT.
    wa_shipday-SHIP_NET_AMOUNT  = wa_ups_summ-SHIP_NET_AMOUNT.
    wa_shipday-HANDLE_INCEN_AMT = wa_ups_summ-HANDLE_INCEN_AMT.
    wa_shipday-HANDLE_NET_AMNT  = wa_ups_summ-HANDLE_NET_AMNT.
    wa_shipday-CORR_SHIP_INCENT = wa_ups_summ-CORR_SHIP_INCENT.
    wa_shipday-CORR_SHIP_NET    = wa_ups_summ-CORR_SHIP_NET.
    wa_shipday-CORR_HANDLE_DISC = wa_ups_summ-CORR_HANDLE_DISC.
    wa_shipday-CORR_HANDLE_NET  =  wa_ups_summ-CORR_HANDLE_NET.
    wa_shipday-MISC_INCENT_CHRG = wa_ups_summ-MISC_INCENT_CHRG.
    wa_shipday-MISC_NET_CHRG    = wa_ups_summ-MISC_NET_CHRG.
    wa_shipday-INV_ADD_DISC     = wa_ups_summ-INV_ADD_DISC .
    wa_shipday-INV_ADD_NET      = wa_ups_summ-INV_ADD_NET .
    wa_shipday-INV_MISC_DISC    = wa_ups_summ-INV_MISC_DISC .
    wa_shipday-INV_MISC_NET     = wa_ups_summ-INV_MISC_NET.

    append wa_shipday to it_shipday.
    clear wa_shipday.








    wa_all_details-ship_date = wa_ups_summ-ship_date.
    wa_all_details-service_type = wa_ups_summ-service_type.
    wa_all_details-zone   =  wa_ups_summ-zzone.
    wa_all_details-tracking_number = wa_ups_summ-tracking_number.
    wa_all_details-container_type = wa_ups_summ-container_type.
    wa_all_details-package_qty = wa_ups_summ-package_qty.
    wa_all_details-actual_weight = wa_ups_summ-entered_weight.
    wa_all_details-billed_weight  = wa_ups_summ-billed_weight.
    wa_all_details-transportation_charge  = wa_ups_summ-ship_net_amount + wa_ups_summ-ship_incent_amnt.
    wa_all_details-handling_charge = wa_ups_summ-handle_net_amnt + wa_ups_summ-handle_incen_amt.
    wa_all_details-total_discounts = wa_ups_summ-ship_incent_amnt + wa_ups_summ-handle_incen_amt.
    wa_all_details-correction_charge  = wa_ups_summ-corr_ship_net + wa_ups_summ-corr_handle_net.
    wa_all_details-adjustment_charge  = wa_ups_summ-misc_net_chrg.
    wa_all_details-net_charge = wa_all_details-transportation_charge +
                                wa_all_details-handling_charge       -
                                wa_all_details-total_discounts       +
                                wa_all_details-correction_charge     +
                                wa_all_details-adjustment_charge.
    wa_all_details-sender_name        = wa_ups_summ-sender_name.
    wa_all_details-sender_company     = wa_ups_summ-sender_company.
    wa_all_details-sender_address1    = wa_ups_summ-sender_address1.
    wa_all_details-sender_city        = wa_ups_summ-sender_city.
    wa_all_details-sender_state       = wa_ups_summ-sender_state.
    wa_all_details-sender_postcode    = wa_ups_summ-sender_postcode.
    wa_all_details-sender_country     = wa_ups_summ-sender_country .
    wa_all_details-receiver_name      = wa_ups_summ-receiver_name.
    wa_all_details-receiver_company   = wa_ups_summ-receiver_company.
    wa_all_details-rec_address1       = wa_ups_summ-rec_address1.
    wa_all_details-receiver_city      = wa_ups_summ-receiver_city.
    wa_all_details-receiver_state     = wa_ups_summ-receiver_state.
    wa_all_details-rec_postcode       = wa_ups_summ-rec_postcode.
    wa_all_details-receiver_country   = wa_ups_summ-receiver_country.

    append wa_all_details to it_all_details.
    clear wa_all_details.



    IF wa_ups_summ-gsr_status = 'X'.
      wa_gsr_ups-tracking_number = wa_ups_summ-tracking_number.
      wa_gsr_ups-pickup_date     = wa_ups_summ-pickup_date.
      wa_gsr_ups-pod_date        = wa_ups_summ-pod_date.
      wa_gsr_ups-gsr_desc        = wa_ups_summ-gsr_desc.

      append wa_gsr_ups to it_gsr_ups.
      CLEAR wa_gsr_ups.
    ENDIF.

  ENDLOOP.

  LOOP AT it_inv_misc into wa_inv_misc.

    v_disc = v_disc + wa_inv_misc-inv_add_disc + wa_inv_misc-inv_misc_disc + WA_INV_MISC-MISC_INCENT_CHRG.
    v_net  = v_net + wa_inv_misc-inv_add_net  + wa_inv_misc-inv_misc_net + wa_inv_misc-addr_corr_charge.
    v_corr = v_corr + wa_inv_misc-addr_corr_charge.



*
*     if wa_inv_misc-ship_date is initial.
*
*  wa_all_details-ship_date = 'MISC'.
*      wa_all_details-ship_date = wa_inv_misc-ship_date.
*  else.
*
*
*data: day(2) type c,
*      month(2) type c,
*      year(4) type c,
*      wa_inv_misc-ship_date1 type char10.
*
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*  EXPORTING
*    INPUT         =  wa_inv_misc-ship_date
* IMPORTING
*  OUTPUT        =  wa_inv_misc-ship_date.
*
*
*day =  wa_inv_misc-ship_date+6(2).
*month = wa_inv_misc-ship_date+4(2).
*year = wa_inv_misc-ship_date+0(4).
*
*
*clear wa_inv_misc-ship_date.
*concatenate day '.'month'.'year into wa_inv_misc-ship_date1.
*  wa_all_details-ship_date = wa_inv_misc-ship_date1.
*
*endif.

   wa_all_details-ship_date = wa_inv_misc-ship_date.
    wa_all_details-service_type = wa_inv_misc-service_type.
    wa_all_details-zone   =  wa_inv_misc-zzone.
    wa_all_details-tracking_number = wa_inv_misc-tracking_number.
    wa_all_details-package_qty = wa_inv_misc-package_qty.
    wa_all_details-actual_weight = wa_inv_misc-entered_weight.
    wa_all_details-billed_weight  = wa_inv_misc-billed_weight.
*      wa_all_details-transportation_charge  = wa_inv_misc-ship_net_amount + wa_inv_misc-ship_incent_amnt.
*      wa_all_details-handling_charge = wa_inv_misc-handle_net_amnt + wa_inv_misc-handle_incen_amt.
    wa_all_details-total_discounts = wa_inv_misc-inv_add_disc + wa_inv_misc-inv_misc_disc.
*      wa_all_details-correction_charge  = wa_inv_misc-corr_ship_net + wa_inv_misc-corr_handle_net.
*      wa_all_details-adjustment_charge  = wa_inv_misc-misc_net_chrg.
    wa_all_details-net_charge = wa_inv_misc-inv_add_net  + wa_inv_misc-inv_misc_net.

    wa_all_details-line_color = 'C100'.

    append wa_all_details to it_all_details.
    clear wa_all_details.

  ENDLOOP.

  sort it_service_type by servicetype.
  sort it_zone_type_ups by zzone.
  sort it_pack_type_ups by package_type.
  sort it_payment_type by payment_method.
  sort it_shipday by ship_date.

ENDFORM.                    " FETCH_DATA_UPS
*&---------------------------------------------------------------------*
*&      Form  ZONE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZONE_TYPE .

  refresh it_output_zone_type_ups.
  clear : wa_output_zone_type_ups.

  LOOP AT it_zone_type_ups into wa_zone_type_ups.

    At NEW zzone.
      sum.

      wa_output_zone_type_ups-zzone = wa_zone_type_ups-zzone.
      wa_output_zone_type_ups-total_packs  = wa_zone_type_ups-packages.
      wa_output_zone_type_ups-actual_weight = wa_zone_type_ups-actualweight.
      wa_output_zone_type_ups-rated_weight = wa_zone_type_ups-ratedweight.
      wa_output_zone_type_ups-transportation_charge = wa_zone_type_ups-ship_net_amount + wa_zone_type_ups-SHIP_INCENT_AMNT.
      wa_output_zone_type_ups-handling_charge = wa_zone_type_ups-handle_net_amnt + wa_zone_type_ups-HANDLE_INCEN_AMT.
      wa_output_zone_type_ups-total_discounts = wa_zone_type_ups-ship_incent_amnt + wa_zone_type_ups-handle_incen_amt.
      wa_output_zone_type_ups-correction_charge = wa_zone_type_ups-corr_ship_net + wa_zone_type_ups-CORR_HANDLE_NET.
      wa_output_zone_type_ups-adjustment_charge = wa_zone_type_ups-misc_net_chrg.
      wa_output_zone_type_ups-net_charge  = wa_output_zone_type_ups-transportation_charge +
                                            wa_output_zone_type_ups-handling_charge       -
                                            wa_output_zone_type_ups-total_discounts       +
                                            wa_output_zone_type_ups-correction_charge     +
                                            wa_output_zone_type_ups-adjustment_charge.

*       IF wa_output_zone_type_ups-correction_charge is not INITIAL or wa_output_zone_type_ups-adjustment_charge is not INITIAL.
*           wa_output_zone_type_ups-line_color = 'C610'.
*       ENDIF.

      append wa_output_zone_type_ups to it_output_zone_type_ups.
      clear wa_output_zone_type_ups.
    endat.

  ENDLOOP.
  wa_output_zone_type_ups-zzone = 'Misc & Other Adj'.
  wa_output_zone_type_ups-total_discounts  =  v_disc.
  wa_output_zone_type_ups-net_charge  = v_net.
  wa_output_zone_type_ups-line_color = 'C600'.

  append wa_output_zone_type_ups to it_output_zone_type_ups.
  clear wa_output_zone_type_ups.




  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'ZZONE'.
  wa_fieldcat-coltext      = 'Zone'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'TOTAL_PACKS'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'RATED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 9.
  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
  wa_fieldcat-coltext      = 'Adjustment_Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Zone Type'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_OUPUT_ZONE_TYPE_UPS'
*      is_layout                     = gs_layout
       is_variant                    = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_output_zone_type_ups
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.



ENDFORM.                    " ZONE_TYPE
*&---------------------------------------------------------------------*
*&      Form  PAYMENT_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PAYMENT_TYPE .

  data : v_inv TYPE ty_output_payment_type.
  refresh it_output_payment_type.
  clear : wa_output_payment_type.

  LOOP AT it_payment_type into wa_payment_type.

    At NEW payment_method.
      sum.

      wa_output_payment_type-payment_method = wa_payment_type-payment_method.
      wa_output_payment_type-total_packs  = wa_payment_type-packages.
      wa_output_payment_type-actual_weight = wa_payment_type-actualweight.
      wa_output_payment_type-rated_weight = wa_payment_type-ratedweight.
      wa_output_payment_type-transportation_charge = wa_payment_type-ship_net_amount + wa_payment_type-SHIP_INCENT_AMNT.
      wa_output_payment_type-handling_charge = wa_payment_type-handle_net_amnt + wa_payment_type-HANDLE_INCEN_AMT.
      wa_output_payment_type-total_discounts = wa_payment_type-ship_incent_amnt + wa_payment_type-handle_incen_amt + wa_payment_type-corr_ship_incent.
      wa_output_payment_type-correction_charge = wa_payment_type-corr_ship_net + wa_payment_type-CORR_HANDLE_NET +  wa_payment_type-ADDR_CORR_CHARGE +
                                                  wa_payment_type-corr_ship_incent.
*      wa_output_payment_type-adjustment_charge = wa_payment_type-misc_net_chrg.
      wa_output_payment_type-net_charge  = wa_output_payment_type-transportation_charge  +
                                            wa_output_payment_type-handling_charge       -
                                            wa_output_payment_type-total_discounts       +
                                            wa_output_payment_type-correction_charge     +
                                            wa_output_payment_type-adjustment_charge.

      IF wa_output_payment_type-payment_method is INITIAL.
        wa_output_payment_type-payment_method = 'Inv Misc'.
        v_inv = wa_output_payment_type.
        CONTINUE.
      ENDIF.

      append wa_output_payment_type to it_output_payment_type.
      clear wa_output_payment_type.
    endat.

  ENDLOOP.

  IF v_inv is NOT INITIAL.
    v_inv-line_color      =  'C600'.
    append v_inv to it_output_payment_type.
    clear v_inv.
  ENDIF.
*  wa_output_payment_type-payment_method =  'Misc & Other Adj'.
*  wa_output_payment_type-total_discounts = v_disc.
*  wa_output_payment_type-net_charge = v_net.
*  wa_output_payment_type-line_color = 'C600'.
*
*  append wa_output_payment_type to it_output_payment_type.
*  clear wa_output_payment_type.



  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'PAYMENT_METHOD'.
  wa_fieldcat-coltext      = 'Payment Method'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'TOTAL_PACKS'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'RATED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 9.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PAYMENT_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Payment Method'.


gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_OUPUT_PAYMENT_TYPE'
*      is_layout                     = gs_layout
    is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_output_payment_type
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.



ENDFORM.                    " PAYMENT_TYPE
*&---------------------------------------------------------------------*
*&      Form  SHIP_DAY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SHIP_DAY .

  data : v_inv TYPE ty_output_shipday.

  refresh it_output_shipday.
  clear : wa_output_shipday.

  LOOP AT it_shipday into wa_shipday.


    At NEW ship_date.
      sum.

IF wa_shipday-ship_date is not INITIAL AND wa_shipday-ship_date NE '00000000' .

data: day(2) type c,
      month(2) type c,
      year(4) type c.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
    INPUT         = wa_shipday-ship_date
 IMPORTING
  OUTPUT        = wa_shipday-ship_date.


day = wa_shipday-ship_date+6(2).
month = wa_shipday-ship_date+4(2).
year = wa_shipday-ship_date+0(4).


clear wa_shipday-ship_date.
concatenate day '.'month'.'year into wa_shipday-ship_date.


     wa_output_shipday-ship_date = wa_shipday-ship_date.
      endif.
      wa_output_shipday-total_packs  = wa_shipday-packages.
      wa_output_shipday-actual_weight = wa_shipday-actualweight.
      wa_output_shipday-rated_weight = wa_shipday-ratedweight.
      wa_output_shipday-transportation_charge = wa_shipday-ship_net_amount + wa_shipday-SHIP_INCENT_AMNT.
      wa_output_shipday-handling_charge = wa_shipday-handle_net_amnt + wa_shipday-HANDLE_INCEN_AMT.
      wa_output_shipday-total_discounts = wa_shipday-ship_incent_amnt + wa_shipday-handle_incen_amt + wa_shipday-corr_ship_incent.
      wa_output_shipday-correction_charge = wa_shipday-corr_ship_net + wa_shipday-CORR_HANDLE_NET + wa_shipday-ADDR_CORR_CHARGE +
                                                  wa_shipday-corr_ship_incent.
*      wa_output_shipday-adjustment_charge = wa_shipday-misc_net_chrg.
      wa_output_shipday-net_charge  = wa_output_shipday-transportation_charge +
                                            wa_output_shipday-handling_charge       -
                                            wa_output_shipday-total_discounts       +
                                            wa_output_shipday-correction_charge     +
                                            wa_output_shipday-adjustment_charge.

      IF wa_output_shipday-ship_date is INITIAL.
        wa_output_shipday-ship_date = 'Miscellaneous'.
        v_inv = wa_output_shipday.
        CONTINUE.
      ENDIF.

      append wa_output_shipday to it_output_shipday.
      clear wa_output_shipday.
    endat.

  ENDLOOP.

  IF v_inv is NOT INITIAL.
    v_inv-line_color      =  'C600'.
    append v_inv to it_output_shipday.
    clear v_inv.
  ENDIF.

*  wa_output_shipday-ship_date = 'Misc'.
*  wa_output_shipday-total_discounts = v_disc.
*  wa_output_shipday-net_charge = v_net.
*  wa_output_shipday-line_color = 'C600'.
*
*  append wa_output_shipday to it_output_shipday.
*  clear wa_output_shipday.

  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'SHIP_DATE'.
  wa_fieldcat-coltext      = 'Ship Date'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'TOTAL_PACKS'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'RATED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 9.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SHIPDAY'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Ship Date'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_OUPUT_SHIPDAY'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_output_shipday
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.



ENDFORM.                    " SHIP_DAY
*&---------------------------------------------------------------------*
*&      Form  GET_ALL_DETAILS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_ALL_DETAILS .
  perform inbound.
*PERFORM FINAL.


  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'SHIP_DATE'.
  wa_fieldcat-coltext      = 'Ship Date'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'SERVICE_TYPE'.
  wa_fieldcat-coltext      = 'Service Type'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wA_fieldcat-OUTPUTLEN = '35'.
  wa_fieldcat-lowercase = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ZONE'.
  wa_fieldcat-coltext      = 'Zone'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'TRACKING_NUMBER'.
  wa_fieldcat-coltext      = 'Tracking Number'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  w_fieldcat-OUTPUTLEN = '20'.
  wa_fieldcat-hotspot      = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'CONTAINER_TYPE'.
  wa_fieldcat-coltext      = 'Shipment Type'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'PACKAGE_QTY'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'BILLED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.
   wa_fieldcat-col_pos      = 9.
  wa_fieldcat-fieldname    = 'CHRG_CATG_DET_CD'.
  wa_fieldcat-coltext      = 'Charge Category Detail Code'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 11.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 12.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 13.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 13.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 14.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 15.
  wa_fieldcat-fieldname    = 'SENDER_NAME'.
  wa_fieldcat-coltext      = 'Sender Name'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 16.
  wa_fieldcat-fieldname    = 'SENDER_COMPANY'.
  wa_fieldcat-coltext      = 'Sender Company'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 17.
  wa_fieldcat-fieldname    = 'SENDER_ADDRESS1'.
  wa_fieldcat-coltext      = 'Sender Address'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 18.
  wa_fieldcat-fieldname    = 'SENDER_CITY'.
  wa_fieldcat-coltext      = 'Sender City'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 19.
  wa_fieldcat-fieldname    = 'SENDER_STATE'.
  wa_fieldcat-coltext      = 'Sender State'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 20.
  wa_fieldcat-fieldname    = 'SENDER_POSTCODE'.
  wa_fieldcat-coltext      = 'Sender Postalcode'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 21.
  wa_fieldcat-fieldname    = 'SENDER_COUNTRY'.
  wa_fieldcat-coltext      = 'Sender Country'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 22.
  wa_fieldcat-fieldname    = 'RECEIVER_NAME'.
  wa_fieldcat-coltext      = 'Receiver Name'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 23.
  wa_fieldcat-fieldname    = 'RECEIVER_COMPANY'.
  wa_fieldcat-coltext      = 'Receiver Company'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 24.
  wa_fieldcat-fieldname    = 'REC_ADDRESS1'.
  wa_fieldcat-coltext      = 'Receiver Address'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 25.
  wa_fieldcat-fieldname    = 'RECEIVER_CITY'.
  wa_fieldcat-coltext      = 'Receiver City'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 26.
  wa_fieldcat-fieldname    = 'RECEIVER_STATE'.
  wa_fieldcat-coltext      = 'Receiver State'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 27.
  wa_fieldcat-fieldname    = 'REC_POSTCODE'.
  wa_fieldcat-coltext      = 'Receiver Postalcode'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 28.
  wa_fieldcat-fieldname    = 'RECEIVER_COUNTRY'.
  wa_fieldcat-coltext      = 'Receiver Country'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'All Details'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
APPEND LINES OF it_all_details1 TO IT_ALL_DETAILS.
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_ALL_DETAILS'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_all_details
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


  set handler g_application->handle_hotspot_click for go_grid .
ENDFORM.                    " GET_ALL_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GSR_UPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM addr_corr_UPS .

  REFRESH : it_addr_corr_data, it_addr.
  CLEAR   : wa_addr_corr_data, wa_addr.
*  select * from /PWEAVER/EFA_UPS_summ into table it_addr_corr_data where invoice_number = v_invoice_number and
*                                                                  addr_corr_flag = 'X'.

  LOOP AT it_addr_corr_data into wa_addr_corr_data.







   wa_addr-ship_date = wa_addr_corr_data-ship_date.


    wa_addr-service_type = wa_addr_corr_data-service_type.
    wa_addr-zone   =  wa_addr_corr_data-zzone.
    wa_addr-tracking_number = wa_addr_corr_data-tracking_number.
    wa_addr-package_qty = wa_addr_corr_data-package_qty.
    wa_addr-actual_weight  = wa_addr_corr_data-entered_weight.
    wa_addr-billed_weight  = wa_addr_corr_data-billed_weight.
    wa_addr-transportation_charge  = wa_addr_corr_data-ship_net_amount + wa_addr_corr_data-ship_incent_amnt.
    wa_addr-handling_charge = wa_addr_corr_data-handle_net_amnt + wa_addr_corr_data-handle_incen_amt.
    wa_addr-correction_charge  = wa_addr_corr_data-corr_ship_net + wa_addr_corr_data-corr_handle_net.
    wa_addr-adjustment_charge  = wa_addr_corr_data-misc_net_chrg.
    wa_addr-address_correction = wa_addr_corr_data-addr_corr_charge.
    wa_addr-net_charge = wa_addr-transportation_charge +
                         wa_addr-handling_charge       -
                         wa_addr-total_discounts       +
                         wa_addr-correction_charge     +
                         wa_addr-adjustment_charge     +
                         wa_addr-address_correction.

    append wa_addr to it_addr.
    clear wa_addr.


  ENDLOOP.

  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'SHIP_DATE'.
  wa_fieldcat-coltext      = 'Ship Date'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'SERVICE_TYPE'.
  wa_fieldcat-coltext      = 'Service Type'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ZONE'.
  wa_fieldcat-coltext      = 'Zone'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'TRACKING_NUMBER'.
  wa_fieldcat-coltext      = 'Tracking Number'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-hotspot      = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'PACKAGE_QTY'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'BILLED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 9.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 11.
  wa_fieldcat-fieldname    = 'ADDRESS_CORRECTION'.
  wa_fieldcat-coltext      = 'Address Correction Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 12.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 13.
  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
  wa_fieldcat-coltext      = 'Adjustment_Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 14.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Address Corrections '.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_ADDR'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_addr
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


ENDFORM.                    " addr_corr_UPS
*&---------------------------------------------------------------------*
*&      Form  GSR_UPS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GSR_UPS .

  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'TRACKING_NUMBER'.
  wa_fieldcat-coltext      = 'Tracking Number'.
  wa_fieldcat-tabname      = 'IT_GSR_UPS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'PICKUP_DATE'.
  wa_fieldcat-coltext      = 'Pick Up Date'.
  wa_fieldcat-tabname      = 'IT_GSR_UPS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'POD_DATE'.
  wa_fieldcat-coltext      = 'Pod Date'.
  wa_fieldcat-tabname      = 'IT_GSR_UPS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.
    wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'POD_SIG'.
  wa_fieldcat-coltext      = 'Pod Sig'.
  wa_fieldcat-tabname      = 'IT_GSR_UPS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'EXP_DATE'.
  wa_fieldcat-coltext      = 'Expected Date'.
  wa_fieldcat-tabname      = 'IT_GSR_UPS'.
*      wa_fieldcat-hotspot      = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By GSR '.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_GSR_UPS'
*      is_layout                     = gs_layout
       is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_GSR_UPS
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
ENDFORM.                    " GSR_UPS
*&---------------------------------------------------------------------*
*&      Form  PACKAGE_TYPE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PACKAGE_TYPE .

  refresh it_output_pack_type_ups.
  clear : wa_output_pack_type_ups.

  LOOP AT it_pack_type_ups into wa_pack_type_ups.

    At NEW package_type.
      sum.

      wa_output_pack_type_ups-package_type = wa_pack_type_ups-package_type.
      wa_output_pack_type_ups-total_packs  = wa_pack_type_ups-packages.
      wa_output_pack_type_ups-actual_weight = wa_pack_type_ups-actualweight.
      wa_output_pack_type_ups-rated_weight = wa_pack_type_ups-ratedweight.
      wa_output_pack_type_ups-transportation_charge = wa_pack_type_ups-ship_net_amount + wa_pack_type_ups-SHIP_INCENT_AMNT.
      wa_output_pack_type_ups-handling_charge = wa_pack_type_ups-handle_net_amnt + wa_pack_type_ups-HANDLE_INCEN_AMT.
      wa_output_pack_type_ups-total_discounts = wa_pack_type_ups-ship_incent_amnt + wa_pack_type_ups-handle_incen_amt.
      wa_output_pack_type_ups-correction_charge = wa_pack_type_ups-corr_ship_net + wa_pack_type_ups-CORR_HANDLE_NET.
      wa_output_pack_type_ups-adjustment_charge = wa_pack_type_ups-misc_net_chrg.
      wa_output_pack_type_ups-net_charge  = wa_output_pack_type_ups-transportation_charge +
                                            wa_output_pack_type_ups-handling_charge       -
                                            wa_output_pack_type_ups-total_discounts       +
                                            wa_output_pack_type_ups-correction_charge     +
                                            wa_output_pack_type_ups-adjustment_charge.

*       IF wa_output_zone_type_ups-correction_charge is not INITIAL or wa_output_zone_type_ups-adjustment_charge is not INITIAL.
*           wa_output_zone_type_ups-line_color = 'C610'.
*       ENDIF.

      append wa_output_pack_type_ups to it_output_pack_type_ups.
      clear wa_output_pack_type_ups.
    endat.

  ENDLOOP.
  wa_output_pack_type_ups-package_type = 'Misc & Other Adj'.
  wa_output_pack_type_ups-total_discounts  =  v_disc.
  wa_output_pack_type_ups-net_charge  = v_net.
  wa_output_pack_type_ups-line_color = 'C600'.

  append wa_output_pack_type_ups to it_output_pack_type_ups.
  clear wa_output_pack_type_ups.




  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'PACKAGE_TYPE'.
  wa_fieldcat-coltext      = 'Shipment Type'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'TOTAL_PACKS'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'RATED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 9.
  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
  wa_fieldcat-coltext      = 'Adjustment_Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Shipment Type'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_OUPUT_PACK_TYPE_UPS'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_output_pack_type_ups
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

ENDFORM.                    " PACKAGE_TYPE
*&---------------------------------------------------------------------*
*&      Form  GET_UPS_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_UPS_DATA .

  TYPES : BEGIN OF ty_cons,
           invoice_no TYPE /pweaver/INVOICE_NUMBER,
         END OF ty_cons.

  DATA : it_cons TYPE STANDARD TABLE OF ty_cons,
         wa_cons LIKE LINE OF it_cons.

  DATA : inv_disc TYPE netwr,
         inv_corr TYPE netwr,
         inv_ship TYPE netwr,
         inv_hand TYPE netwr.

  REFRESH : it_zpwefa_ups, it_ups.
  CLEAR   : is_zpwefa_ups, wa_ups.

  REFRESH : it_service_type, it_zone_type_ups, it_pack_type_ups, it_payment_type  , it_shipday , it_ups_summ, it_all_details, it_gsr_ups.
  CLEAR   : wa_service_type, wa_zone_type_ups, wa_pack_type_ups, wa_payment_type , wa_shipday, wa_ups_summ, wa_all_details, wa_gsr_ups.
  CLEAR   : v_disc, v_net, v_corr.

  IF it_cons_export is initial.
    select * from /PWEAVER/EFA_UPS INTO table it_zpwefa_ups where INVOICE_NUMBER = v_invoice_number.
    PERFORM summerize_data.
  else.
    LOOP AT it_cons_export INTO wa_cons_export WHERE gv_cb = 'X'.
      LOOP AT it_export_output INTO wa_export_output WHERE account_no = wa_cons_export-account_no .
        wa_cons-invoice_no = wa_export_output-invoice_number.
        APPEND  wa_cons TO it_cons.
        CLEAR : wa_cons, wa_export_output.
      ENDLOOP.
      CLEAR wa_cons_export.
    ENDLOOP.

    SELECT * FROM /PWEAVER/EFA_UPS INTO TABLE it_zpwefa_ups
      FOR ALL ENTRIES IN it_cons
      WHERE invoice_number = it_cons-invoice_no.

    PERFORM summerize_data.

  ENDIF.




  LOOP AT it_ups into wa_ups.

    wa_service_type-servicetype      =  wa_ups-service_type.
    wa_service_type-packages         =  wa_ups-package_qty.
    wa_service_type-actualweight     =  wa_ups-entered_weight.
    wa_service_type-ratedweight      =  wa_ups-billed_weight.
    wa_service_type-SHIP_INCENT_AMNT = wa_ups-SHIP_INCENT_AMNT.
    wa_service_type-SHIP_NET_AMOUNT  = wa_ups-SHIP_NET_AMOUNT.
    wa_service_type-HANDLE_INCEN_AMT = wa_ups-HANDLE_INCEN_AMT.
    wa_service_type-HANDLE_NET_AMNT  = wa_ups-HANDLE_NET_AMNT.
    wa_service_type-CORR_SHIP_INCENT = wa_ups-CORR_SHIP_INCENT.
    wa_service_type-CORR_SHIP_NET    = wa_ups-CORR_SHIP_NET.
    wa_service_type-CORR_HANDLE_DISC = wa_ups-CORR_HANDLE_DISC.
    wa_service_type-CORR_HANDLE_NET  =  wa_ups-CORR_HANDLE_NET.
*      wa_service_type-MISC_INCENT_CHRG = wa_ups-MISC_INCENT_CHRG.
*      wa_service_type-MISC_NET_CHRG    = wa_ups-MISC_NET_CHRG.
*      wa_service_type-INV_ADD_DISC     = wa_ups-INV_ADD_DISC .
*      wa_service_type-INV_ADD_NET      = wa_ups-INV_ADD_NET .
*      wa_service_type-INV_MISC_DISC    = wa_ups-INV_MISC_DISC .
*      wa_service_type-INV_MISC_NET     = wa_ups-INV_MISC_NET.
    wa_service_type-ADDR_CORR_CHARGE = wa_ups-ADDR_CORR_CHARGE.
    wa_service_type-chrg_catg_detail = wa_ups-chrg_catg_detail.

    append wa_service_type to it_service_type.
    clear wa_service_type.


    wa_zone_type_ups-zzone            = wa_ups-zzone.
    wa_zone_type_ups-packages         =  wa_ups-package_qty.
    wa_zone_type_ups-actualweight     = wa_ups-entered_weight.
    wa_zone_type_ups-ratedweight      =  wa_ups-billed_weight.
    wa_zone_type_ups-SHIP_INCENT_AMNT = wa_ups-SHIP_INCENT_AMNT.
    wa_zone_type_ups-SHIP_NET_AMOUNT  = wa_ups-SHIP_NET_AMOUNT.
    wa_zone_type_ups-HANDLE_INCEN_AMT = wa_ups-HANDLE_INCEN_AMT.
    wa_zone_type_ups-HANDLE_NET_AMNT  = wa_ups-HANDLE_NET_AMNT.
    wa_zone_type_ups-CORR_SHIP_INCENT = wa_ups-CORR_SHIP_INCENT.
    wa_zone_type_ups-CORR_SHIP_NET    = wa_ups-CORR_SHIP_NET.
    wa_zone_type_ups-CORR_HANDLE_DISC = wa_ups-CORR_HANDLE_DISC.
    wa_zone_type_ups-CORR_HANDLE_NET  =  wa_ups-CORR_HANDLE_NET.
*    wa_zone_type_ups-MISC_INCENT_CHRG = wa_ups-MISC_INCENT_CHRG.
*    wa_zone_type_ups-MISC_NET_CHRG    = wa_ups-MISC_NET_CHRG.
*    wa_zone_type_ups-INV_ADD_DISC     = wa_ups-INV_ADD_DISC .
*    wa_zone_type_ups-INV_ADD_NET      = wa_ups-INV_ADD_NET .
*    wa_zone_type_ups-INV_MISC_DISC    = wa_ups-INV_MISC_DISC .
*    wa_zone_type_ups-INV_MISC_NET     = wa_ups-INV_MISC_NET.
    wa_zone_type_ups-ADDR_CORR_CHARGE = wa_ups-ADDR_CORR_CHARGE.

    append wa_zone_type_ups to it_zone_type_ups.
    clear wa_zone_type_ups.

    wa_pack_type_ups-package_type            = wa_ups-container_type.
    wa_pack_type_ups-packages         =  wa_ups-package_qty.
    wa_pack_type_ups-actualweight     = wa_ups-entered_weight.
    wa_pack_type_ups-ratedweight      =  wa_ups-billed_weight.
    wa_pack_type_ups-SHIP_INCENT_AMNT = wa_ups-SHIP_INCENT_AMNT.
    wa_pack_type_ups-SHIP_NET_AMOUNT  = wa_ups-SHIP_NET_AMOUNT.
    wa_pack_type_ups-HANDLE_INCEN_AMT = wa_ups-HANDLE_INCEN_AMT.
    wa_pack_type_ups-HANDLE_NET_AMNT  = wa_ups-HANDLE_NET_AMNT.
    wa_pack_type_ups-CORR_SHIP_INCENT = wa_ups-CORR_SHIP_INCENT.
    wa_pack_type_ups-CORR_SHIP_NET    = wa_ups-CORR_SHIP_NET.
    wa_pack_type_ups-CORR_HANDLE_DISC = wa_ups-CORR_HANDLE_DISC.
    wa_pack_type_ups-CORR_HANDLE_NET  =  wa_ups-CORR_HANDLE_NET.
*    wa_pack_type_ups-MISC_INCENT_CHRG = wa_ups-MISC_INCENT_CHRG.
*    wa_pack_type_ups-MISC_NET_CHRG    = wa_ups-MISC_NET_CHRG.
*    wa_pack_type_ups-INV_ADD_DISC     = wa_ups-INV_ADD_DISC .
*    wa_pack_type_ups-INV_ADD_NET      = wa_ups-INV_ADD_NET .
*    wa_pack_type_ups-INV_MISC_DISC    = wa_ups-INV_MISC_DISC .
*    wa_pack_type_ups-INV_MISC_NET     = wa_ups-INV_MISC_NET.
    wa_pack_type_ups-ADDR_CORR_CHARGE = wa_ups-ADDR_CORR_CHARGE.

    append wa_pack_type_ups to it_pack_type_ups.
    clear wa_pack_type_ups.

    wa_payment_type-PAYMENT_METHOD   = wa_ups-payment_method.
    wa_payment_type-packages         =  wa_ups-package_qty.
    wa_payment_type-actualweight     = wa_ups-entered_weight.
    wa_payment_type-ratedweight      =  wa_ups-billed_weight.
    wa_payment_type-SHIP_INCENT_AMNT = wa_ups-SHIP_INCENT_AMNT.
    wa_payment_type-SHIP_NET_AMOUNT  = wa_ups-SHIP_NET_AMOUNT.
    wa_payment_type-HANDLE_INCEN_AMT = wa_ups-HANDLE_INCEN_AMT.
    wa_payment_type-HANDLE_NET_AMNT  = wa_ups-HANDLE_NET_AMNT.
    wa_payment_type-CORR_SHIP_INCENT = wa_ups-CORR_SHIP_INCENT.
    wa_payment_type-CORR_SHIP_NET    = wa_ups-CORR_SHIP_NET.
    wa_payment_type-CORR_HANDLE_DISC = wa_ups-CORR_HANDLE_DISC.
    wa_payment_type-CORR_HANDLE_NET  =  wa_ups-CORR_HANDLE_NET.
*    wa_payment_type-MISC_INCENT_CHRG = wa_ups_summ-MISC_INCENT_CHRG.
*    wa_payment_type-MISC_NET_CHRG    = wa_ups_summ-MISC_NET_CHRG.
*    wa_payment_type-INV_ADD_DISC     = wa_ups_summ-INV_ADD_DISC .
*    wa_payment_type-INV_ADD_NET      = wa_ups_summ-INV_ADD_NET .
*    wa_payment_type-INV_MISC_DISC    = wa_ups_summ-INV_MISC_DISC .
*    wa_payment_type-INV_MISC_NET     = wa_ups_summ-INV_MISC_NET.
    wa_payment_type-ADDR_CORR_CHARGE = wa_ups-ADDR_CORR_CHARGE.

    append wa_payment_type to it_payment_type.
    clear wa_payment_type.

    wa_shipday-ship_date        =  wa_ups-ship_date.
    wa_shipday-packages         =  wa_ups-package_qty.
    wa_shipday-actualweight     =  wa_ups-entered_weight.
    wa_shipday-ratedweight      =  wa_ups-billed_weight.
    wa_shipday-SHIP_INCENT_AMNT = wa_ups-SHIP_INCENT_AMNT.
    wa_shipday-SHIP_NET_AMOUNT  = wa_ups-SHIP_NET_AMOUNT.
    wa_shipday-HANDLE_INCEN_AMT = wa_ups-HANDLE_INCEN_AMT.
    wa_shipday-HANDLE_NET_AMNT  = wa_ups-HANDLE_NET_AMNT.
    wa_shipday-CORR_SHIP_INCENT = wa_ups-CORR_SHIP_INCENT.
    wa_shipday-CORR_SHIP_NET    = wa_ups-CORR_SHIP_NET.
    wa_shipday-CORR_HANDLE_DISC = wa_ups-CORR_HANDLE_DISC.
    wa_shipday-CORR_HANDLE_NET  =  wa_ups-CORR_HANDLE_NET.
*    wa_shipday-MISC_INCENT_CHRG = wa_ups-MISC_INCENT_CHRG.
*    wa_shipday-MISC_NET_CHRG    = wa_ups-MISC_NET_CHRG.
*    wa_shipday-INV_ADD_DISC     = wa_ups-INV_ADD_DISC .
*    wa_shipday-INV_ADD_NET      = wa_ups-INV_ADD_NET .
*    wa_shipday-INV_MISC_DISC    = wa_ups-INV_MISC_DISC .
*    wa_shipday-INV_MISC_NET     = wa_ups-INV_MISC_NET.
    wa_shipday-ADDR_CORR_CHARGE = wa_ups-ADDR_CORR_CHARGE.

    append wa_shipday to it_shipday.
    clear wa_shipday.

if  wa_ups-ship_date is  initial.


* ELSE.
   wa_ups-ship_date = 'MISCELLANEOUS'.
      wa_all_details-ship_date = wa_ups-ship_date.
  else.


data: day(2) type c,
      month(2) type c,
      year(4) type c,
     wa_ups-ship_date1 type char10.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
    INPUT         =  wa_ups-ship_date
 IMPORTING
  OUTPUT        =  wa_ups-ship_date.


day =  wa_ups-ship_date+6(2).
month = wa_ups-ship_date+4(2).
year = wa_ups-ship_date+0(4).


clear wa_ups-ship_date.
concatenate day '.'month'.'year into wa_ups-ship_date1.
 wa_all_details-ship_date = wa_ups-ship_date1.


endif.


   IF wa_ups-service_type  IS NOT INITIAL.







*      wa_all_details-ship_date = wa_ups-ship_date.
      wa_all_details-invoice_number = wa_ups-invoice_number.
      wa_all_details-service_type = wa_ups-service_type.
      wa_all_details-zone   =  wa_ups-zzone.
      wa_all_details-tracking_number = wa_ups-tracking_number.
      wa_all_details-container_type = wa_ups-container_type.
      wa_all_details-package_qty = wa_ups-package_qty.
      wa_all_details-actual_weight = wa_ups-entered_weight.
      wa_all_details-billed_weight  = wa_ups-billed_weight.
      wa_all_details-chrg_catg_det_cd = wa_ups-chrg_catg_detail.

      wa_all_details-transportation_charge  = wa_ups-ship_net_amount + wa_ups-ship_incent_amnt.
      wa_all_details-handling_charge = wa_ups-handle_net_amnt + wa_ups-handle_incen_amt.
      wa_all_details-total_discounts = wa_ups-ship_incent_amnt + wa_ups-handle_incen_amt + wa_ups-corr_ship_incent..
      wa_all_details-correction_charge  = wa_ups-corr_ship_net + wa_ups-corr_handle_net + wa_ups-ADDR_CORR_CHARGE +
                                                    wa_ups-corr_ship_incent.
*    wa_all_details-adjustment_charge  = wa_ups-misc_net_chrg.
      wa_all_details-net_charge = wa_all_details-transportation_charge +
                                  wa_all_details-handling_charge       -
                                  wa_all_details-total_discounts       +
                                  wa_all_details-correction_charge     +
                                  wa_all_details-adjustment_charge.
      wa_all_details-sender_name        = wa_ups-sender_name.
      wa_all_details-sender_company     = wa_ups-sender_company.
      wa_all_details-sender_address1    = wa_ups-sender_address1.
      wa_all_details-sender_city        = wa_ups-sender_city.
      wa_all_details-sender_state       = wa_ups-sender_state.
      wa_all_details-sender_postcode    = wa_ups-sender_postcode.
      wa_all_details-sender_country     = wa_ups-sender_country .
      wa_all_details-receiver_name      = wa_ups-receiver_name.
      wa_all_details-receiver_company   = wa_ups-receiver_company.
      wa_all_details-rec_address1       = wa_ups-rec_address1.
      wa_all_details-receiver_city      = wa_ups-receiver_city.
      wa_all_details-receiver_state     = wa_ups-receiver_state.
      wa_all_details-rec_postcode       = wa_ups-rec_postcode.
      wa_all_details-receiver_country   = wa_ups-receiver_country.

      append wa_all_details to it_all_details.
      clear wa_all_details.
    ELSE.
      inv_ship = inv_ship + wa_ups-ship_net_amount + wa_ups-ship_incent_amnt.
      inv_hand = inv_hand + wa_ups-handle_net_amnt + wa_ups-handle_incen_amt.
      inv_disc = inv_disc + wa_ups-ship_incent_amnt + wa_ups-handle_incen_amt + wa_ups-corr_ship_incent..
      inv_corr = inv_corr + wa_ups-corr_ship_net + wa_ups-corr_handle_net + wa_ups-ADDR_CORR_CHARGE +
                                                    wa_ups-corr_ship_incent.
    ENDIF.
    clear :  is_zpwefa_upstrc.

select single * from /pweaver/efa_trc into is_zpwefa_upstrc where TRACKING_NO  = wa_ups-tracking_number.
*IF wa_ups_summ-gsr_status = 'X'.
*      wa_gsr_ups-tracking_number = wa_ups_summ-tracking_number.
*      wa_gsr_ups-pickup_date     = wa_ups_summ-pickup_date.
*      wa_gsr_ups-pod_date        = wa_ups_summ-pod_date.
*      wa_gsr_ups-gsr_desc        = wa_ups_summ-gsr_desc.
*
*      append wa_gsr_ups to it_gsr_ups.
*      CLEAR wa_gsr_ups.
*    ENDIF.
  if is_zpwefa_upstrc-gsr_status = 'X'.
        wa_gsr_ups-tracking_number = is_zpwefa_upstrc-tracking_no.
      wa_gsr_ups-pickup_date     = is_zpwefa_upstrc-TRANSACTION_DATE.
      wa_gsr_ups-pod_date        = is_zpwefa_upstrc-pod_date.
*      wa_gsr_ups-gsr_desc        = wa_ups_summ-gsr_desc.
      wa_gsr_ups-pod_sig      = is_zpwefa_upstrc-pod_signature.
      wa_gsr_ups-exp_date   = is_zpwefa_upstrc-exp_del_date.
 append wa_gsr_ups to it_gsr_ups.
      CLEAR wa_gsr_ups.
 endif.

  endloop.
  it_all_track = it_all_details.


  IF inv_disc IS NOT INITIAL OR
     inv_corr IS NOT INITIAL OR
     inv_ship IS NOT INITIAL OR
     inv_hand IS NOT INITIAL.
    wa_all_details-line_color      =  'C600'.
    wa_all_details-transportation_charge  = inv_ship.
    wa_all_details-handling_charge = inv_hand.
    wa_all_details-total_discounts = inv_disc.
    wa_all_details-correction_charge  = inv_corr.

    wa_all_details-net_charge = wa_all_details-transportation_charge +
                                wa_all_details-handling_charge       -
                                wa_all_details-total_discounts       +
                                wa_all_details-correction_charge .
    append wa_all_details to it_all_details.
    clear wa_all_details.

  ENDIF.

  sort it_service_type by servicetype.
  sort it_zone_type_ups by zzone.
  sort it_pack_type_ups by package_type.
  sort it_payment_type by payment_method.
  sort it_shipday by ship_date.
ENDFORM.                    " GET_UPS_DATA
*&---------------------------------------------------------------------*
*&      Form  SERVICE_TYPE_TEMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SERVICE_TYPE_TEMP .

  data : v_inv TYPE ty_output_service_type.
  refresh it_output_service_type.
  clear : wa_output_service_type.

  LOOP AT it_service_type into wa_service_type. "where chrg_catg_detail ne 'FC' .
*    IF WA_SERVICE_TYPE-CHRG_CATG_DETAIL NE 'RS'.

    At NEW servicetype.
      sum.

      wa_output_service_type-service_type = wa_service_type-servicetype.
      wa_output_service_type-total_packs  = wa_service_type-packages.
      wa_output_service_type-actual_weight = wa_service_type-actualweight.
      wa_output_service_type-rated_weight = wa_service_type-ratedweight.
      wa_output_service_type-transportation_charge = wa_service_type-ship_net_amount + wa_service_type-SHIP_INCENT_AMNT.
      wa_output_service_type-handling_charge = wa_service_type-handle_net_amnt + wa_service_type-HANDLE_INCEN_AMT.
      wa_output_service_type-total_discounts = wa_service_type-ship_incent_amnt +
                                               wa_service_type-handle_incen_amt + wa_service_type-corr_ship_incent.
      wa_output_service_type-correction_charge = wa_service_type-corr_ship_net   +
                                                 wa_service_type-CORR_HANDLE_NET  +
                                                 wa_service_type-ADDR_CORR_CHARGE + wa_service_type-corr_ship_incent..
*       wa_output_service_type-adjustment_charge = wa_service_type-MISC_INCENT_CHRG + wa_service_type-misc_net_chrg.
      wa_output_service_type-net_charge  = wa_output_service_type-transportation_charge +
                                           wa_output_service_type-handling_charge       -
                                           wa_output_service_type-total_discounts       +
                                           wa_output_service_type-correction_charge     +
                                           wa_output_service_type-adjustment_charge.

      IF wa_output_service_type-service_type is INITIAL.
        wa_output_service_type-service_type = 'Invoice Misc'.
        v_inv = wa_output_service_type.
        CONTINUE.
      ENDIF.

      append wa_output_service_type to it_output_service_type.
      clear wa_output_service_type.
    endat.
*    ENDIF.

  ENDLOOP.

  IF v_inv is NOT INITIAL.
    v_inv-line_color      =  'C600'.
    append v_inv to it_output_service_type.
    clear v_inv.
  ENDIF.

  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'SERVICE_TYPE'.
  wa_fieldcat-coltext      = 'Service Type'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wA_fieldcat-OUTPUTLEN = '40'.
  wa_fieldcat-LOWERCASE = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'TOTAL_PACKS'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'RATED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 9.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_SERVICE_TYPE'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Service Type'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_OUTPUT_SERVICE_TYPE'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_output_service_type
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

ENDFORM.                    " SERVICE_TYPE_TEMP
*&---------------------------------------------------------------------*
*&      Form  SUMMERIZE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUMMERIZE_DATA .
*  SORT IT_ZPWEFA_UPS BY chrg_catg_det_cd.


  loop at it_ZPWEFA_UPS into is_ZPWEFA_UPS ." 1)WHERE chrg_catg_det_cd NE 'FC'  .

    clear wa_ups.
    read table it_ups into wa_ups with key lead_ship_number = is_zpwefa_ups-lead_ship_number
                                           tracking_number  = is_zpwefa_ups-trck_num
                                           chrg_catg_detail = is_zpwefa_ups-chrg_catg_det_cd.

    IF sy-subrc ne 0.  """""" Add the record into internal table

      wa_ups-lead_ship_number  =  is_zpwefa_ups-lead_ship_number.
      wa_ups-tracking_number   =  is_zpwefa_ups-trck_num.
      wa_ups-invoice_number    =  is_zpwefa_ups-invoice_number.

********** For shipping and handling charges *************

      IF is_zpwefa_ups-chrg_catg_code eq 'SHP' or is_zpwefa_ups-chrg_catg_code eq 'RTN'
        .
*         IF IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'RS'.
        IF is_zpwefa_ups-chrg_class_code = 'FRT'    or  is_zpwefa_ups-chrg_class_code = 'INF' .   """""" shipping charges
          wa_ups-account_no        =  is_zpwefa_ups-account_number.
          wa_ups-service_type      =  is_zpwefa_ups-chrg_desc.
          wa_ups-ship_date         =  is_zpwefa_ups-transaction_date.
          wa_ups-zzone             =  is_zpwefa_ups-z_one.
          wa_ups-payment_method    =  is_zpwefa_ups-bill_opt_code.
          wa_ups-invoice_date      =  is_zpwefa_ups-invoice_date.
          wa_ups-invoice_amount    =  is_zpwefa_ups-invoice_amount.
          wa_ups-invoice_curr_key  =  is_zpwefa_ups-invc_curr_code.
          wa_ups-entered_weight    =  is_zpwefa_ups-entered_weight.
          wa_ups-entered_wt_unit   =  is_zpwefa_ups-ent_wgt_uom.
          wa_ups-billed_weight     =  is_zpwefa_ups-billed_weight.
          wa_ups-billed_wt_unit    =  is_zpwefa_ups-billed_wgt_uom.
          wa_ups-container_type    =  is_zpwefa_ups-container_type.
          wa_ups-chrg_catg_code    =  is_zpwefa_ups-chrg_catg_code.
          wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
          wa_ups-chrg_class_code   =  is_zpwefa_ups-chrg_class_code.
          wa_ups-chrg_unit_qty     =  is_zpwefa_ups-chrg_unit_qty.

          wa_ups-package_qty       =  is_zpwefa_ups-pckg_qty.
          wa_ups-chrg_desc_code    =  is_zpwefa_ups-chrg_desc_code.
          wa_ups-chrg_desc         =  is_zpwefa_ups-chrg_desc.

          wa_ups-ship_incent_amnt    =  is_zpwefa_ups-incen_amt.
          wa_ups-ship_net_amount     =  is_zpwefa_ups-net_amnt.
          wa_ups-sender_name         =  is_zpwefa_ups-sender_name.
          wa_ups-sender_company      =  is_zpwefa_ups-sender_cmp_name.
          wa_ups-sender_address1     =  is_zpwefa_ups-sender_addr_lin1.
          wa_ups-sender_address2     =  is_zpwefa_ups-sender_addr_lin2.
          wa_ups-sender_city         =  is_zpwefa_ups-sender_city.
          wa_ups-sender_state        =  is_zpwefa_ups-sender_state.
          wa_ups-sender_postcode     =  is_zpwefa_ups-sender_postal.
          wa_ups-sender_country      =  is_zpwefa_ups-sender_country.
          wa_ups-receiver_name       =  is_zpwefa_ups-receiver_name.
          wa_ups-receiver_company    =  is_zpwefa_ups-receiver_comp_nm.
          wa_ups-rec_address1        =  is_zpwefa_ups-rec_addr_line_1.
          wa_ups-rec_address2        =  is_zpwefa_ups-rec_addr_line_2.
          wa_ups-receiver_city       =  is_zpwefa_ups-receiver_city.
          wa_ups-receiver_state      =  is_zpwefa_ups-receiver_state.
          wa_ups-rec_postcode   =  is_zpwefa_ups-receiver_postal.
          wa_ups-receiver_country    =  is_zpwefa_ups-receiver_country.
        ELSE .                                                      """"""""""""""""" Other/handling charges
          wa_ups-handle_incen_amt    =  is_zpwefa_ups-incen_amt.
          wa_ups-handle_net_amnt     =  is_zpwefa_ups-net_amnt.
        ENDIF.
*        ENDIF.
      ELSEIF is_zpwefa_ups-chrg_catg_code eq 'ADJ'.

        IF is_zpwefa_ups-chrg_catg_det_cd = 'ADC' and
           is_zpwefa_ups-chrg_class_code  = 'FRT' .

          wa_ups-addr_corr_flag = 'X'.
          wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.

          wa_ups-addr_corr_charge   = is_zpwefa_ups-net_amnt.

          wa_ups-corr_rec_name      =  is_zpwefa_ups-receiver_name.
          wa_ups-corr_rec_company   =  is_zpwefa_ups-receiver_comp_nm.
          wa_ups-corr_recaddress1   =  is_zpwefa_ups-rec_addr_line_1.
          wa_ups-corr_recaddress2   =  is_zpwefa_ups-rec_addr_line_2.
          wa_ups-corr_rec_city      =  is_zpwefa_ups-receiver_city.
          wa_ups-corr_rec_state     =  is_zpwefa_ups-receiver_state.
          wa_ups-corr_rec_zip       =  is_zpwefa_ups-receiver_postal.
          wa_ups-corr_rec_country   =  is_zpwefa_ups-receiver_country.
*          ELSEIF is_zpwefa_ups1-chrg_catg_code eq 'ADJ'.
*
        ELSEIF ( IS_ZPWEFA_UPS-chrg_catg_det_cd = 'SCC' OR IS_ZPWEFA_UPS-chrg_catg_det_cd = 'RADJ' )  and
           is_zpwefa_ups-chrg_class_code  = 'FRT' .
*
*          wa_ups-addr_corr_flag = 'X'.
*
*          wa_ups-addr_corr_charge   = is_zpwefa_ups-net_amnt.
*
*          wa_ups-corr_rec_name      =  is_zpwefa_ups-receiver_name.
*          wa_ups-corr_rec_company   =  is_zpwefa_ups-receiver_comp_nm.
*          wa_ups-corr_recaddress1   =  is_zpwefa_ups-rec_addr_line_1.
*          wa_ups-corr_recaddress2   =  is_zpwefa_ups-rec_addr_line_2.
*          wa_ups-corr_rec_city      =  is_zpwefa_ups-receiver_city.
*          wa_ups-corr_rec_state     =  is_zpwefa_ups-receiver_state.
*          wa_ups-corr_rec_zip       =  is_zpwefa_ups-receiver_postal.
*          wa_ups-corr_rec_country   =  is_zpwefa_ups-receiver_country.
*        ELSEIF IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'SCC' .
*          wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
          wa_ups-account_no        =  is_zpwefa_ups-account_number.
          wa_ups-service_type      =  is_zpwefa_ups-chrg_desc.
          wa_ups-ship_date         =  is_zpwefa_ups-transaction_date.
          wa_upS-zzone             =  is_zpwefa_ups-z_one.
          wa_ups-payment_method    =  is_zpwefa_ups-bill_opt_code.
          wa_ups-invoice_date      =  is_zpwefa_ups-invoice_date.
          wa_ups-invoice_amount    =  is_zpwefa_ups-invoice_amount.
          wa_ups-invoice_curr_key  =  is_zpwefa_ups-invc_curr_code.
          wa_ups-entered_weight    =  is_zpwefa_ups-entered_weight.
          wa_ups-entered_wt_unit   =  is_zpwefa_ups-ent_wgt_uom.
          wa_ups-billed_weight     =  is_zpwefa_ups-billed_weight.
          wa_ups-billed_wt_unit    =  is_zpwefa_ups-billed_wgt_uom.
          wa_ups-container_type    =  is_zpwefa_ups-container_type.
          wa_ups-chrg_catg_code    =  is_zpwefa_ups-chrg_catg_code.
          wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
          wa_ups-chrg_class_code   =  is_zpwefa_ups-chrg_class_code.
          wa_ups-chrg_unit_qty     =  is_zpwefa_ups-chrg_unit_qty.

          wa_ups-package_qty       =  is_zpwefa_ups-pckg_qty.
          wa_ups-chrg_desc_code    =  is_zpwefa_ups-chrg_desc_code.
          wa_ups-chrg_desc         =  is_zpwefa_ups-chrg_desc.

*          wa_ups1-ship_incent_amnt    =  is_zpwefa_ups1-incen_amt.
*          wa_ups1-ship_net_amount     =  is_zpwefa_ups1-net_amnt.
          wa_ups-sender_name         =  is_zpwefa_ups-sender_name.
          wa_ups-sender_company      =  is_zpwefa_ups-sender_cmp_name.
          wa_ups-sender_address1     =  is_zpwefa_ups-sender_addr_lin1.
          wa_ups-sender_address2     =  is_zpwefa_ups-sender_addr_lin2.
          wa_ups-sender_city         =  is_zpwefa_ups-sender_city.
          wa_ups-sender_state        =  is_zpwefa_ups-sender_state.
          wa_ups-sender_postcode     =  is_zpwefa_ups-sender_postal.
          wa_ups-sender_country      =  is_zpwefa_ups-sender_country.
          wa_ups-receiver_name       =  is_zpwefa_ups-receiver_name.
          wa_ups-receiver_company    =  is_zpwefa_ups-receiver_comp_nm.
          wa_ups-rec_address1        =  is_zpwefa_ups-rec_addr_line_1.
          wa_ups-rec_address2        =  is_zpwefa_ups-rec_addr_line_2.
          wa_ups-receiver_city       =  is_zpwefa_ups-receiver_city.
          wa_ups-receiver_state      =  is_zpwefa_ups-receiver_state.
          wa_ups-rec_postcode   =  is_zpwefa_ups-receiver_postal.
          wa_ups-receiver_country    =  is_zpwefa_ups-receiver_country.
          wa_ups-corr_ship_incent   =  is_zpwefa_ups-incen_amt.
          wa_ups-corr_ship_net      =  is_zpwefa_ups-net_amnt.


        ELSE ."IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SCC' or is_zpwefa_ups-chrg_catg_det_cd ne 'RADJ' .
          wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
          wa_ups-corr_ship_incent   =  is_zpwefa_ups-incen_amt.
          wa_ups-corr_ship_net      =  is_zpwefa_ups-net_amnt.

        ENDIF.
      ELSEIF is_zpwefa_ups-chrg_catg_code eq 'MIS'.
         IF IS_ZPWEFA_UPS-chrg_catg_det_cd eq 'MISC'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC' .
       wa_ups-service_type      =  'Other Charges'.
        wa_ups-chrg_catg_code    =  is_zpwefa_ups-chrg_catg_code.
         wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
        wa_ups-corr_ship_incent   =  is_zpwefa_ups-incen_amt.
        wa_ups-corr_ship_net      =  is_zpwefa_ups-net_amnt.
          ELSEIF  IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'SVCH'.
            wa_ups-service_type      =  is_zpwefa_ups-chrg_desc.
         wa_ups1-chrg_catg_code    =  is_zpwefa_ups-chrg_catg_code.
          wa_ups1-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
          wa_ups1-chrg_class_code   =  is_zpwefa_ups-chrg_class_code.
        wa_ups-corr_ship_incent   =  is_zpwefa_ups-incen_amt.
        wa_ups-corr_ship_net      =  is_zpwefa_ups-net_amnt.
        ENDIF.

*         if IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SVCH' AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC' .
*           wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
*        wa_ups-corr_ship_incent   =  is_zpwefa_ups-incen_amt.
*        wa_ups-corr_ship_net      =  is_zpwefa_ups-net_amnt.
*        else.
**          clear wa_ups.
*         endif.
      ENDIF.
      APPEND wa_ups to it_ups.
      CLEAR  wa_ups.
    ELSE.  """""" Modify existing record

********** For shipping and handling charges *************

      IF is_zpwefa_ups-chrg_catg_code eq 'SHP' or is_zpwefa_ups-chrg_catg_code eq 'RTN'.
        IF is_zpwefa_ups-chrg_class_code = 'FRT'."( is_zpwefa_ups-chrg_class_code = 'FRT' OR is_zpwefa_ups-chrg_class_code = 'INF' ).   """""" shipping charges
          wa_ups-account_no        =  is_zpwefa_ups-account_number.
          wa_ups-service_type      =  is_zpwefa_ups-chrg_desc.
          wa_ups-ship_date         =  is_zpwefa_ups-transaction_date.
          wa_ups-zzone             =  is_zpwefa_ups-z_one.
          wa_ups-payment_method    =  is_zpwefa_ups-bill_opt_code.
          wa_ups-invoice_date      =  is_zpwefa_ups-invoice_date.
          wa_ups-invoice_amount    =  is_zpwefa_ups-invoice_amount.
          wa_ups-invoice_curr_key  =  is_zpwefa_ups-invc_curr_code.
          wa_ups-entered_weight    =  is_zpwefa_ups-entered_weight.
          wa_ups-entered_wt_unit   =  is_zpwefa_ups-ent_wgt_uom.
          wa_ups-billed_weight     =  is_zpwefa_ups-billed_weight.
          wa_ups-billed_wt_unit    =  is_zpwefa_ups-billed_wgt_uom.
          wa_ups-container_type    =  is_zpwefa_ups-container_type.
          wa_ups-chrg_catg_code    =  is_zpwefa_ups-chrg_catg_code.
          wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
          wa_ups-chrg_class_code   =  is_zpwefa_ups-chrg_class_code.
          wa_ups-chrg_unit_qty     =  is_zpwefa_ups-chrg_unit_qty.

          wa_ups-package_qty       =  is_zpwefa_ups-pckg_qty.
          wa_ups-chrg_desc_code    =  is_zpwefa_ups-chrg_desc_code.
          wa_ups-chrg_desc         =  is_zpwefa_ups-chrg_desc.

          wa_ups-ship_incent_amnt    =  is_zpwefa_ups-incen_amt.
          wa_ups-ship_net_amount     =  is_zpwefa_ups-net_amnt.
          wa_ups-sender_name         =  is_zpwefa_ups-sender_name.
          wa_ups-sender_company      =  is_zpwefa_ups-sender_cmp_name.
          wa_ups-sender_address1     =  is_zpwefa_ups-sender_addr_lin1.
          wa_ups-sender_address2     =  is_zpwefa_ups-sender_addr_lin2.
          wa_ups-sender_city         =  is_zpwefa_ups-sender_city.
          wa_ups-sender_state        =  is_zpwefa_ups-sender_state.
          wa_ups-sender_postcode     =  is_zpwefa_ups-sender_postal.
          wa_ups-sender_country      =  is_zpwefa_ups-sender_country.
          wa_ups-receiver_name       =  is_zpwefa_ups-receiver_name.
          wa_ups-receiver_company    =  is_zpwefa_ups-receiver_comp_nm.
          wa_ups-rec_address1        =  is_zpwefa_ups-rec_addr_line_1.
          wa_ups-rec_address2        =  is_zpwefa_ups-rec_addr_line_2.
          wa_ups-receiver_city       =  is_zpwefa_ups-receiver_city.
          wa_ups-receiver_state      =  is_zpwefa_ups-receiver_state.
          wa_ups-rec_postcode   =  is_zpwefa_ups-receiver_postal.
          wa_ups-receiver_country    =  is_zpwefa_ups-receiver_country.
        ELSE. """"""""""""""""" Other/handling charges
          wa_ups-handle_incen_amt    =  wa_ups-handle_incen_amt + is_zpwefa_ups-incen_amt.
          wa_ups-handle_net_amnt     =  wa_ups-handle_net_amnt  + is_zpwefa_ups-net_amnt.
        ENDIF.
        modify it_ups from wa_ups index sy-tabix.
      ELSEIF is_zpwefa_ups-chrg_catg_code eq 'ADJ'.

        IF is_zpwefa_ups-chrg_catg_det_cd = 'ADC' and
           is_zpwefa_ups-chrg_class_code  = 'FRT' .

          wa_ups-addr_corr_flag = 'X'.

          wa_ups-addr_corr_charge   = is_zpwefa_ups-net_amnt.

          wa_ups-corr_rec_name      =  is_zpwefa_ups-receiver_name.
          wa_ups-corr_rec_company   =  is_zpwefa_ups-receiver_comp_nm.
          wa_ups-corr_recaddress1   =  is_zpwefa_ups-rec_addr_line_1.
          wa_ups-corr_recaddress2   =  is_zpwefa_ups-rec_addr_line_2.
          wa_ups-corr_rec_city      =  is_zpwefa_ups-receiver_city.
          wa_ups-corr_rec_state     =  is_zpwefa_ups-receiver_state.
          wa_ups-corr_rec_zip       =  is_zpwefa_ups-receiver_postal.
          wa_ups-corr_rec_country   =  is_zpwefa_ups-receiver_country.
        ELSE ."IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SCC' .
*          wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
          wa_ups-corr_ship_incent   =  wa_ups-corr_ship_incent + is_zpwefa_ups-incen_amt.
          wa_ups-corr_ship_net      =  wa_ups-corr_ship_net    + is_zpwefa_ups-net_amnt.
        ENDIF.
        modify it_ups from wa_ups index sy-tabix.
*      ELSEIF is_zpwefa_ups-chrg_catg_code eq 'MIS' AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SVCH' AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC'.
*        wa_ups-corr_ship_incent   =  wa_ups-corr_ship_incent + is_zpwefa_ups-incen_amt.
*        wa_ups-corr_ship_net      =  wa_ups-corr_ship_net    + is_zpwefa_ups-net_amnt.
*        modify it_ups from wa_ups index sy-tabix.
         ELSEIF is_zpwefa_ups-chrg_catg_code eq 'MIS' AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC'.
         wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
        wa_ups-corr_ship_incent   =  wa_ups-corr_ship_incent + is_zpwefa_ups-incen_amt.
        wa_ups-corr_ship_net      =  wa_ups-corr_ship_net    + is_zpwefa_ups-net_amnt.
        modify it_ups from wa_ups index sy-tabix.
        ELSEIF is_zpwefa_ups-chrg_catg_code eq 'MIS' AND IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC' .
         wa_ups-chrg_catg_code    =  is_zpwefa_ups-chrg_catg_code.
          wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
          wa_ups-chrg_class_code   =  is_zpwefa_ups-chrg_class_code.
        wa_ups-corr_ship_incent   =  is_zpwefa_ups-incen_amt.
        wa_ups-corr_ship_net      =  is_zpwefa_ups-net_amnt.
        append wa_ups to it_ups.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " SUMMERIZE_DATA
*&---------------------------------------------------------------------*
*&      Form  ZONE_TYPE_TEMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ZONE_TYPE_TEMP .

  data : v_inv TYPE ty_output_zone_type.
  refresh it_output_zone_type_ups.
  clear : wa_output_zone_type_ups.

  LOOP AT it_zone_type_ups into wa_zone_type_ups.

    At NEW zzone.
      sum.

      wa_output_zone_type_ups-zzone = wa_zone_type_ups-zzone.
      wa_output_zone_type_ups-total_packs  = wa_zone_type_ups-packages.
      wa_output_zone_type_ups-actual_weight = wa_zone_type_ups-actualweight.
      wa_output_zone_type_ups-rated_weight = wa_zone_type_ups-ratedweight.
      wa_output_zone_type_ups-transportation_charge = wa_zone_type_ups-ship_net_amount + wa_zone_type_ups-SHIP_INCENT_AMNT.
      wa_output_zone_type_ups-handling_charge = wa_zone_type_ups-handle_net_amnt + wa_zone_type_ups-HANDLE_INCEN_AMT.
      wa_output_zone_type_ups-total_discounts = wa_zone_type_ups-ship_incent_amnt + wa_zone_type_ups-handle_incen_amt + wa_zone_type_ups-corr_ship_incent.
      wa_output_zone_type_ups-correction_charge = wa_zone_type_ups-corr_ship_net +
                                                  wa_zone_type_ups-CORR_HANDLE_NET +
                                                  wa_zone_type_ups-ADDR_CORR_CHARGE +
                                                  wa_zone_type_ups-corr_ship_incent.
*      wa_output_zone_type_ups-adjustment_charge = wa_zone_type_ups-misc_net_chrg.
      wa_output_zone_type_ups-net_charge  = wa_output_zone_type_ups-transportation_charge +
                                            wa_output_zone_type_ups-handling_charge       -
                                            wa_output_zone_type_ups-total_discounts       +
                                            wa_output_zone_type_ups-correction_charge     +
                                            wa_output_zone_type_ups-adjustment_charge.

      IF wa_output_zone_type_ups-zzone is INITIAL.
        wa_output_zone_type_ups-zzone = 'Inv Misc'.
        v_inv = wa_output_zone_type_ups.
        CONTINUE.
      ENDIF.

      append wa_output_zone_type_ups to it_output_zone_type_ups.
      clear wa_output_zone_type_ups.
    endat.

  ENDLOOP.

  IF v_inv is NOT INITIAL.
    v_inv-line_color      =  'C600'.
    append v_inv to it_output_zone_type_ups.
    clear v_inv.
  ENDIF.




  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'ZZONE'.
  wa_fieldcat-coltext      = 'Zone'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'TOTAL_PACKS'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'RATED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 9.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_ZONE_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Zone Type'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_OUPUT_ZONE_TYPE_UPS'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_output_zone_type_ups
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
ENDFORM.                    " ZONE_TYPE_TEMP
*&---------------------------------------------------------------------*
*&      Form  PACKAGE_TYPE_TEMP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PACKAGE_TYPE_TEMP .

  DATA : v_inv type ty_output_pack_type.
  refresh it_output_pack_type_ups.
  clear : wa_output_pack_type_ups.

  LOOP AT it_pack_type_ups into wa_pack_type_ups.

    At NEW package_type.
      sum.

      wa_output_pack_type_ups-package_type = wa_pack_type_ups-package_type.
      wa_output_pack_type_ups-total_packs  = wa_pack_type_ups-packages.
      wa_output_pack_type_ups-actual_weight = wa_pack_type_ups-actualweight.
      wa_output_pack_type_ups-rated_weight = wa_pack_type_ups-ratedweight.
      wa_output_pack_type_ups-transportation_charge = wa_pack_type_ups-ship_net_amount + wa_pack_type_ups-SHIP_INCENT_AMNT.
      wa_output_pack_type_ups-handling_charge = wa_pack_type_ups-handle_net_amnt + wa_pack_type_ups-HANDLE_INCEN_AMT.
      wa_output_pack_type_ups-total_discounts = wa_pack_type_ups-ship_incent_amnt + wa_pack_type_ups-handle_incen_amt + wa_pack_type_ups-corr_ship_incent.
      wa_output_pack_type_ups-correction_charge = wa_pack_type_ups-corr_ship_net + wa_pack_type_ups-CORR_HANDLE_NET +
                                                  wa_pack_type_ups-ADDR_CORR_CHARGE + wa_pack_type_ups-corr_ship_incent.
*      wa_output_pack_type_ups-adjustment_charge = wa_pack_type_ups-misc_net_chrg.
      wa_output_pack_type_ups-net_charge  = wa_output_pack_type_ups-transportation_charge +
                                            wa_output_pack_type_ups-handling_charge       -
                                            wa_output_pack_type_ups-total_discounts       +
                                            wa_output_pack_type_ups-correction_charge     +
                                            wa_output_pack_type_ups-adjustment_charge.

*       IF wa_output_zone_type_ups-correction_charge is not INITIAL or wa_output_zone_type_ups-adjustment_charge is not INITIAL.
*           wa_output_zone_type_ups-line_color = 'C610'.
*       ENDIF.

      IF wa_output_pack_type_ups-package_type is INITIAL.
        wa_output_pack_type_ups-package_type = 'Inv Miscellaneous'.
        v_inv = wa_output_pack_type_ups.
        CONTINUE.
      ENDIF.

      append wa_output_pack_type_ups to it_output_pack_type_ups.
      clear wa_output_pack_type_ups.
    endat.

  ENDLOOP.

  IF v_inv is NOT INITIAL.
    v_inv-line_color      =  'C600'.
    append v_inv to it_output_pack_type_ups.
    clear v_inv.
  ENDIF.
*  wa_output_pack_type_ups-package_type = 'Misc & Other Adj'.
*  wa_output_pack_type_ups-total_discounts  =  v_disc.
*  wa_output_pack_type_ups-net_charge  = v_net.
*  wa_output_pack_type_ups-line_color = 'C600'.

*  append wa_output_pack_type_ups to it_output_pack_type_ups.
*  clear wa_output_pack_type_ups.




  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'PACKAGE_TYPE'.
  wa_fieldcat-coltext      = 'Shipment Type'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'TOTAL_PACKS'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'RATED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 9.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_OUPUT_PACK_TYPE_UPS'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Shipment Type'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_OUPUT_PACK_TYPE_UPS'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_output_pack_type_ups
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

ENDFORM.                    " PACKAGE_TYPE_TEMP
*&---------------------------------------------------------------------*
*&      Form  ADDR_CORRECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ADDR_CORRECTION .
  DATA : it_ups_prev TYPE STANDARD TABLE OF /PWEAVER/EFA_UPS,
         wa_ups_prev TYPE /PWEAVER/EFA_UPS.

  DATA : it_addr_prev TYPE STANDARD TABLE OF /PWEAVER/efa_ups_summ,
       wa_addr_prev TYPE  /PWEAVER/efa_ups_summ.

  REFRESH : it_addr_corr_data, it_addr.
  CLEAR   : wa_addr_corr_data, wa_addr.

  LOOP AT it_ups INTO wa_ups WHERE addr_corr_flag = 'X'.

    wa_addr-tracking_number = wa_ups-tracking_number.
    wa_addr-address_correction = wa_ups-addr_corr_charge.
    IF wa_ups-service_type IS NOT INITIAL.
      wa_addr-ship_date = wa_ups-ship_date.
      wa_addr-service_type = wa_ups-service_type.
      wa_addr-zone   =  wa_ups-zzone.
      wa_addr-package_qty = wa_ups-package_qty.
      wa_addr-actual_weight  = wa_ups-entered_weight.
      wa_addr-billed_weight  = wa_ups-billed_weight.
      wa_addr-transportation_charge  = wa_ups-ship_net_amount + wa_ups-ship_incent_amnt.
      wa_addr-handling_charge = wa_ups-handle_net_amnt + wa_ups-handle_incen_amt.
      wa_addr-correction_charge  = wa_ups-corr_ship_net + wa_ups-corr_handle_net + wa_ups-corr_ship_incent.
      wa_addr-total_discounts  = wa_ups-ship_incent_amnt + wa_ups-handle_incen_amt + wa_ups-corr_ship_incent.
      wa_addr-net_charge = wa_addr-transportation_charge +
                           wa_addr-handling_charge       -
                           wa_addr-total_discounts       +
                           wa_addr-correction_charge     +
                           wa_addr-adjustment_charge     +
                           wa_addr-address_correction.
    ELSE.

  SELECT * FROM /PWEAVER/EFA_UPS INTO TABLE it_ups_prev WHERE lead_ship_number = wa_ups-lead_ship_number AND
                                                            trck_num         = wa_ups-tracking_number  AND
                                                            invoice_number   NE wa_ups-invoice_number.

  loop at it_ups_prev into wa_ups_prev .

    clear wa_addr_prev.
    read table it_addr_prev into wa_addr_prev with key lead_ship_number = wa_ups_prev-lead_ship_number
                                                       tracking_number  = wa_ups_prev-trck_num.

    IF sy-subrc ne 0.  """""" Add the record into internal table

      wa_addr_prev-lead_ship_number  =  wa_ups_prev-lead_ship_number.
      wa_addr_prev-tracking_number   =  wa_ups_prev-trck_num.
      wa_addr_prev-invoice_number    =  wa_ups_prev-invoice_number.

********** For shipping and handling charges *************

      IF wa_ups_prev-chrg_catg_code eq 'SHP' or wa_ups_prev-chrg_catg_code eq 'RTN'.

        IF is_zpwefa_ups-chrg_class_code = 'FRT' ."( is_zpwefa_ups-chrg_class_code = 'FRT' OR is_zpwefa_ups-chrg_class_code = 'INF' ).   """""" shipping charges
          wa_addr_prev-account_no        =  wa_ups_prev-account_number.
          wa_addr_prev-service_type      =  wa_ups_prev-chrg_desc.
          wa_addr_prev-ship_date         =  wa_ups_prev-transaction_date.
          wa_addr_prev-zzone             =  wa_ups_prev-z_one.
          wa_addr_prev-payment_method    =  wa_ups_prev-bill_opt_code.
          wa_addr_prev-invoice_date      =  wa_ups_prev-invoice_date.
          wa_addr_prev-invoice_amount    =  wa_ups_prev-invoice_amount.
          wa_addr_prev-invoice_curr_key  =  wa_ups_prev-invc_curr_code.
          wa_addr_prev-entered_weight    =  wa_ups_prev-entered_weight.
          wa_addr_prev-entered_wt_unit   =  wa_ups_prev-ent_wgt_uom.
          wa_addr_prev-billed_weight     =  wa_ups_prev-billed_weight.
          wa_addr_prev-billed_wt_unit    =  wa_ups_prev-billed_wgt_uom.
          wa_addr_prev-container_type    =  wa_ups_prev-container_type.
          wa_addr_prev-chrg_catg_code    =  wa_ups_prev-chrg_catg_code.
          wa_addr_prev-chrg_catg_detail  =  wa_ups_prev-chrg_catg_det_cd.
          wa_addr_prev-chrg_class_code   =  wa_ups_prev-chrg_class_code.
          wa_addr_prev-chrg_unit_qty     =  wa_ups_prev-chrg_unit_qty.

          wa_addr_prev-package_qty       =  wa_ups_prev-pckg_qty.
          wa_addr_prev-chrg_desc_code    =  wa_ups_prev-chrg_desc_code.
          wa_addr_prev-chrg_desc         =  wa_ups_prev-chrg_desc.

          wa_addr_prev-ship_incent_amnt    =  wa_ups_prev-incen_amt.
          wa_addr_prev-ship_net_amount     =  wa_ups_prev-net_amnt.
          wa_addr_prev-sender_name         =  wa_ups_prev-sender_name.
          wa_addr_prev-sender_company      =  wa_ups_prev-sender_cmp_name.
          wa_addr_prev-sender_address1     =  wa_ups_prev-sender_addr_lin1.
          wa_addr_prev-sender_address2     =  wa_ups_prev-sender_addr_lin2.
          wa_addr_prev-sender_city         =  wa_ups_prev-sender_city.
          wa_addr_prev-sender_state        =  wa_ups_prev-sender_state.
          wa_addr_prev-sender_postcode     =  wa_ups_prev-sender_postal.
          wa_addr_prev-sender_country      =  wa_ups_prev-sender_country.
          wa_addr_prev-receiver_name       =  wa_ups_prev-receiver_name.
          wa_addr_prev-receiver_company    =  wa_ups_prev-receiver_comp_nm.
          wa_addr_prev-rec_address1        =  wa_ups_prev-rec_addr_line_1.
          wa_addr_prev-rec_address2        =  wa_ups_prev-rec_addr_line_2.
          wa_addr_prev-receiver_city       =  wa_ups_prev-receiver_city.
          wa_addr_prev-receiver_state      =  wa_ups_prev-receiver_state.
          wa_addr_prev-rec_postcode        =  wa_ups_prev-receiver_postal.
          wa_addr_prev-receiver_country    =  wa_ups_prev-receiver_country.
        ELSE.                                                      """"""""""""""""" Other/handling charges
          wa_addr_prev-handle_incen_amt    =  wa_ups_prev-incen_amt.
          wa_addr_prev-handle_net_amnt     =  wa_ups_prev-net_amnt.
        ENDIF.
      ELSEIF wa_ups_prev-chrg_catg_code eq 'ADJ'.

        IF wa_ups_prev-chrg_catg_det_cd = 'ADC' and
           wa_ups_prev-chrg_class_code  = 'FRT' .

          wa_addr_prev-addr_corr_flag = 'X'.

          wa_addr_prev-addr_corr_charge   = wa_ups_prev-net_amnt.

          wa_addr_prev-corr_rec_name      =  wa_ups_prev-receiver_name.
          wa_addr_prev-corr_rec_company   =  wa_ups_prev-receiver_comp_nm.
          wa_addr_prev-corr_recaddress1   =  wa_ups_prev-rec_addr_line_1.
          wa_addr_prev-corr_recaddress2   =  wa_ups_prev-rec_addr_line_2.
          wa_addr_prev-corr_rec_city      =  wa_ups_prev-receiver_city.
          wa_addr_prev-corr_rec_state     =  wa_ups_prev-receiver_state.
          wa_addr_prev-corr_rec_zip       =  wa_ups_prev-receiver_postal.
          wa_addr_prev-corr_rec_country   =  wa_ups_prev-receiver_country.
        ELSE.
          wa_addr_prev-corr_ship_incent   =  wa_ups_prev-incen_amt.
          wa_addr_prev-corr_ship_net      =  wa_ups_prev-net_amnt.
        ENDIF.
      ELSEIF is_zpwefa_ups-chrg_catg_code eq 'MIS'.
        wa_addr_prev-corr_ship_incent   =  wa_ups_prev-incen_amt.
        wa_addr_prev-corr_ship_net      =  wa_ups_prev-net_amnt.
      ENDIF.
      APPEND wa_addr_prev to it_addr_prev.
      CLEAR  wa_addr_prev.
    ELSE.  """""" Modify existing record

********** For shipping and handling charges *************

      IF wa_ups_prev-chrg_catg_code eq 'SHP' or wa_ups_prev-chrg_catg_code eq 'RTN'.
        IF wa_ups_prev-chrg_class_code = 'FRT'."( is_zpwefa_ups-chrg_class_code = 'FRT' OR is_zpwefa_ups-chrg_class_code = 'INF' ).   """""" shipping charges
          wa_addr_prev-account_no        =  wa_ups_prev-account_number.
          wa_addr_prev-service_type      =  wa_ups_prev-chrg_desc.
          wa_addr_prev-ship_date         =  wa_ups_prev-transaction_date.
          wa_addr_prev-zzone             =  wa_ups_prev-z_one.
          wa_addr_prev-payment_method    =  wa_ups_prev-bill_opt_code.
          wa_addr_prev-invoice_date      =  wa_ups_prev-invoice_date.
          wa_addr_prev-invoice_amount    =  wa_ups_prev-invoice_amount.
          wa_addr_prev-invoice_curr_key  =  wa_ups_prev-invc_curr_code.
          wa_addr_prev-entered_weight    =  wa_ups_prev-entered_weight.
          wa_addr_prev-entered_wt_unit   =  wa_ups_prev-ent_wgt_uom.
          wa_addr_prev-billed_weight     =  wa_ups_prev-billed_weight.
          wa_addr_prev-billed_wt_unit    =  wa_ups_prev-billed_wgt_uom.
          wa_addr_prev-container_type    =  wa_ups_prev-container_type.
          wa_addr_prev-chrg_catg_code    =  wa_ups_prev-chrg_catg_code.
          wa_addr_prev-chrg_catg_detail  =  wa_ups_prev-chrg_catg_det_cd.
          wa_addr_prev-chrg_class_code   =  wa_ups_prev-chrg_class_code.
          wa_addr_prev-chrg_unit_qty     =  wa_ups_prev-chrg_unit_qty.

          wa_addr_prev-package_qty       =  wa_ups_prev-pckg_qty.
          wa_addr_prev-chrg_desc_code    =  wa_ups_prev-chrg_desc_code.
          wa_addr_prev-chrg_desc         =  wa_ups_prev-chrg_desc.

          wa_addr_prev-ship_incent_amnt    =  wa_ups_prev-incen_amt.
          wa_addr_prev-ship_net_amount     =  wa_ups_prev-net_amnt.
          wa_addr_prev-sender_name         =  wa_ups_prev-sender_name.
          wa_addr_prev-sender_company      =  wa_ups_prev-sender_cmp_name.
          wa_addr_prev-sender_address1     =  wa_ups_prev-sender_addr_lin1.
          wa_addr_prev-sender_address2     =  wa_ups_prev-sender_addr_lin2.
          wa_addr_prev-sender_city         =  wa_ups_prev-sender_city.
          wa_addr_prev-sender_state        =  wa_ups_prev-sender_state.
          wa_addr_prev-sender_postcode     =  wa_ups_prev-sender_postal.
          wa_addr_prev-sender_country      =  wa_ups_prev-sender_country.
          wa_addr_prev-receiver_name       =  wa_ups_prev-receiver_name.
          wa_addr_prev-receiver_company    =  wa_ups_prev-receiver_comp_nm.
          wa_addr_prev-rec_address1        =  wa_ups_prev-rec_addr_line_1.
          wa_addr_prev-rec_address2        =  wa_ups_prev-rec_addr_line_2.
          wa_addr_prev-receiver_city       =  wa_ups_prev-receiver_city.
          wa_addr_prev-receiver_state      =  wa_ups_prev-receiver_state.
          wa_addr_prev-rec_postcode   =  wa_ups_prev-receiver_postal.
          wa_addr_prev-receiver_country    =  wa_ups_prev-receiver_country.
        ELSE. """"""""""""""""" Other/handling charges
          wa_addr_prev-handle_incen_amt    =  wa_ups-handle_incen_amt + wa_ups_prev-incen_amt.
          wa_addr_prev-handle_net_amnt     =  wa_ups-handle_net_amnt  + wa_ups_prev-net_amnt.
        ENDIF.
        modify it_addr_prev from wa_addr_prev index sy-tabix.
      ELSEIF wa_ups_prev-chrg_catg_code eq 'ADJ'.

        IF wa_ups_prev-chrg_catg_det_cd = 'ADC' and
           wa_ups_prev-chrg_class_code  = 'FRT' .

          wa_addr_prev-addr_corr_flag = 'X'.

          wa_addr_prev-addr_corr_charge   = wa_ups_prev-net_amnt.

          wa_addr_prev-corr_rec_name      =  wa_ups_prev-receiver_name.
          wa_addr_prev-corr_rec_company   =  wa_ups_prev-receiver_comp_nm.
          wa_addr_prev-corr_recaddress1   =  wa_ups_prev-rec_addr_line_1.
          wa_addr_prev-corr_recaddress2   =  wa_ups_prev-rec_addr_line_2.
          wa_addr_prev-corr_rec_city      =  wa_ups_prev-receiver_city.
          wa_addr_prev-corr_rec_state     =  wa_ups_prev-receiver_state.
          wa_addr_prev-corr_rec_zip       =  wa_ups_prev-receiver_postal.
          wa_addr_prev-corr_rec_country   =  wa_ups_prev-receiver_country.
        ELSE.
          wa_addr_prev-corr_ship_incent   =  wa_addr_prev-corr_ship_incent + wa_ups_prev-incen_amt.
          wa_addr_prev-corr_ship_net      =  wa_addr_prev-corr_ship_net    + wa_ups_prev-net_amnt.
        ENDIF.
        modify it_addr_prev from wa_addr_prev index sy-tabix.
      ELSEIF wa_ups_prev-chrg_catg_code eq 'MIS'.
        wa_addr_prev-corr_ship_incent   =  wa_addr_prev-corr_ship_incent + wa_ups_prev-incen_amt.
        wa_addr_prev-corr_ship_net      =  wa_addr_prev-corr_ship_net    + wa_ups_prev-net_amnt.
        modify it_addr_prev from wa_addr_prev index sy-tabix.
      ENDIF.

    ENDIF.
  ENDLOOP.

LOOP AT it_addr_prev INTO wa_addr_prev WHERE lead_ship_number = wa_ups-lead_ship_number AND
                                             tracking_number  = wa_ups-tracking_number.

      wa_addr-ship_date = wa_addr_prev-ship_date.
      wa_addr-service_type = wa_addr_prev-service_type.
      wa_addr-zone   =  wa_addr_prev-zzone.
      wa_addr-package_qty = wa_addr_prev-package_qty.
      wa_addr-actual_weight  = wa_addr_prev-entered_weight.
      wa_addr-billed_weight  = wa_addr_prev-billed_weight.
      wa_addr-transportation_charge  = wa_addr_prev-ship_net_amount + wa_addr_prev-ship_incent_amnt.
      wa_addr-handling_charge = wa_addr_prev-handle_net_amnt + wa_addr_prev-handle_incen_amt.
      wa_addr-correction_charge  = wa_addr_prev-corr_ship_net + wa_addr_prev-corr_handle_net + wa_addr_prev-corr_ship_incent.
      wa_addr-total_discounts  = wa_addr_prev-ship_incent_amnt + wa_addr_prev-handle_incen_amt + wa_addr_prev-corr_ship_incent.
      wa_addr-net_charge = wa_addr-transportation_charge +
                           wa_addr-handling_charge       -
                           wa_addr-total_discounts       +
                           wa_addr-correction_charge     +
                           wa_addr-adjustment_charge     +
                           wa_addr-address_correction.

ENDLOOP.
    ENDIF.


    append wa_addr to it_addr.
    clear wa_addr.
  ENDLOOP.




  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'SHIP_DATE'.
  wa_fieldcat-coltext      = 'Ship Date'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'SERVICE_TYPE'.
  wa_fieldcat-coltext      = 'Service Type'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  WA_FIELDCAT-OUTPUTLEN  = '35'.
  wa_fieldcat-lowercase = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ZONE'.
  wa_fieldcat-coltext      = 'Zone'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'TRACKING_NUMBER'.
  wa_fieldcat-coltext      = 'Tracking Number'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-hotspot      = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'PACKAGE_QTY'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'BILLED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 9.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'Other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 11.
  wa_fieldcat-fieldname    = 'ADDRESS_CORRECTION'.
  wa_fieldcat-coltext      = 'Address Correction Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 12.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Other Correction Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 13.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_ADDR'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 14.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_ADDR'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'By Address Corrections '.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_ADDR'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_addr
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


ENDFORM.                    " ADDR_CORRECTION
*&---------------------------------------------------------------------*
*&      Form  MANIFEST_FREIGHT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form MANIFEST_FREIGHT .


*types :
*        begin of x_efa_bill,
*
*
*         inv_no          type  ZPWINVOICE_NUMBER,
*         inv_date        type  sy-datum,
*         SHIP_DATE       type   zpwefa_track-TRANSACTION_DATE ,
*
*
*         ship_method     LIKE lfa1-name1,
*         Track_no        type  ZPWTRCK_NUM,
*         v_discount      type  FRAKO,                  " discount amount
*         v_bill_efa      type  ZPWEFA_UPS-NET_AMNT,    " billed charges
*         v_bill_man      type  FRAKO,                  "frieght charges
*         v_bill_diff(18) type  c,                      " difference between billed charges and frieght ant for each Tracking number
*
*        end of x_efa_bill,
*
*
*       begin of x_zpwmanifest,
*
*         DATE_ADDED      type  ERZDT ,
*         TRACKING_NUMBER type ZPWTRCK_NUM,
*         carrier_code    LIKE vbpa-lifnr ,
*         ship_method     LIKE lfa1-name1,
*         FREIGHTAMT(18)  type c,
*         DISCOUNTAMT(18) type c,
*
*        end of x_zpwmanifest.
**
**
*data :
**
*       t_efa_bill  type standard table of x_efa_bill,
*       w_efa_bill  type x_efa_bill,
*       t_efa_ups   type standard table of zpwefa_ups,
*       w_efa_ups   type zpwefa_ups,
*       t_efa_track type standard table of zpwefa_track,
*       w_efa_track type zpwefa_track,
*       t_man       type standard table of X_zpwmanifest,
*       w_man       type X_zpwmanifest          ,
*       t_fieldcat type standard table of lvc_s_fcat,
*       w_fieldcat type lvc_s_fcat  .
*
*
*
*data: v_tot_bill type ZPWEFA_UPS-NET_AMNT,
*      lv_bill_efa(18) type c,
*      lv_bill_man(18) type c.
*
*data: ls_lfa1 type lfa1,
*      ls_carrierconfig type zpwcarrierconfig   ,
*      t_carrierconfig type standard table of zpwcarrierconfig            .
*data :W_LAYOUT TYPE lvc_s_layo,
*     v_repid type string value sy-repid .
data lv_bill_disc(18) type c.
CLEAR: w_layout ,lv_bill_disc .

REFRESH : T_FIELDCAT ,
          T_EFA_BILL,
          t_efa_bill,
*          t_carrierconfig,
          t_efa_ups,
          t_efa_ups1,
          t_efa_track.


*
*select * from zpwcarrierconfig into table t_carrierconfig  .

*select * from zpwefa_ups into table t_efa_ups
*                                           where  INVOICE_NUMBER = v_invoice_number  .
*
clear t_man.
refresh t_man.
t_efa_ups = it_zpwefa_ups.

t_efa_ups1 = it_zpwefa_ups.
     if t_efa_ups is not initial.

       sort t_efa_ups by INVOICE_NUMBER invoice_date .


          select *  from /pweaver/efa_trc into table t_efa_track for all entries in t_efa_ups

                                                     where INVOICE_NO = t_efa_ups-INVOICE_NUMBER         "v_invoice_number.

                                                      and  TRACKING_NO = t_efa_ups-TRCK_NUM.


         if t_efa_track is not initial.
*
          sort t_efa_track by TRACKING_NO.
*       IF t_efa_ups IS NOT INITIAL.

          select
                    DATE_ADDED
                    VBELN
                    PLANT
                    TRACKING_NUMBER
                    CARRIER_CODE
                    FREIGHTAMT
                    DISCOUNTAMT
                    MASTERTRACKING
                    SALES_ORDER
                    PURCHASE_ORDER



                  from /PWEAVER/MANFEST into CORRESPONDING FIELDS OF table t_man for all entries in   t_efa_track
                                  where TRACKING_NUMBER = t_efa_track-TRACKING_NO.





***********************************************

*
*
*  LOOP AT T_MAN INTO W_MAN.
*
*
***    Read table t_carrierconfig into ls_carrierconfig with key lifnr = w_man-carrier_code plant = w_man-plant.
**
**    if sy-subrc = 0 .
**      concatenate  ls_carrierconfig-lifnr '-'  ls_carrierconfig-description into W_MAN-ship_method.
**    endif.
***
**
**    W_MAN-ship_method = W_MAN-ship_method.
*
*    MODIFY T_MAN FROM W_MAN  transporting ship_method.
*    clear w_man.
*
* ENDLOOP.
****************************************************
*           sort t_man by TRACKING_NUMBER.
 sort t_man by MASTERTRACKING.


           endif.
      endif.
*******************changes to in vs manifest report***********************
      clear : v_tot_bill,T_EFA_INVSMAN.
      refresh T_EFA_INVSMAN.
       loop at t_efa_track into w_efa_track.
         W_EFA_INVSMAN-TRACK_NO = w_efa_track-TRACKING_NO.

*
          loop at t_efa_ups into w_efa_ups

                         where TRCK_NUM = w_efa_track-TRACKING_NO
                          and  INVOICE_NUMBER = w_efa_track-INVOICE_NO
                                   and INVOICE_DATE = w_efa_track-INVOICE_DATE.


               v_tot_bill = v_tot_bill + w_efa_ups-NET_AMNT.


              clear w_efa_ups.
          endloop.
          W_EFA_INVSMAN-FRIGHT = v_tot_bill.
          CLEAR :  V_TOT_BILL,
                   W_MAN.
          read table t_man into w_man with key TRACKING_NUMBER = w_efa_track-TRACKING_NO.
          W_EFA_INVSMAN-MASTER_TRACK = W_MAN-MASTERTRACKING.
          APPEND W_EFA_INVSMAN TO T_EFA_INVSMAN.
          clear : w_efa_invsman.

          endloop.
          SORT T_MAN BY MASTERTRACKING.
          delete adjacent duplicates from t_man comparing mastertracking.
          clear : v_tot_bill.
*             loop at t_efa_invsman into w_efa_invsman.
*
*
*
*
*
*
*               v_tot_bill = v_tot_bill + w_efa_invsman-fright.
*
*
*              clear w_efa_invsman.
*          endloop.
          loop at t_man into w_man.
                  loop at t_efa_invsman into w_efa_invsman

                         where master_track = w_man-MASTERTRACKING.




               v_tot_bill = v_tot_bill + w_efa_invsman-fright.


              clear w_efa_invsman.
          endloop.
 READ TABLE t_efa_ups INTO w_efa_ups with key TRCK_NUM = w_man-TRACKING_NUMBER CHRG_CLASS_CODE = 'FRT'.
              if sy-subrc = 0.

*
*          w_efa_bill-line_color      =  'C600'.
         else.

         w_efa_bill-line_color      =  'C600'.

       ENDIF.
 READ TABLE t_efa_track INTO w_efa_track with key TRACKING_NO = w_man-TRACKING_NUMBER .
      w_efa_bill-inv_no       =  w_efa_track-INVOICE_NO.

           w_efa_bill-inv_date     = w_efa_track-INVOICE_DATE.
           if  w_efa_ups-TRANSACTION_DATE is initial.

    w_efa_bill-SHIP_DATE = 'inv mesllaneous'.
      w_efa_bill-SHIP_DATE = w_efa_ups-TRANSACTION_DATE.
  else.


data: day(2) type c,
      month(2) type c,
      year(4) type c,
      w_efa_ups-TRANSACTION_DATE1 type char10.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
    INPUT         =  w_efa_ups-TRANSACTION_DATE
 IMPORTING
  OUTPUT        =  w_efa_ups-TRANSACTION_DATE.


day =  w_efa_ups-TRANSACTION_DATE+6(2).
month = w_efa_ups-TRANSACTION_DATE+4(2).
year = w_efa_ups-TRANSACTION_DATE+0(4).


clear w_efa_ups-TRANSACTION_DATE.
concatenate day '.'month'.'year into w_efa_ups-TRANSACTION_DATE1.
  w_efa_bill-SHIP_DATE = w_efa_ups-TRANSACTION_DATE1.

endif.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = W_MAN-VBELN
      IMPORTING
        output = W_MAN-VBELN.
    W_EFA_BILL-VBELN = W_MAN-VBELN.
    W_EFA_BILL-PURCHASE_ORDER = W_MAN-PURCHASE_ORDER.
    w_efa_bill-SALES_ORDER = w_man-SALES_ORDER.
*TRANSLATE  w_efa_ups-CHRG_DESC TO LOWER CASE.
 w_efa_bill-SHIP_METHOD =    w_efa_ups-CHRG_DESC  .     "  W_MAN-ship_method .

           w_efa_bill-track_no     = w_efa_track-TRACKING_NO .
                   lv_bill_disc        = w_man-DISCOUNTAMT.


           w_efa_bill-v_discount   =  lv_bill_disc.
           w_efa_bill-v_bill_efa   = v_tot_bill.

           lv_bill_man             = w_man-FREIGHTAMT.

           w_efa_bill-v_bill_man   = lv_bill_man.   "w_man-FREIGHTAMT.
            lv_bill_efa             = v_tot_bill.
*            w_efa_bill-v_bill_diff  = w_efa_bill-v_discount -  w_efa_bill-v_bill_efa .
              w_efa_bill-v_bill_diff  = w_efa_bill-v_bill_efa - w_efa_bill-v_discount.
*             w_efa_bill-v_bill_diff  =    lv_bill_efa - lv_bill_man. "difference between CARRIERS BILLING WHCIH IS UPLOADED AND MANIFEST FREIGHT AMT
*            if lv_bill_efa ne lv_bill_man.
                w_efa_bill-line_color      =  'C600'.
           append  w_efa_bill to t_efa_bill.
*           endif.
            clear :w_efa_bill,v_tot_bill, w_man,
                 lv_bill_efa, lv_bill_man,w_efa_ups ,lv_bill_disc,w_efa_track .


          endloop.
*          loop at t_efa_invsman  into w_efa_invsman.
*
*          endloop.



*************************end of change************************************
***************************COMMENTED******************************************


*
*      loop at t_efa_track into w_efa_track.
**
*          loop at t_efa_ups into w_efa_ups
*
*                         where TRCK_NUM = w_efa_track-TRACKING_NO
*                          and  INVOICE_NUMBER = w_efa_track-INVOICE_NO
*                                   and INVOICE_DATE = w_efa_track-INVOICE_DATE.
*
*
*               v_tot_bill = v_tot_bill + w_efa_ups-NET_AMNT.
*
*
*              clear w_efa_ups.
*          endloop.
*
*
*
*
*       READ TABLE t_efa_ups INTO w_efa_ups with key TRCK_NUM = w_efa_track-TRACKING_NO CHRG_CLASS_CODE = 'FRT'.
*
*         read table t_man into w_man with key TRACKING_NUMBER = w_efa_track-TRACKING_NO.
*
*       if sy-subrc = 0.
*
**
**          w_efa_bill-line_color      =  'C600'.
*         else.
*
*         w_efa_bill-line_color      =  'C600'.
*
*       ENDIF.
*       data:  v_inv type /pweaver/efa_ups.
*
*           w_efa_bill-inv_no       =  w_efa_track-INVOICE_NO.
*
*           w_efa_bill-inv_date     = w_efa_track-INVOICE_DATE.
*
**           w_efa_bill-ship_inv_date = w_efa_track-TRANSACTION_DATE.
*
*
**IF w_efa_ups-TRANSACTION_DATE is not INITIAL.
*
**data: day(2) type c,
**      month(2) type c,
**      year(4) type c.
**
**
**
**
**
**concatenate sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) into  w_efa_bill-SHIP_DATE separated by '.'  .
*
*
*
*if  w_efa_ups-TRANSACTION_DATE is initial.
*
*    w_efa_bill-SHIP_DATE = 'inv mesllaneous'.
*      w_efa_bill-SHIP_DATE = w_efa_ups-TRANSACTION_DATE.
*  else.
*
*
**data: day(2) type c,
**      month(2) type c,
**      year(4) type c,
**  data :     w_efa_ups-TRANSACTION_DATE1 type char10.
*
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*  EXPORTING
*    INPUT         =  w_efa_ups-TRANSACTION_DATE
* IMPORTING
*  OUTPUT        =  w_efa_ups-TRANSACTION_DATE.
*
*
*day =  w_efa_ups-TRANSACTION_DATE+6(2).
*month = w_efa_ups-TRANSACTION_DATE+4(2).
*year = w_efa_ups-TRANSACTION_DATE+0(4).
*
*
*clear w_efa_ups-TRANSACTION_DATE.
*concatenate day '.'month'.'year into w_efa_ups-TRANSACTION_DATE1.
*  w_efa_bill-SHIP_DATE = w_efa_ups-TRANSACTION_DATE1.
*
*endif.
*
*
***      endif.
** IF w_efa_ups-TRANSACTION_DATE is INITIAL.
**        w_efa_ups-TRANSACTION_DATE = 'Miscellaneous'.
**        v_inv = w_efa_ups.
**        CONTINUE.
**        endif.
*
*
**w_efa_bill-SHIP_DATE =  w_efa_ups-TRANSACTION_DATE.
*
*
**          w_efa_bill-SHIP_DATE    =  w_efa_ups-TRANSACTION_DATE   . "W_MAN-DATE_ADDED  .
*
***
***          if w_efa_ups-TRANSACTION_DATE is initial.
***
***  endif.
*
*           w_efa_bill-SHIP_METHOD =    w_efa_ups-CHRG_DESC  .     "  W_MAN-ship_method .
*
*           w_efa_bill-track_no     = w_efa_track-TRACKING_NO .
*
*                lv_bill_disc        = w_man-DISCOUNTAMT.
*
*
*           w_efa_bill-v_discount   =  lv_bill_disc.     "w_man-DISCOUNTAMT.
*
*           w_efa_bill-v_bill_efa   = v_tot_bill.
*
*           lv_bill_man             = w_man-FREIGHTAMT.
*
*           w_efa_bill-v_bill_man   = lv_bill_man.   "w_man-FREIGHTAMT.
*
*           lv_bill_efa             = v_tot_bill.
*
*
*
*           w_efa_bill-v_bill_diff  =    lv_bill_efa - lv_bill_man. "difference between CARRIERS BILLING WHCIH IS UPLOADED AND MANIFEST FREIGHT AMT
*            if lv_bill_efa ne lv_bill_man.
*           append  w_efa_bill to t_efa_bill.
*           endif.
*
*           clear :w_efa_bill,v_tot_bill, w_man,
*                 lv_bill_efa, lv_bill_man,w_efa_ups ,lv_bill_disc .
**        endif.
*
**      endif.
*
*    endloop.
*************************************END OF COMMENTED.*************************




     loop at t_efa_track into w_efa_track.

          if t_efa_ups1 is not initial.

             delete t_efa_ups1 where TRCK_NUM = w_efa_track-TRACKING_NO.

          endif.
     endloop.


     if t_efa_ups1 is not initial.

        loop at t_efa_ups1 into w_efa_ups1 WHERE  CHRG_CATG_DET_CD NE 'FC' AND CHRG_CATG_DET_CD NE 'MISC' AND CHRG_CATG_DET_CD NE 'SCC' AND CHRG_CATG_DET_CD NE 'SVCH'  .

             v_tot_bill  = v_tot_bill + w_efa_ups1-NET_AMNT.
             clear w_efa_ups1.
        endloop.



     w_efa_bill-v_bill_efa   = v_tot_bill.
*   w_efa_bill-SHIP_METHOD    = 'MIS'.
     if w_efa_bill-SHIP_DATE is initial.
            w_efa_bill-SHIP_DATE = 'inv misllaneous'.
    replace all occurrences of '.' in    w_efa_bill-SHIP_DATE with ' '.
    condense  w_efa_bill-SHIP_DATE.
 endif.
 w_efa_bill-v_bill_diff  = w_efa_bill-v_bill_efa - w_efa_bill-v_discount.

     w_efa_bill-line_color      =  'C111'.

     append w_efa_bill to t_efa_bill.



 clear :  w_efa_bill , v_tot_bill.
     endif.


*    CLEAR v_invoice_number.

    SORT T_EFA_BILL BY SHIP_DATE track_no.
*
*w_fieldcat-col_pos = '1'.
*w_fieldcat-fieldname = 'INV_NO'.
*w_fieldcat-tabname = 'T_EFA_BILL'.
*w_fieldcat-SCRTEXT_L = 'INVOICE NUMBER'.
*
*APPEND w_fieldcat to T_fieldcat.
*
*w_fieldcat-col_pos = '2'.
*w_fieldcat-fieldname = 'INV_DATE'.
*w_fieldcat-tabname = 'T_EFA_BILL'.
*w_fieldcat-SCRTEXT_L = 'INVOICE DATE'.


*APPEND w_fieldcat to T_fieldcat.


*
*w_fieldcat-col_pos = '1'.
*w_fieldcat-fieldname = 'SHIP_INV_DATE'. "ship_inv_date
*w_fieldcat-tabname = 'T_EFA_BILL'.
*w_fieldcat-SCRTEXT_L = 'SHIP DATE '.
*
*APPEND w_fieldcat to T_fieldcat.

w_fieldcat-col_pos = '1'.
w_fieldcat-fieldname = 'SHIP_DATE'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'SHIP DATE'.
w_fieldcat-OUTPUTLEN = '20'.

APPEND w_fieldcat to T_fieldcat.


w_fieldcat-col_pos = '2'.
w_fieldcat-fieldname = 'SHIP_METHOD'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'SHIP METHOD'.
w_fieldcat-LOWERCASE = 'X'.
w_fieldcat-OUTPUTLEN = '40'.


APPEND w_fieldcat to T_fieldcat.

w_fieldcat-col_pos = '3'.
w_fieldcat-fieldname = 'TRACK_NO'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'TRACKING NUMBER'.
w_fieldcat-OUTPUTLEN = '20'.

*w_fieldcat-hotspot      = 'X'.

APPEND w_fieldcat to T_fieldcat.

w_fieldcat-col_pos = '4'.
w_fieldcat-fieldname = 'VBELN'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'DELIVERY NUMBER'.
w_fieldcat-OUTPUTLEN = '20'.

*w_fieldcat-hotspot      = 'X'.

APPEND w_fieldcat to T_fieldcat.
w_fieldcat-col_pos = '5'.
w_fieldcat-fieldname = 'PURCHASE_ORDER'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'REFERENCE1'.
w_fieldcat-OUTPUTLEN = '20'.

APPEND w_fieldcat to T_fieldcat.
w_fieldcat-col_pos = '6'.
w_fieldcat-fieldname = 'SALES_ORDER'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'REFERENCE2'.
w_fieldcat-OUTPUTLEN = '20'.

*w_fieldcat-hotspot      = 'X'.

APPEND w_fieldcat to T_fieldcat.

w_fieldcat-col_pos = '7'.
w_fieldcat-fieldname = 'V_DISCOUNT'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'DISCOUNT AMOUNT'.

w_fieldcat-do_sum = 'X'.
APPEND w_fieldcat to T_fieldcat.

w_fieldcat-col_pos = '8'.
w_fieldcat-fieldname = 'V_BILL_EFA'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'BILLED CHARGES'.
w_fieldcat-do_sum = 'X'.
APPEND w_fieldcat to T_fieldcat.

w_fieldcat-col_pos = '9'.
w_fieldcat-fieldname = 'V_BILL_MAN'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'FREIGHT CHARGES FROM MANIFEST'.
w_fieldcat-do_sum = 'X'.
APPEND w_fieldcat to T_fieldcat.

w_fieldcat-col_pos = '10'.
w_fieldcat-fieldname = 'V_BILL_DIFF'.
w_fieldcat-tabname = 'T_EFA_BILL'.
w_fieldcat-SCRTEXT_L = 'DIFFERENCE IN CHARGES'.
w_fieldcat-do_sum = 'X'.
APPEND w_fieldcat to T_fieldcat.




w_layout-zebra = 'X'.
w_layout-cwidth_opt = 'X'.
w_layout-info_fname = 'LINE_COLOR'.
w_layout-grid_title = 'Comparing INVOICE vs MANIFEST Details'.
*  gs_layout-cwidth_opt = 'X'.
*  gs_layout-info_fname = 'LINE_COLOR'.
*  gs_layout-grid_title = 'By Shipment Type'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
CALL METHOD go_grid->set_table_for_first_display
  EXPORTING
    I_BUFFER_ACTIVE               = 'X'
*    I_BYPASSING_BUFFER            =
*    I_CONSISTENCY_CHECK           =
    I_STRUCTURE_NAME              =  'T_EFA_BILL'
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
**    IS_LAYOUT                     = w_layout
 is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
*    IS_PRINT                      =
*    IT_SPECIAL_GROUPS             =
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
*    IT_EXCEPT_QINFO               =
  CHANGING
    it_outtab                     = T_EFA_BILL
    IT_FIELDCATALOG               = T_fieldcat
*    IT_SORT                       =
*    IT_FILTER                     =
*  EXCEPTIONS
*    INVALID_PARAMETER_COMBINATION = 1
*    PROGRAM_ERROR                 = 2
*    TOO_MANY_LINES                = 3
*    others                        = 4
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

*
*  CALL METHOD go_grid->set_table_for_first_display
*    EXPORTING
*      i_structure_name              = 'T_EFA_BILL'
*      is_layout                     = w_layout
*    CHANGING
*      it_outtab                     = T_EFA_BILL
*      it_fieldcatalog               = T_fieldcat
**    IT_SORT                       =
*  EXCEPTIONS
*    invalid_parameter_combination = 1
*    program_error                 = 2
*    too_many_lines                = 3
*    OTHERS                        = 4  .
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.



endform.                    " MANIFEST_FREIGHT


*&---------------------------------------------------------------------*
*&      Form  Manifest_EFA_Fedex
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM Manifest_EFA_Fedex .

     clear w_efa_f.
     refresh t_man_fed.

     LOOP at t_efa_f into w_efa_f.


        w_efa_fed-ship_date    = w_efa_f-shipment_date.

        w_efa_fed-ship_method  = w_efa_f-service_type.
        w_efa_fed-track_num    = w_efa_f-exp_grd_trck_id.
        w_efa_fed-transp_chg   = w_efa_f-tran_charg_amnt.
        w_efa_fed-efa_net_chg  = w_efa_f-net_chrg_amnt.



****Handling charges
        if w_efa_f-trk_id_chg_des = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des = 'Declared Value' or
      w_efa_f-trk_id_chg_des = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des = 'Address Correction' or
      w_efa_f-trk_id_chg_des = 'Residential' or
      w_efa_f-trk_id_chg_des = 'DAS Comm' or
      w_efa_f-trk_id_chg_des = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des =  'Direct Signature' or
      w_efa_f-trk_id_chg_des = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des = 'Call Tag'.    .

          w_efa_fed-handl_chg =  w_efa_f-trk_id_chg_amt.
        endif.

        if w_efa_f-trk_id_chg_des1 = 'Fuel Surcharge' or
      w_efa_f-trk_id_chg_des1 = 'Declared Value' or
      w_efa_f-trk_id_chg_des1 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des1 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des1 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des1 = 'Address Correction' or
      w_efa_f-trk_id_chg_des1 = 'Residential' or
      w_efa_f-trk_id_chg_des1 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des1 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des1 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des1 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des1 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des1 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des1 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des1 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des1 = 'Call Tag'..    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt1.
        endif.

        if w_efa_f-trk_id_chg_des2 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des2 = 'Declared Value' or
      w_efa_f-trk_id_chg_des2 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des2 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des2 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des2 = 'Address Correction' or
      w_efa_f-trk_id_chg_des2 = 'Residential' or
      w_efa_f-trk_id_chg_des2 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des2 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des2 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des2 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des2 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des2 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des2 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des2 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des2 = 'Call Tag'..    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt2.
        endif.

        if w_efa_f-trk_id_chg_des3 = 'Fuel Surcharge' or
      w_efa_f-trk_id_chg_des3 = 'Declared Value' or
      w_efa_f-trk_id_chg_des3 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des3 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des3 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des3 = 'Address Correction' or
      w_efa_f-trk_id_chg_des3 = 'Residential' or
      w_efa_f-trk_id_chg_des3 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des3 = 'DAS Extended Comm' or
     w_efa_f-trk_id_chg_des3 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des3 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des3 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des3 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des3 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des3 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des3 = 'Call Tag'.    .

          .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt3.
        endif.

        if w_efa_f-trk_id_chg_des4 = 'Fuel Surcharge' or
    w_efa_f-trk_id_chg_des4 = 'Declared Value' or
      w_efa_f-trk_id_chg_des4 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des4 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des4 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des4 = 'Address Correction' or
      w_efa_f-trk_id_chg_des4 = 'Residential' or
      wa_serv_type-trk_id_chg_des4 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des4 = 'DAS Extended Comm' or
        w_efa_f-trk_id_chg_des4 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des4 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des4 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des4 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des4 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des4 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des4 = 'Call Tag'.    .
   .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt4.
        endif.

        if w_efa_f-trk_id_chg_des5 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des5 = 'Declared Value' or
      w_efa_f-trk_id_chg_des5 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des5 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des5 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des5 = 'Address Correction' or
      w_efa_f-trk_id_chg_des5 = 'Residential' or
      w_efa_f-trk_id_chg_des5 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des5 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des5 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des5 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des5 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des5 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des5 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des5 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des5 = 'Call Tag'.    .
    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt5.
        endif.

        if w_efa_f-trk_id_chg_des6 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des6 = 'Declared Value' or
      w_efa_f-trk_id_chg_des6 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des6 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des6 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des6 = 'Address Correction' or
      w_efa_f-trk_id_chg_des6 = 'Residential' or
      w_efa_f-trk_id_chg_des6 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des6 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des6 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des6 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des6 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des6 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des6 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des6 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des6 = 'Call Tag'.       .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt6.
        endif.

        if w_efa_f-trk_id_chg_des7 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des7 = 'Declared Value' or
      w_efa_f-trk_id_chg_des7 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des7 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des7 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des7 = 'Address Correction' or
      w_efa_f-trk_id_chg_des7 = 'Residential' or
      w_efa_f-trk_id_chg_des7 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des7 = 'DAS Extended Comm' or
       w_efa_f-trk_id_chg_des7 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des7 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des7 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des7 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des7 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des7 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des7 = 'Call Tag'.    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt7.
        endif.

        if w_efa_f-trk_id_chg_des8 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des8 = 'Declared Value' or
      w_efa_f-trk_id_chg_des8 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des8 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des8 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des8 = 'Address Correction' or
      w_efa_f-trk_id_chg_des8 = 'Residential' or
      w_efa_f-trk_id_chg_des8 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des8 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des8 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des8 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des8 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des8 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des8 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des8 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des8 = 'Call Tag'.    .    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt8.
        endif.


        if w_efa_f-trk_id_chg_des9 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des9 = 'Declared Value' or
      w_efa_f-trk_id_chg_des9 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des9 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des9 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des9 = 'Address Correction' or
      w_efa_f-trk_id_chg_des9 = 'Residential' or
      w_efa_f-trk_id_chg_des9 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des9 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des9 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des9 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des9 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des9 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des9 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des9 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des9 = 'Call Tag'.    .    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt9.
        endif.


        if w_efa_f-trk_id_chg_des10 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des10 = 'Declared Value' or
      w_efa_f-trk_id_chg_des10 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des10 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des10 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des10 = 'Address Correction' or
      w_efa_f-trk_id_chg_des10 = 'Residential' or
      w_efa_f-trk_id_chg_des10 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des10 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des10 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des10 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des10 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des10 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des10 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des10 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des10 = 'Call Tag'.    .
    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt10.
        endif.


        if w_efa_f-trk_id_chg_des11 = 'Fuel Surcharge' or
     w_efa_f-trk_id_chg_des11 = 'Declared Value' or
      w_efa_f-trk_id_chg_des11 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des11 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des11 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des11 = 'Address Correction' or
      w_efa_f-trk_id_chg_des11 = 'Residential' or
      w_efa_f-trk_id_chg_des11 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des11 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des11 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des11 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des11 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des11 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des11 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des11 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des11 = 'Call Tag'..    .
    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt11.
        endif.


        if w_efa_f-trk_id_chg_des12 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des12 = 'Declared Value' or
      w_efa_f-trk_id_chg_des12 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des12 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des12 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des12 = 'Address Correction' or
      w_efa_f-trk_id_chg_des12 = 'Residential' or
      w_efa_f-trk_id_chg_des12 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des12 = 'DAS Extended Comm' or
       w_efa_f-trk_id_chg_des12 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des12 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des12 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des12 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des12 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des12 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des12 = 'Call Tag'..    .
   .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt12.
        endif.


        if w_efa_f-trk_id_chg_des13 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des13 = 'Declared Value' or
      w_efa_f-trk_id_chg_des13 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des13 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des13 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des13 = 'Address Correction' or
      w_efa_f-trk_id_chg_des13 = 'Residential' or
      w_efa_f-trk_id_chg_des13 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des13 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des13 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des13 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des13 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des13 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des13 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des13 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des13 = 'Call Tag'..    .
    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt13.
        endif.


        if w_efa_f-trk_id_chg_des14 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des14 = 'Declared Value' or
      w_efa_f-trk_id_chg_des14 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des14 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des14 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des14 = 'Address Correction' or
      w_efa_f-trk_id_chg_des14 = 'Residential' or
      w_efa_f-trk_id_chg_des14 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des14 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des14 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des14 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des14 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des14 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des14 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des14 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des14 = 'Call Tag'..    .
    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt14.
        endif.


        if w_efa_f-trk_id_chg_des15 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des15 = 'Declared Value' or
      w_efa_f-trk_id_chg_des15 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des15 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des15 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des15 = 'Address Correction' or
      w_efa_f-trk_id_chg_des15 = 'Residential' or
      w_efa_f-trk_id_chg_des15 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des15 = 'DAS Extended Comm' or
       w_efa_f-trk_id_chg_des15 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des15 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des15 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des15 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des15 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des15 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des15 = 'Call Tag'..    .   .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt15.
        endif.


        if w_efa_f-trk_id_chg_des16 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des16 = 'Declared Value' or
      w_efa_f-trk_id_chg_des16 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des16 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des16 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des16 = 'Address Correction' or
      w_efa_f-trk_id_chg_des16 = 'Residential' or
      w_efa_f-trk_id_chg_des16 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des16 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des16 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des16 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des16 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des16 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des16 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des16 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des16 = 'Call Tag'..    .    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt16.
        endif.


        if w_efa_f-trk_id_chg_des17 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des17 = 'Declared Value' or
      w_efa_f-trk_id_chg_des17 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des17 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des17 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des17 = 'Address Correction' or
      w_efa_f-trk_id_chg_des17 = 'Residential' or
      w_efa_f-trk_id_chg_des17 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des17 = 'DAS Extended Comm'or
      w_efa_f-trk_id_chg_des17 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des17 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des17 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des17 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des17 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des17 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des17 = 'Call Tag'..    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt17.
        endif.


        if w_efa_f-trk_id_chg_des18 = 'Fuel Surcharge' or
      w_efa_f-trk_id_chg_des18 = 'Declared Value' or
      w_efa_f-trk_id_chg_des18 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des18 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des18 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des18 = 'Address Correction' or
      w_efa_f-trk_id_chg_des18 = 'Residential' or
      w_efa_f-trk_id_chg_des18 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des18 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des18 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des18 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des18 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des18 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des18 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des18 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des18 = 'Call Tag'..    .    .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt18.
        endif.


        if w_efa_f-trk_id_chg_des19 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des19 = 'Declared Value' or
w_efa_f-trk_id_chg_des19 = 'Weekly Service Chg' or
w_efa_f-trk_id_chg_des19 = 'Weekday Delivery' or
w_efa_f-trk_id_chg_des19 = 'Residential Delivery' or
w_efa_f-trk_id_chg_des19 = 'Address Correction' or
w_efa_f-trk_id_chg_des19 = 'Residential' or
w_efa_f-trk_id_chg_des19 = 'DAS Comm' or
w_efa_f-trk_id_chg_des19 = 'DAS Extended Comm' or
 w_efa_f-trk_id_chg_des19 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des19 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des19 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des19 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des19 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des19 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des19 = 'Call Tag'..               .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt19.
        endif.

        if w_efa_f-trk_id_chg_des20 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des20 = 'Declared Value' or
w_efa_f-trk_id_chg_des20 = 'Weekly Service Chg' or
w_efa_f-trk_id_chg_des20 = 'Weekday Delivery' or
w_efa_f-trk_id_chg_des20 = 'Residential Delivery' or
w_efa_f-trk_id_chg_des20 = 'Address Correction' or
w_efa_f-trk_id_chg_des20 = 'Residential' or
w_efa_f-trk_id_chg_des20 = 'DAS Comm' or
w_efa_f-trk_id_chg_des20 = 'DAS Extended Comm' or
w_efa_f-trk_id_chg_des20 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des20 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des20 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des20 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des20 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des20 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des20 = 'Call Tag'..          .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt20.
        endif.

        if w_efa_f-trk_id_chg_des21 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des21 = 'Declared Value' or
w_efa_f-trk_id_chg_des21 = 'Weekly Service Chg' or
w_efa_f-trk_id_chg_des21 = 'Weekday Delivery' or
w_efa_f-trk_id_chg_des21 = 'Residential Delivery' or
w_efa_f-trk_id_chg_des21 = 'Address Correction' or
w_efa_f-trk_id_chg_des21 = 'Residential' or
w_efa_f-trk_id_chg_des21 = 'DAS Comm' or
w_efa_f-trk_id_chg_des21 = 'DAS Extended Comm' or
 w_efa_f-trk_id_chg_des21 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des21 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des21 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des21 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des21 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des21 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des21 = 'Call Tag'..         .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt21.
        endif.

        if w_efa_f-trk_id_chg_des22 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des22 = 'Declared Value' or
      w_efa_f-trk_id_chg_des22 = 'Weekly Service Chg' or
      w_efa_f-trk_id_chg_des22 = 'Weekday Delivery' or
      w_efa_f-trk_id_chg_des22 = 'Residential Delivery' or
      w_efa_f-trk_id_chg_des22 = 'Address Correction' or
      w_efa_f-trk_id_chg_des22 = 'Residential' or
      w_efa_f-trk_id_chg_des22 = 'DAS Comm' or
      w_efa_f-trk_id_chg_des22 = 'DAS Extended Comm' or
      w_efa_f-trk_id_chg_des22 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des22 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des22 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des22 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des22 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des22 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des22 = 'Call Tag'..          .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt22.
        endif.

        if w_efa_f-trk_id_chg_des23 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des23 = 'Declared Value' or
w_efa_f-trk_id_chg_des23 = 'Weekly Service Chg' or
w_efa_f-trk_id_chg_des23 = 'Weekday Delivery' or
w_efa_f-trk_id_chg_des23 = 'Residential Delivery' or
w_efa_f-trk_id_chg_des23 = 'Address Correction' or
w_efa_f-trk_id_chg_des23 = 'Residential' or
w_efa_f-trk_id_chg_des23 = 'DAS Comm' or
w_efa_f-trk_id_chg_des23 = 'DAS Extended Comm' or
 w_efa_f-trk_id_chg_des23 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des23 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des23 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des23 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des23 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des23 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des23 = 'Call Tag'..         .

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt23.
        endif.

        if w_efa_f-trk_id_chg_des24 = 'Fuel Surcharge' or
          w_efa_f-trk_id_chg_des24 = 'Declared Value' or
w_efa_f-trk_id_chg_des24 = 'Weekly Service Chg' or
w_efa_f-trk_id_chg_des24 = 'Weekday Delivery' or
w_efa_f-trk_id_chg_des24 = 'Residential Delivery' or
w_efa_f-trk_id_chg_des24 = 'Address Correction' or
w_efa_f-trk_id_chg_des24 = 'Residential' or
w_efa_f-trk_id_chg_des24 = 'DAS Comm' or
w_efa_f-trk_id_chg_des24 = 'DAS Extended Comm' or
 w_efa_f-trk_id_chg_des24 = 'DAS Extended Resi' or
      w_efa_f-trk_id_chg_des24 = 'DAS Resi'          or
      w_efa_f-trk_id_chg_des24 =  'Direct Signature' or
      w_efa_f-trk_id_chg_des24 = 'DAS Hawaii Comm' or
      w_efa_f-trk_id_chg_des24 = 'AHS - Dimensions' or
      w_efa_f-trk_id_chg_des24 = 'AHS - Weight' or
      w_efa_f-trk_id_chg_des24 = 'Call Tag'.

          w_efa_fed-handl_chg =  w_efa_fed-handl_chg + w_efa_f-trk_id_chg_amt24.
        endif.







***************Earned Discount
        if w_efa_f-trk_id_chg_des = 'Earned Discount' or w_efa_f-trk_id_chg_des = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_f-trk_id_chg_amt.
        endif.

        if w_efa_f-trk_id_chg_des1 = 'Earned Discount'or w_efa_f-trk_id_chg_des1 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt1.
        endif.

        if w_efa_f-trk_id_chg_des2 = 'Earned Discount'or w_efa_f-trk_id_chg_des2 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt2.
        endif.

        if w_efa_f-trk_id_chg_des3 = 'Earned Discount' or w_efa_f-trk_id_chg_des3 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt3.
        endif.

        if w_efa_f-trk_id_chg_des4 = 'Earned Discount' or  w_efa_f-trk_id_chg_des4 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt4.
        endif.

        if w_efa_f-trk_id_chg_des5 = 'Earned Discount' or w_efa_f-trk_id_chg_des5 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt5.
        endif.

        if w_efa_f-trk_id_chg_des6 = 'Earned Discount' or w_efa_f-trk_id_chg_des6 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt6.
        endif.

        if w_efa_f-trk_id_chg_des7 = 'Earned Discount' or w_efa_f-trk_id_chg_des = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt7.
        endif.

        if w_efa_f-trk_id_chg_des8 = 'Earned Discount' or w_efa_f-trk_id_chg_des8 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt8.
        endif.

        if w_efa_f-trk_id_chg_des9 = 'Earned Discount' or  w_efa_f-trk_id_chg_des9 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt9.
        endif.


        if w_efa_f-trk_id_chg_des10 = 'Earned Discount' or w_efa_f-trk_id_chg_des10 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt10.
        endif.

        if w_efa_f-trk_id_chg_des11 = 'Earned Discount' or w_efa_f-trk_id_chg_des11 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt11.
        endif.

        if w_efa_f-trk_id_chg_des12 = 'Earned Discount' or w_efa_f-trk_id_chg_des12 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt12.
        endif.

        if w_efa_f-trk_id_chg_des13 = 'Earned Discount' or  w_efa_f-trk_id_chg_des13 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt13.
        endif.


        if w_efa_f-trk_id_chg_des14 = 'Earned Discount' or w_efa_f-trk_id_chg_des14 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt14.
        endif.

        if w_efa_f-trk_id_chg_des15 = 'Earned Discount' or w_efa_f-trk_id_chg_des15 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt15.
        endif.


        if w_efa_f-trk_id_chg_des16 = 'Earned Discount' or w_efa_f-trk_id_chg_des16 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt16.
        endif.

        if w_efa_f-trk_id_chg_des17 = 'Earned Discount' or  w_efa_f-trk_id_chg_des17 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt17.
        endif.

        if w_efa_f-trk_id_chg_des18 = 'Earned Discount' or  w_efa_f-trk_id_chg_des18 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt18.
        endif.

        if w_efa_f-trk_id_chg_des19 = 'Earned Discount' or w_efa_f-trk_id_chg_des19 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt19.
        endif.

        if w_efa_f-trk_id_chg_des20 = 'Earned Discount' or  w_efa_f-trk_id_chg_des20 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt20.
        endif.

        if w_efa_f-trk_id_chg_des21 = 'Earned Discount' or w_efa_f-trk_id_chg_des21 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt21.
        endif.

        if w_efa_f-trk_id_chg_des22 = 'Earned Discount' or w_efa_f-trk_id_chg_des22 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt22.
        endif.

        if w_efa_f-trk_id_chg_des23 = 'Earned Discount' or w_efa_f-trk_id_chg_des23 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt + w_efa_f-trk_id_chg_amt23.
        endif.

        if w_efa_f-trk_id_chg_des24 = 'Earned Discount' or  w_efa_f-trk_id_chg_des24 = 'Performance Pricing'.
         w_efa_fed-disc_amt =  w_efa_fed-disc_amt  + w_efa_f-trk_id_chg_amt24.
        endif.

      append w_efa_fed to t_efa_fed.


    clear : w_efa_fed , w_efa_f.


     ENDLOOP.

sort t_efa_fed by track_num.

   if t_efa_fed is not initial.

     select TRACKING_NUMBER
             FREIGHTAMT
            DISCOUNTAMT
            VBELN
            PURCHASE_ORDER
           from /PWEAVER/MANFEST into table t_man_fed for all entries in t_efa_fed WHERE
                                                                          TRACKING_NUMBER = t_efa_fed-track_num.
  endif.
data lv_ind type sy-tabix.
clear lv_ind.
Loop at t_efa_fed into w_efa_fed.
lv_ind = sy-tabix.
     Read table t_man_fed into w_man_fed with key trk_num = w_efa_fed-track_num.

           if sy-subrc = 0.
             w_efa_fed-FREIGHTAMT   = w_man_fed-FREIGHTAMT.
               CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = W_MAN_FED-VBELN
      IMPORTING
        output = W_MAN_FED-VBELN.
             W_EFA_FED-VBELN        = W_MAN_FED-VBELN.
             W_EFA_FED-PURCHASE_ORDER   = W_MAN_FED-PURCHASE_ORDER.


           else.
             w_efa_fed-line_color  = 'C600'.
            endif.

           w_efa_fed-DIFF   = w_efa_fed-efa_net_chg -  w_efa_fed-FREIGHTAMT.

    modify t_efa_fed INDEX lv_ind from w_efa_fed." TRANSPORTING FREIGHTAMT DIFF line_color." where  track_num =   w_efa_fed-track_num.

    clear : w_efa_fed , w_man_fed.

Endloop.


refresh t_fieldcat.
clear w_fieldcat.


      w_fieldcat-col_pos      = 1.
          w_fieldcat-fieldname    = 'SHIP_DATE'.
          w_fieldcat-coltext    = 'Shipment Date'.
          w_fieldcat-tabname      = 'T_EFA_FED'.
          w_fieldcat-OUTPUTLEN = '20'.
      append     w_fieldcat to  t_fieldcat.
      clear     w_fieldcat.

          w_fieldcat-col_pos      = 2.
          w_fieldcat-fieldname    = 'SHIP_METHOD'.
          w_fieldcat-coltext    = 'Service Type'.
          w_fieldcat-tabname      = 'T_EFA_FED'.
          w_fieldcat-OUTPUTLEN = '40'.
          w_fieldcat-lowercase = 'X'.
      append     w_fieldcat to  t_fieldcat.
      clear     w_fieldcat.

      w_fieldcat-col_pos      = 3.
      w_fieldcat-fieldname    = 'TRACK_NUM'.
      w_fieldcat-coltext    = 'Tracking Number'.
      w_fieldcat-tabname      = 'T_EFA_FED'.
      w_fieldcat-OUTPUTLEN = '20'.
*      w_fieldcat-hotspot   = 'x'.
      append w_fieldcat to t_fieldcat.
      clear w_fieldcat.
         w_fieldcat-col_pos      = 4.
      w_fieldcat-fieldname    = 'VBELN'.
      w_fieldcat-coltext    = 'Delivery Number'.
      w_fieldcat-tabname      = 'T_EFA_FED'.
      w_fieldcat-OUTPUTLEN = '20'.
*      w_fieldcat-hotspot   = 'x'.
      append w_fieldcat to t_fieldcat.
      clear w_fieldcat.
         w_fieldcat-col_pos      = 5.
      w_fieldcat-fieldname    = 'PURCHASE_ORDER'.
      w_fieldcat-coltext    = 'Reference'.
      w_fieldcat-tabname      = 'T_EFA_FED'.
      w_fieldcat-OUTPUTLEN = '20'.
*      w_fieldcat-hotspot   = 'x'.
      append w_fieldcat to t_fieldcat.
      clear w_fieldcat.

      w_fieldcat-col_pos      = 6.
      w_fieldcat-fieldname    = 'TRANSP_CHG'.
      w_fieldcat-coltext    = 'Transportation Charges'.
      w_fieldcat-tabname      = 'T_EFA_FED'.
      w_fieldcat-DO_SUM = 'X'.
      append w_fieldcat to T_fieldcat.
      clear w_fieldcat.

      w_fieldcat-col_pos      = 7.
      w_fieldcat-fieldname    = 'HANDL_CHG'.
      w_fieldcat-coltext    = 'Handling Charges'.
      w_fieldcat-tabname      = 'T_EFA_FED'.
           w_fieldcat-DO_SUM = 'X'.
                 append w_fieldcat to T_fieldcat.
      clear w_fieldcat.

 w_fieldcat-col_pos      = 8.
      w_fieldcat-fieldname    = 'DISC_AMT'.
      w_fieldcat-coltext    = 'Discount Amount'.
      w_fieldcat-tabname      = 'T_EFA_FED'.

    w_fieldcat-DO_SUM = 'X'.
      append w_fieldcat to t_fieldcat.
      clear w_fieldcat.

      w_fieldcat-col_pos      = 9.
      w_fieldcat-fieldname    = 'EFA_NET_CHG'.
      w_fieldcat-coltext    = 'Net Charegs'.
      w_fieldcat-tabname      = 'T_EFA_FED'.
    w_fieldcat-DO_SUM = 'X'.
      append w_fieldcat to t_fieldcat.
      clear w_fieldcat.

      w_fieldcat-col_pos      = 10.
      w_fieldcat-fieldname    = 'FREIGHTAMT'.
      w_fieldcat-coltext    = 'Freight from Manifest'.
      w_fieldcat-tabname      = 'T_EFA_FED'.
      w_fieldcat-DO_SUM = 'X'.
      append w_fieldcat to t_fieldcat.
      clear w_fieldcat.

      w_fieldcat-col_pos      = 12.
      w_fieldcat-fieldname    = 'DIFF'.
      w_fieldcat-coltext    = 'Difeerence between two Charges'.
      w_fieldcat-tabname      = 'T_EFA_FED'.
      w_fieldcat-DO_SUM = 'X'.
      append w_fieldcat to t_fieldcat.
      clear w_fieldcat.

w_layout-zebra = 'X'.
w_layout-cwidth_opt = 'X'.
w_layout-info_fname = 'LINE_COLOR'.
w_layout-grid_title = 'Comparing INVOICE vs MANIFEST Details'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs


 CALL METHOD go_grid->set_table_for_first_display
  EXPORTING
    I_BUFFER_ACTIVE               = 'X'
*    I_BYPASSING_BUFFER            =
*    I_CONSISTENCY_CHECK           =
    I_STRUCTURE_NAME              =  'T_EFA_FED'
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
**    IS_LAYOUT                     = w_layout
 is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
*    IS_PRINT                      =
*    IT_SPECIAL_GROUPS             =
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
*    IT_EXCEPT_QINFO               =
  CHANGING
    it_outtab                     = T_EFA_FED
    IT_FIELDCATALOG               = T_fieldcat
*    IT_SORT                       =
*    IT_FILTER                     =
*  EXCEPTIONS
*    INVALID_PARAMETER_COMBINATION = 1
*    PROGRAM_ERROR                 = 2
*    TOO_MANY_LINES                = 3
*    others                        = 4
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.


ENDFORM.                    " Manifest_EFA_Fedex
*&---------------------------------------------------------------------*
*&      Form  MANIFEST_DEMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form MANIFEST_DEMO .

DATA : LT_MANI TYPE TABLE OF  /pweaver/efamani,
       LS_MANI TYPE /pweaver/efamani.

 DATA: t_fieldcat1 type standard table of lvc_s_fcat,
       w_fieldcat1 type lvc_s_fcat  .

  SELECT * FROM /pweaver/efamani INTO TABLE LT_MANI .



  refresh it_fieldcat.
  clear w_fieldcat1.

      w_fieldcat1-col_pos      = 1.
      w_fieldcat1-fieldname    = 'SHIPDATE'.
      w_fieldcat1-coltext    = 'Shipment Date'.
      w_fieldcat1-tabname      = 'LT_MANI'.
       w_fieldcat1-outputlen  = '14'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

      w_fieldcat1-col_pos      = 2.
      w_fieldcat1-fieldname    = 'SHIP_METHOD'.
      w_fieldcat1-coltext    = 'Ship Method'.
      w_fieldcat1-tabname      = 'LT_MANI'.
      w_fieldcat1-outputlen  = '12'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

       w_fieldcat1-col_pos      = 3.
      w_fieldcat1-fieldname    = 'TRACKINGNUMBER'.
      w_fieldcat1-coltext    = 'Tracking Number'.
      w_fieldcat1-tabname      = 'LT_MANI'.
       w_fieldcat1-hotspot      = 'X'.
        w_fieldcat1-outputlen  = '25'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

      w_fieldcat1-col_pos      = 4.
      w_fieldcat1-fieldname    = 'REFNUMBER'.
      w_fieldcat1-coltext    = 'Reference Number'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '17'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.


       w_fieldcat1-col_pos      = 5.
      w_fieldcat1-fieldname    = 'PONUMBER'.
      w_fieldcat1-coltext    = 'PO Number'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '10'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.


       w_fieldcat1-col_pos      = 6.
      w_fieldcat1-fieldname    = 'COSTCENTER'.
      w_fieldcat1-coltext    = 'Cost Center'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '12'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

       w_fieldcat1-col_pos      = 7.
      w_fieldcat1-fieldname    = 'DOCTYPE'.
      w_fieldcat1-coltext    = 'Doc Type'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '12'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

        w_fieldcat1-col_pos      = 8.
      w_fieldcat1-fieldname    = 'WEIGHT'.
      w_fieldcat1-coltext    = 'Weight'.
      w_fieldcat1-tabname      = 'LT_MANI'.
*        wa_fieldcat-outputlen  = '8'.
      w_fieldcat1-DO_SUM = 'X'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

        w_fieldcat1-col_pos      = 9.
      w_fieldcat1-fieldname    = 'FREIGHT'.
      w_fieldcat1-coltext    = 'Freight'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '15'.
      w_fieldcat1-DO_SUM = 'X'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

        w_fieldcat1-col_pos      = 10.
     w_fieldcat1-fieldname    = 'RATEDWEIGHT'.
      w_fieldcat1-coltext    = 'Invoiced Weight'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '14'.
      w_fieldcat1-DO_SUM = 'X'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.


        w_fieldcat1-col_pos      = 11.
      w_fieldcat1-fieldname    = 'DIFWEIGHT'.
      w_fieldcat1-coltext    = 'Invoice Diff Weight'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '20'.
      w_fieldcat1-DO_SUM = 'X'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear    w_fieldcat1.


        w_fieldcat1-col_pos      = 12.
      w_fieldcat1-fieldname    = 'CARRIERFREIGHT'.
      w_fieldcat1-coltext    = 'Invoiced Freight'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '16'.
      w_fieldcat1-DO_SUM = 'X'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

        w_fieldcat1-col_pos      = 12.
      w_fieldcat1-fieldname    = 'CARRIERDIFREIGHT'.
      w_fieldcat1-coltext    = 'Invoiced Diff Freight'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '20'.
      w_fieldcat1-DO_SUM = 'X'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.

         w_fieldcat1-col_pos      = 13.
      w_fieldcat1-fieldname    = 'ADDCORRCHARGE'.
      w_fieldcat1-coltext    = 'Address Correction Charges'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '29'.
      w_fieldcat1-DO_SUM = 'X'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.



        w_fieldcat1-col_pos      = 14.
      w_fieldcat1-fieldname    = 'COMMENTS'.
      w_fieldcat1-coltext    = 'Comments'.
      w_fieldcat1-tabname      = 'LT_MANI'.
        w_fieldcat1-outputlen  = '30'.
      append     w_fieldcat1 to  t_fieldcat1.
      clear     w_fieldcat1.




*w_layout-zebra = 'X'.
w_layout-cwidth_opt = 'X'.
w_layout-info_fname = 'LINE_COLOR'.
w_layout-grid_title = 'Comparing INVOICE vs MANIFEST Details'.


gs_variant-report = sy-repid. "Enable users save own LAYOUTs

CALL METHOD go_grid->set_table_for_first_display
  EXPORTING
    I_BUFFER_ACTIVE               = 'X'
*    I_BYPASSING_BUFFER            =
*    I_CONSISTENCY_CHECK           =
    I_STRUCTURE_NAME              =  'LT_MANI'
*    IS_VARIANT                    =
*    I_SAVE                        =
*    I_DEFAULT                     = 'X'
**    IS_LAYOUT                     = w_layout
is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
*    IS_PRINT                      =
*    IT_SPECIAL_GROUPS             =
*    IT_TOOLBAR_EXCLUDING          =
*    IT_HYPERLINK                  =
*    IT_ALV_GRAPHICS               =
*    IT_EXCEPT_QINFO               =
  CHANGING
    it_outtab                     = LT_MANI
    IT_FIELDCATALOG               = T_fieldcat1
*    IT_SORT                       =
*    IT_FILTER                     =
  EXCEPTIONS
    INVALID_PARAMETER_COMBINATION = 1
    PROGRAM_ERROR                 = 2
    TOO_MANY_LINES                = 3
    others                        = 4
        .
IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.
 set handler g_application->handle_hotspot_click for go_grid .
endform.                    " MANIFEST_DEMO
*&---------------------------------------------------------------------*
*&      Form  AUDIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM AUDIT .
  DATA : N TYPE I.
  IF IT_ALL_TRACK IS NOT INITIAL .
    LOOP AT IT_ALL_TRACK INTO WA_ALL_TRACK  .
      SELECT TRACKING_NO INVOICE_NO  FROM /PWEAVER/EFA_TRC INTO CORRESPONDING FIELDS OF  TABLE  IT_TRACK WHERE TRACKING_NO = WA_ALL_TRACK-tracking_number.
        DESCRIBE TABLE IT_TRACK LINES N .
        IF N GT 1.
          LOOP AT IT_TRACK INTO WA_TRACK WHERE INVOICE_NO  NE WA_ALL_TRACK-INVOICE_NUMBER .
            WA_FINAL-TRACKING_NO = WA_TRACK-TRACKING_NO.
            WA_FINAL-INVOICE_NO = WA_TRACK-INVOICE_NO.
            APPEND WA_FINAL TO IT_FINAL.
            CLEAR : WA_FINAL.
          ENDLOOP.
          ReFRESH : IT_TRACK.
          else.
            ReFRESH : IT_TRACK.



        ENDIF.
          clear :  wa_all_track,
                   n.



    ENDLOOP.
    refresh : it_all_track.
  ENDIF.
  IF IT_FINAL IS NOT INITIAL .
    refresh : it_zpwefa_track[],
              it_all_detailsd_final[].
    clear :   is_ZPWEFA_track .
    loop at it_final into wa_final.
      select * from /PWEAVER/EFA_UPS INTO table it_zpwefa_track where INVOICE_NUMBER = wa_final-invoice_no and LEAD_SHIP_NUMBER = wa_final-TRACKING_NO.
      if it_zpwefa_track is not initial.
         loop at it_ZPWEFA_track into is_ZPWEFA_track .
*           ***********

*  loop at it_ZPWEFA_UPS into is_ZPWEFA_UPS ." 1)WHERE chrg_catg_det_cd NE 'FC'  .

*    clear wa_upsd.
    read table it_upsd into wa_upsd with key lead_ship_number = is_zpwefa_track-lead_ship_number
                                           tracking_number  = is_zpwefa_track-trck_num.

    IF sy-subrc ne 0.  """""" Add the record into internal table

      wa_upsd-lead_ship_number  =  is_zpwefa_track-lead_ship_number.
      wa_upsd-tracking_number   =  is_zpwefa_track-trck_num.
      wa_upsd-invoice_number    =  is_zpwefa_track-invoice_number.

********** For shipping and handling charges *************

      IF is_ZPWEFA_track-chrg_catg_code eq 'SHP' or is_ZPWEFA_track-chrg_catg_code eq 'RTN'
        .
*         IF IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'RS'.
        IF is_ZPWEFA_track-chrg_class_code = 'FRT'    or  is_ZPWEFA_track-chrg_class_code = 'INF' .   """""" shipping charges
          wa_upsd-account_no        =  is_zpwefa_track-account_number.
          wa_upsd-service_type      =  is_zpwefa_track-chrg_desc.
          wa_upsd-ship_date         =  is_zpwefa_track-transaction_date.
          wa_upsd-zzone             =  is_zpwefa_track-z_one.
          wa_upsd-payment_method    =  is_zpwefa_track-bill_opt_code.
          wa_upsd-invoice_date      =  is_zpwefa_track-invoice_date.
          wa_upsd-invoice_amount    =  is_zpwefa_track-invoice_amount.
          wa_upsd-invoice_curr_key  =  is_zpwefa_track-invc_curr_code.
          wa_upsd-entered_weight    =  is_zpwefa_track-entered_weight.
          wa_upsd-entered_wt_unit   =  is_zpwefa_track-ent_wgt_uom.
          wa_upsd-billed_weight     =  is_zpwefa_track-billed_weight.
          wa_upsd-billed_wt_unit    =  is_zpwefa_track-billed_wgt_uom.
          wa_upsd-container_type    =  is_zpwefa_track-container_type.
          wa_upsd-chrg_catg_code    =  is_zpwefa_track-chrg_catg_code.
          wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.
          wa_upsd-chrg_class_code   =  is_zpwefa_track-chrg_class_code.
          wa_upsd-chrg_unit_qty     =  is_zpwefa_track-chrg_unit_qty.

          wa_upsd-package_qty       =  is_zpwefa_track-pckg_qty.
          wa_upsd-chrg_desc_code    =  is_zpwefa_track-chrg_desc_code.
          wa_upsd-chrg_desc         =  is_zpwefa_track-chrg_desc.

          wa_upsd-ship_incent_amnt    =  is_zpwefa_track-incen_amt.
          wa_upsd-ship_net_amount     =  is_zpwefa_track-net_amnt.
          wa_upsd-sender_name         =  is_zpwefa_track-sender_name.
          wa_upsd-sender_company      =  is_zpwefa_track-sender_cmp_name.
          wa_upsd-sender_address1     =  is_zpwefa_track-sender_addr_lin1.
          wa_upsd-sender_address2     =  is_zpwefa_track-sender_addr_lin2.
          wa_upsd-sender_city         =  is_zpwefa_track-sender_city.
          wa_upsd-sender_state        =  is_zpwefa_track-sender_state.
          wa_upsd-sender_postcode     =  is_zpwefa_track-sender_postal.
          wa_upsd-sender_country      =  is_zpwefa_track-sender_country.
          wa_upsd-receiver_name       =  is_zpwefa_track-receiver_name.
          wa_upsd-receiver_company    =  is_zpwefa_track-receiver_comp_nm.
          wa_upsd-rec_address1        =  is_zpwefa_track-rec_addr_line_1.
          wa_upsd-rec_address2        =  is_zpwefa_track-rec_addr_line_2.
          wa_upsd-receiver_city       =  is_zpwefa_track-receiver_city.
          wa_upsd-receiver_state      =  is_zpwefa_track-receiver_state.
          wa_upsd-rec_postcode   =  is_zpwefa_track-receiver_postal.
          wa_upsd-receiver_country    =  is_zpwefa_track-receiver_country.
        ELSE .                                                      """"""""""""""""" Other/handling charges
          wa_upsd-handle_incen_amt    =  is_zpwefa_track-incen_amt.
          wa_upsd-handle_net_amnt     =  is_zpwefa_track-net_amnt.
        ENDIF.
*        ENDIF.
      ELSEIF is_ZPWEFA_track-chrg_catg_code eq 'ADJ'.

        IF is_zpwefa_track-chrg_catg_det_cd = 'ADC' and
           is_zpwefa_track-chrg_class_code  = 'FRT' .

          wa_upsd-addr_corr_flag = 'X'.
          wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.

          wa_upsd-addr_corr_charge   = is_zpwefa_track-net_amnt.

          wa_upsd-corr_rec_name      =  is_zpwefa_track-receiver_name.
          wa_upsd-corr_rec_company   =  is_zpwefa_track-receiver_comp_nm.
          wa_upsd-corr_recaddress1   =  is_zpwefa_track-rec_addr_line_1.
          wa_upsd-corr_recaddress2   =  is_zpwefa_track-rec_addr_line_2.
          wa_upsd-corr_rec_city      =  is_zpwefa_track-receiver_city.
          wa_upsd-corr_rec_state     = is_zpwefa_track-receiver_state.
          wa_upsd-corr_rec_zip       =  is_zpwefa_track-receiver_postal.
          wa_upsd-corr_rec_country   =  is_zpwefa_track-receiver_country.
*          ELSEIF is_zpwefa_ups1-chrg_catg_code eq 'ADJ'.
*
        ELSEIF ( is_zpwefa_track-chrg_catg_det_cd = 'SCC' OR is_zpwefa_track-chrg_catg_det_cd = 'RADJ' )  and
           is_zpwefa_track-chrg_class_code  = 'FRT' .
*
*          wa_ups-addr_corr_flag = 'X'.
*
*          wa_ups-addr_corr_charge   = is_zpwefa_ups-net_amnt.
*
*          wa_ups-corr_rec_name      =  is_zpwefa_ups-receiver_name.
*          wa_ups-corr_rec_company   =  is_zpwefa_ups-receiver_comp_nm.
*          wa_ups-corr_recaddress1   =  is_zpwefa_ups-rec_addr_line_1.
*          wa_ups-corr_recaddress2   =  is_zpwefa_ups-rec_addr_line_2.
*          wa_ups-corr_rec_city      =  is_zpwefa_ups-receiver_city.
*          wa_ups-corr_rec_state     =  is_zpwefa_ups-receiver_state.
*          wa_ups-corr_rec_zip       =  is_zpwefa_ups-receiver_postal.
*          wa_ups-corr_rec_country   =  is_zpwefa_ups-receiver_country.
*        ELSEIF IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'SCC' .
*          wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
          wa_upsd-account_no        =  is_zpwefa_track-account_number.
          wa_upsd-service_type      =  is_zpwefa_track-chrg_desc.
          wa_upsd-ship_date         =  is_zpwefa_track-transaction_date.
          wa_upSd-zzone             =  is_zpwefa_track-z_one.
          wa_upsd-payment_method    =  is_zpwefa_track-bill_opt_code.
          wa_upsd-invoice_date      =  is_zpwefa_track-invoice_date.
          wa_upsd-invoice_amount    =  is_zpwefa_track-invoice_amount.
          wa_upsd-invoice_curr_key  =  is_zpwefa_track-invc_curr_code.
          wa_upsd-entered_weight    =  is_zpwefa_track-entered_weight.
          wa_upsd-entered_wt_unit   =  is_zpwefa_track-ent_wgt_uom.
          wa_upsd-billed_weight     =  is_zpwefa_track-billed_weight.
          wa_upsd-billed_wt_unit    =  is_zpwefa_track-billed_wgt_uom.
          wa_upsd-container_type    =  is_zpwefa_track-container_type.
          wa_upsd-chrg_catg_code    = is_zpwefa_track-chrg_catg_code.
          wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.
          wa_upsd-chrg_class_code   =  is_zpwefa_track-chrg_class_code.
          wa_upsd-chrg_unit_qty     =  is_zpwefa_track-chrg_unit_qty.

          wa_upsd-package_qty       =  is_zpwefa_track-pckg_qty.
          wa_upsd-chrg_desc_code    =  is_zpwefa_track-chrg_desc_code.
          wa_upsd-chrg_desc         =  is_zpwefa_track-chrg_desc.

*          wa_ups1-ship_incent_amnt    =  is_zpwefa_ups1-incen_amt.
*          wa_ups1-ship_net_amount     =  is_zpwefa_ups1-net_amnt.
          wa_upsd-sender_name         =  is_zpwefa_track-sender_name.
          wa_upsd-sender_company      =  is_zpwefa_track-sender_cmp_name.
          wa_upsd-sender_address1     =  is_zpwefa_track-sender_addr_lin1.
          wa_upsd-sender_address2     =  is_zpwefa_track-sender_addr_lin2.
          wa_upsd-sender_city         =  is_zpwefa_track-sender_city.
          wa_upsd-sender_state        =  is_zpwefa_track-sender_state.
          wa_upsd-sender_postcode     =  is_zpwefa_track-sender_postal.
          wa_upsd-sender_country      =  is_zpwefa_track-sender_country.
          wa_upsd-receiver_name       =  is_zpwefa_track-receiver_name.
          wa_upsd-receiver_company    =  is_zpwefa_track-receiver_comp_nm.
          wa_upsd-rec_address1        =  is_zpwefa_track-rec_addr_line_1.
          wa_upsd-rec_address2        =  is_zpwefa_track-rec_addr_line_2.
          wa_upsd-receiver_city       =  is_zpwefa_track-receiver_city.
          wa_upsd-receiver_state      =  is_zpwefa_track-receiver_state.
          wa_upsd-rec_postcode   =  is_zpwefa_track-receiver_postal.
          wa_upsd-receiver_country    =  is_zpwefa_track-receiver_country.
          wa_upsd-corr_ship_incent   =  is_zpwefa_track-incen_amt.
          wa_upsd-corr_ship_net      =  is_zpwefa_track-net_amnt.


        ELSE ."IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SCC' or is_zpwefa_ups-chrg_catg_det_cd ne 'RADJ' .
          wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.
          wa_upsd-corr_ship_incent   =  is_zpwefa_track-incen_amt.
          wa_upsd-corr_ship_net      =  is_zpwefa_track-net_amnt.

        ENDIF.
      ELSEIF is_zpwefa_track-chrg_catg_code eq 'MIS'.
         IF is_zpwefa_track-chrg_catg_det_cd eq 'MISC'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC' .
       wa_upsd-service_type      =  'Other Charges'.
        wa_upsd-chrg_catg_code    =  is_zpwefa_track-chrg_catg_code.
         wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.
        wa_upsd-corr_ship_incent   =  is_zpwefa_track-incen_amt.
        wa_upsd-corr_ship_net      =  is_zpwefa_track-net_amnt.
          ELSEIF  is_zpwefa_track-chrg_catg_det_cd EQ 'SVCH'.
            wa_upsd-service_type      =  is_zpwefa_track-chrg_desc.
         wa_upsd-chrg_catg_code    =  is_zpwefa_track-chrg_catg_code.
          wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.
          wa_upsd-chrg_class_code   =  is_zpwefa_track-chrg_class_code.
        wa_upsd-corr_ship_incent   =  is_zpwefa_track-incen_amt.
        wa_upsd-corr_ship_net      =  is_zpwefa_track-net_amnt.
        ENDIF.

*         if IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SVCH' AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC' .
*           wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
*        wa_ups-corr_ship_incent   =  is_zpwefa_ups-incen_amt.
*        wa_ups-corr_ship_net      =  is_zpwefa_ups-net_amnt.
*        else.
**          clear wa_ups.
*         endif.
      ENDIF.
      APPEND wa_upsd to it_upsd.
      CLEAR  wa_upsd.
    ELSE.  """""" Modify existing record

********** For shipping and handling charges *************

      IF is_zpwefa_track-chrg_catg_code eq 'SHP' or is_zpwefa_track-chrg_catg_code eq 'RTN'.
        IF is_zpwefa_track-chrg_class_code = 'FRT'."( is_zpwefa_ups-chrg_class_code = 'FRT' OR is_zpwefa_ups-chrg_class_code = 'INF' ).   """""" shipping charges
          wa_upsd-account_no        =  is_zpwefa_track-account_number.
          wa_upsd-service_type      =  is_zpwefa_track-chrg_desc.
          wa_upsd-ship_date         =  is_zpwefa_track-transaction_date.
          wa_upsd-zzone             =  is_zpwefa_track-z_one.
          wa_upsd-payment_method    =  is_zpwefa_track-bill_opt_code.
          wa_upsd-invoice_date      =  is_zpwefa_track-invoice_date.
          wa_upsd-invoice_amount    =  is_zpwefa_track-invoice_amount.
          wa_upsd-invoice_curr_key  =  is_zpwefa_track-invc_curr_code.
          wa_upsd-entered_weight    =  is_zpwefa_track-entered_weight.
          wa_upsd-entered_wt_unit   =  is_zpwefa_track-ent_wgt_uom.
          wa_upsd-billed_weight     =  is_zpwefa_track-billed_weight.
          wa_upsd-billed_wt_unit    =  is_zpwefa_track-billed_wgt_uom.
          wa_upsd-container_type    =  is_zpwefa_track-container_type.
          wa_upsd-chrg_catg_code    =  is_zpwefa_track-chrg_catg_code.
          wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.
          wa_upsd-chrg_class_code   =  is_zpwefa_track-chrg_class_code.
          wa_upsd-chrg_unit_qty     =  is_zpwefa_track-chrg_unit_qty.

          wa_upsd-package_qty       =  is_zpwefa_track-pckg_qty.
          wa_upsd-chrg_desc_code    =  is_zpwefa_track-chrg_desc_code.
          wa_upsd-chrg_desc         =  is_zpwefa_track-chrg_desc.

          wa_upsd-ship_incent_amnt    =  is_zpwefa_track-incen_amt.
          wa_upsd-ship_net_amount     =  is_zpwefa_track-net_amnt.
          wa_upsd-sender_name         =  is_zpwefa_track-sender_name.
          wa_upsd-sender_company      =  is_zpwefa_track-sender_cmp_name.
          wa_upsd-sender_address1     =  is_zpwefa_track-sender_addr_lin1.
          wa_upsd-sender_address2     =  is_zpwefa_track-sender_addr_lin2.
          wa_upsd-sender_city         =  is_zpwefa_track-sender_city.
          wa_upsd-sender_state        =  is_zpwefa_track-sender_state.
          wa_upsd-sender_postcode     =  is_zpwefa_track-sender_postal.
          wa_upsd-sender_country      =  is_zpwefa_track-sender_country.
          wa_upsd-receiver_name       =  is_zpwefa_track-receiver_name.
          wa_upsd-receiver_company    =  is_zpwefa_track-receiver_comp_nm.
          wa_upsd-rec_address1        =  is_zpwefa_track-rec_addr_line_1.
          wa_upsd-rec_address2        =  is_zpwefa_track-rec_addr_line_2.
          wa_upsd-receiver_city       =  is_zpwefa_track-receiver_city.
          wa_upsd-receiver_state      =  is_zpwefa_track-receiver_state.
          wa_upsd-rec_postcode   =  is_zpwefa_track-receiver_postal.
          wa_upsd-receiver_country    =  is_zpwefa_track-receiver_country.
        ELSE. """"""""""""""""" Other/handling charges
          wa_upsd-handle_incen_amt    =  wa_upsd-handle_incen_amt + is_zpwefa_track-incen_amt.
          wa_upsd-handle_net_amnt     =  wa_upsd-handle_net_amnt  + is_zpwefa_track-net_amnt.
        ENDIF.
        modify it_upsd from wa_upsd index sy-tabix.
      ELSEIF is_zpwefa_track-chrg_catg_code eq 'ADJ'.

        IF is_zpwefa_track-chrg_catg_det_cd = 'ADC' and
           is_zpwefa_track-chrg_class_code  = 'FRT' .

          wa_upsd-addr_corr_flag = 'X'.

          wa_upsd-addr_corr_charge   = is_zpwefa_track-net_amnt.

          wa_upsd-corr_rec_name      =  is_zpwefa_track-receiver_name.
          wa_upsd-corr_rec_company   =  is_zpwefa_track-receiver_comp_nm.
          wa_upsd-corr_recaddress1   =  is_zpwefa_track-rec_addr_line_1.
          wa_upsd-corr_recaddress2   =  is_zpwefa_track-rec_addr_line_2.
          wa_upsd-corr_rec_city      =  is_zpwefa_track-receiver_city.
          wa_upsd-corr_rec_state     =  is_zpwefa_track-receiver_state.
          wa_upsd-corr_rec_zip       =  is_zpwefa_track-receiver_postal.
          wa_upsd-corr_rec_country   =  is_zpwefa_track-receiver_country.
        ELSE ."IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SCC' .
*          wa_ups-chrg_catg_detail  =  is_zpwefa_ups-chrg_catg_det_cd.
          wa_upsd-corr_ship_incent   =  wa_upsd-corr_ship_incent + is_zpwefa_track-incen_amt.
          wa_upsd-corr_ship_net      =  wa_upsd-corr_ship_net    + is_zpwefa_track-net_amnt.
        ENDIF.
        modify it_upsd from wa_upsd index sy-tabix.
*      ELSEIF is_zpwefa_ups-chrg_catg_code eq 'MIS' AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'SVCH' AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC'.
*        wa_ups-corr_ship_incent   =  wa_ups-corr_ship_incent + is_zpwefa_ups-incen_amt.
*        wa_ups-corr_ship_net      =  wa_ups-corr_ship_net    + is_zpwefa_ups-net_amnt.
*        modify it_ups from wa_ups index sy-tabix.
         ELSEIF is_zpwefa_track-chrg_catg_code eq 'MIS' AND is_zpwefa_track-chrg_catg_det_cd NE 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC'.
         wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.
        wa_upsd-corr_ship_incent   =  wa_upsd-corr_ship_incent + is_zpwefa_track-incen_amt.
        wa_upsd-corr_ship_net      =  wa_upsd-corr_ship_net    + is_zpwefa_track-net_amnt.
        modify it_upsd from wa_upsd index sy-tabix.
        ELSEIF is_zpwefa_track-chrg_catg_code eq 'MIS' AND is_zpwefa_track-chrg_catg_det_cd EQ 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC' .
         wa_upsd-chrg_catg_code    =  is_zpwefa_track-chrg_catg_code.
          wa_upsd-chrg_catg_detail  =  is_zpwefa_track-chrg_catg_det_cd.
          wa_upsd-chrg_class_code   =  is_zpwefa_track-chrg_class_code.
        wa_upsd-corr_ship_incent   =  is_zpwefa_track-incen_amt.
        wa_upsd-corr_ship_net      =  is_zpwefa_track-net_amnt.
        append wa_upsd to it_upsd.
      ENDIF.

    ENDIF.
*  ENDLOOP.
***************************

*
*    IF sy-subrc ne 0.  """""" Add the record into internal table
*
*      wa_upsd-lead_ship_number  =  is_zpwefa_track-lead_ship_number.
*      wa_upsd-tracking_number   =  is_zpwefa_track-trck_num.
*      wa_upsd-invoice_number    =  is_zpwefa_track-invoice_number.
*
*********** For shipping and handling charges *************
*
*      IF is_zpwefa_track-chrg_catg_code eq 'SHP' or is_zpwefa_track-chrg_catg_code eq 'RTN'.
*
*        IF is_zpwefa_track-chrg_class_code = 'FRT' ."( is_zpwefa_ups-chrg_class_code = 'FRT' OR is_zpwefa_ups-chrg_class_code = 'INF' ).   """""" shipping charges
*          wa_upsd-account_no        =  is_ZPWEFA_track-account_number.
*          wa_upsd-service_type      =  is_ZPWEFA_track-chrg_desc.
*          wa_upsd-ship_date         =  is_ZPWEFA_track-transaction_date.
*          wa_upsd-zzone             =  is_ZPWEFA_track-z_one.
*          wa_upsd-payment_method    =  is_ZPWEFA_track-bill_opt_code.
*          wa_upsd-invoice_date      =  is_ZPWEFA_track-invoice_date.
*          wa_upsd-invoice_amount    =  is_ZPWEFA_track-invoice_amount.
*          wa_upsd-invoice_curr_key  =  is_ZPWEFA_track-invc_curr_code.
*          wa_upsd-entered_weight    =  is_ZPWEFA_track-entered_weight.
*          wa_upsd-entered_wt_unit   =  is_ZPWEFA_track-ent_wgt_uom.
*          wa_upsd-billed_weight     =  is_ZPWEFA_track-billed_weight.
*          wa_upsd-billed_wt_unit    =  is_ZPWEFA_track-billed_wgt_uom.
*          wa_upsd-container_type    =  is_ZPWEFA_track-container_type.
*          wa_upsd-chrg_catg_code    =  is_ZPWEFA_track-chrg_catg_code.
*          wa_upsd-chrg_catg_detail  =  is_ZPWEFA_track-chrg_catg_det_cd.
*          wa_upsd-chrg_class_code   =  is_ZPWEFA_track-chrg_class_code.
*          wa_upsd-chrg_unit_qty     =  is_ZPWEFA_track-chrg_unit_qty.
*
*          wa_upsd-package_qty       =  is_ZPWEFA_track-pckg_qty.
*          wa_upsd-chrg_desc_code    =  is_ZPWEFA_track-chrg_desc_code.
*          wa_upsd-chrg_desc         =  is_ZPWEFA_track-chrg_desc.
*
*          wa_upsd-ship_incent_amnt    =  is_ZPWEFA_track-incen_amt.
*          wa_upsd-ship_net_amount     =  is_ZPWEFA_track-net_amnt.
*          wa_upsd-sender_name         =  is_ZPWEFA_track-sender_name.
*          wa_upsd-sender_company      =  is_ZPWEFA_track-sender_cmp_name.
*          wa_upsd-sender_address1     =  is_ZPWEFA_track-sender_addr_lin1.
*          wa_upsd-sender_address2     =  is_ZPWEFA_track-sender_addr_lin2.
*          wa_upsd-sender_city         =  is_ZPWEFA_track-sender_city.
*          wa_upsd-sender_state        =  is_ZPWEFA_track-sender_state.
*          wa_upsd-sender_postcode     =  is_ZPWEFA_track-sender_postal.
*          wa_upsd-sender_country      =  is_ZPWEFA_track-sender_country.
*          wa_upsd-receiver_name       =  is_ZPWEFA_track-receiver_name.
*          wa_upsd-receiver_company    =  is_ZPWEFA_track-receiver_comp_nm.
*          wa_upsd-rec_address1        =  is_ZPWEFA_track-rec_addr_line_1.
*          wa_upsd-rec_address2        =  is_ZPWEFA_track-rec_addr_line_2.
*          wa_upsd-receiver_city       =  is_ZPWEFA_track-receiver_city.
*          wa_upsd-receiver_state      =  is_ZPWEFA_track-receiver_state.
*          wa_upsd-rec_postcode   =  is_ZPWEFA_track-receiver_postal.
*          wa_upsd-receiver_country    =  is_ZPWEFA_track-receiver_country.
*        ELSE.                                                      """"""""""""""""" Other/handling charges
*          wa_upsd-handle_incen_amt    =  is_ZPWEFA_track-incen_amt.
*          wa_upsd-handle_net_amnt     =  is_ZPWEFA_track-net_amnt.
*        ENDIF.
*      ELSEIF is_zpwefa_track-chrg_catg_code eq 'ADJ'.
*
*        IF is_zpwefa_track-chrg_catg_det_cd = 'ADC' and
*           is_zpwefa_track-chrg_class_code  = 'FRT' .
*
*          wa_upsd-addr_corr_flag = 'X'.
*
*          wa_upsd-addr_corr_charge   = is_ZPWEFA_track-net_amnt.
*
*          wa_upsd-corr_rec_name      =  is_ZPWEFA_track-receiver_name.
*          wa_upsd-corr_rec_company   =  is_ZPWEFA_track-receiver_comp_nm.
*          wa_upsd-corr_recaddress1   =  is_ZPWEFA_track-rec_addr_line_1.
*          wa_upsd-corr_recaddress2   =  is_ZPWEFA_track-rec_addr_line_2.
*          wa_upsd-corr_rec_city      =  is_ZPWEFA_track-receiver_city.
*          wa_upsd-corr_rec_state     =  is_ZPWEFA_track-receiver_state.
*          wa_upsd-corr_rec_zip       =  is_ZPWEFA_track-receiver_postal.
*          wa_upsd-corr_rec_country   =  is_ZPWEFA_track-receiver_country.
*        ELSE.
*          wa_upsd-corr_ship_incent   =  is_ZPWEFA_track-incen_amt.
*          wa_upsd-corr_ship_net      =  is_ZPWEFA_track-net_amnt.
*        ENDIF.
*      ELSEIF is_zpwefa_track-chrg_catg_code eq 'MIS'.
*        wa_upsd-corr_ship_incent   =  is_ZPWEFA_track-incen_amt.
*        wa_upsd-corr_ship_net      =  is_ZPWEFA_track-net_amnt.
*      ENDIF.
*      APPEND wa_upsd to it_upsd.
*      CLEAR  wa_upsd.
*    ELSE.  """""" Modify existing record
*
*********** For shipping and handling charges *************
*
*      IF is_zpwefa_track-chrg_catg_code eq 'SHP' or is_zpwefa_track-chrg_catg_code eq 'RTN'.
*        IF is_zpwefa_track-chrg_class_code = 'FRT'."( is_zpwefa_ups-chrg_class_code = 'FRT' OR is_zpwefa_ups-chrg_class_code = 'INF' ).   """""" shipping charges
*          wa_upsd-account_no        =  is_ZPWEFA_track-account_number.
*          wa_upsd-service_type      =  is_ZPWEFA_track-chrg_desc.
*          wa_upsd-ship_date         =  is_ZPWEFA_track-transaction_date.
*          wa_upsd-zzone             =  is_ZPWEFA_track-z_one.
*          wa_upsd-payment_method    =  is_ZPWEFA_track-bill_opt_code.
*          wa_upsd-invoice_date      =  is_ZPWEFA_track-invoice_date.
*          wa_upsd-invoice_amount    =  is_ZPWEFA_track-invoice_amount.
*          wa_upsd-invoice_curr_key  =  is_ZPWEFA_track-invc_curr_code.
*          wa_upsd-entered_weight    =  is_ZPWEFA_track-entered_weight.
*          wa_upsd-entered_wt_unit   =  is_ZPWEFA_track-ent_wgt_uom.
*          wa_upsd-billed_weight     =  is_ZPWEFA_track-billed_weight.
*          wa_upsd-billed_wt_unit    =  is_ZPWEFA_track-billed_wgt_uom.
*          wa_upsd-container_type    =  is_ZPWEFA_track-container_type.
*          wa_upsd-chrg_catg_code    =  is_ZPWEFA_track-chrg_catg_code.
*          wa_upsd-chrg_catg_detail  =  is_ZPWEFA_track-chrg_catg_det_cd.
*          wa_upsd-chrg_class_code   =  is_ZPWEFA_track-chrg_class_code.
*          wa_upsd-chrg_unit_qty     =  is_ZPWEFA_track-chrg_unit_qty.
*
*          wa_upsd-package_qty       =  is_ZPWEFA_track-pckg_qty.
*          wa_upsd-chrg_desc_code    =  is_ZPWEFA_track-chrg_desc_code.
*          wa_upsd-chrg_desc         =  is_ZPWEFA_track-chrg_desc.
*
*          wa_upsd-ship_incent_amnt    =  is_ZPWEFA_track-incen_amt.
*          wa_upsd-ship_net_amount     =  is_ZPWEFA_track-net_amnt.
*          wa_upsd-sender_name         =  is_ZPWEFA_track-sender_name.
*          wa_upsd-sender_company      =  is_ZPWEFA_track-sender_cmp_name.
*          wa_upsd-sender_address1     =  is_ZPWEFA_track-sender_addr_lin1.
*          wa_upsd-sender_address2     =  is_ZPWEFA_track-sender_addr_lin2.
*          wa_upsd-sender_city         =  is_ZPWEFA_track-sender_city.
*          wa_upsd-sender_state        =  is_ZPWEFA_track-sender_state.
*          wa_upsd-sender_postcode     =  is_ZPWEFA_track-sender_postal.
*          wa_upsd-sender_country      =  is_ZPWEFA_track-sender_country.
*          wa_upsd-receiver_name       =  is_ZPWEFA_track-receiver_name.
*          wa_upsd-receiver_company    =  is_ZPWEFA_track-receiver_comp_nm.
*          wa_upsd-rec_address1        =  is_ZPWEFA_track-rec_addr_line_1.
*          wa_upsd-rec_address2        =  is_ZPWEFA_track-rec_addr_line_2.
*          wa_upsd-receiver_city       =  is_ZPWEFA_track-receiver_city.
*          wa_upsd-receiver_state      =  is_ZPWEFA_track-receiver_state.
*          wa_upsd-rec_postcode   =  is_ZPWEFA_track-receiver_postal.
*          wa_upsd-receiver_country    =  is_ZPWEFA_track-receiver_country.
*        ELSE. """"""""""""""""" Other/handling charges
*          wa_upsd-handle_incen_amt    =  wa_ups-handle_incen_amt + is_ZPWEFA_track-incen_amt.
*          wa_upsd-handle_net_amnt     =  wa_ups-handle_net_amnt  + is_ZPWEFA_track-net_amnt.
*        ENDIF.
*        modify it_upsd from wa_upsd index sy-tabix.
*      ELSEIF is_zpwefa_track-chrg_catg_code eq 'ADJ'.
*
*        IF is_zpwefa_track-chrg_catg_det_cd = 'ADC' and
*           is_zpwefa_track-chrg_class_code  = 'FRT' .
*
*          wa_upsd-addr_corr_flag = 'X'.
*
*          wa_upsd-addr_corr_charge   = is_ZPWEFA_track-net_amnt.
*
*          wa_upsd-corr_rec_name      =  is_ZPWEFA_track-receiver_name.
*          wa_upsd-corr_rec_company   =  is_ZPWEFA_track-receiver_comp_nm.
*          wa_upsd-corr_recaddress1   =  is_ZPWEFA_track-rec_addr_line_1.
*          wa_upsd-corr_recaddress2   =  is_ZPWEFA_track-rec_addr_line_2.
*          wa_upsd-corr_rec_city      =  is_ZPWEFA_track-receiver_city.
*          wa_upsd-corr_rec_state     =  is_ZPWEFA_track-receiver_state.
*          wa_upsd-corr_rec_zip       = is_ZPWEFA_track-receiver_postal.
*          wa_upsd-corr_rec_country   =  is_ZPWEFA_track-receiver_country.
*        ELSE.
*          wa_upsd-corr_ship_incent   =  wa_ups-corr_ship_incent + is_ZPWEFA_track-incen_amt.
*          wa_upsd-corr_ship_net      =  wa_ups-corr_ship_net    + is_ZPWEFA_track-net_amnt.
*        ENDIF.
*        modify it_upsd from wa_upsd index sy-tabix.
*      ELSEIF is_zpwefa_track-chrg_catg_code eq 'MIS'.
*        wa_upsd-corr_ship_incent   =  wa_upsd-corr_ship_incent + is_ZPWEFA_track-incen_amt.
*        wa_upsd-corr_ship_net      =  wa_upsd-corr_ship_net    + is_ZPWEFA_track-net_amnt.
*        modify it_upsd from wa_upsd index sy-tabix.
*      ENDIF.
*
*    ENDIF.
  ENDLOOP.
  refresh : it_ZPWEFA_track .
  if it_upsd is not initial.
    clear : wa_upsd.
    refresh : IT_ALL_DETAILSD.
    loop at it_upsd into wa_upsd where tracking_number = wa_final-tracking_no and invoice_number = wa_final-invoice_no.  .
      if  wa_upsd-ship_date is  initial.


* ELSE.
   wa_upsd-ship_date = 'MISCELLANEOUS'.
      wa_all_detailsd-ship_date = wa_upsd-ship_date.
  else.


data: day(2) type c,
      month(2) type c,
      year(4) type c,
     wa_upsd-ship_date1 type char10.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
    INPUT         =  wa_upsd-ship_date
 IMPORTING
  OUTPUT        =  wa_upsd-ship_date.


day =  wa_upsd-ship_date+6(2).
month = wa_upsd-ship_date+4(2).
year = wa_upsd-ship_date+0(4).


clear wa_upsd-ship_date.
concatenate day '.'month'.'year into wa_upsd-ship_date1.
 wa_all_detailsd-ship_date = wa_upsd-ship_date1.


endif.


   IF wa_upsd-service_type  IS NOT INITIAL.







*      wa_all_details-ship_date = wa_ups-ship_date.
      wa_all_detailsd-service_type = wa_upsd-service_type.
      wa_all_detailsd-invoice_no  = wa_upsd-invoice_number.
      wa_all_detailsd-zone   =  wa_upsd-zzone.
      wa_all_detailsd-tracking_number = wa_upsd-tracking_number.
      wa_all_detailsd-container_type = wa_upsd-container_type.
      wa_all_detailsd-package_qty = wa_upsd-package_qty.
      wa_all_detailsd-actual_weight = wa_upsd-entered_weight.
      wa_all_detailsd-billed_weight  = wa_upsd-billed_weight.
      wa_all_detailsd-transportation_charge  = wa_upsd-ship_net_amount + wa_upsd-ship_incent_amnt.
      wa_all_detailsd-handling_charge = wa_upsd-handle_net_amnt + wa_upsd-handle_incen_amt.
      wa_all_detailsd-total_discounts = wa_upsd-ship_incent_amnt + wa_upsd-handle_incen_amt + wa_upsd-corr_ship_incent..
      wa_all_detailsd-correction_charge  = wa_upsd-corr_ship_net + wa_upsd-corr_handle_net + wa_upsd-ADDR_CORR_CHARGE +
                                                    wa_upsd-corr_ship_incent.
*    wa_all_details-adjustment_charge  = wa_ups-misc_net_chrg.
      wa_all_detailsd-net_charge = wa_all_detailsd-transportation_charge +
                                  wa_all_detailsd-handling_charge       -
                                  wa_all_detailsd-total_discounts       +
                                  wa_all_detailsd-correction_charge     +
                                  wa_all_detailsd-adjustment_charge.
      wa_all_detailsd-sender_name        = wa_upsd-sender_name.
      wa_all_detailsd-sender_company     = wa_upsd-sender_company.
      wa_all_detailsd-sender_address1    = wa_upsd-sender_address1.
      wa_all_detailsd-sender_city        = wa_upsd-sender_city.
      wa_all_detailsd-sender_state       = wa_upsd-sender_state.
      wa_all_detailsd-sender_postcode    = wa_upsd-sender_postcode.
      wa_all_detailsd-sender_country     = wa_upsd-sender_country .
      wa_all_detailsd-receiver_name      = wa_upsd-receiver_name.
      wa_all_detailsd-receiver_company   = wa_upsd-receiver_company.
      wa_all_detailsd-rec_address1       = wa_upsd-rec_address1.
      wa_all_detailsd-receiver_city      = wa_upsd-receiver_city.
      wa_all_detailsd-receiver_state     = wa_upsd-receiver_state.
      wa_all_detailsd-rec_postcode       = wa_upsd-rec_postcode.
      wa_all_detailsd-receiver_country   = wa_upsd-receiver_country.

      append wa_all_detailsd to it_all_detailsd.
      append lines of it_all_detailsd to it_all_detailsd_final.
*      it_all_detailsd_final[] = it_all_detailsd[].
      clear wa_all_detailsd.
    ELSE.
*      inv_ship = inv_ship + wa_ups-ship_net_amount + wa_ups-ship_incent_amnt.
*      inv_hand = inv_hand + wa_ups-handle_net_amnt + wa_ups-handle_incen_amt.
*      inv_disc = inv_disc + wa_ups-ship_incent_amnt + wa_ups-handle_incen_amt + wa_ups-corr_ship_incent..
*      inv_corr = inv_corr + wa_ups-corr_ship_net + wa_ups-corr_handle_net + wa_ups-ADDR_CORR_CHARGE +
*                                                    wa_ups-corr_ship_incent.
    ENDIF.
    clear : wa_upsd.
    endloop.
    refresh : it_upsd.
   endif.


      endif.
      clear :  wa_final.
*      if
    endloop.
    refresh : it_final.

  ENDIF.
  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'SHIP_DATE'.
  wa_fieldcat-coltext      = 'Ship Date'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'SERVICE_TYPE'.
  wa_fieldcat-coltext      = 'Service Type'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  wA_fieldcat-OUTPUTLEN = '35'.
  wa_fieldcat-lowercase = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

   wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'INVOICE_NO'.
  wa_fieldcat-coltext      = 'Invoice No'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  wA_fieldcat-OUTPUTLEN = '35'.
  wa_fieldcat-lowercase = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'ZONE'.
  wa_fieldcat-coltext      = 'Zone'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'TRACKING_NUMBER'.
  wa_fieldcat-coltext      = 'Tracking Number'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  w_fieldcat-OUTPUTLEN = '20'.
*  wa_fieldcat-hotspot      = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'CONTAINER_TYPE'.
  wa_fieldcat-coltext      = 'Shipment Type'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'PACKAGE_QTY'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 9.
  wa_fieldcat-fieldname    = 'BILLED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 11.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 12.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 13.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 13.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 15.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
*  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 16.
  wa_fieldcat-fieldname    = 'SENDER_NAME'.
  wa_fieldcat-coltext      = 'Sender Name'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 17.
  wa_fieldcat-fieldname    = 'SENDER_COMPANY'.
  wa_fieldcat-coltext      = 'Sender Company'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 18.
  wa_fieldcat-fieldname    = 'SENDER_ADDRESS1'.
  wa_fieldcat-coltext      = 'Sender Address'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 19.
  wa_fieldcat-fieldname    = 'SENDER_CITY'.
  wa_fieldcat-coltext      = 'Sender City'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 20.
  wa_fieldcat-fieldname    = 'SENDER_STATE'.
  wa_fieldcat-coltext      = 'Sender State'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 21.
  wa_fieldcat-fieldname    = 'SENDER_POSTCODE'.
  wa_fieldcat-coltext      = 'Sender Postalcode'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 22.
  wa_fieldcat-fieldname    = 'SENDER_COUNTRY'.
  wa_fieldcat-coltext      = 'Sender Country'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 23.
  wa_fieldcat-fieldname    = 'RECEIVER_NAME'.
  wa_fieldcat-coltext      = 'Receiver Name'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 24.
  wa_fieldcat-fieldname    = 'RECEIVER_COMPANY'.
  wa_fieldcat-coltext      = 'Receiver Company'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 25.
  wa_fieldcat-fieldname    = 'REC_ADDRESS1'.
  wa_fieldcat-coltext      = 'Receiver Address'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 26.
  wa_fieldcat-fieldname    = 'RECEIVER_CITY'.
  wa_fieldcat-coltext      = 'Receiver City'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 27.
  wa_fieldcat-fieldname    = 'RECEIVER_STATE'.
  wa_fieldcat-coltext      = 'Receiver State'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 28.
  wa_fieldcat-fieldname    = 'REC_POSTCODE'.
  wa_fieldcat-coltext      = 'Receiver Postalcode'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 29.
  wa_fieldcat-fieldname    = 'RECEIVER_COUNTRY'.
  wa_fieldcat-coltext      = 'Receiver Country'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILSD'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'All Details'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_ALL_DETAILSD'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_all_detailsd_final
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


  set handler g_application->handle_hotspot_click for go_grid .

ENDFORM.                    " AUDIT
*&---------------------------------------------------------------------*
*&      Form  INBOUND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM INBOUND .
*      REFRESH :  IT_ZPWEFA_UPS1.
*
*   IT_ZPWEFA_UPS1 =  IT_ZPWEFA_UPS.
*LOOP AT IT_UPS INTO WA_UPS.
*  DELETE IT_ZPWEFA_UPS1 WHERE LEAD_SHIP_NUMBER = WA_UPS-LEAD_SHIP_NUMBER and chrg_catg_det_cd NE 'FC'.
*ENDLOOP.
*CLEAR : is_ZPWEFA_UPS.
*LOOP AT IT_ZPWEFA_UPS INTO IS_ZPWEFA_UPS WHERE chrg_catg_det_cd EQ 'SVCH' OR chrg_catg_det_cd EQ 'MISC' OR chrg_catg_det_cd EQ 'SCC'.
*APPEND IS_ZPWEFA_UPS TO IT_ZPWEFA_UPS1.
*CLEAR : IS_ZPWEFA_UPS.
*ENDLOOP.
*  REFRESH :  IT_UPS1[].
* loop at it_ZPWEFA_UPS1 into is_ZPWEFA_UPS1 ."WHERE chrg_catg_det_cd NE 'FC' .
*
*    clear wa_ups1.
*    read table it_ups1 into wa_ups1 with key lead_ship_number = is_zpwefa_ups1-lead_ship_number
*                                           tracking_number  = is_zpwefa_ups1-trck_num
*                                           chrg_catg_detail = is_zpwefa_ups1-chrg_catg_det_cd.
*
*
*    IF sy-subrc ne 0.  """""" Add the record into internal table
*
*      wa_ups1-lead_ship_number  =  is_zpwefa_ups1-lead_ship_number.
*      wa_ups1-tracking_number   =  is_zpwefa_ups1-trck_num.
*      wa_ups1-invoice_number    =  is_zpwefa_ups1-invoice_number.
*
*********** For shipping and handling charges *************
*
*      IF is_zpwefa_ups1-chrg_catg_code eq 'SHP' or is_zpwefa_ups1-chrg_catg_code eq 'RTN'.
*
*        IF is_zpwefa_ups1-chrg_class_code = 'FRT'  ."( is_zpwefa_ups-chrg_class_code = 'FRT' OR is_zpwefa_ups-chrg_class_code = 'INF' ).   """""" shipping charges
*          wa_ups1-account_no        =  is_zpwefa_ups1-account_number.
*          wa_ups1-service_type      =  is_zpwefa_ups1-chrg_desc.
*          wa_ups1-ship_date         =  is_zpwefa_ups1-transaction_date.
*          wa_ups1-zzone             =  is_zpwefa_ups1-z_one.
*          wa_ups1-payment_method    =  is_zpwefa_ups1-bill_opt_code.
*          wa_ups1-invoice_date      =  is_zpwefa_ups1-invoice_date.
*          wa_ups1-invoice_amount    =  is_zpwefa_ups1-invoice_amount.
*          wa_ups1-invoice_curr_key  =  is_zpwefa_ups1-invc_curr_code.
*          wa_ups1-entered_weight    =  is_zpwefa_ups1-entered_weight.
*          wa_ups1-entered_wt_unit   =  is_zpwefa_ups1-ent_wgt_uom.
*          wa_ups1-billed_weight     =  is_zpwefa_ups1-billed_weight.
*          wa_ups1-billed_wt_unit    =  is_zpwefa_ups1-billed_wgt_uom.
*          wa_ups1-container_type    =  is_zpwefa_ups1-container_type.
*          wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
*          wa_ups1-chrg_catg_detail  =  is_zpwefa_ups1-chrg_catg_det_cd.
*          wa_ups1-chrg_class_code   =  is_zpwefa_ups1-chrg_class_code.
*          wa_ups1-chrg_unit_qty     =  is_zpwefa_ups1-chrg_unit_qty.
*
*          wa_ups1-package_qty       =  is_zpwefa_ups1-pckg_qty.
*          wa_ups1-chrg_desc_code    =  is_zpwefa_ups1-chrg_desc_code.
*          wa_ups1-chrg_desc         =  is_zpwefa_ups1-chrg_desc.
*
*          wa_ups1-ship_incent_amnt    =  is_zpwefa_ups1-incen_amt.
*          wa_ups1-ship_net_amount     =  is_zpwefa_ups1-net_amnt.
*          wa_ups1-sender_name         =  is_zpwefa_ups1-sender_name.
*          wa_ups1-sender_company      =  is_zpwefa_ups1-sender_cmp_name.
*          wa_ups1-sender_address1     =  is_zpwefa_ups1-sender_addr_lin1.
*          wa_ups1-sender_address2     =  is_zpwefa_ups1-sender_addr_lin2.
*          wa_ups1-sender_city         =  is_zpwefa_ups1-sender_city.
*          wa_ups1-sender_state        =  is_zpwefa_ups1-sender_state.
*          wa_ups1-sender_postcode     =  is_zpwefa_ups1-sender_postal.
*          wa_ups1-sender_country      =  is_zpwefa_ups1-sender_country.
*          wa_ups1-receiver_name       =  is_zpwefa_ups1-receiver_name.
*          wa_ups1-receiver_company    =  is_zpwefa_ups1-receiver_comp_nm.
*          wa_ups1-rec_address1        =  is_zpwefa_ups1-rec_addr_line_1.
*          wa_ups1-rec_address2        =  is_zpwefa_ups1-rec_addr_line_2.
*          wa_ups1-receiver_city       =  is_zpwefa_ups1-receiver_city.
*          wa_ups1-receiver_state      =  is_zpwefa_ups1-receiver_state.
*          wa_ups1-rec_postcode   =  is_zpwefa_ups1-receiver_postal.
*          wa_ups1-receiver_country    =  is_zpwefa_ups1-receiver_country.
*        ELSE .                                                      """"""""""""""""" Other/handling charges
*          wa_ups1-handle_incen_amt    =  is_zpwefa_ups1-incen_amt.
*          wa_ups1-handle_net_amnt     =  is_zpwefa_ups1-net_amnt.
*        ENDIF.
*      ELSEIF is_zpwefa_ups1-chrg_catg_code eq 'ADJ'.
**
*        IF is_zpwefa_ups1-chrg_catg_det_cd = 'SCC' and
*           is_zpwefa_ups1-chrg_class_code  = 'FRT' .
**
*          wa_ups1-addr_corr_flag = 'X'.
**
**          wa_ups-addr_corr_charge   = is_zpwefa_ups-net_amnt.
**
**          wa_ups-corr_rec_name      =  is_zpwefa_ups-receiver_name.
**          wa_ups-corr_rec_company   =  is_zpwefa_ups-receiver_comp_nm.
**          wa_ups-corr_recaddress1   =  is_zpwefa_ups-rec_addr_line_1.
**          wa_ups-corr_recaddress2   =  is_zpwefa_ups-rec_addr_line_2.
**          wa_ups-corr_rec_city      =  is_zpwefa_ups-receiver_city.
**          wa_ups-corr_rec_state     =  is_zpwefa_ups-receiver_state.
**          wa_ups-corr_rec_zip       =  is_zpwefa_ups-receiver_postal.
**          wa_ups-corr_rec_country   =  is_zpwefa_ups-receiver_country.
**        ELSEIF IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'SCC' .
**          wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
*          wa_ups1-account_no        =  is_zpwefa_ups1-account_number.
*          wa_ups1-service_type      =  is_zpwefa_ups1-chrg_desc.
*          wa_ups1-ship_date         =  is_zpwefa_ups1-transaction_date.
*          wa_ups1-zzone             =  is_zpwefa_ups1-z_one.
*          wa_ups1-payment_method    =  is_zpwefa_ups1-bill_opt_code.
*          wa_ups1-invoice_date      =  is_zpwefa_ups1-invoice_date.
*          wa_ups1-invoice_amount    =  is_zpwefa_ups1-invoice_amount.
*          wa_ups1-invoice_curr_key  =  is_zpwefa_ups1-invc_curr_code.
*          wa_ups1-entered_weight    =  is_zpwefa_ups1-entered_weight.
*          wa_ups1-entered_wt_unit   =  is_zpwefa_ups1-ent_wgt_uom.
*          wa_ups1-billed_weight     =  is_zpwefa_ups1-billed_weight.
*          wa_ups1-billed_wt_unit    =  is_zpwefa_ups1-billed_wgt_uom.
*          wa_ups1-container_type    =  is_zpwefa_ups1-container_type.
*          wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
*          wa_ups1-chrg_catg_detail  =  is_zpwefa_ups1-chrg_catg_det_cd.
*          wa_ups1-chrg_class_code   =  is_zpwefa_ups1-chrg_class_code.
*          wa_ups1-chrg_unit_qty     =  is_zpwefa_ups1-chrg_unit_qty.
*
*          wa_ups1-package_qty       =  is_zpwefa_ups1-pckg_qty.
*          wa_ups1-chrg_desc_code    =  is_zpwefa_ups1-chrg_desc_code.
*          wa_ups1-chrg_desc         =  is_zpwefa_ups1-chrg_desc.
*
**          wa_ups1-ship_incent_amnt    =  is_zpwefa_ups1-incen_amt.
**          wa_ups1-ship_net_amount     =  is_zpwefa_ups1-net_amnt.
*          wa_ups1-sender_name         =  is_zpwefa_ups1-sender_name.
*          wa_ups1-sender_company      =  is_zpwefa_ups1-sender_cmp_name.
*          wa_ups1-sender_address1     =  is_zpwefa_ups1-sender_addr_lin1.
*          wa_ups1-sender_address2     =  is_zpwefa_ups1-sender_addr_lin2.
*          wa_ups1-sender_city         =  is_zpwefa_ups1-sender_city.
*          wa_ups1-sender_state        =  is_zpwefa_ups1-sender_state.
*          wa_ups1-sender_postcode     =  is_zpwefa_ups1-sender_postal.
*          wa_ups1-sender_country      =  is_zpwefa_ups1-sender_country.
*          wa_ups1-receiver_name       =  is_zpwefa_ups1-receiver_name.
*          wa_ups1-receiver_company    =  is_zpwefa_ups1-receiver_comp_nm.
*          wa_ups1-rec_address1        =  is_zpwefa_ups1-rec_addr_line_1.
*          wa_ups1-rec_address2        =  is_zpwefa_ups1-rec_addr_line_2.
*          wa_ups1-receiver_city       =  is_zpwefa_ups1-receiver_city.
*          wa_ups1-receiver_state      =  is_zpwefa_ups1-receiver_state.
*          wa_ups1-rec_postcode   =  is_zpwefa_ups1-receiver_postal.
*          wa_ups1-receiver_country    =  is_zpwefa_ups1-receiver_country.
*          wa_ups1-corr_ship_incent   =  is_zpwefa_ups1-incen_amt.
*          wa_ups1-corr_ship_net      =  is_zpwefa_ups1-net_amnt.
*
*        ENDIF.
*      ELSEIF is_zpwefa_ups1-chrg_catg_code eq 'MIS' AND IS_ZPWEFA_UPS1-chrg_catg_det_cd NE 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC' .
*       wa_ups1-service_type      =  is_zpwefa_ups1-chrg_desc.
*        wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
*         wa_ups1-chrg_catg_detail  =  is_zpwefa_ups1-chrg_catg_det_cd.
*        wa_ups1-corr_ship_incent   =  is_zpwefa_ups1-incen_amt.
*        wa_ups1-corr_ship_net      =  is_zpwefa_ups1-net_amnt.
*          ELSEIF is_zpwefa_ups1-chrg_catg_code EQ 'MIS' AND IS_ZPWEFA_UPS1-chrg_catg_det_cd EQ 'SVCH'.
*         wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
*          wa_ups1-chrg_catg_detail  =  is_zpwefa_ups1-chrg_catg_det_cd.
*          wa_ups1-chrg_class_code   =  is_zpwefa_ups1-chrg_class_code.
*        wa_ups1-corr_ship_incent   =  is_zpwefa_ups1-incen_amt.
*        wa_ups1-corr_ship_net      =  is_zpwefa_ups1-net_amnt.
*
*
*      ENDIF.
*      APPEND wa_ups1 to it_ups1.
*      CLEAR  wa_ups.
*    ELSE.  """""" Modify existing record
*
*********** For shipping and handling charges *************
*
*      IF is_zpwefa_ups1-chrg_catg_code eq 'SHP' or is_zpwefa_ups1-chrg_catg_code eq 'RTN'.
*        IF is_zpwefa_ups1-chrg_class_code = 'FRT'."( is_zpwefa_ups-chrg_class_code = 'FRT' OR is_zpwefa_ups-chrg_class_code = 'INF' ).   """""" shipping charges
*          wa_ups1-account_no        =  is_zpwefa_ups1-account_number.
*          wa_ups1-service_type      =  is_zpwefa_ups1-chrg_desc.
*          wa_ups1-ship_date         =  is_zpwefa_ups1-transaction_date.
*          wa_ups1-zzone             =  is_zpwefa_ups1-z_one.
*          wa_ups1-payment_method    =  is_zpwefa_ups1-bill_opt_code.
*          wa_ups1-invoice_date      =  is_zpwefa_ups1-invoice_date.
*          wa_ups1-invoice_amount    =  is_zpwefa_ups1-invoice_amount.
*          wa_ups1-invoice_curr_key  =  is_zpwefa_ups1-invc_curr_code.
*          wa_ups1-entered_weight    =  is_zpwefa_ups1-entered_weight.
*          wa_ups1-entered_wt_unit   =  is_zpwefa_ups1-ent_wgt_uom.
*          wa_ups1-billed_weight     =  is_zpwefa_ups1-billed_weight.
*          wa_ups1-billed_wt_unit    =  is_zpwefa_ups1-billed_wgt_uom.
*          wa_ups1-container_type    =  is_zpwefa_ups1-container_type.
*          wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
*          wa_ups1-chrg_catg_detail  =  is_zpwefa_ups1-chrg_catg_det_cd.
*          wa_ups1-chrg_class_code   =  is_zpwefa_ups1-chrg_class_code.
*          wa_ups1-chrg_unit_qty     =  is_zpwefa_ups1-chrg_unit_qty.
*
*          wa_ups1-package_qty       =  is_zpwefa_ups1-pckg_qty.
*          wa_ups1-chrg_desc_code    =  is_zpwefa_ups1-chrg_desc_code.
*          wa_ups1-chrg_desc         =  is_zpwefa_ups1-chrg_desc.
*
*          wa_ups1-ship_incent_amnt    =  is_zpwefa_ups1-incen_amt.
*          wa_ups1-ship_net_amount     =  is_zpwefa_ups1-sender_name.
*          wa_ups1-sender_company      =  is_zpwefa_ups1-sender_cmp_name.
*          wa_ups1-sender_address1     =  is_zpwefa_ups1-sender_addr_lin1.
*          wa_ups1-sender_address2     =  is_zpwefa_ups1-sender_addr_lin2.
*          wa_ups1-sender_city         =  is_zpwefa_ups1-sender_city.
*          wa_ups1-sender_state        =  is_zpwefa_ups1-sender_state.
*          wa_ups1-sender_postcode     =  is_zpwefa_ups1-sender_postal.
*          wa_ups1-sender_country      =  is_zpwefa_ups1-sender_country.
*          wa_ups1-receiver_name       =  is_zpwefa_ups1-receiver_name.
*          wa_ups1-receiver_company    =  is_zpwefa_ups1-receiver_comp_nm.
*          wa_ups1-rec_address1        =  is_zpwefa_ups1-rec_addr_line_1.
*          wa_ups1-rec_address2        =  is_zpwefa_ups1-rec_addr_line_2.
*          wa_ups1-receiver_city       =  is_zpwefa_ups1-receiver_city.
*          wa_ups1-receiver_state      =  is_zpwefa_ups1-receiver_state.
*          wa_ups1-rec_postcode   =  is_zpwefa_ups1-receiver_postal.
*          wa_ups1-receiver_country    =  is_zpwefa_ups1-receiver_country.
*        ELSE. """"""""""""""""" Other/handling charges
*          wa_ups1-handle_incen_amt    =  wa_ups1-handle_incen_amt + is_zpwefa_ups1-incen_amt.
*          wa_ups1-handle_net_amnt     =  wa_ups1-handle_net_amnt  + is_zpwefa_ups1-net_amnt.
*        ENDIF.
*        modify it_ups1 from wa_ups1 index sy-tabix.
*      ELSEIF is_zpwefa_ups1-chrg_catg_code eq 'ADJ'.
**
*        IF is_zpwefa_ups1-chrg_catg_det_cd = 'SCC' and
*           is_zpwefa_ups1-chrg_class_code  = 'FRT' .
**
**          wa_ups1-addr_corr_flag = 'X'.
**
**          wa_ups-addr_corr_charge   = is_zpwefa_ups-net_amnt.
**
**          wa_ups-corr_rec_name      =  is_zpwefa_ups-receiver_name.
**          wa_ups-corr_rec_company   =  is_zpwefa_ups-receiver_comp_nm.
**          wa_ups-corr_recaddress1   =  is_zpwefa_ups-rec_addr_line_1.
**          wa_ups-corr_recaddress2   =  is_zpwefa_ups-rec_addr_line_2.
**          wa_ups-corr_rec_city      =  is_zpwefa_ups-receiver_city.
**          wa_ups-corr_rec_state     =  is_zpwefa_ups-receiver_state.
**          wa_ups-corr_rec_zip       =  is_zpwefa_ups-receiver_postal.
**          wa_ups-corr_rec_country   =  is_zpwefa_ups-receiver_country.
**        ELSEIF IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'SCC' .
*          wa_ups1-account_no        =  is_zpwefa_ups1-account_number.
*          wa_ups1-service_type      =  is_zpwefa_ups1-chrg_desc.
*          wa_ups1-ship_date         =  is_zpwefa_ups1-transaction_date.
*          wa_ups1-zzone             =  is_zpwefa_ups1-z_one.
*          wa_ups1-payment_method    =  is_zpwefa_ups1-bill_opt_code.
*          wa_ups1-invoice_date      =  is_zpwefa_ups1-invoice_date.
*          wa_ups1-invoice_amount    =  is_zpwefa_ups1-invoice_amount.
*          wa_ups1-invoice_curr_key  =  is_zpwefa_ups1-invc_curr_code.
*          wa_ups1-entered_weight    =  is_zpwefa_ups1-entered_weight.
*          wa_ups1-entered_wt_unit   =  is_zpwefa_ups1-ent_wgt_uom.
*          wa_ups1-billed_weight     =  is_zpwefa_ups1-billed_weight.
*          wa_ups1-billed_wt_unit    =  is_zpwefa_ups1-billed_wgt_uom.
*          wa_ups1-container_type    =  is_zpwefa_ups1-container_type.
*          wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
*          wa_ups1-chrg_catg_detail  =  is_zpwefa_ups1-chrg_catg_det_cd.
*
*          wa_ups1-chrg_class_code   =  is_zpwefa_ups1-chrg_class_code.
*          wa_ups1-chrg_unit_qty     =  is_zpwefa_ups1-chrg_unit_qty.
*
*          wa_ups1-package_qty       =  is_zpwefa_ups1-pckg_qty.
*          wa_ups1-chrg_desc_code    =  is_zpwefa_ups1-chrg_desc_code.
*          wa_ups1-chrg_desc         =  is_zpwefa_ups1-chrg_desc.
*
**          wa_ups1-ship_incent_amnt    =  is_zpwefa_ups1-incen_amt.
**          wa_ups1-ship_net_amount     =  is_zpwefa_ups1-net_amnt.
*          wa_ups1-sender_name         =  is_zpwefa_ups1-sender_name.
*          wa_ups1-sender_company      =  is_zpwefa_ups1-sender_cmp_name.
*          wa_ups1-sender_address1     =  is_zpwefa_ups1-sender_addr_lin1.
*          wa_ups1-sender_address2     =  is_zpwefa_ups1-sender_addr_lin2.
*          wa_ups1-sender_city         =  is_zpwefa_ups1-sender_city.
*          wa_ups1-sender_state        =  is_zpwefa_ups1-sender_state.
*          wa_ups1-sender_postcode     =  is_zpwefa_ups1-sender_postal.
*          wa_ups1-sender_country      =  is_zpwefa_ups1-sender_country.
*          wa_ups1-receiver_name       =  is_zpwefa_ups1-receiver_name.
*          wa_ups1-receiver_company    =  is_zpwefa_ups1-receiver_comp_nm.
*          wa_ups1-rec_address1        =  is_zpwefa_ups1-rec_addr_line_1.
*          wa_ups1-rec_address2        =  is_zpwefa_ups1-rec_addr_line_2.
*          wa_ups1-receiver_city       =  is_zpwefa_ups1-receiver_city.
*          wa_ups1-receiver_state      =  is_zpwefa_ups1-receiver_state.
*          wa_ups1-rec_postcode   =  is_zpwefa_ups1-receiver_postal.
*          wa_ups1-receiver_country    =  is_zpwefa_ups1-receiver_country.
*          wa_ups1-corr_ship_incent   =  is_zpwefa_ups1-incen_amt.
*          wa_ups1-corr_ship_net      =  is_zpwefa_ups1-net_amnt.
**        ELSEIF. "IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'SCC' .
*          ELSE.
*          wa_ups1-corr_ship_incent   =  wa_ups1-corr_ship_incent + is_zpwefa_ups1-incen_amt.
*          wa_ups1-corr_ship_net      =  wa_ups1-corr_ship_net    + is_zpwefa_ups1-net_amnt.
*        ENDIF.
*        modify it_ups1 from wa_ups1 index sy-tabix.
*      ELSEIF is_zpwefa_ups1-chrg_catg_code eq 'MIS' AND IS_ZPWEFA_UPS1-chrg_catg_det_cd NE 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC'.
*         wa_ups1-chrg_catg_detail  =  is_zpwefa_ups1-chrg_catg_det_cd.
*        wa_ups1-corr_ship_incent   =  wa_ups1-corr_ship_incent + is_zpwefa_ups1-incen_amt.
*        wa_ups1-corr_ship_net      =  wa_ups1-corr_ship_net    + is_zpwefa_ups1-net_amnt.
*        modify it_ups1 from wa_ups1 index sy-tabix.
*        ELSEIF is_zpwefa_ups1-chrg_catg_code eq 'MIS' AND IS_ZPWEFA_UPS1-chrg_catg_det_cd EQ 'SVCH'." AND IS_ZPWEFA_UPS-chrg_catg_det_cd NE 'MISC' .
*         wa_ups1-chrg_catg_code    =  is_zpwefa_ups1-chrg_catg_code.
*          wa_ups1-chrg_catg_detail  =  is_zpwefa_ups1-chrg_catg_det_cd.
*          wa_ups1-chrg_class_code   =  is_zpwefa_ups1-chrg_class_code.
*        wa_ups1-corr_ship_incent   =  is_zpwefa_ups1-incen_amt.
*        wa_ups1-corr_ship_net      =  is_zpwefa_ups1-net_amnt.
*        append wa_ups1 to it_ups1.
*      ENDIF.
*
*    ENDIF.
*  ENDLOOP.
  PERFORM SUMMERIZE_DATA1.

ENDFORM.                    " INBOUND
*&---------------------------------------------------------------------*
*&      Form  SUMMERIZE_DATA1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUMMERIZE_DATA1 .
  refresh : it_all_details[].
  clear : wa_ups,
          wa_all_details.
  LOOP AT IT_UPS INTO WA_UPS.

    if  wa_ups-ship_date is initial .


* ELSE.
*   wa_ups1-ship_date = .
      wa_all_details-ship_date = wa_ups-ship_date.
  else.


data: day(2) type c,
      month(2) type c,
      year(4) type c,
     wa_ups-ship_date1 type char10.


    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
  EXPORTING
    INPUT         =  wa_ups-ship_date
 IMPORTING
  OUTPUT        =  wa_ups-ship_date.


day =  wa_ups-ship_date+6(2).
month = wa_ups-ship_date+4(2).
year = wa_ups-ship_date+0(4).


clear wa_ups-ship_date.
concatenate day '.'month'.'year into wa_ups-ship_date1.
 wa_all_details-ship_date = wa_ups-ship_date1.


endif.
IF wa_ups-service_type  IS NOT INITIAL AND  wa_ups-addr_corr_flag NE 'X'.






*      wa_all_details-ship_date = wa_ups-ship_date.
      wa_all_details-service_type = wa_ups-service_type.
      wa_all_details-zone   =  wa_ups-zzone.
      wa_all_details-tracking_number = wa_ups-tracking_number.
      wa_all_details-container_type = wa_ups-container_type.
      wa_all_details-package_qty = wa_ups-package_qty.
      wa_all_details-actual_weight = wa_ups-entered_weight.
      wa_all_details-billed_weight  = wa_ups-billed_weight.
        wa_all_details-chrg_catg_det_cd = wa_ups-chrg_catg_detail.
      wa_all_details-transportation_charge  = wa_ups-ship_net_amount + wa_ups-ship_incent_amnt.
      wa_all_details-handling_charge = wa_ups-handle_net_amnt + wa_ups-handle_incen_amt.
      wa_all_details-total_discounts = wa_ups-ship_incent_amnt + wa_ups-handle_incen_amt + wa_ups-corr_ship_incent..
      wa_all_details-correction_charge  = wa_ups-corr_ship_net + wa_ups-corr_handle_net + wa_ups-ADDR_CORR_CHARGE +
                                                    wa_ups-corr_ship_incent.
*    wa_all_details-adjustment_charge  = wa_ups-misc_net_chrg.
      wa_all_details-net_charge = wa_all_details-transportation_charge +
                                  wa_all_details-handling_charge       -
                                  wa_all_details-total_discounts       +
                                  wa_all_details-correction_charge     +
                                  wa_all_details-adjustment_charge.
      wa_all_details-sender_name        = wa_ups-sender_name.
      wa_all_details-sender_company     = wa_ups-sender_company.
      wa_all_details-sender_address1    = wa_ups-sender_address1.
      wa_all_details-sender_city        = wa_ups-sender_city.
      wa_all_details-sender_state       = wa_ups-sender_state.
      wa_all_details-sender_postcode    = wa_ups-sender_postcode.
      wa_all_details-sender_country     = wa_ups-sender_country .
      wa_all_details-receiver_name      = wa_ups-receiver_name.
      wa_all_details-receiver_company   = wa_ups-receiver_company.
      wa_all_details-rec_address1       = wa_ups-rec_address1.
      wa_all_details-receiver_city      = wa_ups-receiver_city.
      wa_all_details-receiver_state     = wa_ups-receiver_state.
      wa_all_details-rec_postcode       = wa_ups-rec_postcode.
      wa_all_details-receiver_country   = wa_ups-receiver_country.

      append wa_all_details to it_all_details.
      clear wa_all_details.
    ELSEIF wa_ups-addr_corr_flag EQ 'X' .

      wa_all_details-service_type = 'Adjustment Charges'.
      wa_all_details-zone   =  wa_ups-zzone.
      wa_all_details-tracking_number = wa_ups-tracking_number.
      wa_all_details-container_type = wa_ups-container_type.
      wa_all_details-package_qty = wa_ups-package_qty.
      wa_all_details-actual_weight = wa_ups-entered_weight.
      wa_all_details-billed_weight  = wa_ups-billed_weight.
      wa_all_details-chrg_catg_det_cd = wa_ups-chrg_catg_detail.
      wa_all_details-transportation_charge  = wa_ups-ship_net_amount + wa_ups-ship_incent_amnt.
      wa_all_details-handling_charge = wa_ups-handle_net_amnt + wa_ups-handle_incen_amt.
      wa_all_details-total_discounts = wa_ups-ship_incent_amnt + wa_ups-handle_incen_amt + wa_ups-corr_ship_incent..
      wa_all_details-correction_charge  = wa_ups-corr_ship_net + wa_ups-corr_handle_net + wa_ups-ADDR_CORR_CHARGE +
                                                    wa_ups-corr_ship_incent.
*    wa_all_details-adjustment_charge  = wa_ups-misc_net_chrg.
      wa_all_details-net_charge = wa_all_details-transportation_charge +
                                  wa_all_details-handling_charge       -
                                  wa_all_details-total_discounts       +
                                  wa_all_details-correction_charge     +
                                  wa_all_details-adjustment_charge.
      wa_all_details-sender_name        = wa_ups-sender_name.
      wa_all_details-sender_company     = wa_ups-sender_company.
      wa_all_details-sender_address1    = wa_ups-sender_address1.
      wa_all_details-sender_city        = wa_ups-sender_city.
      wa_all_details-sender_state       = wa_ups-sender_state.
      wa_all_details-sender_postcode    = wa_ups-sender_postcode.
      wa_all_details-sender_country     = wa_ups-sender_country .
      wa_all_details-receiver_name      = wa_ups-receiver_name.
      wa_all_details-receiver_company   = wa_ups-receiver_company.
      wa_all_details-rec_address1       = wa_ups-rec_address1.
      wa_all_details-receiver_city      = wa_ups-receiver_city.
      wa_all_details-receiver_state     = wa_ups-receiver_state.
      wa_all_details-rec_postcode       = wa_ups-rec_postcode.
      wa_all_details-receiver_country   = wa_ups-receiver_country.

      append wa_all_details to it_all_details.
      clear wa_all_details.
      elseif wa_ups-service_type  IS INITIAL and wa_ups-chrg_catg_detail NE 'SVCH' .
        wa_all_details-service_type = 'Other Charges'.
      wa_all_details-zone   =  wa_ups-zzone.
      wa_all_details-tracking_number = wa_ups-tracking_number.
      wa_all_details-container_type = wa_ups-container_type.
      wa_all_details-package_qty = wa_ups-package_qty.
      wa_all_details-actual_weight = wa_ups-entered_weight.
      wa_all_details-billed_weight  = wa_ups-billed_weight.
      wa_all_details-chrg_catg_det_cd = wa_ups-chrg_catg_detail.
      wa_all_details-transportation_charge  = wa_ups-ship_net_amount + wa_ups-ship_incent_amnt.
      wa_all_details-handling_charge = wa_ups-handle_net_amnt + wa_ups-handle_incen_amt.
      wa_all_details-total_discounts = wa_ups-ship_incent_amnt + wa_ups-handle_incen_amt + wa_ups-corr_ship_incent..
      wa_all_details-correction_charge  = wa_ups-corr_ship_net + wa_ups-corr_handle_net + wa_ups-ADDR_CORR_CHARGE +
                                                    wa_ups-corr_ship_incent.
*    wa_all_details-adjustment_charge  = wa_ups-misc_net_chrg.
      wa_all_details-net_charge = wa_all_details-transportation_charge +
                                  wa_all_details-handling_charge       -
                                  wa_all_details-total_discounts       +
                                  wa_all_details-correction_charge     +
                                  wa_all_details-adjustment_charge.
      wa_all_details-sender_name        = wa_ups-sender_name.
      wa_all_details-sender_company     = wa_ups-sender_company.
      wa_all_details-sender_address1    = wa_ups-sender_address1.
      wa_all_details-sender_city        = wa_ups-sender_city.
      wa_all_details-sender_state       = wa_ups-sender_state.
      wa_all_details-sender_postcode    = wa_ups-sender_postcode.
      wa_all_details-sender_country     = wa_ups-sender_country .
      wa_all_details-receiver_name      = wa_ups-receiver_name.
      wa_all_details-receiver_company   = wa_ups-receiver_company.
      wa_all_details-rec_address1       = wa_ups-rec_address1.
      wa_all_details-receiver_city      = wa_ups-receiver_city.
      wa_all_details-receiver_state     = wa_ups-receiver_state.
      wa_all_details-rec_postcode       = wa_ups-rec_postcode.
      wa_all_details-receiver_country   = wa_ups-receiver_country.

      append wa_all_details to it_all_details.
      clear wa_all_details.


*      inv_ship = inv_ship + wa_ups1-ship_net_amount + wa_ups1-ship_incent_amnt.
*      inv_hand = inv_hand + wa_ups1-handle_net_amnt + wa_ups1-handle_incen_amt.
*      inv_disc = inv_disc + wa_ups1-ship_incent_amnt + wa_ups1-handle_incen_amt + wa_ups1-corr_ship_incent..
*      inv_corr = inv_corr + wa_ups1-corr_ship_net + wa_ups1-corr_handle_net + wa_ups1-ADDR_CORR_CHARGE +
*                                                    wa_ups1-corr_ship_incent.
ELSE.
  wa_all_details-service_type = 'Service Charges'.
  wa_all_details-chrg_catg_det_cd = wa_ups-chrg_catg_detail.
  wa_all_details-transportation_charge  = wa_ups-ship_net_amount + wa_ups-ship_incent_amnt.
      wa_all_details-handling_charge = wa_ups-handle_net_amnt + wa_ups-handle_incen_amt.
      wa_all_details-total_discounts = wa_ups-ship_incent_amnt + wa_ups-handle_incen_amt + wa_ups-corr_ship_incent..
      wa_all_details-correction_charge  = wa_ups-corr_ship_net + wa_ups-corr_handle_net + wa_ups-ADDR_CORR_CHARGE +
                                                    wa_ups-corr_ship_incent.
*    wa_all_details-adjustment_charge  = wa_ups-misc_net_chrg.
      wa_all_details-net_charge = wa_all_details-transportation_charge +
                                  wa_all_details-handling_charge       -
                                  wa_all_details-total_discounts       +
                                  wa_all_details-correction_charge     +
                                  wa_all_details-adjustment_charge.
  append wa_all_details to it_all_details.
      clear wa_all_details.
    ENDIF.
ENDLOOP.
ENDFORM.                    " SUMMERIZE_DATA1
*&---------------------------------------------------------------------*
*&      Form  FINAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM FINAL .
  refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'SHIP_DATE'.
  wa_fieldcat-coltext      = 'Ship Date'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'SERVICE_TYPE'.
  wa_fieldcat-coltext      = 'Service Type'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wA_fieldcat-OUTPUTLEN = '35'.
  wa_fieldcat-lowercase = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'ZONE'.
  wa_fieldcat-coltext      = 'Zone'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 4.
  wa_fieldcat-fieldname    = 'TRACKING_NUMBER'.
  wa_fieldcat-coltext      = 'Tracking Number'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  w_fieldcat-OUTPUTLEN = '20'.
  wa_fieldcat-hotspot      = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 5.
  wa_fieldcat-fieldname    = 'CONTAINER_TYPE'.
  wa_fieldcat-coltext      = 'Shipment Type'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 6.
  wa_fieldcat-fieldname    = 'PACKAGE_QTY'.
  wa_fieldcat-coltext      = 'Total Packages'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 7.
  wa_fieldcat-fieldname    = 'ACTUAL_WEIGHT'.
  wa_fieldcat-coltext      = 'Actual Weight'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 8.
  wa_fieldcat-fieldname    = 'BILLED_WEIGHT'.
  wa_fieldcat-coltext      = 'Rated Weight'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 9.
  wa_fieldcat-fieldname    = 'TRANSPORTATION_CHARGE'.
  wa_fieldcat-coltext      = 'Shipping Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 10.
  wa_fieldcat-fieldname    = 'HANDLING_CHARGE'.
  wa_fieldcat-coltext      = 'other/handling Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 11.
  wa_fieldcat-fieldname    = 'TOTAL_DISCOUNTS'.
  wa_fieldcat-coltext      = 'Total Discounts'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 12.
  wa_fieldcat-fieldname    = 'CORRECTION_CHARGE'.
  wa_fieldcat-coltext      = 'Correction Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

*  wa_fieldcat-col_pos      = 13.
*  wa_fieldcat-fieldname    = 'ADJUSTMENT_CHARGE'.
*  wa_fieldcat-coltext      = 'Adjustment_Charges'.
*  wa_fieldcat-tabname      = 'IT_ALL_DETAILS'.
*  wa_fieldcat-do_sum       = 'X'.
*  append wa_fieldcat to it_fieldcat.
*  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 14.
  wa_fieldcat-fieldname    = 'NET_CHARGE'.
  wa_fieldcat-coltext      = 'Net Charges'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 15.
  wa_fieldcat-fieldname    = 'SENDER_NAME'.
  wa_fieldcat-coltext      = 'Sender Name'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 16.
  wa_fieldcat-fieldname    = 'SENDER_COMPANY'.
  wa_fieldcat-coltext      = 'Sender Company'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 17.
  wa_fieldcat-fieldname    = 'SENDER_ADDRESS1'.
  wa_fieldcat-coltext      = 'Sender Address'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 18.
  wa_fieldcat-fieldname    = 'SENDER_CITY'.
  wa_fieldcat-coltext      = 'Sender City'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 19.
  wa_fieldcat-fieldname    = 'SENDER_STATE'.
  wa_fieldcat-coltext      = 'Sender State'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 20.
  wa_fieldcat-fieldname    = 'SENDER_POSTCODE'.
  wa_fieldcat-coltext      = 'Sender Postalcode'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 21.
  wa_fieldcat-fieldname    = 'SENDER_COUNTRY'.
  wa_fieldcat-coltext      = 'Sender Country'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 22.
  wa_fieldcat-fieldname    = 'RECEIVER_NAME'.
  wa_fieldcat-coltext      = 'Receiver Name'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 23.
  wa_fieldcat-fieldname    = 'RECEIVER_COMPANY'.
  wa_fieldcat-coltext      = 'Receiver Company'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 24.
  wa_fieldcat-fieldname    = 'REC_ADDRESS1'.
  wa_fieldcat-coltext      = 'Receiver Address'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 25.
  wa_fieldcat-fieldname    = 'RECEIVER_CITY'.
  wa_fieldcat-coltext      = 'Receiver City'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 26.
  wa_fieldcat-fieldname    = 'RECEIVER_STATE'.
  wa_fieldcat-coltext      = 'Receiver State'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 27.
  wa_fieldcat-fieldname    = 'REC_POSTCODE'.
  wa_fieldcat-coltext      = 'Receiver Postalcode'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 28.
  wa_fieldcat-fieldname    = 'RECEIVER_COUNTRY'.
  wa_fieldcat-coltext      = 'Receiver Country'.
  wa_fieldcat-tabname      = 'IT_ALL_DETAILS1'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.


  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'All Details'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_ALL_DETAILS1'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_all_details1
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.


  set handler g_application->handle_hotspot_click for go_grid .

ENDFORM.                    " FINAL
*&---------------------------------------------------------------------*
*&      Form  SUMMARY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SUMMARY .
  DATA : V_ASD type netwr,
         V_WWS type netwr,
         V_ISS type netwr,
         v_man type netwr,
         V_FC type netwr,
         V_AOC type netwr,
         V_SER type netwr,
         V_RTS TYPE NETWR,
         V_ITP TYPE NETWR,
          V_CHRB TYPE NETWR,
           v_MIS TYPE NETWR,
         V_TOT(8) TYPE c.
  CLEAR : V_ASD, "UPS shipping document
         V_WWS ,  " WOrld wide service
         V_ISS ,   " shipping api
         V_MAN ,    " ups worldship
         V_FC ,     "collect
         V_AOC ,    "Adjustment and other charges
         V_SER ,    " service
         V_RTS,     " ups returns
         V_ITP,     " INBOUND THIRD PARTY
         V_CHRB,
          v_MIS,
         V_TOT.
  REFRESH : IT_SUMM.
  CLEAR : WA_SUMM.

   loop at it_ZPWEFA_UPS into is_ZPWEFA_UPS ."WHERE chrg_catg_det_cd NE 'FC' .
     IF IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'ASD'.
       V_ASD = V_ASD + is_zpwefa_ups-net_amnt.
     ELSEIF IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'WWS'.
       V_WWS = V_WWS + is_zpwefa_ups-net_amnt.
     ELSEIF  IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'ISS'.
       V_ISS = V_ISS + is_zpwefa_ups-net_amnt.
        ELSEIF  IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'MAN'.
       V_MAN = V_MAN + is_zpwefa_ups-net_amnt.
       ELSEIF  IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'FC'.
         V_FC = V_FC + is_zpwefa_ups-net_amnt.
        ELSEIF ( is_zpwefa_ups-chrg_catg_code eq 'ADJ' OR is_zpwefa_ups-chrg_catg_code eq 'MIS' OR is_zpwefa_ups-chrg_catg_code eq 'RTN' ) AND ( is_zpwefa_ups-chrg_catg_det_cd eq 'MISC' OR is_zpwefa_ups-chrg_catg_det_cd eq 'SCC' or
is_zpwefa_ups-chrg_catg_det_cd eq 'RADJ' OR is_zpwefa_ups-chrg_catg_det_cd eq 'RTS'   )  .
         V_AOC = V_AOC + is_zpwefa_ups-net_amnt.
       ELSEIF  IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'SVCH'.
         V_SER = V_SER + is_zpwefa_ups-net_amnt.
           ELSEIF   IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'RS'.
         V_RTS = V_RTS + is_zpwefa_ups-net_amnt.
           ELSEIF   IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'TP'.
         V_ITP = V_ITP + is_zpwefa_ups-net_amnt.
           ELSEIF   IS_ZPWEFA_UPS-chrg_catg_det_cd EQ 'CHBK'.
         V_CHRB = V_CHRB + is_zpwefa_ups-net_amnt.
         else.
         v_MIS = V_MIS + is_zpwefa_ups-net_amnt.
ENDIF.
CLEAR :  IS_ZPWEFA_UPS.
    ENDLOOP.
    V_TOT = V_ASD + V_WWS + V_ISS + V_FC + V_AOC + V_SER + V_MAN + V_RTS + v_itp + V_CHRB + V_MIS.
    if V_ASD IS NOT INITIAL or V_WWS IS NOT INITIAL or V_ISS IS NOT INITIAL OR V_MAN IS NOT INITIAL OR V_CHRB IS NOT INITIAL .
        WA_SUMM-DESC = 'Outbound'.
         append wa_summ to it_summ.
         CLEAR : WA_SUMM.
endif.

    IF V_ASD IS NOT INITIAL .
      WA_SUMM-DESC = 'UPS Shipping Document'.
      WA_SUMM-CODE = 'ASD'.
      wa_summ-net = v_asd.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
      if v_wws is not initial.
          WA_SUMM-DESC = 'Worldwide Service'.
           WA_SUMM-CODE = 'WWS'.
      wa_summ-net = v_wws.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
      if v_iss is not initial.
                WA_SUMM-DESC = 'Shiping API'.
                 WA_SUMM-CODE = 'ISS'.
      wa_summ-net = v_iss.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
        if v_MAN is not initial.
                WA_SUMM-DESC = 'UPS WorldShip'.
                 WA_SUMM-CODE = 'MAN'.
      wa_summ-net = v_man.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
        if v_CHRB is not initial.
                WA_SUMM-DESC = 'Chargeback'.
                 WA_SUMM-CODE = 'CHBK'.
      wa_summ-net = v_chrb.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
      if v_fc is not initial or v_aoc is not initial or v_ser is not initial OR V_RTS IS NOT INITIAL.
         WA_SUMM-DESC = 'Inbound'.
         append wa_summ to it_summ.
         CLEAR : WA_SUMM.
         endif.
          if v_fc is not initial.
                WA_SUMM-DESC = 'Collect'.
                 WA_SUMM-CODE = 'FC'.
      wa_summ-net = v_fc.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
          if v_RTS is not initial.
                WA_SUMM-DESC = 'UPS Returns'.
                 WA_SUMM-CODE = 'RS'.
      wa_summ-net = v_rts.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
        if v_ITP is not initial.
                WA_SUMM-DESC = 'Inbound Third Party'.
                 WA_SUMM-CODE = 'TP'.
      wa_summ-net = v_itp.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
          if v_aoc is not initial.
                WA_SUMM-DESC = 'Adjustment & Other Charges'.
      wa_summ-net = v_aoc.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
          if v_ser is not initial.
                WA_SUMM-DESC = 'Service Charge'.
                WA_SUMM-CODE = 'SVCH'.
      wa_summ-net = v_ser.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
         if v_MIS is not initial.
                WA_SUMM-DESC = 'Miscellanous Charge'.
      wa_summ-net = v_MIS.
      append wa_summ to it_summ.
      CLEAR : WA_SUMM.
      endif.
      IF V_TOT IS NOT INITIAL.
         WA_SUMM-DESC = 'Total'.
        wa_summ-net = v_TOT.
          append wa_summ to it_summ.
         CLEAR : WA_SUMM.
       ENDIF.


      refresh : it_fieldcat.
  clear :  wa_fieldcat.

  wa_fieldcat-col_pos      = 1.
  wa_fieldcat-fieldname    = 'DESC'.
  wa_fieldcat-coltext      = 'Description'.
  wa_fieldcat-tabname      = 'IT_SUMM'.
    wA_fieldcat-OUTPUTLEN = '35'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.
  wa_fieldcat-col_pos      = 2.
  wa_fieldcat-fieldname    = 'CODE'.
  wa_fieldcat-coltext      = 'CODE'.
  wa_fieldcat-tabname      = 'IT_SUMM'.
    wA_fieldcat-OUTPUTLEN = '35'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.

  wa_fieldcat-col_pos      = 3.
  wa_fieldcat-fieldname    = 'NET'.
  wa_fieldcat-coltext      = 'Net Charge'.
  wa_fieldcat-tabname      = 'IT_SUMM'.
*  wA_fieldcat-OUTPUTLEN = '35'.
*  wa_fieldcat-lowercase = 'X'.
  wa_fieldcat-do_sum       = 'X'.
  append wa_fieldcat to it_fieldcat.
  clear wa_fieldcat.






  gs_layout-cwidth_opt = 'X'.
  gs_layout-info_fname = 'LINE_COLOR'.
  gs_layout-grid_title = 'Summary Of Charges'.

gs_variant-report = sy-repid. "Enable users save own LAYOUTs
  call method go_grid->set_table_for_first_display
    exporting
      i_structure_name              = 'IT_SUMM'
*      is_layout                     = gs_layout
      is_variant                       = gs_variant
      i_save                        = 'X'
      i_default                     = 'A'
    changing
      it_outtab                     = it_SUMM
      it_fieldcatalog               = it_fieldcat
*    IT_SORT                       =
  exceptions
    invalid_parameter_combination = 1
    program_error                 = 2
    too_many_lines                = 3
    others                        = 4  .
  if sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.




ENDFORM.                    " SUMMARY
