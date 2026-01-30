*&---------------------------------------------------------------------*
*& Include          ZSAPER_GROOVY_IDE_CLS
*&---------------------------------------------------------------------*
CLASS lcl_groovy DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      cv_tmp_key     TYPE ZSAPERGROOVYTST-id VALUE 'TMP', "ZSAPERGROOVYTST
      cv_destination TYPE rfcdest VALUE 'CPI_GROOVY'.
    METHODS: display_ide,
      pai,
      pbo.
  PRIVATE SECTION.
    TYPES:
      BEGIN OF t_params,
        type TYPE ZSAPER_HDR_PROP_DTE,
        name TYPE text50,
        val  TYPE text50,
      END OF t_params,
      BEGIN OF t_test_case,
        id         TYPE ZSAPERGROOVYTST-id,
        changed_at TYPE ZSAPERGROOVYTST-changed_at,
      END OF t_test_case,
      tt_test_case TYPE TABLE OF t_test_case,
      tt_params    TYPE TABLE OF t_params.
    "layout
    "         ao_docker_main
    "|---------------------------------|
    "|        ao_split_main            |
    "|  ao_s_1    ao_s_2     ao_s_3    |
    "|          |          |          ||
    "|  ao_t_1  | ao_t_2   | ao_t_3   ||
    "|----------|          |----------||
    "|          |----------|          ||
    "|ao_grid_p | ao_t_4   | ao_salv_p||
    "|          |          |          ||
    "|---------------------------------|
    "layout objects
    DATA: ao_docker_main TYPE REF TO cl_gui_docking_container,
          ao_split_main  TYPE REF TO cl_gui_splitter_container,
          ao_s_1         TYPE REF TO cl_gui_splitter_container,
          ao_s_2         TYPE REF TO cl_gui_splitter_container,
          ao_s_3         TYPE REF TO cl_gui_splitter_container,
          ao_t_1         TYPE REF TO cl_gui_textedit,
          ao_t_2         TYPE REF TO cl_gui_textedit,
          ao_t_3         TYPE REF TO cl_gui_textedit,
          ao_t_4         TYPE REF TO cl_gui_textedit,
          ao_grid_p      TYPE REF TO cl_gui_alv_grid,
          ao_salv_p      TYPE REF TO cl_salv_table,
          "screen data
          at_in_params   TYPE tt_params,
          at_out_params  TYPE tt_params,
          av_script_out  TYPE string,
          av_console_log TYPE string.
    "screen I/O
    METHODS:
      pbo_0001,
      pai_0001.
    "utils
    METHODS:
      build_screen_0001,
      build_headers_in_tab IMPORTING i_parent       TYPE REF TO cl_gui_container
                           RETURNING VALUE(ro_grid) TYPE REF TO cl_gui_alv_grid
                           RAISING   cx_salv_msg,
      call_script,
      encode_parameters    EXPORTING ev_params  TYPE string
                                     ev_headers TYPE string,
      decode_parameters    IMPORTING iv_value  TYPE string
                                     iv_type   TYPE c
                           CHANGING  ct_params TYPE tt_params,
      get_grid_exclude_fun RETURNING VALUE(rt_excluded) TYPE ui_functions,
      get_initial_data     RETURNING VALUE(rs_test_rec) TYPE ZSAPERGROOVYTST,
      get_screen_data      RETURNING VALUE(rs_test_rec) TYPE ZSAPERGROOVYTST,
      parse_service_call_response IMPORTING iv_response TYPE string
                                            iv_code     TYPE i,
      set_display_data     IMPORTING is_test_rec TYPE ZSAPERGROOVYTST.
ENDCLASS.

CLASS lcl_groovy IMPLEMENTATION.
  METHOD display_ide.
    CALL SCREEN 0001.
  ENDMETHOD.
  METHOD pai.
    CASE sy-dynnr.
      WHEN '0001'.
        me->pai_0001( ).
    ENDCASE.
  ENDMETHOD.
  METHOD pbo.
    SET PF-STATUS sy-dynnr.
    SET TITLEBAR sy-dynnr.
    CASE sy-dynnr.
      WHEN '0001'.
        me->pbo_0001( ).
    ENDCASE.
  ENDMETHOD.
  "PROTECTED
  METHOD pai_0001.
    CASE sy-ucomm.
      WHEN 'EXECUTE'.
        me->call_script( ).
      WHEN 'EXIT'.
        "save current data as tmp/last changed/cache/whatever
        DATA(ls_current_data) = me->get_screen_data( ).
        ls_current_data-id = cv_tmp_key.
        MODIFY ZSAPERGROOVYTST FROM ls_current_data.
        LEAVE PROGRAM.
      WHEN 'CANCEL'.
        "don't save anything, just leave
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.
  METHOD pbo_0001.
    me->build_screen_0001( ).
  ENDMETHOD.
  METHOD build_screen_0001.
    CHECK ao_docker_main IS NOT BOUND.
    TRY.
        "build docker and splitter
        ao_docker_main = NEW cl_gui_docking_container( parent     = cl_gui_container=>screen0
                                                       side       = cl_gui_docking_container=>dock_at_top
                                                       extension  = 1000 ).
        ao_split_main = NEW cl_gui_splitter_container( rows = 1 columns = 3 parent = ao_docker_main ).
        "build screen parts
        "left part - input
        DATA(lo_cont_left) = ao_split_main->get_container( row = 1 column = 1 ).
        ao_s_1 = NEW cl_gui_splitter_container( rows = 2 columns = 1 parent = lo_cont_left ).
        ao_s_1->set_row_height( id = 1 height = 70 ).
        "editor
        DATA(lo_cont_l_top) = ao_s_1->get_container( row = 1 column = 1 ).
        ao_t_1 = NEW cl_gui_textedit( parent = lo_cont_l_top ).
        "input headers and properties. Grid
        ao_grid_p = me->build_headers_in_tab( ao_s_1->get_container( row = 2 column = 1 ) ).
        "middle part
        DATA(lo_cont_mid) = ao_split_main->get_container( row = 1 column = 2 ).
        ao_s_2 = NEW cl_gui_splitter_container( rows = 2 columns = 1 parent = lo_cont_mid ).
        ao_s_2->set_row_height( id = 1 height = 50 ).
        "script editor
        DATA(lo_cont_2_top) = ao_s_2->get_container( row = 1 column = 1 ).
        ao_t_2 = NEW cl_gui_textedit( parent = lo_cont_2_top ).
        "console
        DATA(lo_cont_2_bt) = ao_s_2->get_container( row = 2 column = 1 ).
        ao_t_4 = NEW cl_gui_textedit( parent = lo_cont_2_bt ).
        ao_t_4->set_readonly_mode( ).
        ao_t_4->set_toolbar_mode( 0 ).
        ao_t_4->set_textstream( text = '- Console logs goes here -' ).
        "right part
        DATA(lo_cont_right) = ao_split_main->get_container( row = 1 column = 3 ).
        ao_s_3 = NEW cl_gui_splitter_container( rows = 2 columns = 1 parent = lo_cont_right ).
        ao_s_3->set_row_height( id = 1 height = 70 ).
        "output
        DATA(lo_cont_3_top) = ao_s_3->get_container( row = 1 column = 1 ).
        ao_t_3 = NEW cl_gui_textedit( parent = lo_cont_3_top ).
        "headers and params salv
        DATA(lo_cont_3_bot) = ao_s_3->get_container( row = 2 column = 1 ).
        cl_salv_table=>factory( EXPORTING r_container  = lo_cont_3_bot
                                IMPORTING r_salv_table = ao_salv_p
                                 CHANGING t_table      = at_out_params ).
        ao_salv_p->display( ).
        "load initial data
        DATA(ls_init_data) = me->get_initial_data( ).
        me->set_display_data( ls_init_data ).
      CATCH cx_root INTO DATA(lx_root).
        MESSAGE |Critical error: { lx_root->get_text( ) } | TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE PROGRAM.
    ENDTRY.
  ENDMETHOD.
  METHOD build_headers_in_tab.
    "get fcat
    cl_salv_table=>factory( IMPORTING r_salv_table   = DATA(salv_table)
                             CHANGING t_table        = at_in_params  ).
    DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
      r_columns      = salv_table->get_columns( )
      r_aggregations = salv_table->get_aggregations( )
    ).
    LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>).
      <fs_fcat>-edit = 'X'.
    ENDLOOP.
    ro_grid = NEW cl_gui_alv_grid( i_parent = i_parent ).
    DATA(ls_layout) = VALUE lvc_s_layo( grid_title = 'Import parameters and headers'
                                        sel_mode   = 'A'
                                        stylefname = 'CELLTAB' ).
    ro_grid->set_table_for_first_display( EXPORTING is_layout = ls_layout
                                                    it_toolbar_excluding = me->get_grid_exclude_fun( )
                                           CHANGING it_outtab = at_in_params
                                                    it_fieldcatalog = lt_fcat ).
    ro_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_enter ).
    ro_grid->register_edit_event( i_event_id = cl_gui_alv_grid=>mc_evt_modified ).
  ENDMETHOD.
  METHOD call_script.
    cl_http_client=>create_by_destination( EXPORTING destination = cv_destination
                                           IMPORTING client      = DATA(lo_http) ).
    DATA(ls_current_screen_data) = me->get_screen_data( ).
    DATA(lv_payload) =
    |\{| &&
      |"inputScript":"{ ls_current_screen_data-script  }",| &&
      |"inputPayload":"{ ls_current_screen_data-payload  }",| &&
      |"inputParams":"{ ls_current_screen_data-props  }",| &&
      |"inputHdrs":"{ ls_current_screen_data-hdrs  }"| &&
    |\}|.
    lo_http->request->set_cdata( lv_payload ).
    lo_http->send( ).
    lo_http->receive( ).
    lo_http->response->get_status( IMPORTING code = DATA(lv_status_code) ).
    IF lv_status_code EQ 500.
      MESSAGE 'Internal server error occured' TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      me->parse_service_call_response( iv_response = lo_http->response->get_cdata( ) iv_code = lv_status_code ).
    ENDIF.
  ENDMETHOD.
  METHOD encode_parameters.
    LOOP AT at_in_params ASSIGNING FIELD-SYMBOL(<fs_param>).
      IF <fs_param>-type = 'P'.
        IF ev_params IS INITIAL.
          ev_params = |{ <fs_param>-name }={ <fs_param>-val };|.
        ELSE.
          ev_params = ev_params && |{ <fs_param>-name }={ <fs_param>-val };|.
        ENDIF.
      ELSEIF <fs_param>-type = 'H'.
        IF ev_headers IS INITIAL.
          ev_headers = |{ <fs_param>-name }={ <fs_param>-val };|.
        ELSE.
          ev_headers = ev_headers && |{ <fs_param>-name }={ <fs_param>-val };|.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  METHOD decode_parameters.
    "data(LV_HEADERS_RET) = CL_HTTP_UTILITY=>IF_HTTP_UTILITY~DECODE_BASE64( IV_VALUE ).
    SPLIT iv_value AT ';' INTO TABLE DATA(lt_table).
    LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<fs_hdr>).
      SPLIT <fs_hdr> AT '=' INTO DATA(lv_parameter) DATA(lv_value).
      APPEND VALUE #( type = iv_type name = lv_parameter val = lv_value ) TO ct_params.
    ENDLOOP.
  ENDMETHOD.
  METHOD get_grid_exclude_fun.
    APPEND cl_gui_alv_grid=>mc_fc_sort     TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_info     TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_sum      TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_subtot   TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_average  TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_views   TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_check   TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_refresh   TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_print   TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_expcrdata TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_graph TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_maintain_variant TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_save_variant TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_expcrdesig TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_expcrtempl TO rt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_expmdb TO rt_excluded.
  ENDMETHOD.
  METHOD get_initial_data.
    SELECT SINGLE * FROM ZSAPERGROOVYTST INTO @rs_test_rec WHERE uname = @sy-uname AND id = @cv_tmp_key.
    IF rs_test_rec IS INITIAL.
      rs_test_rec-payload = |<data>example xml data</data>|.
      rs_test_rec-hdrs = |Header1=ValueHdr1|.
      rs_test_rec-props = |Property1=ValuePrp1|.
      rs_test_rec-script = |import com.sap.gateway.ip.core.customdev.util.Message;| && cl_abap_char_utilities=>newline &&
      || && cl_abap_char_utilities=>newline &&
      |def Message processData(Message message) \{| && cl_abap_char_utilities=>newline &&
      cl_abap_char_utilities=>horizontal_tab && |def body = message.getBody(String.class);| && cl_abap_char_utilities=>newline &&
      cl_abap_char_utilities=>horizontal_tab && |// Your code here| && cl_abap_char_utilities=>newline &&
      cl_abap_char_utilities=>horizontal_tab && |message.setBody(body);| && cl_abap_char_utilities=>newline &&
      cl_abap_char_utilities=>horizontal_tab && |return message;| && cl_abap_char_utilities=>newline &&
      |\}|.
      rs_test_rec-payload = cl_http_utility=>if_http_utility~encode_base64( rs_test_rec-payload ).
      rs_test_rec-script = cl_http_utility=>if_http_utility~encode_base64( rs_test_rec-script ).
      rs_test_rec-hdrs = cl_http_utility=>if_http_utility~encode_base64( rs_test_rec-hdrs ).
      rs_test_rec-props = cl_http_utility=>if_http_utility~encode_base64( rs_test_rec-props ).
    ENDIF.
  ENDMETHOD.
  METHOD get_screen_data.
    ao_t_1->get_textstream( IMPORTING text = rs_test_rec-payload ).
    ao_t_2->get_textstream( IMPORTING text = rs_test_rec-script ).
    cl_gui_cfw=>flush( ).
    me->encode_parameters( IMPORTING ev_headers = rs_test_rec-hdrs ev_params = rs_test_rec-props ).
    rs_test_rec-payload = cl_http_utility=>if_http_utility~encode_base64( rs_test_rec-payload ).
    rs_test_rec-script = cl_http_utility=>if_http_utility~encode_base64( rs_test_rec-script ).
    rs_test_rec-hdrs = cl_http_utility=>if_http_utility~encode_base64( rs_test_rec-hdrs ).
    rs_test_rec-props = cl_http_utility=>if_http_utility~encode_base64( rs_test_rec-props ).
    rs_test_rec-uname = sy-uname.
  ENDMETHOD.
  METHOD parse_service_call_response.
    DATA(lo_parser) = NEW /ui5/cl_json_parser( ).
    lo_parser->parse( iv_response ).
    DATA(lt_entries) = lo_parser->m_entries.
    av_console_log = COND string( WHEN iv_code EQ 200 THEN '- Script executed -' ELSE '- Error -' ).
    LOOP AT lt_entries ASSIGNING FIELD-SYMBOL(<fs_entry>).
      CASE <fs_entry>-name.
        WHEN 'payload'.
          av_script_out = cl_http_utility=>if_http_utility~decode_base64( <fs_entry>-value ).
        WHEN 'headers'.
          me->decode_parameters( EXPORTING iv_type = 'H' iv_value = cl_http_utility=>if_http_utility~decode_base64( <fs_entry>-value )
                                  CHANGING ct_params = at_out_params ).
        WHEN 'properties'.
          me->decode_parameters( EXPORTING iv_type = 'P' iv_value = cl_http_utility=>if_http_utility~decode_base64( <fs_entry>-value )
                                  CHANGING ct_params = at_out_params ).
        WHEN 'console'.
          av_console_log = av_console_log && cl_abap_char_utilities=>newline && cl_http_utility=>if_http_utility~decode_base64( <fs_entry>-value ).
        WHEN 'error'.
          av_console_log = av_console_log && cl_abap_char_utilities=>newline && cl_http_utility=>if_http_utility~decode_base64( <fs_entry>-value ).
      ENDCASE.
    ENDLOOP.
    ao_salv_p->refresh( ).
    ao_t_4->set_textstream( av_console_log ).
    ao_t_3->set_textstream( av_script_out ).
    cl_gui_cfw=>flush( ).
  ENDMETHOD.
  METHOD set_display_data.
    CHECK ao_t_1 IS BOUND.
    ao_t_1->set_textstream( cl_http_utility=>if_http_utility~decode_base64( is_test_rec-payload ) ).
    CHECK ao_t_2 IS BOUND.
    ao_t_2->set_textstream( cl_http_utility=>if_http_utility~decode_base64( is_test_rec-script ) ).
    CLEAR: at_out_params.
    me->decode_parameters( EXPORTING iv_type = 'H' iv_value = cl_http_utility=>if_http_utility~decode_base64( is_test_rec-hdrs )
                            CHANGING ct_params = at_in_params ).
    me->decode_parameters( EXPORTING iv_type = 'PH' iv_value = cl_http_utility=>if_http_utility~decode_base64( is_test_rec-props )
                            CHANGING ct_params = at_in_params ).
    CHECK: ao_grid_p IS BOUND.
    ao_grid_p->refresh_table_display( ).
    CHECK: ao_salv_p IS BOUND.
    ao_salv_p->refresh( ).
  ENDMETHOD.
ENDCLASS.
