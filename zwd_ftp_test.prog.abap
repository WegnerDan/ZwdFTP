*&---------------------------------------------------------------------*
*& Report  ZWD_FTP_TEST
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zwd_ftp_test.

*======================================================================
CLASS lcl_ftp DEFINITION.

*======================================================================
  PUBLIC SECTION.

    METHODS connect IMPORTING iv_hostname TYPE zcl_wd_ftp=>mty_hostname
                              iv_port     TYPE i
                              iv_username TYPE zcl_wd_ftp=>mty_username
                              iv_password TYPE zcl_wd_ftp=>mty_password.

    METHODS disconnect.

*======================================================================
  PRIVATE SECTION.

    CONSTANTS:
      mc_func_upload TYPE salv_de_function VALUE '%%_UPLOAD_%%'.

    DATA:
      mo_ftp     TYPE REF TO zcl_wd_ftp,
      mo_salv    TYPE REF TO cl_salv_table,
      mt_listing TYPE zcl_wd_ftp=>mty_directory_listing_tt.

    METHODS:
      set_header,
      refresh_listing,
      display,
      handle_double_click FOR EVENT if_salv_events_actions_table~double_click OF cl_salv_events_table IMPORTING row column,
      handle_before_salv_function FOR EVENT if_salv_events_functions~before_salv_function OF cl_salv_events_table IMPORTING e_salv_function,
      change_dir IMPORTING iv_dir TYPE zcl_wd_ftp=>mty_filename,
      download IMPORTING iv_fname TYPE zcl_wd_ftp=>mty_filename.

*======================================================================
ENDCLASS.

*======================================================================
DATA:
  go_ftp TYPE REF TO lcl_ftp.

PARAMETERS:
  hostname TYPE zcl_wd_ftp=>mty_hostname LOWER CASE,
  port     TYPE i DEFAULT 21,
  username TYPE zcl_wd_ftp=>mty_username LOWER CASE,
  password TYPE zcl_wd_ftp=>mty_password LOWER CASE.

*======================================================================
INITIALIZATION.

* ---------------------------------------------------------------------
  go_ftp = NEW #( ).

*======================================================================
START-OF-SELECTION.

* ---------------------------------------------------------------------
  go_ftp->connect( iv_hostname = hostname
                   iv_port     = port
                   iv_username = username
                   iv_password = password ).

* ---------------------------------------------------------------------
  go_ftp->disconnect( ).


CLASS lcl_ftp IMPLEMENTATION.

*======================================================================
  METHOD connect.
* ---------------------------------------------------------------------
    TRY.
        mo_ftp = NEW #( iv_hostname = iv_hostname
                        iv_port     = iv_port
                        iv_username = iv_username
                        iv_password = iv_password ).

        refresh_listing( ).

      CATCH zcx_wd_ftp_error INTO DATA(gx_error).
        MESSAGE gx_error->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

* ---------------------------------------------------------------------
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = mo_salv
                                CHANGING  t_table      = mt_listing ).

        mo_salv->get_columns( )->get_column( 'TYPE' )->set_output_length( 2 ).
        mo_salv->get_columns( )->get_column( 'FILESIZE' )->set_output_length( 14 ).
        mo_salv->get_columns( )->get_column( 'FILEDATE' )->set_output_length( 12 ).
        mo_salv->get_columns( )->get_column( 'FILENAME' )->set_output_length( 200 ).

        mo_salv->get_functions( )->set_default( ).
        mo_salv->get_functions( )->set_export_localfile( ).

        mo_salv->get_display_settings( )->set_striped_pattern( abap_true ).

        SET HANDLER handle_double_click         FOR mo_salv->get_event( ).
        SET HANDLER handle_before_salv_function FOR mo_salv->get_event( ).

        display( ).

      CATCH cx_salv_msg cx_salv_not_found.
        BREAK-POINT.
    ENDTRY.



* ---------------------------------------------------------------------
  ENDMETHOD.

*======================================================================
  METHOD disconnect.
* ---------------------------------------------------------------------
    mo_ftp->disconnect( ).

* ---------------------------------------------------------------------
  ENDMETHOD.

*======================================================================
  METHOD set_header.
* ---------------------------------------------------------------------
    DATA:
      lv_header TYPE lvc_title.

* ---------------------------------------------------------------------
    lv_header = mo_ftp->get_current_directory( ).

* ---------------------------------------------------------------------
    mo_salv->get_display_settings( )->set_list_header( lv_header ).

* ---------------------------------------------------------------------
  ENDMETHOD.

*======================================================================
  METHOD refresh_listing.
* ---------------------------------------------------------------------
    mt_listing = mo_ftp->get_directory_listing( ).
    INSERT VALUE #( type     = zcl_wd_ftp=>mc_type_dir
                    filename = '..'                    ) INTO mt_listing INDEX 1.

* ---------------------------------------------------------------------
  ENDMETHOD.

*======================================================================
  METHOD display.
* ---------------------------------------------------------------------
    set_header( ).

* ---------------------------------------------------------------------
    mo_salv->display( ).

* ---------------------------------------------------------------------
  ENDMETHOD.

*======================================================================
  METHOD handle_double_click.
* ---------------------------------------------------------------------
    FIELD-SYMBOLS:
      <ls_line> LIKE LINE OF mt_listing.

* ---------------------------------------------------------------------
    UNASSIGN <ls_line>.
    READ TABLE mt_listing ASSIGNING <ls_line> INDEX row.
    IF  <ls_line> IS ASSIGNED.
      CASE <ls_line>-type.
        WHEN zcl_wd_ftp=>mc_type_dir
        OR   zcl_wd_ftp=>mc_type_link.
          change_dir( <ls_line>-filename ).
        WHEN zcl_wd_ftp=>mc_type_file.
          download( <ls_line>-filename ).
        WHEN OTHERS.
      ENDCASE.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.

*======================================================================
  METHOD handle_before_salv_function.
* ---------------------------------------------------------------------
    DATA:
      lt_file_table  TYPE filetable,
      lv_rc          TYPE i,
      lv_action      TYPE i,
      lv_blob_length TYPE i,
      lt_blob_data   TYPE zcl_wd_ftp=>mty_blob_tt,
      lv_filename    TYPE zcl_wd_ftp=>mty_filename,
      lv_dummy       TYPE string.

* ---------------------------------------------------------------------
    " only hijack "local file" button, ignore all others
    IF e_salv_function <> '%PC'.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------
    FREE: lt_file_table, lv_rc, lv_action.
    cl_gui_frontend_services=>file_open_dialog( EXPORTING  multiselection = abap_true
                                                CHANGING   file_table     = lt_file_table
                                                           rc             = lv_rc
                                                           user_action    = lv_action
                                                EXCEPTIONS OTHERS         = 4             ).
    IF sy-subrc <> 0
    OR lv_rc    <  0
    OR lv_action <> cl_gui_frontend_services=>action_ok
    OR lt_file_table IS INITIAL.
      MESSAGE 'Canceled' TYPE 'E' DISPLAY LIKE 'W'.
    ENDIF.

* ---------------------------------------------------------------------
    LOOP AT lt_file_table ASSIGNING FIELD-SYMBOL(<ls_file>).
      FREE: lv_blob_length, lt_blob_data, lv_filename.
      cl_gui_frontend_services=>gui_upload( EXPORTING  filename   = <ls_file>-filename && ``
                                                       filetype   = 'BIN'
                                            IMPORTING  filelength = lv_blob_length
                                            CHANGING   data_tab   = lt_blob_data
                                            EXCEPTIONS OTHERS     = 4                        ).
      IF sy-subrc = 0.
        lv_filename = <ls_file>-filename.
        lv_filename = reverse( lv_filename ).
        SPLIT lv_filename AT '\' INTO lv_filename lv_dummy.
        lv_filename = reverse( lv_filename ).
        mo_ftp->upload_table( iv_filename    = lv_filename
                              it_bin_data    = lt_blob_data
                              iv_blob_length = lv_blob_length ).
      ENDIF.
    ENDLOOP.

* ---------------------------------------------------------------------
    refresh_listing( ).
    mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).

* ---------------------------------------------------------------------
    " this forces the default function to be canceled
    MESSAGE '' TYPE 'E'.

* ---------------------------------------------------------------------
  ENDMETHOD.

*======================================================================
  METHOD change_dir.
* ---------------------------------------------------------------------
    DATA:
      lv_dir TYPE zcl_wd_ftp=>mty_directory.

* ---------------------------------------------------------------------
    lv_dir = iv_dir.
    mo_ftp->change_directory( lv_dir ).

* ---------------------------------------------------------------------
    refresh_listing( ).

* ---------------------------------------------------------------------
    mo_salv->refresh( refresh_mode = if_salv_c_refresh=>full ).

* ---------------------------------------------------------------------
    display( ).

* ---------------------------------------------------------------------
  ENDMETHOD.

*======================================================================
  METHOD download.
* ---------------------------------------------------------------------
    DATA:
      lv_default_file_name TYPE string,
      lv_file_filter       TYPE string,
      lv_ext               TYPE string,
      lv_initial_directory TYPE string,
      lv_filename          TYPE string,
      lv_path              TYPE string,
      lv_fullpath          TYPE string,
      lv_action            TYPE i,
      lt_bin_data          TYPE zcl_wd_ftp=>mty_blob_tt,
      lv_blob_length       TYPE i.

* ---------------------------------------------------------------------
    lv_default_file_name = iv_fname.

* ---------------------------------------------------------------------
    " get file extension
    lv_file_filter = reverse( iv_fname ).
    SPLIT lv_file_filter AT '.' INTO lv_ext lv_file_filter.
    IF lv_file_filter IS NOT INITIAL.
      lv_ext = reverse( lv_ext ).
    ELSE.
      FREE lv_ext.
    ENDIF.

* ---------------------------------------------------------------------
    " build filter string like (*.TXT)|*.TXT|
    IF lv_ext IS NOT INITIAL.
      FREE lv_file_filter.
      lv_file_filter = '(*.' && lv_ext && ')|*.' && lv_ext && '|'.
    ENDIF.

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>file_save_dialog( EXPORTING  default_file_name = lv_default_file_name
                                                           file_filter       = lv_file_filter
                                                CHANGING   filename          = lv_filename
                                                           path              = lv_path
                                                           fullpath          = lv_fullpath
                                                           user_action       = lv_action
                                                EXCEPTIONS OTHERS            = 4                    ).

    IF sy-subrc <> 0
    OR lv_action = cl_gui_frontend_services=>action_cancel
    OR lv_fullpath IS INITIAL.
      MESSAGE 'Canceled' TYPE 'S' DISPLAY LIKE 'W'.
      RETURN.
    ENDIF.

* ---------------------------------------------------------------------
    mo_ftp->download_table( EXPORTING iv_filename    = iv_fname
                            IMPORTING et_bin_data    = lt_bin_data
                                      ev_blob_length = lv_blob_length ).

* ---------------------------------------------------------------------
    cl_gui_frontend_services=>gui_download( EXPORTING  bin_filesize = lv_blob_length
                                                       filename     = lv_fullpath
                                                       filetype     = 'BIN'
                                            CHANGING   data_tab     = lt_bin_data
                                            EXCEPTIONS OTHERS       = 4               ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                 WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

* ---------------------------------------------------------------------
  ENDMETHOD.

ENDCLASS.
