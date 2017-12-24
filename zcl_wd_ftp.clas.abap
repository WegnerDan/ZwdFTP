CLASS zcl_wd_ftp DEFINITION PUBLIC FINAL CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS:
      mc_blob_length TYPE i VALUE 1024 ##NO_TEXT.
    TYPES:
      mty_type     TYPE c LENGTH 1,
      mty_filesize TYPE i,
      mty_filename TYPE c LENGTH 255,
      mty_filedate TYPE d,
      BEGIN OF mty_directory_listing,
        type     TYPE mty_type,
        filesize TYPE mty_filesize,
        filedate TYPE mty_filedate,
        filename TYPE mty_filename,
      END OF mty_directory_listing ,
      mty_directory_listing_tt TYPE STANDARD TABLE OF mty_directory_listing WITH DEFAULT KEY,
      mty_blob                 TYPE x LENGTH mc_blob_length,
      mty_blob_tt              TYPE STANDARD TABLE OF mty_blob WITH DEFAULT KEY,
      mty_username             TYPE c LENGTH 100,
      mty_password             TYPE c LENGTH 100,
      mty_hostname             TYPE c LENGTH 100,
      mty_directory            TYPE c LENGTH 500,
      mty_command              TYPE c LENGTH 1000,
      mty_command_tt           TYPE STANDARD TABLE OF mty_command WITH DEFAULT KEY.
    CONSTANTS:
      mc_type_dir  TYPE mty_type VALUE 'D' ##NO_TEXT,
      mc_type_file TYPE mty_type VALUE 'F' ##NO_TEXT,
      mc_type_link TYPE mty_type VALUE 'L' ##NO_TEXT.
    CLASS-METHODS:
      scramble_pw IMPORTING !iv_password       TYPE mty_password
                  RETURNING VALUE(rv_password) TYPE mty_password.
    METHODS:
      constructor IMPORTING !iv_hostname            TYPE mty_hostname
                            !iv_port                TYPE i DEFAULT 21
                            !iv_username            TYPE mty_username
                            !iv_password            TYPE mty_password
                            !iv_connect_immediately TYPE abap_bool DEFAULT abap_true
                            !iv_scramble_password   TYPE abap_bool DEFAULT abap_true
                  RAISING   zcx_wd_ftp_error,
      connect RAISING zcx_wd_ftp_error,
      disconnect,
      is_connected RETURNING VALUE(rv_connected) TYPE abap_bool,
      get_directory_listing IMPORTING !iv_only_files    TYPE abap_bool DEFAULT abap_false
                            RETURNING VALUE(rt_listing) TYPE mty_directory_listing_tt,
      upload_xstring IMPORTING !iv_filename TYPE mty_filename
                               !iv_bin_data TYPE xstring
                     RAISING   zcx_wd_ftp_error,
      upload_table IMPORTING !iv_filename    TYPE mty_filename
                             !it_bin_data    TYPE mty_blob_tt
                             !iv_blob_length TYPE i
                   RAISING   zcx_wd_ftp_error,
      download_xstring IMPORTING !iv_filename       TYPE mty_filename
                       RETURNING VALUE(rv_bin_data) TYPE xstring,
      download_table IMPORTING !iv_filename    TYPE mty_filename
                     EXPORTING !et_bin_data    TYPE mty_blob_tt
                               !ev_blob_length TYPE i,
      delete IMPORTING !iv_filename TYPE char255
             RAISING   zcx_wd_ftp_error,
      change_directory IMPORTING !iv_directory TYPE mty_directory
                       RAISING   zcx_wd_ftp_error,
      get_current_directory RETURNING VALUE(rv_directory) TYPE mty_directory.
  PROTECTED SECTION.

    TYPES:
      mty_ftp_data    TYPE c LENGTH 1000,
      mty_ftp_data_tt TYPE STANDARD TABLE OF mty_ftp_data WITH DEFAULT KEY,
      BEGIN OF pattern_r,
        sign   TYPE char1,
        option TYPE char2,
        low    TYPE c LENGTH 100,
        high   TYPE c LENGTH 100,
      END OF pattern_r,
      pattern_r_tt TYPE STANDARD TABLE OF pattern_r WITH DEFAULT KEY.
    CONSTANTS:
      mc_scrambling_key TYPE i VALUE 26101957 ##NO_TEXT.
    DATA:
      mv_current_directory TYPE mty_directory,
      mv_handle            TYPE i,
      mv_hostname          TYPE mty_hostname,
      mv_port              TYPE i,
      mv_username          TYPE mty_username,
      mv_password          TYPE mty_password.
    METHODS:
      split_dir IMPORTING !iv_directory TYPE mty_directory
                RETURNING VALUE(et_cmd) TYPE mty_command_tt,
      convert_ftp_date IMPORTING !iv_date       TYPE char30
                       RETURNING VALUE(ev_date) TYPE sy-datum,
      convert_dir_listing IMPORTING !iv_only_files  TYPE abap_bool
                                    !it_ftp_data    TYPE mty_ftp_data_tt
                          RETURNING VALUE(rt_files) TYPE mty_directory_listing_tt,
      convert_dir_listing_line IMPORTING !iv_line       TYPE mty_ftp_data
                               RETURNING VALUE(rs_line) TYPE mty_directory_listing,
      set_passive_mode RAISING zcx_wd_ftp_error,
      set_current_directory RAISING zcx_wd_ftp_error.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_wd_ftp IMPLEMENTATION.


  METHOD change_directory.
* ----------------------------------------------------------------------
    DATA:
      lv_command TYPE mty_command,
      lv_cmd_len TYPE i,
      lt_cmd     TYPE mty_command_tt,
      lt_data    TYPE mty_ftp_data_tt.
    FIELD-SYMBOLS:
      <lv_cmd> TYPE mty_command.

* ----------------------------------------------------------------------
    lv_cmd_len = strlen( iv_directory ) + 3.
    IF lv_cmd_len <= 100.
      CONCATENATE 'cd' iv_directory INTO lv_command SEPARATED BY space.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          handle        = mv_handle
          command       = lv_command
*         COMPRESS      =
*         RFC_DESTINATION       =
*         VERIFY        =
*     IMPORTING
*         FILESIZE      =
*         FILEDATE      =
*         FILETIME      =
        TABLES
          data          = lt_data
        EXCEPTIONS
          tcpip_error   = 1
          command_error = 2
          data_error    = 3
          OTHERS        = 4.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = sy.
      ENDIF.
    ELSE.
      lt_cmd = split_dir( iv_directory ).
      LOOP AT lt_cmd ASSIGNING <lv_cmd>.
        CALL FUNCTION 'FTP_COMMAND'
          EXPORTING
            handle        = mv_handle
            command       = <lv_cmd>
*           COMPRESS      =
*           RFC_DESTINATION       =
*           VERIFY        =
*     IMPORTING
*           FILESIZE      =
*           FILEDATE      =
*           FILETIME      =
          TABLES
            data          = lt_data
          EXCEPTIONS
            tcpip_error   = 1
            command_error = 2
            data_error    = 3
            OTHERS        = 4.
        IF sy-subrc <> 0.
          RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = sy.
        ENDIF.
      ENDLOOP.
    ENDIF.

* ----------------------------------------------------------------------
    set_current_directory( ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD connect.
* ----------------------------------------------------------------------
    DATA:
      lv_hostname LIKE mv_hostname.

* ----------------------------------------------------------------------
    IF mv_port <> 21.
      lv_hostname = mv_hostname && ` ` && mv_port.
    ELSE.
      lv_hostname = mv_hostname.
    ENDIF.

* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_CONNECT'
      EXPORTING
        user            = mv_username
        password        = mv_password
        host            = mv_hostname
        rfc_destination = 'SAPFTPA'   " always connect from application server
      IMPORTING
        handle          = mv_handle
      EXCEPTIONS
        not_connected   = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = sy.
    ENDIF.


* ----------------------------------------------------------------------
    TRY.
        set_passive_mode( ).

        set_current_directory( ).
      CATCH zcx_wd_ftp_error INTO DATA(lx_err).
        RAISE EXCEPTION lx_err.
    ENDTRY.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD constructor.
* ----------------------------------------------------------------------
    mv_hostname = iv_hostname.
    mv_port     = iv_port.
    mv_username = iv_username.
    IF iv_scramble_password = abap_true.
      mv_password = scramble_pw( iv_password ).
    ELSE.
      mv_password = iv_password.
    ENDIF.

* ----------------------------------------------------------------------
    IF iv_connect_immediately = abap_true.
      TRY.
          connect( ).
        CATCH zcx_wd_ftp_error INTO DATA(lx_conn_error).
          RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = lx_conn_error->ms_sy.
      ENDTRY.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD convert_dir_listing.
* ----------------------------------------------------------------------
* logic copied from method GET_FILE_LISTING_UNIX class CL_CFX_FTP_FILEPROVIDER
* ----------------------------------------------------------------------
    CONSTANTS:
      lc_prefix_dir  TYPE c LENGTH 1 VALUE 'd',
      lc_prefix_link TYPE c LENGTH 1 VALUE 'l',
      lc_prefix_file TYPE c LENGTH 1 VALUE '-'.
    DATA:
      lv_start    TYPE i,
      lv_end      TYPE i,
      lv_linelen  TYPE i,
      lv_totalstr TYPE c LENGTH 5,
      ls_file     TYPE mty_directory_listing.
    FIELD-SYMBOLS:
      <lv_textline> TYPE mty_ftp_data.

* ----------------------------------------------------------------------
    lv_start = 1.
    lv_end = 0.
    LOOP AT it_ftp_data ASSIGNING <lv_textline>.
      IF lv_start = 1.
        ADD 1 TO lv_start.
        CONTINUE.                            " repeat the command
      ENDIF.
      lv_linelen = strlen( <lv_textline> ).

      IF ( lv_linelen > 4 ) AND
         ( ( <lv_textline>(4) = '200 ' ) OR
         ( <lv_textline>(4) = '150 ' ) ).     " initial status
        ADD 1 TO lv_start.
        CONTINUE.
      ENDIF.
      IF lv_linelen > 5.
        lv_totalstr = <lv_textline>(5).
        TRANSLATE lv_totalstr TO UPPER CASE.
        IF lv_totalstr = 'TOTAL'.
          ADD 1 TO lv_start.
          CONTINUE.                      " length sum line
        ENDIF.
      ENDIF.
      IF ( lv_linelen > 4 ) AND
         ( <lv_textline>(4) = '226 ' ).
        EXIT.
      ENDIF.
      " ok, listing line
      IF lv_end = 0.
        lv_end = lv_start.
      ELSE.
        ADD 1 TO lv_end.
      ENDIF.
    ENDLOOP.

    IF lv_end < lv_start.   " nothing
      RETURN.
    ENDIF.

    " extract
    LOOP AT it_ftp_data ASSIGNING <lv_textline> FROM lv_start TO lv_end.
      ls_file = convert_dir_listing_line( <lv_textline> ).
      CASE <lv_textline>(1).
        WHEN lc_prefix_dir. " Directory
          IF  NOT iv_only_files = abap_true
          AND NOT ls_file-filename = '.'
          AND NOT ls_file-filename = '..'.
            ls_file-type     = mc_type_dir.
            ls_file-filesize = '0'.
          ENDIF.
        WHEN lc_prefix_link. " Link
          ls_file-type = mc_type_link.
        WHEN lc_prefix_file. " File
          ls_file-type = mc_type_file.
      ENDCASE.
      IF ls_file-type = mc_type_dir
      OR ls_file-type = mc_type_file
      OR ls_file-type = mc_type_link.
        APPEND ls_file TO rt_files.
      ENDIF.
    ENDLOOP.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD convert_dir_listing_line.
* ----------------------------------------------------------------------
* logic copied from method GET_VALUES_FROM_LINE class CL_CFX_FTP_FILEPROVIDER
* ----------------------------------------------------------------------
    DATA:
      lv_f1    TYPE mty_ftp_data,
      lv_f2    TYPE mty_ftp_data,
      lv_f3    TYPE mty_ftp_data,
      lv_f4    TYPE mty_ftp_data,
      lv_f5    TYPE mty_ftp_data,
      lv_f6    TYPE mty_ftp_data,
      lv_f7    TYPE mty_ftp_data,
      lv_f8    TYPE mty_ftp_data,
      lv_ftail TYPE mty_ftp_data,
      lv_date  TYPE c LENGTH 30.

* ----------------------------------------------------------------------
    " 1
    IF iv_line IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT iv_line AT space INTO lv_f1 lv_ftail.
    SHIFT lv_ftail LEFT DELETING LEADING space.
    " 2
    IF lv_ftail IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT lv_ftail AT space INTO lv_f2 lv_ftail.
    SHIFT lv_ftail LEFT DELETING LEADING space.
    " 3
    IF lv_ftail IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT lv_ftail AT space INTO lv_f3 lv_ftail.
    SHIFT lv_ftail LEFT DELETING LEADING space.
    " 4
    IF lv_ftail IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT lv_ftail AT space INTO lv_f4 lv_ftail.
    SHIFT lv_ftail LEFT DELETING LEADING space.
    " 5
    IF lv_ftail IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT lv_ftail AT space INTO lv_f5 lv_ftail.
    SHIFT lv_ftail LEFT DELETING LEADING space.
    " 6
    IF lv_ftail IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT lv_ftail AT space INTO lv_f6 lv_ftail.
    SHIFT lv_ftail LEFT DELETING LEADING space.
    " 7
    IF lv_ftail IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT lv_ftail AT space INTO lv_f7 lv_ftail.
    SHIFT lv_ftail LEFT DELETING LEADING space.
    " 8
    IF lv_ftail IS INITIAL.
      RETURN.
    ENDIF.
    SPLIT lv_ftail AT space INTO lv_f8 lv_ftail.
    SHIFT lv_ftail LEFT DELETING LEADING space.

    FREE rs_line.

    IF lv_f5 = 'Jan'
    OR lv_f5 = 'Feb'
    OR lv_f5 = 'Mar'
    OR lv_f5 = 'Apr'
    OR lv_f5 = 'May'
    OR lv_f5 = 'Jun'
    OR lv_f5 = 'Jul'
    OR lv_f5 = 'Aug'
    OR lv_f5 = 'Sep'
    OR lv_f5 = 'Oct'
    OR lv_f5 = 'Nov'
    OR lv_f5 = 'Dec'.
      rs_line-filesize = lv_f4.
      IF iv_line(1) = 'l'.
        SPLIT lv_f8 AT ` -> ` INTO rs_line-filename lv_ftail.
      ELSE.
        rs_line-filename = lv_f8.
      ENDIF.
      CONCATENATE lv_f5 lv_f6 lv_f7 INTO lv_date SEPARATED BY space.
    ENDIF.

    IF lv_f6 = 'Jan'
    OR lv_f6 = 'Feb'
    OR lv_f6 = 'Mar'
    OR lv_f6 = 'Apr'
    OR lv_f6 = 'May'
    OR lv_f6 = 'Jun'
    OR lv_f6 = 'Jul'
    OR lv_f6 = 'Aug'
    OR lv_f6 = 'Sep'
    OR lv_f6 = 'Oct'
    OR lv_f6 = 'Nov'
    OR lv_f6 = 'Dec'.
      rs_line-filesize = lv_f5.
      IF iv_line(1) = 'l'.
        SPLIT lv_ftail AT ` -> ` INTO rs_line-filename lv_ftail.
      ELSE.
        rs_line-filename = lv_ftail.
      ENDIF.
      CONCATENATE lv_f6 lv_f7 lv_f8 INTO lv_date SEPARATED BY space.
    ENDIF.
    rs_line-filedate = convert_ftp_date( lv_date ).

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD convert_ftp_date.
* ----------------------------------------------------------------------
    DATA:
      lv_month       TYPE c LENGTH 3,
      lv_day         TYPE c LENGTH 2,
      lv_year        TYPE c LENGTH 5,
      lv_match_count TYPE i.

* ----------------------------------------------------------------------
    SPLIT iv_date AT space INTO lv_month lv_day lv_year.

* ----------------------------------------------------------------------
    " determine month
    CASE lv_month.
      WHEN 'Jan'.
        ev_date+4(2) = '01'.
      WHEN 'Feb'.
        ev_date+4(2) = '02'.
      WHEN 'Mar'.
        ev_date+4(2) = '03'.
      WHEN 'Apr'.
        ev_date+4(2) = '04'.
      WHEN 'May'.
        ev_date+4(2) = '05'.
      WHEN 'Jun'.
        ev_date+4(2) = '06'.
      WHEN 'Jul'.
        ev_date+4(2) = '07'.
      WHEN 'Aug'.
        ev_date+4(2) = '08'.
      WHEN 'Sep'.
        ev_date+4(2) = '09'.
      WHEN 'Oct'.
        ev_date+4(2) = '10'.
      WHEN 'Nov'.
        ev_date+4(2) = '11'.
      WHEN 'Dec'.
        ev_date+4(2) = '12'.
      WHEN OTHERS.
        ev_date+4(2) = '00'.
    ENDCASE.

* ----------------------------------------------------------------------
    " determine day
    IF lv_day IS NOT INITIAL.
      ev_date+6(2) = lv_day.
    ELSE.
      ev_date+6(2) = '00'.
    ENDIF.

* ----------------------------------------------------------------------
    " determine year
    " If there's a colon in lv_year it is a timestamp in hours:minutes and it means
    " the file/dir is younger than 6 month --> it's from the current year
    FIND ALL OCCURRENCES OF ':' IN lv_year MATCH COUNT lv_match_count.
    IF lv_match_count > 0.
      ev_date(4) = sy-datum(4).
    ELSE.
      ev_date(4) = lv_year.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD delete.
* ----------------------------------------------------------------------
    DATA:
      lv_cmd  TYPE mty_command,
      lt_data TYPE mty_ftp_data_tt.

* ----------------------------------------------------------------------
*    CONCATENATE '"' iv_filename '"' INTO lv_cmd.
*    CONCATENATE 'delete' lv_cmd INTO lv_cmd SEPARATED BY space.
    lv_cmd = 'delete "' && iv_filename && '"'.
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle        = mv_handle
        command       = lv_cmd
*       COMPRESS      =
*       RFC_DESTINATION       =
*       VERIFY        =
*     IMPORTING
*       FILESIZE      =
*       FILEDATE      =
*       FILETIME      =
      TABLES
        data          = lt_data
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.

* ----------------------------------------------------------------------
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = sy.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD disconnect.
* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_DISCONNECT'
      EXPORTING
        handle = mv_handle.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD download_table.
* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle        = mv_handle
        fname         = iv_filename
*       character_mode =
      IMPORTING
        blob_length   = ev_blob_length
      TABLES
        blob          = et_bin_data
*       text          =
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD download_xstring.
* ----------------------------------------------------------------------
    DATA:
      lv_blob_length TYPE i,
      lt_bin_data    TYPE mty_blob_tt.

* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_SERVER_TO_R3'
      EXPORTING
        handle        = mv_handle
        fname         = iv_filename
*       character_mode =
      IMPORTING
        blob_length   = lv_blob_length
      TABLES
        blob          = lt_bin_data
*       text          =
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.

* ----------------------------------------------------------------------
    IF sy-subrc = 0.
      CONCATENATE LINES OF lt_bin_data INTO rv_bin_data IN BYTE MODE.
      rv_bin_data = rv_bin_data(lv_blob_length).
      FREE lt_bin_data.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_current_directory.
* ----------------------------------------------------------------------
    rv_directory = mv_current_directory.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD get_directory_listing.
* ----------------------------------------------------------------------
    DATA:
      lt_data TYPE mty_ftp_data_tt.

* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle        = mv_handle
        command       = 'dir'
*       COMPRESS      =
*       RFC_DESTINATION       =
*       VERIFY        =
*     IMPORTING
*       FILESIZE      =
*       FILEDATE      =
*       FILETIME      =
      TABLES
        data          = lt_data
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.

* ----------------------------------------------------------------------
    " convert LT_DATA to RT_LISTING
    IF sy-subrc = 0.
      rt_listing = convert_dir_listing( iv_only_files = iv_only_files
                                        it_ftp_data   = lt_data       ).
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD is_connected.
* ----------------------------------------------------------------------
    DATA:
      lt_data          TYPE mty_ftp_data_tt.

* ----------------------------------------------------------------------
    IF mv_handle IS NOT INITIAL.
      CALL FUNCTION 'FTP_COMMAND'
        EXPORTING
          handle        = mv_handle
          command       = 'pwd'
        TABLES
          data          = lt_data
        EXCEPTIONS
          tcpip_error   = 1
          command_error = 2
          data_error    = 3
          OTHERS        = 4.
      IF sy-subrc = 0.
        rv_connected = abap_true.
      ELSE.
        rv_connected = abap_false.
      ENDIF.
    ELSE.
      rv_connected = abap_false.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD scramble_pw.
* ----------------------------------------------------------------------
    DATA:
      lv_pw_length TYPE i.

* ----------------------------------------------------------------------
    FREE: lv_pw_length, rv_password.

* ----------------------------------------------------------------------
    lv_pw_length = strlen( iv_password ).
    CALL FUNCTION 'HTTP_SCRAMBLE'
      EXPORTING
        source      = iv_password
        sourcelen   = lv_pw_length
        key         = mc_scrambling_key
      IMPORTING
        destination = rv_password.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_current_directory.
* ----------------------------------------------------------------------
    DATA:
      lt_data          TYPE mty_ftp_data_tt,
      lv_match_count   TYPE i,
      lt_match_results TYPE match_result_tab,
      lv_dir_pos       TYPE i,
      lv_dir_length    TYPE i.
    FIELD-SYMBOLS:
      <lv_data>     TYPE mty_ftp_data,
      <ls_result_1> TYPE match_result,
      <ls_result_2> TYPE match_result.

* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle        = mv_handle
        command       = 'pwd'
      TABLES
        data          = lt_data
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = sy.
    ENDIF.

* ----------------------------------------------------------------------
    LOOP AT lt_data ASSIGNING <lv_data> WHERE table_line CP '257*'.
      FIND ALL OCCURRENCES OF '"' IN <lv_data> MATCH COUNT lv_match_count RESULTS lt_match_results.
      IF lv_match_count = 2.
        READ TABLE lt_match_results ASSIGNING <ls_result_1> INDEX 1.
        READ TABLE lt_match_results ASSIGNING <ls_result_2> INDEX 2.
        IF  <ls_result_1> IS ASSIGNED
        AND <ls_result_2> IS ASSIGNED.
          lv_dir_pos = <ls_result_1>-offset + 1.
          lv_dir_length = <ls_result_2>-offset - <ls_result_1>-offset - 1.
          mv_current_directory = <lv_data>+lv_dir_pos(lv_dir_length).
        ENDIF.
      ELSE.
        RAISE EXCEPTION TYPE zcx_wd_ftp_error.
      ENDIF.
    ENDLOOP.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD set_passive_mode.
* ----------------------------------------------------------------------
    DATA:
      lt_data TYPE mty_ftp_data_tt.

* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_COMMAND'
      EXPORTING
        handle        = mv_handle
        command       = 'set passive on'
      TABLES
        data          = lt_data
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = sy.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD split_dir.
* ----------------------------------------------------------------------
    CONSTANTS:
      lc_divider TYPE c LENGTH 1 VALUE '/'.
    DATA:
      lv_tmp_dir  TYPE mty_directory,
      lt_dir      TYPE STANDARD TABLE OF mty_directory,
      lv_cmd_tmp  TYPE mty_command,
      lv_cmd      TYPE mty_command,
      lv_cmd_len  TYPE i,
      lv_len_left TYPE i.
    FIELD-SYMBOLS:
      <lv_dir> TYPE mty_directory.

* ----------------------------------------------------------------------
    SPLIT iv_directory AT lc_divider INTO TABLE lt_dir.
    LOOP AT lt_dir ASSIGNING <lv_dir>.
      IF ( <lv_dir> IS INITIAL
      OR lv_cmd IS NOT INITIAL )
      AND lv_cmd <> lc_divider.
        CONCATENATE lv_cmd lc_divider <lv_dir> INTO lv_cmd_tmp.
      ELSEIF lv_cmd = lc_divider.
        CONCATENATE lc_divider <lv_dir> INTO lv_cmd_tmp.
      ELSE.
        lv_cmd_tmp = <lv_dir>.
      ENDIF.
      lv_cmd_len = strlen( lv_cmd_tmp ).
      lv_len_left = 97 - lv_cmd_len.
      IF  lv_len_left < 10
      AND lv_len_left > 0.
        lv_cmd = lv_cmd_tmp.
        CONCATENATE 'cd' lv_cmd INTO lv_cmd SEPARATED BY space.
        APPEND lv_cmd TO et_cmd.
        FREE: lv_cmd, lv_cmd_tmp.
      ELSEIF  lv_len_left < 0.
        CONCATENATE 'cd' lv_cmd INTO lv_cmd SEPARATED BY space.
        APPEND lv_cmd TO et_cmd.
        FREE: lv_cmd, lv_cmd_tmp.
        lv_cmd = <lv_dir>.
      ELSE.
        lv_cmd = lv_cmd_tmp.
      ENDIF.
    ENDLOOP.
    IF lv_cmd IS NOT INITIAL.
      CONCATENATE 'cd' lv_cmd INTO lv_cmd SEPARATED BY space.
      APPEND lv_cmd TO et_cmd.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD upload_table.
* ----------------------------------------------------------------------
    DATA:
      lv_blob_length TYPE i,
      lt_blob        TYPE mty_blob_tt.

* ----------------------------------------------------------------------
    lv_blob_length = iv_blob_length.
    lt_blob        = it_bin_data.

* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle        = mv_handle
        fname         = iv_filename
        blob_length   = lv_blob_length
      TABLES
        blob          = lt_blob
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = sy.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.


  METHOD upload_xstring.
* ----------------------------------------------------------------------
    DATA:
      lv_blob_length TYPE i,
      lv_offset      TYPE i,
      lt_blob        TYPE mty_blob_tt.
    FIELD-SYMBOLS:
      <lv_blob> TYPE mty_blob.

* ----------------------------------------------------------------------
    lv_blob_length = xstrlen( iv_bin_data ).
    WHILE lv_offset <= lv_blob_length.
      UNASSIGN <lv_blob>.
      APPEND INITIAL LINE TO lt_blob ASSIGNING <lv_blob>.
      IF <lv_blob> IS ASSIGNED.
        IF ( lv_blob_length - lv_offset ) > mc_blob_length.
          <lv_blob> = iv_bin_data+lv_offset(mc_blob_length).
          lv_offset = lv_offset + mc_blob_length.
        ELSE.
          <lv_blob> = iv_bin_data+lv_offset.
          lv_offset = lv_offset + mc_blob_length.
        ENDIF.
      ENDIF.
    ENDWHILE.

* ----------------------------------------------------------------------
    CALL FUNCTION 'FTP_R3_TO_SERVER'
      EXPORTING
        handle        = mv_handle
        fname         = iv_filename
        blob_length   = lv_blob_length
*       CHARACTER_MODE =
      TABLES
        blob          = lt_blob
*       TEXT          =
      EXCEPTIONS
        tcpip_error   = 1
        command_error = 2
        data_error    = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_wd_ftp_error EXPORTING ms_sy = sy.
    ENDIF.

* ----------------------------------------------------------------------
  ENDMETHOD.
ENDCLASS.
