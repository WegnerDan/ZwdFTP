class ZCX_WD_FTP_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

  data MS_SY type SYST .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MS_SY type SYST optional .

  methods IF_MESSAGE~GET_TEXT
    redefinition .
  methods IF_MESSAGE~GET_LONGTEXT
    redefinition .
protected section.

  methods UPDATE_T100KEY_FROM_SYST .
private section.
ENDCLASS.



CLASS ZCX_WD_FTP_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MS_SY = MS_SY .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.


  METHOD if_message~get_longtext.
    update_t100key_from_syst( ).
    result = super->if_message~get_longtext( preserve_newlines ).
  ENDMETHOD.


  METHOD if_message~get_text.
    update_t100key_from_syst( ).
    result = super->if_message~get_text( ).
  ENDMETHOD.


  METHOD update_t100key_from_syst.
    if_t100_message~t100key-msgid = ms_sy-msgid.
    if_t100_message~t100key-msgno = ms_sy-msgno.
    if_t100_message~t100key-attr1 = ms_sy-msgv1.
    if_t100_message~t100key-attr2 = ms_sy-msgv2.
    if_t100_message~t100key-attr3 = ms_sy-msgv3.
    if_t100_message~t100key-attr4 = ms_sy-msgv4.
  ENDMETHOD.
ENDCLASS.
