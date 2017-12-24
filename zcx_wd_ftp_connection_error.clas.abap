class ZCX_WD_FTP_CONNECTION_ERROR definition
  public
  inheriting from ZCX_WD_FTP_ERROR
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MS_SY type SYST optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_WD_FTP_CONNECTION_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
MS_SY = MS_SY
.
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
