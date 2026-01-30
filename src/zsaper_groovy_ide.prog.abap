*&---------------------------------------------------------------------*
*& Report ZSAPER_GROOVY_IDE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSAPER_GROOVY_IDE.

INCLUDE ZSAPER_GROOVY_IDE_TOP.
INCLUDE ZSAPER_GROOVY_IDE_CLS.
INCLUDE ZSAPER_GROOVY_IDE_IO.

START-OF-SELECTION.
  go_model = NEW #( ).
  go_model->display_ide( ).
