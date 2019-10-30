*&---------------------------------------------------------------------*
*& Report ZML_R_LANG_DETECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zml_r_lang_detection.

DATA mo_lang_detection TYPE REF TO zcl_ml_lang_detection.


PARAMETERS p_text TYPE c LENGTH 60 LOWER CASE DEFAULT 'hello'.

START-OF-SELECTION.

  " Se instancia la clase encarga de detectar el idioma
  mo_lang_detection = NEW zcl_ml_lang_detection( ).

  " Se rellena la estructura de envio de datos
  DATA(ls_response) = VALUE zcl_ml_lang_detection=>ts_request( ).

  TRY.
      mo_lang_detection->zif_ml_services~call_api(
        EXPORTING
          is_request  = VALUE zcl_ml_lang_detection=>ts_request( message = p_text )
      IMPORTING
        es_response =  ls_response ).
    CATCH zcx_ml_api INTO DATA(lo_excep).
      DATA(lv_msg) = lo_excep->get_text( ).
      WRITE:/ lv_msg.
  ENDTRY.
