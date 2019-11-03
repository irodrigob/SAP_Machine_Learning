*&---------------------------------------------------------------------*
*& Report ZML_R_LANG_DETECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zml_r_lang_detection.

FIELD-SYMBOLS <response> TYPE zcl_ml_lang_detection=>ts_response.
DATA mo_lang_detection TYPE REF TO zcl_ml_lang_detection.


PARAMETERS p_text TYPE c LENGTH 60 LOWER CASE DEFAULT 'hello'.

START-OF-SELECTION.

  " Se instancia la clase encarga de detectar el idioma
  mo_lang_detection = NEW zcl_ml_lang_detection( ).



  TRY.
      mo_lang_detection->zif_ml_services~call_api(
        EXPORTING
          is_request  = VALUE zcl_ml_lang_detection=>ts_request( message = p_text )
      IMPORTING
        es_service_response = DATA(ls_service_response) ).

      IF ls_service_response-there_error = abap_false.
        ASSIGN ls_service_response-response->* TO <response>.
        WRITE:/ 'Result of service -->'.

        LOOP AT <response>-detections ASSIGNING FIELD-SYMBOL(<ls_detections>).
          WRITE: / 'Language code: ', <ls_detections>-lang_code,
                 / 'Confidence: ', <ls_detections>-confidence,
                 / 'Language string: ', <ls_detections>-lang_str.
        ENDLOOP.
      ELSE.
        WRITE:/ 'An error has occurred -->',
              / 'Status: ', ls_service_response-error_response-status, ls_service_response-error_response-status_error,
              / 'Message: ', ls_service_response-error_response-message,
              / 'Exception: ', ls_service_response-error_response-exception.
      ENDIF.

    CATCH zcx_ml_api INTO DATA(lo_excep).
      DATA(lv_msg) = lo_excep->get_text( ).
      WRITE:/ lv_msg.
  ENDTRY.
