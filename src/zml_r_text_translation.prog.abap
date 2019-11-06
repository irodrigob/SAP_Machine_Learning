*&---------------------------------------------------------------------*
*& Report ZML_R_LANG_DETECTION
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zml_r_text_translation.

FIELD-SYMBOLS <response> TYPE zcl_ml_text_translation=>ts_response.
DATA mo_text_translation TYPE REF TO zcl_ml_text_translation.


PARAMETERS p_slangu TYPE laiso LOWER CASE OBLIGATORY.
PARAMETERS p_tlangu TYPE laiso LOWER CASE OBLIGATORY.
PARAMETERS p_text TYPE c LENGTH 60 LOWER CASE DEFAULT 'hello'.

START-OF-SELECTION.

  " Se instancia la clase encarga de detectar el idioma
  mo_text_translation = NEW zcl_ml_text_translation( ).



  TRY.
      mo_text_translation->zif_ml_services~call_api(
        EXPORTING
          is_request  = VALUE zcl_ml_text_translation=>ts_request( source_language = p_slangu
                                                                      target_languages = VALUE #( ( |{ p_tlangu }| ) )
                                                                      units = VALUE #( ( value = p_text
                                                                                       key = |{ p_text }| ) ) )
      IMPORTING
        es_service_response = DATA(ls_service_response) ).

      IF ls_service_response-there_error = abap_false.
        ASSIGN ls_service_response-response->* TO <response>.
        WRITE:/ 'Result of service -->'.
        LOOP AT <response>-units ASSIGNING FIELD-SYMBOL(<ls_units>).
          WRITE: / 'Source text: ', <ls_units>-value,
                 / 'Key: ', <ls_units>-key.

          LOOP AT <ls_units>-translations ASSIGNING FIELD-SYMBOL(<ls_translations>).
            WRITE: / 'Target language: ', <ls_translations>-language,
                   / 'Target text: ', <ls_translations>-value.
          ENDLOOP.
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
