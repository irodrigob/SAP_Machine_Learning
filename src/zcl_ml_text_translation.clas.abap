CLASS zcl_ml_text_translation DEFINITION
  PUBLIC
  INHERITING FROM zcl_ml_services
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_target_languages TYPE STANDARD TABLE OF string WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_units,
        value TYPE string,
        key   TYPE string,
      END OF ts_units .
    TYPES:
      tt_units TYPE STANDARD TABLE OF ts_units WITH EMPTY KEY .
    TYPES:
      BEGIN OF ts_request,
        source_language  TYPE string,
        target_languages TYPE tt_target_languages,
        units            TYPE tt_units,
      END OF ts_request .


    TYPES:
      BEGIN OF ts_translations,
        language TYPE string,
        value    TYPE string,
      END OF ts_translations .
    TYPES:
      tt_translations TYPE STANDARD TABLE OF ts_translations WITH EMPTY KEY .
    TYPES: BEGIN OF ts_units_response,
             value        TYPE string,
             key          TYPE string,
             translations TYPE tt_translations,
           END OF ts_units_response.
    TYPES: tt_units_response TYPE STANDARD TABLE OF ts_units_response WITH EMPTY KEY.
    TYPES:
      BEGIN OF ts_response,
        units TYPE tt_units_response,
      END OF ts_response .

    METHODS constructor
      IMPORTING
        !iv_langu TYPE sylangu DEFAULT sy-langu .

    METHODS zif_ml_services~call_api
        REDEFINITION .
  PROTECTED SECTION.
    METHODS fill_configuration REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ML_TEXT_TRANSLATION IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_langu = iv_langu ).

    " Lectura de la configuración
    fill_configuration(  ).
  ENDMETHOD.


  METHOD fill_configuration.
    " Se pasa la configuración del servicio a la clase encargada de hacer las llamadas
    mo_rest_api->set_api_configuration( is_services_conf = VALUE #( http_method = if_http_request=>co_request_method_post
                                                                    resource = |{ zif_ml_data=>cs_api_connection-url }/mlfs/api/v2/text/translation|
                                                                    api_key = |VIBTf10lAEGWm6Ae0KAZC8aong7BtNd3|
                                                                    accept = |application/json| ) ).
  ENDMETHOD.


  METHOD zif_ml_services~call_api.


    DATA ls_response TYPE ts_response.

    CLEAR: es_service_response.
    es_service_response-there_error = abap_false.

    " Los idiomas a traducir se pasan en minisculas para que funcione correctamente.
    DATA(ls_request) = CORRESPONDING ts_request( is_request ).
    ls_request-source_language = |{ ls_request-source_language CASE = LOWER }|.
    LOOP AT ls_request-target_languages ASSIGNING FIELD-SYMBOL(<ls_languages>).
      <ls_languages> = |{ <ls_languages> CASE = LOWER }|.
    ENDLOOP.

    " Se rellena la estructura para el envio de datos REST
    DATA(ls_request_api) = VALUE zcl_ml_rest_api=>ts_request_api( request_type = if_rest_media_type=>gc_appl_json
                                                                  post-body = /ui2/cl_json=>serialize( data = ls_request pretty_name = /ui2/cl_json=>pretty_mode-camel_case ) ).

    TRY.

        mo_rest_api->call_api( EXPORTING is_request  = ls_request_api
                               IMPORTING es_response = DATA(ls_response_serv) ).

        " Si hay error se notifica en el parámetro de salida y se devuelve el error
        IF ls_response_serv-there_error = abap_true.
          es_service_response-there_error = abap_true.
          es_service_response-error_response = ls_response_serv-error_response.

        ELSE.
          " Se devuelve la estructura para que pueda ser usada dinamicamente
          es_service_response-response_structure = |ZCL_ML_TEXT_TRANSLATION=>TS_RESPONSE|.


          " Se crea la variable donde se guardará el resultado
          CREATE DATA es_service_response-response TYPE (es_service_response-response_structure).
          ASSIGN es_service_response-response->* TO FIELD-SYMBOL(<response>).

          " Si no hay error se convierte el string a la estructura de salida
          " Este método por la definición data no permite usar punteros, por ello hay que usar una variable local

          /ui2/cl_json=>deserialize( EXPORTING json = ls_response_serv-response
                                               pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                                     CHANGING data = ls_response ).

          " Luego de la variable local se mueve al puntero
          <response> = ls_response.

        ENDIF.



      CATCH zcx_ml_api INTO DATA(lo_excep).
        " Para la excepcion de la API propaga la misma excepcion captura
        RAISE EXCEPTION TYPE zcx_ml_api
          EXPORTING
            textid = lo_excep->textid.
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
