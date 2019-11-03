CLASS zcl_ml_lang_detection DEFINITION
  PUBLIC
  INHERITING FROM zcl_ml_services
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_request,
             message TYPE string,
           END OF ts_request.
    TYPES: BEGIN OF ts_detections,
             lang_code  TYPE string,
             confidence TYPE string,
             lang_str   TYPE string,
           END OF ts_detections.
    TYPES: tt_detections TYPE STANDARD TABLE OF ts_detections WITH EMPTY KEY.
    TYPES: BEGIN OF ts_response,
             detections TYPE tt_detections,
           END OF ts_response.
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.
    METHODS zif_ml_services~call_api REDEFINITION.

  PROTECTED SECTION.
    METHODS fill_configuration REDEFINITION.


  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ML_LANG_DETECTION IMPLEMENTATION.


  METHOD constructor.
    super->constructor( iv_langu = iv_langu ).

    " Se llama al proceso para cargar la configuración
    fill_configuration(  ).
  ENDMETHOD.


  METHOD fill_configuration.

    " Se pasa la configuración del servicio a la clase encargada de hacer las llamadas
    mo_rest_api->set_api_configuration( is_services_conf = VALUE #( http_method = if_http_request=>co_request_method_post
                                                                    resource = |{ zif_ml_data=>cs_api_connection-url }/ml/api/v2alpha1/text/lang-detect|
                                                                    api_key = |VIBTf10lAEGWm6Ae0KAZC8aong7BtNd3|
                                                                    accept = |application/json| ) ).
  ENDMETHOD.


  METHOD zif_ml_services~call_api.
    DATA ls_response TYPE ts_response.

    CLEAR: es_service_response.
    es_service_response-there_error = abap_false.

    " Se pasa los valores
    DATA(ls_request) = CORRESPONDING ts_request( is_request ).

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
          es_service_response-response_structure = |ZCL_ML_LANG_DETECTION=>TS_RESPONSE|.


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
