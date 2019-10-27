CLASS zcl_ml_lang_detection DEFINITION
  PUBLIC
  INHERITING FROM zcl_ml_api_base
  FINAL
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
    METHODS call_api REDEFINITION.

  PROTECTED SECTION.
    METHODS fill_configuration REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ml_lang_detection IMPLEMENTATION.


  METHOD call_api.
    " Primer paso es establecer la conexión
    create_connection(  ).

    " Se pasa los valores
    DATA(ls_request) = CORRESPONDING ts_request( is_request ).

    " Se convierte la estructura en un JSON para poderlo pasar al body

    " Se rellena la estructura para el envio de datos REST
    DATA(ls_request_api) = VALUE ts_request_api( request_type = if_rest_media_type=>gc_appl_json
                                                  post-body = /ui2/cl_json=>serialize( data = ls_request pretty_name = /ui2/cl_json=>pretty_mode-camel_case ) ).
    " Se envian los datos
    send_request_api( is_request = ls_request_api  ).

    " Se recuperarán
    get_response_api( IMPORTING es_response = DATA(ls_response) ).


  ENDMETHOD.


  METHOD constructor.
    super->constructor( iv_langu = iv_langu ).

    " Se llama al proceso para cargar la configuración
    fill_configuration(  ).
  ENDMETHOD.


  METHOD fill_configuration.
    zif_ml_resource_conf~mv_http_method = if_http_request=>co_request_method_post.
    zif_ml_resource_conf~mv_resource = |{ zif_ml_data=>cs_api_connection-url }/ml/api/v2alpha1/text/lang-detect/|.
    zif_ml_resource_conf~mv_api_key = 'VIBTf10lAEGWm6Ae0KAZC8aong7BtNd3'.
    zif_ml_resource_conf~mv_accept = 'application/json'.
  ENDMETHOD.
ENDCLASS.
