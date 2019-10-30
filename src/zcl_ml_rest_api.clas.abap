CLASS zcl_ml_rest_api DEFINITION
  PUBLIC
  INHERITING FROM zcl_ca_rest_http_services
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_services_conf,
             resource    TYPE string,
             api_key     TYPE string,
             http_method TYPE string,
             accept      TYPE string,
           END OF ts_services_conf.
    TYPES: BEGIN OF ts_request_api.
        INCLUDE TYPE zcl_ca_rest_http_services=>ts_request_rest.
    TYPES:
           END OF ts_request_api.
    TYPES: BEGIN OF ts_response_api,
             response       TYPE string,
             content_length TYPE string,
             content_type   TYPE string,
           END OF ts_response_api.

    "! <p class="shorttext synchronized" lang="en">CONSTRUCTOR</p>
    "! Constructor
    "! @parameter iv_langu | <p class="shorttext synchronized" lang="en">Langauge</p>
    METHODS constructor
      IMPORTING
        !iv_langu TYPE sylangu DEFAULT sy-langu .
    "! <p class="shorttext synchronized" lang="en">Call API</p>
    "! The values of IS_REQUEST and ES_RESPONSE depend on the class called.
    "! For the class: ZCL_ML_LANG_DETECTION:
    "!          IS_REQUEST TYPE ZCL_ML_LANG_DETECTION=>IS_REQUEST
    "!          ES_RESPONSE TYPE ZCL_ML_LANG_DETECTION=>IS_RESPONSE
    "! @parameter io_services_conf | <p class="shorttext synchronized" lang="en">Configuration of service</p>
    "! @parameter is_request | <p class="shorttext synchronized" lang="en">Request</p>
    "! @parameter es_response | <p class="shorttext synchronized" lang="en">Response</p>
    "! @raising zcx_ml_api | <p class="shorttext synchronized" lang="en"></p>
    METHODS call_api
      IMPORTING
        !is_request  TYPE zcl_ml_rest_api=>ts_request_api
      EXPORTING
        !es_response TYPE zcl_ml_rest_api=>ts_response_api
      RAISING
        zcx_ml_api .
    "! <p class="shorttext synchronized" lang="en"></p>
    "!
    "! @parameter io_api_configuration | <p class="shorttext synchronized" lang="en">Save the configuration of services</p>
    METHODS set_api_configuration
      IMPORTING
        !is_services_conf TYPE ts_services_conf.
  PROTECTED SECTION.



    DATA mv_langu TYPE sylangu .
    DATA ms_services_conf TYPE ts_services_conf.
    "! <p class="shorttext synchronized" lang="en">Fill configuration</p>
    "! Fill the configuration for the connection to the API
    METHODS fill_configuration.
    "! <p class="shorttext synchronized" lang="en">Validate configuration for the connection</p>
    "!
    "! @raising zcx_ml_api | <p class="shorttext synchronized" lang="en">zcx_ml_api</p>
    METHODS validate_conf_connection
      RAISING zcx_ml_api.

    "! <p class="shorttext synchronized" lang="en">Create connection to API</p>
    "!
    "! @raising zcx_ml_api | <p class="shorttext synchronized" lang="en">API Exceptions</p>
    METHODS create_connection
      RAISING zcx_ml_api.
    "! <p class="shorttext synchronized" lang="en">Send request API to REST client</p>
    "! Encapsulates the call to the REST service to do pre-treatment or error control
    "! @parameter is_request | <p class="shorttext synchronized" lang="en"></p>
    "! @raising zcx_ml_api | <p class="shorttext synchronized" lang="en"></p>
    METHODS send_request_api
      IMPORTING
                is_request TYPE ts_request_api
      RAISING   zcx_ml_api.
    METHODS get_response_api
      EXPORTING es_response TYPE ts_response_api.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ml_rest_api IMPLEMENTATION.


  METHOD call_api.
*    " Primer paso es establecer la conexión
    create_connection(  ).

    " Se envian los datos
    send_request_api( is_request = is_request  ).

    " Se recuperarán
    get_response_api( IMPORTING es_response = es_response ).
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    mv_langu = sy-langu.
  ENDMETHOD.


  METHOD create_connection.

    " Se valida la cofguración de la conexión
    validate_conf_connection(  ).

    TRY.

        " Se conecta con la API
*        create_rest_client_by_url( iv_url = ms_services_conf-resource ).
        create_rest_client( iv_host = |{ zif_ml_data=>cs_api_connection-url }| iv_is_https = abap_true ).
        set_request_uri( |{ ms_services_conf-resource }| ).


        " Se pasa el método de comunicación HTTP
        set_request_method( ms_services_conf-http_method ).

        " Se pasa el "accept". Si no esta informada se informa el por defecto que es el JSON
        set_header_value( iv_name  = 'Accept' iv_value = COND #( WHEN ms_services_conf-accept IS INITIAL THEN zif_ml_data=>cs_api_connection-default_accept ELSE ms_services_conf-accept ) ).

        " Se pasa la API Key
        set_header_value( iv_name  = 'APIKey' iv_value = ms_services_conf-api_key ).

        mo_http_client->propertytype_logon_popup = if_http_client=>co_disabled.

      CATCH zcx_ca_rest_http_services. " Cualquier se lanza excepción en la conexión
        RAISE EXCEPTION TYPE zcx_ml_api
          EXPORTING
            textid = zcx_ml_api=>error_connection.
    ENDTRY.



  ENDMETHOD.


  METHOD fill_configuration.

  ENDMETHOD.


  METHOD get_response_api.
    TRY.
        " Se obtiene la respuesta del servicio REST
        get_response( IMPORTING es_response = DATA(ls_response) ).

        BREAK-POINT.

      CATCH zcx_ca_rest_http_services INTO DATA(lo_excep).
    ENDTRY.
  ENDMETHOD.


  METHOD send_request_api.

    TRY.
        send_request( is_request = CORRESPONDING #( is_request ) ).
      CATCH zcx_ca_rest_http_services INTO DATA(lo_excep).
        RAISE EXCEPTION TYPE zcx_ml_api
          EXPORTING
            textid = zcx_ml_api=>error_send_request.
    ENDTRY.

  ENDMETHOD.


  METHOD set_api_configuration.
    ms_services_conf = is_services_conf.

  ENDMETHOD.


  METHOD validate_conf_connection.
    " Si algunos de los parámetros de configuración principales esta en blanco se lanza excepción ya que son necesarios
    IF ms_services_conf-api_key IS INITIAL OR ms_services_conf-resource IS INITIAL OR ms_services_conf-http_method IS INITIAL.
      RAISE EXCEPTION TYPE zcx_ml_api
        EXPORTING
          textid = zcx_ml_api=>conf_missing.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
