CLASS zcl_ml_rest_api DEFINITION
  PUBLIC
  INHERITING FROM zcl_ca_rest_http_services
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_error_resp_500,
             timestamp TYPE timestamp,
             status    TYPE i,
             error     TYPE string,
             exception TYPE string,
             message   TYPE string,
             path      TYPE string,
           END OF ts_error_resp_500.
    TYPES: BEGIN OF ts_error_resp_400,
             error             TYPE string,
             error_description TYPE string,
           END OF ts_error_resp_400.
    TYPES: BEGIN OF ts_error_resp_401_dtl,
             errorcode TYPE string,
           END OF ts_error_resp_401_dtl.
    TYPES: BEGIN OF ts_error_resp_401_fault,
             faultstring TYPE string,
             detail      TYPE ts_error_resp_401_dtl,
           END OF ts_error_resp_401_fault.
    TYPES: BEGIN OF ts_error_resp_401,
             fault TYPE ts_error_resp_401_fault,
           END OF ts_error_resp_401.
    TYPES: BEGIN OF ts_error_response_api,
             status       TYPE string,
             status_error TYPE string,
             message      TYPE string,
             exception    TYPE string,
           END OF ts_error_response_api.
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
             there_error    TYPE sap_bool,
             error_response TYPE ts_error_response_api,
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
        create_rest_client_by_url( iv_url = ms_services_conf-resource ).
*        create_rest_client( iv_host = |{ zif_ml_data=>cs_api_connection-url }| iv_is_https = abap_true ).
*        set_request_uri( |{ ms_services_conf-resource }| ).


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
    DATA ls_error_401 TYPE ts_error_resp_401.
    DATA ls_error_500 TYPE ts_error_resp_500.
    DATA ls_error_400 TYPE ts_error_resp_400.
    CLEAR: es_response.
    TRY.
        " Se obtiene la respuesta del servicio REST
        get_response( IMPORTING es_response = DATA(ls_response) ).

        es_response-there_error = abap_false.
        es_response-response = ls_response-response.
        es_response-content_length = ls_response-content_length.
        es_response-content_type = ls_response-content_type.


      CATCH zcx_ca_rest_http_services INTO DATA(lo_excep).
        es_response-there_error = abap_true. " Se indica que hay error

        " Valores genéricos indistintamente del tipo de error.
        es_response-error_response-status = lo_excep->mv_status_code.
        es_response-error_response-status_error = lo_excep->mv_status_text.

        "De momentos hay tres errores distintos: 401 sin autorizacion, 400 parámetros erroneos y 500 que
        " cuando se pasa la URL del servicio incorrectamente
        " El 500 se le considera como "otros" porque es una estructura de datos
        " Por ello la estructura de error serán común para ambos.
        CASE lo_excep->mv_status_code.
          WHEN '401'. " Unauthorized
            /ui2/cl_json=>deserialize( EXPORTING json = lo_excep->mv_content_response
                                        CHANGING data = ls_error_401 ).


            es_response-error_response-message = ls_error_401-fault-faultstring.
            es_response-error_response-exception = ls_error_401-fault-detail-errorcode.

          WHEN 400. " Invalid params
            /ui2/cl_json=>deserialize( EXPORTING json = lo_excep->mv_content_response
                                         CHANGING data = ls_error_400 ).

            es_response-error_response-message = ls_error_400-error.
            es_response-error_response-exception = ls_error_400-error_description.


          WHEN 500. " Bad request
            " La excepción se convierte a la estructura de errores ya que tiene un formato fijo
            /ui2/cl_json=>deserialize( EXPORTING json = lo_excep->mv_content_response
                                       CHANGING data = ls_error_500 ).

            es_response-error_response-message = ls_error_500-message.
            es_response-error_response-exception = ls_error_500-exception.

        ENDCASE.
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
