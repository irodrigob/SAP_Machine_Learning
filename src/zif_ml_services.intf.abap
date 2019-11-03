INTERFACE zif_ml_services
  PUBLIC .
  TYPES: BEGIN OF ts_service_response,
           response       TYPE REF TO data,
           response_structure type string,
           there_error    TYPE sap_bool,
           error_response TYPE zcl_ml_rest_api=>ts_error_response_api,
         END OF ts_service_response.
  "! <p class="shorttext synchronized" lang="en">Call API</p>
  "! The values of IS_REQUEST and ES_RESPONSE depend on the class called.
  "! For the class: ZCL_ML_LANG_DETECTION:
  "!          IS_REQUEST TYPE ZCL_ML_LANG_DETECTION=>IS_REQUEST
  "!          ES_RESPONSE TYPE ZCL_ML_LANG_DETECTION=>IS_RESPONSE
  "! @parameter is_request | <p class="shorttext synchronized" lang="en">Data request</p>
  "! @parameter es_service_response | <p class="shorttext synchronized" lang="en">Service response</p>
  "! the fields of the structure are as follows:
  "! - there_error -> There was a business error calling the service. System exceptions are captured with TRY..CATCH
  "! - error_response -> Structure with standard machine learning error information
  "! - response -> Reference to the structure, response_structure field, with the result of the service
  "! - ls_service_response -> Name of the structure used in RESPONSE
  "! @raising zcx_ml_api | <p class="shorttext synchronized" lang="en"></p>
  METHODS call_api
    IMPORTING
      !is_request          TYPE any
    EXPORTING
      !es_service_response TYPE zif_ml_services=>ts_service_response
    RAISING
      zcx_ml_api .

ENDINTERFACE.
