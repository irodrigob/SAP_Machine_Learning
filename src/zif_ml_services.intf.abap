INTERFACE zif_ml_services
  PUBLIC .
  "! <p class="shorttext synchronized" lang="en">Call API</p>
  "! The values of IS_REQUEST and ES_RESPONSE depend on the class called.
  "! For the class: ZCL_ML_LANG_DETECTION:
  "!          IS_REQUEST TYPE ZCL_ML_LANG_DETECTION=>IS_REQUEST
  "!          ES_RESPONSE TYPE ZCL_ML_LANG_DETECTION=>IS_RESPONSE
  "! @parameter is_request | <p class="shorttext synchronized" lang="en">Data request</p>
  "! @parameter es_response | <p class="shorttext synchronized" lang="en"></p>
  "! @raising zcx_ml_api | <p class="shorttext synchronized" lang="en"></p>
  METHODS call_api
    IMPORTING
      !is_request  TYPE any
    EXPORTING
      !es_response TYPE any
    RAISING
      zcx_ml_api .

ENDINTERFACE.
