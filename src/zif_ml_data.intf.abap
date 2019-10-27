INTERFACE zif_ml_data
  PUBLIC .
  CONSTANTS: BEGIN OF cs_api_connection,
               url   TYPE string VALUE 'https://sandbox.api.sap.com',
               default_accept type string value 'application/json',
             END OF cs_api_connection.
  CONSTANTS: BEGIN OF cs_api_resources,
               BEGIN OF lang_detection,
                 class TYPE seoclsname VALUE 'ZCL_ML_LANG_DETECTION',
               END OF lang_detection,
             END OF cs_api_resources.
ENDINTERFACE.
