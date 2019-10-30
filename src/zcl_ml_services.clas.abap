CLASS zcl_ml_services DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PROTECTED .

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_langu TYPE sylangu DEFAULT sy-langu.

    INTERFACES zif_ml_services
      ALL METHODS ABSTRACT .
  PROTECTED SECTION.
    DATA mo_rest_api TYPE REF TO zcl_ml_rest_api.
    DATA mv_langu TYPE sy-langu.

    "! <p class="shorttext synchronized" lang="en">Fill the configuration for the service</p>
    "! mandatory method that each child has to redefine to inform the service configuration
    METHODS fill_configuration ABSTRACT.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ml_services IMPLEMENTATION.
  METHOD constructor.
    mv_langu = sy-langu.

    " Se instanci la clase para poder hacer las llamadas
    mo_rest_api = NEW zcl_ml_rest_api(  ).

  ENDMETHOD.

ENDCLASS.
