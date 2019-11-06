# Objetivo / Objective
Clases que facilitan la conexión a los servicios de machine learning de SAP Leonardo. / Classes that facilitate the connection to machine learning services of SAP Leonardo.


# Prerequisitos / Prerequisites

Versión ABAP 7.4 o superior / ABAP 7.4 or higher

Tener instalado el repositorio de HTTP Services: (https://github.com/irodrigob/http_services) / Have the HTTP Services repository installed: (https://github.com/irodrigob/http_services)


Instalar los siguientes certificados en la transacción STRUST / Install the next certificate in the transaction STRUST: https://mlfproduction-language-detection.cfapps.eu10.hana.ondemand.com/ and https://mlfproduction-language-detection.cfapps.eu10.hana.ondemand.com/

# SAP Machine Learning API

Esta URL / This URL: (https://api.sap.com/package/SAPLeonardoMLFunctionalServices?section=Artifacts)

# Servicios implementados / Services implemented

Actualmente hay dos servicios implementados / There are currently two services implemented:

* Service for language detection -> Implementado en la clase ZCL_ML_LANG_DETECTION y con el programa de ejemplo ZML_R_LANG_DETECTION para ver como funciona / and with the sample program ZML_R_LANG_DETECTION to see how it works

* Service for text translation -> Implementado en la clase ZCL_ML_TEX_TRANSLATION y con el programa de ejemplo ZML_R_LANG_DETECTION para ver como funciona / and with the sample program ZML_R_TEXT_TRANSLATION to see how it works

# Modelo de clases / Class Model

ZCL_ML_LANG_DETECTION y ZCL_ML_TEX_TRANSLATION heredan de la clase ZCL_ML_SERVICES que contiene la interface ZIF_ML_SERVICES. Esta interface en la clase abstracta para que sus métodos se tengan que implementar. Además, en la clase padre el método FILL_CONFIGURATION es abstracto para que tenga que implementar en cada clase hija, ya que se ha de informar de los datos de configuración del servicio. 
/ ZCL_ML_LANG_DETECTION and ZCL_ML_TEX_TRANSLATION inherit from the ZCL_ML_SERVICES class that contains the ZIF_ML_SERVICES interface. This interface in the abstract class so that its methods have to be implemented. In addition, in the parent class the FILL_CONFIGURATION method is abstract so that you have to implement in each daughter class, since the service configuration data must be reported.

En el proyecto (https://github.com/irodrigob/http_services)  se ha añadido la clase ZCL_CA_REST_HTTP_SERVICES, que hereda de ZCL_CA_HTTP_SERVICES, que contiene los método necesarios para llamar a servicios REST como los del machine learning. Actualmente soporta llamadas POST y Multipart(no probado!!!). / In the project (https://github.com/irodrigob/http_services) the class ZCL_CA_REST_HTTP_SERVICES has been added, which inherits from ZCL_CA_HTTP_SERVICES, which contains the necessary methods to call REST services such as machine learning. It currently supports POST and Multipart calls (not tested !!!).

