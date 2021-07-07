interface ZIF_XTT
  public .

  type-pools ABAP .
  type-pools OLE2 .

  types:
    BEGIN OF TS_RECIPIENT_BCS,
      RECIPIENT  TYPE REF TO IF_RECIPIENT_BCS,
      EXPRESS    TYPE OS_BOOLEAN,
      COPY       TYPE OS_BOOLEAN,
      BLIND_COPY TYPE OS_BOOLEAN,
      NO_FORWARD TYPE OS_BOOLEAN,
   END OF TS_RECIPIENT_BCS .
  types:
    TT_RECIPIENTS_BCS type STANDARD TABLE OF TS_RECIPIENT_BCS WITH DEFAULT KEY .

  constants:
    BEGIN OF MC_BY,
     ole        type STRING value 'OLE',
     ole_hide   type STRING value 'OLE_HIDE',
     app_server type STRING value 'APP_SERVER',
   END OF MC_BY .

  methods MERGE
    importing
      !IS_BLOCK type ANY
      !IV_BLOCK_NAME type CSEQUENCE default `R`
      !IO_HELPER type ref to OBJECT optional
    returning
      value(RO_XTT) type ref to ZIF_XTT .
  methods GET_RAW
    returning
      value(RV_CONTENT) type XSTRING .
  methods DOWNLOAD
    importing
      !IV_OPEN type CSEQUENCE default ABAP_TRUE
      !IV_ZIP type ABAP_BOOL optional
    changing
      !CV_OLE_APP type OLE2_OBJECT optional
      !CV_OLE_DOC type OLE2_OBJECT optional
      !CV_FULLPATH type CSEQUENCE optional .
  methods SHOW
    importing
      !IO_HANDLER type ref to OBJECT optional .
  methods SEND
    importing
      !IT_RECIPIENTS type RMPS_RECIPIENT_BCS optional
      !IT_RECIPIENTS_BCS type TT_RECIPIENTS_BCS optional
      !IV_SUBJECT type SO_OBJ_DES
      !IV_BODY type CSEQUENCE
      !IV_EXPRESS type ABAP_BOOL default ABAP_TRUE
      !IO_SENDER type ref to IF_SENDER_BCS optional
      !IV_COMMIT type ABAP_BOOL default ABAP_FALSE .
endinterface.
