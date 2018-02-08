interface ZIF_XTT_FILE
  public .


  methods GET_NAME
    returning
      value(RV_NAME) type STRING .
  methods GET_CONTENT
    exporting
      !EV_AS_STRING type STRING
      !EV_AS_XSTRING type XSTRING .
endinterface.
