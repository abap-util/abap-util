CLASS zabaputil_cl_pdf_data_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " A data class by example: the data of a delivery note. A form that
    " names this class prints {VBELN}, {NAME} and {ADDRESS-CITY}, and a
    " table element takes its rows from T_ITEMS.

    INTERFACES zabaputil_if_pdf_data.

    TYPES:
      BEGIN OF ty_s_address,
        street   TYPE string,
        postcode TYPE string,
        city     TYPE string,
      END OF ty_s_address.

    TYPES:
      BEGIN OF ty_s_item,
        posnr TYPE i,
        matnr TYPE string,
        arktx TYPE string,
        lfimg TYPE p LENGTH 8 DECIMALS 2,
        meins TYPE string,
      END OF ty_s_item.
    TYPES ty_t_item TYPE STANDARD TABLE OF ty_s_item WITH EMPTY KEY.

    DATA vbeln   TYPE string.
    DATA name    TYPE string.
    DATA address TYPE ty_s_address.
    DATA bstnk   TYPE string.
    DATA bstdk   TYPE string.
    DATA t_items TYPE ty_t_item.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


CLASS zabaputil_cl_pdf_data_demo IMPLEMENTATION.

  METHOD zabaputil_if_pdf_data~fill_sample.

    vbeln = `80004711`.
    name = `Ada Lovelace`.
    address-street = `12 Analytical Row`.
    address-postcode = `53111`.
    address-city = `Bonn`.
    bstnk = `PO-2026-118`.
    bstdk = `2026-11-20`.

    t_items = VALUE #( ( posnr = 10 matnr = `MAT-100` arktx = `Hex nut M8, zinc plated`    lfimg = 250 meins = `PC` )
                       ( posnr = 20 matnr = `MAT-205` arktx = `Hex bolt M8 x 40`           lfimg = 250 meins = `PC` )
                       ( posnr = 30 matnr = `MAT-310` arktx = `Washer 8.4 mm`              lfimg = 500 meins = `PC` )
                       ( posnr = 40 matnr = `MAT-777` arktx = `Thread locking fluid 50 ml` lfimg = 2   meins = `BT` ) ).

  ENDMETHOD.

ENDCLASS.
