CLASS zabaputil_cl_pdf_data_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    " A form by example: a delivery note. The public attributes are its
    " data - the form prints {VBELN}, {NAME} and {ADDRESS-CITY}, and its
    " table takes its rows from T_ITEMS - and get( ) returns the form, as
    " an editor wrote it.
    "
    "   DATA(lo_note) = NEW zabaputil_cl_pdf_data_demo( ).
    "   lo_note->vbeln = `80004711`.
    "   DATA(lv_pdf) = zabaputil_cl_pdf_form=>print( lo_note ).

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

  METHOD zabaputil_if_pdf_data~get.

    " as an editor wrote it - open the class in one to see and change it
    result = `<form version="1"><NAME>DELIVERY_NOTE</NAME><TITLE>Delivery note</TITLE><FORMAT>A4</FORMAT><ORIENTAT` &&
             `ION>P</ORIENTATION><MARGIN_LEFT>20.00</MARGIN_LEFT><MARGIN_TOP>20.00</MARGIN_TOP><MARGIN_RIGHT>20.00` &&
             `</MARGIN_RIGHT><MARGIN_BOTTOM>20.00</MARGIN_BOTTOM><HEADER>Delivery note {VBELN}</HEADER><FOOTER>Pag` &&
             `e {page} of {pages}</FOOTER><T_ELEMENT><item><TYPE>RECT</TYPE><NAME>Address window</NAME><FIXED>X</F` &&
             `IXED><X>20.00</X><Y>45.00</Y><W>85.00</W><H>40.00</H><COLOR>BBBBBB</COLOR></item><item><TYPE>TEXT</T` &&
             `YPE><NAME>Address</NAME><FIXED>X</FIXED><X>24.00</X><Y>50.00</Y><W>77.00</W><TEXT>{NAME}&#10;{ADDRES` &&
             `S-STREET}&#10;{ADDRESS-POSTCODE} {ADDRESS-CITY}</TEXT><FONT>Helvetica</FONT><SIZE>10.00</SIZE><ALIGN` &&
             `>L</ALIGN></item><item><TYPE>TEXT</TYPE><NAME>Sender</NAME><FIXED>X</FIXED><EVERY_PAGE>X</EVERY_PAGE` &&
             `><X>120.00</X><Y>20.00</Y><W>70.00</W><TEXT>ACME Corporation</TEXT><FONT>Helvetica-Bold</FONT><SIZE>` &&
             `12.00</SIZE><COLOR>003366</COLOR><ALIGN>R</ALIGN></item><item><TYPE>SPACE</TYPE><NAME>Below the addr` &&
             `ess</NAME><H>75.00</H></item><item><TYPE>TEXT</TYPE><NAME>Headline</NAME><TEXT>Delivery note {VBELN}` &&
             `</TEXT><FONT>Helvetica-Bold</FONT><SIZE>16.00</SIZE><ALIGN>L</ALIGN></item><item><TYPE>SPACE</TYPE><` &&
             `H>3.00</H></item><item><TYPE>TEXT</TYPE><NAME>Introduction</NAME><TEXT>Dear {NAME}, we deliver the f` &&
             `ollowing items of your order {BSTNK} from {BSTDK}.</TEXT><FONT>Helvetica</FONT><SIZE>10.00</SIZE><AL` &&
             `IGN>L</ALIGN></item><item><TYPE>SPACE</TYPE><H>4.00</H></item><item><TYPE>TABLE</TYPE><NAME>Items</N` &&
             `AME><FONT>Helvetica</FONT><SIZE>9.00</SIZE><SOURCE>T_ITEMS</SOURCE><T_COLUMN><item><NAME>POSNR</NAME` &&
             `><TITLE>Item</TITLE><WIDTH>15.00</WIDTH><ALIGN>R</ALIGN></item><item><NAME>MATNR</NAME><TITLE>Materi` &&
             `al</TITLE><WIDTH>35.00</WIDTH><ALIGN>L</ALIGN></item><item><NAME>ARKTX</NAME><TITLE>Description</TIT` &&
             `LE><ALIGN>L</ALIGN></item><item><NAME>LFIMG</NAME><TITLE>Quantity</TITLE><WIDTH>25.00</WIDTH><ALIGN>` &&
             `R</ALIGN></item><item><NAME>MEINS</NAME><TITLE>Unit</TITLE><WIDTH>15.00</WIDTH><ALIGN>L</ALIGN></ite` &&
             `m></T_COLUMN></item><item><TYPE>LINE</TYPE></item><item><TYPE>TEXT</TYPE><NAME>Closing</NAME><TEXT>T` &&
             `hank you for your order.</TEXT><FONT>Helvetica</FONT><SIZE>10.00</SIZE><ALIGN>L</ALIGN></item></T_EL` &&
             `EMENT></form>`.

  ENDMETHOD.

ENDCLASS.
