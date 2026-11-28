INTERFACE zabaputil_if_pdf_data
  PUBLIC.

  " A form is a class: its PUBLIC attributes are the data the form prints,
  " and get( ) returns the form itself. A value is printed where a text
  " names it in braces ({NAME}, {HEADER-CITY} for a component of a
  " structure), an internal table is where a table element takes its rows
  " from.
  "
  "   DATA(lo_note) = NEW zcl_my_delivery_note( ).
  "   lo_note->vbeln = `80004711`.
  "   DATA(lv_pdf) = zabaputil_cl_pdf_form=>print( lo_note ).
  "
  " The form is a text (zabaputil_cl_pdf_form=>stringify( ) writes it,
  " parse( ) reads it): an editor shows it as the source of get( ), to be
  " copied into the class. So a form is designed anywhere - on a system or
  " off it - and lives where every other development object does: in the
  " repository, with its transports and its versions.
  "
  " An editor reads the attributes to offer them and creates an instance
  " for its preview; that is why the interface has to be there: a class
  " that does not implement it is never created from a name somebody typed.
  " The class needs a constructor without mandatory parameters. What it
  " leaves initial, a preview fills with sample values
  " (zabaputil_cl_pdf_form=>data_sample( )).

  "! The form: the text zabaputil_cl_pdf_form=>stringify( ) wrote, as an
  "! editor hands it over. Empty for a class that has no form yet
  METHODS get
    RETURNING
      VALUE(result) TYPE string.

ENDINTERFACE.
