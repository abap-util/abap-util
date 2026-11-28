INTERFACE zabaputil_if_pdf_data
  PUBLIC.

  " The data of a form: a class that implements this interface is what a
  " form names as its data class. Its PUBLIC attributes are the interface
  " to the form - a value is printed where a text names it in braces
  " ({NAME}, {HEADER-CITY} for a component of a structure), an internal
  " table is where a table element takes its rows from. An editor reads the
  " attributes to offer them, and creates an instance for its preview; that
  " is why the interface has to be there: a class that does not implement
  " it is never created from a name somebody typed.
  "
  " The class needs a constructor without mandatory parameters. How it gets
  " its data for a real print - a method of its own that reads an order,
  " attributes the caller fills - is up to the class.

  "! Fill the public attributes with values a preview can show
  METHODS fill_sample.

ENDINTERFACE.
