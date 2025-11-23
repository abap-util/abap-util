"! <p class="shorttext synchronized" lang="en">Serializable RTTI class</p>
CLASS zabaputil_cl_srt_classdescr DEFINITION
  PUBLIC
  INHERITING FROM zabaputil_cl_srt_objectdescr
  CREATE PUBLIC.

  PUBLIC SECTION.

    DATA class_kind        LIKE cl_abap_classdescr=>class_kind.
    DATA create_visibility LIKE cl_abap_classdescr=>create_visibility.

    METHODS constructor
      IMPORTING
        !rtti TYPE REF TO cl_abap_classdescr.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS zabaputil_cl_srt_classdescr IMPLEMENTATION.
  METHOD constructor.
    super->constructor( rtti ).
    class_kind        = rtti->class_kind.
    create_visibility = rtti->create_visibility.
  ENDMETHOD.
ENDCLASS.
