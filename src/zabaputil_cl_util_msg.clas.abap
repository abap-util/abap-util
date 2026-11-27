CLASS zabaputil_cl_util_msg DEFINITION PUBLIC
  FINAL CREATE PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS msg_get_text
      IMPORTING
        val           TYPE any
        val2          TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS msg_map
      IMPORTING
        name          TYPE clike
        val           TYPE data
        is_msg        TYPE zabaputil_cl_util_context=>ty_s_msg
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_s_msg.

    CLASS-METHODS msg_get
      IMPORTING
        val           TYPE any
        val2          TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_t_msg.

    CLASS-METHODS msg_get_by_sy
      RETURNING
        VALUE(result) TYPE zabaputil_cl_util_context=>ty_t_msg.

    CLASS-METHODS msg_get_collect
      IMPORTING
        val           TYPE any
        val2          TYPE any OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.


" Every method hands over to the message methods of
" zabaputil_cl_util_context. This class used to carry a fork of them that had
" fallen behind: the RAP fail causes as a running number (1..11) instead of
" the codes IF_ABAP_BEHV=>T_FAIL_CAUSE holds (401, 404, 409, 423, ...), a
" structured value moved into a text without a check, %ELEMENT- that never
" matched - one implementation is kept, the one every consumer copies.
CLASS zabaputil_cl_util_msg IMPLEMENTATION.

  METHOD msg_get_text.

    result = zabaputil_cl_util_context=>msg_get_text( val  = val
                                                      val2 = val2 ).

  ENDMETHOD.

  METHOD msg_map.

    result = zabaputil_cl_util_context=>msg_map( name = name
                                                 val  = val
                                                 msg  = is_msg ).

  ENDMETHOD.

  METHOD msg_get.

    " a table of messages - zabaputil_cl_util_context=>msg_get returns one
    result = zabaputil_cl_util_context=>msg_get_t( val  = val
                                                   val2 = val2 ).

  ENDMETHOD.

  METHOD msg_get_by_sy.

    result = zabaputil_cl_util_context=>msg_get_by_sy( ).

  ENDMETHOD.

  METHOD msg_get_collect.

    result = zabaputil_cl_util_context=>msg_get_collect( val  = val
                                                         val2 = val2 ).

  ENDMETHOD.

ENDCLASS.
