// MzScheme inetrface to the Expat API.
// Copyright (C) 2007, 2008  Vijay Mathew Pandyalakal
 
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3 of the License, or
// (at your option) any later version.
  
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
  
// You should have received a copy of the GNU General Public License along
// with this program; If not, see <http://www.gnu.org/licenses/>.
  
// Please contact Vijay Mathew Pandyalakal if you need additional 
// information or have any questions.
// (Electronic mail: vijay.the.schemer@gmail.com)

#include "spark.h"
#include "spark_expat.h"
using namespace spark_expat;

static const char* MODULE_NAME = "#%spark-expat";


struct Expat_xml_parser
{
  XML_Parser parser;
  Scheme_Object* start_element_handler;
  Scheme_Object* end_element_handler;
  Scheme_Object* character_data_handler;
  Scheme_Object* comment_handler;
  Scheme_Object* start_cdata_section_handler;
  Scheme_Object* end_cdata_section_handler;
  Scheme_Object* default_handler;
  Scheme_Object* external_entity_ref_handler;
  Scheme_Object* skipped_entity_handler;
  // Scheme_Object* unknown_encoding_handler;
  Scheme_Object* start_namespace_decl_handler;
  Scheme_Object* end_namespace_decl_handler;
  Scheme_Object* xml_decl_handler;
  Scheme_Object* start_doctype_decl_handler;
  Scheme_Object* end_doctype_decl_handler;
  // Scheme_Object* element_decl_handler;
  Scheme_Object* attlist_decl_handler;
  Scheme_Object* entity_decl_handler;
  Scheme_Object* unparsed_entity_decl_handler;
  Scheme_Object* notation_decl_handler;
  Scheme_Object* not_standalone_handler;
  Scheme_Object* user_data;

  Expat_xml_parser(XML_Parser p)
    : parser(p),
      start_element_handler(scheme_null),
      end_element_handler(scheme_null),
      character_data_handler(scheme_null),
      comment_handler(scheme_null),
      start_cdata_section_handler(scheme_null),
      end_cdata_section_handler(scheme_null),
      default_handler(scheme_null),
      external_entity_ref_handler(scheme_null),
      skipped_entity_handler(scheme_null),
      // unknown_encoding_handler(scheme_null),
      start_namespace_decl_handler(scheme_null),
      end_namespace_decl_handler(scheme_null),
      xml_decl_handler(scheme_null),
      start_doctype_decl_handler(scheme_null),
      end_doctype_decl_handler(scheme_null),
      // element_decl_handler(scheme_null),
      attlist_decl_handler(scheme_null),
      entity_decl_handler(scheme_null),
      unparsed_entity_decl_handler(scheme_null),
      notation_decl_handler(scheme_null),
      not_standalone_handler(scheme_null),
      user_data(scheme_null)
  {
    XML_SetUserData(p, reinterpret_cast<void*>(this));
  }

  ~Expat_xml_parser()
  {
    if (parser)
      XML_ParserFree(parser);
    start_element_handler = scheme_null;
    end_element_handler = scheme_null;
    character_data_handler = scheme_null;
    comment_handler = scheme_null;
    start_cdata_section_handler = scheme_null;
    end_cdata_section_handler = scheme_null;
    default_handler = scheme_null;
    external_entity_ref_handler = scheme_null;
    skipped_entity_handler = scheme_null;
    // unknown_encoding_handler = scheme_null;
    start_namespace_decl_handler = scheme_null;
    end_namespace_decl_handler = scheme_null;
    xml_decl_handler = scheme_null;
    start_doctype_decl_handler = scheme_null;
    end_doctype_decl_handler = scheme_null;
    // element_decl_handler = scheme_null;
    attlist_decl_handler = scheme_null;
    entity_decl_handler = scheme_null;
    unparsed_entity_decl_handler = scheme_null;
    notation_decl_handler = scheme_null;
    not_standalone_handler = scheme_null;
    user_data = scheme_null;
  }
}; // struct ExpatXmlParser

enum Expat_tag
  {
    EXPAT_XML_PARSER_TAG
  }; // enum Expat_tag

static spark::Status_code _add_constants(Scheme_Env* env);
static spark::Status_code _add_procedures(Scheme_Env* env);
static Expat_xml_parser* _scheme_object_to_expat(int argc, 
						 Scheme_Object** argv,
						 int idx);	
static void XMLCALL _start_element_handler(void* user_data, 
					   const XML_Char* elem,
					   const XML_Char** attrs);
static void XMLCALL _end_element_handler(void* user_data, 
					 const XML_Char* elem);
static void XMLCALL _character_data_handler(void* user_data, 
					    const XML_Char* s, 
					    int len);
static void XMLCALL _comment_handler(void* user_data,
				     const XML_Char* data);

static void XMLCALL _start_cdata_section_handler(void* user_data);
static void XMLCALL _end_cdata_section_handler(void* user_data);
static void XMLCALL _default_handler(void* user_data, const char* data, 
				     int len);
static int XMLCALL _external_entity_ref_handler(XML_Parser p,
						 const XML_Char* context,
						 const XML_Char* base,
						 const XML_Char* systemId,
						 const XML_Char* publicId);
static void XMLCALL _skipped_entity_handler(void* user_data,
					    const XML_Char* entity_name,
					    int is_parameter_entity);
/*static void XMLCALL _unknown_encoding_handler(void* encodingHandlerData,
  const XML_Char* name,
  XML_Encoding* info);*/
static void XMLCALL _start_namespace_decl_handler(void* user_data,
						  const XML_Char* prefix,
						  const XML_Char* uri);
static void XMLCALL _end_namespace_decl_handler(void* userData,
						const XML_Char* prefix);
static void XMLCALL _xml_decl_handler(void* userData,
				      const XML_Char* version,
				      const XML_Char* encoding,
				      int standalone);
static void XMLCALL _start_doctype_decl_handler(void* user_data,
						const XML_Char* doctype_name,
						const XML_Char* sysid,
						const XML_Char* pubid,
						int has_internal_subset);
static void XMLCALL _end_doctype_decl_handler(void* user_data);
/*static void XMLCALL _element_decl_handler(void* user_data,
  const XML_Char* name,
  XML_Content* model);*/
static void XMLCALL _attlist_decl_handler(void* user_data,
					  const XML_Char *elname,
					  const XML_Char *attname,
					  const XML_Char *att_type,
					  const XML_Char *dflt,
					  int isrequired);
static void XMLCALL _entity_decl_handler(void* user_data,
					 const XML_Char* entity_name,
					 int is_parameter_entity,
					 const XML_Char* value,
					 int value_length, 
					 const XML_Char* base,
					 const XML_Char* system_id,
					 const XML_Char* public_id,
					 const XML_Char* notation_name);
static void XMLCALL _unparsed_entity_decl_handler(void* user_data,
						  const XML_Char* entity_name, 
						  const XML_Char* base,
						  const XML_Char* system_id,
						  const XML_Char* public_id,
						  const XML_Char* notation_name);
static void XMLCALL _notation_decl_handler(void* user_data, 
					   const XML_Char* notation_name,
					   const XML_Char* base,
					   const XML_Char* system_id,
					   const XML_Char* public_id);
static int XMLCALL _not_standalone_handler(void* user_data);

spark::Status_code
spark_expat::initialize(Scheme_Env* env)
{
  Scheme_Object* module_symbol = NULL;
  Scheme_Env* new_env = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, module_symbol);
  MZ_GC_VAR_IN_REG(1, new_env);
  MZ_GC_REG();
  
  module_symbol = scheme_intern_symbol(MODULE_NAME);
  assert_scheme_object(module_symbol, "scheme_intern_symbol");
  new_env = scheme_primitive_module(module_symbol, env);
  assert_scheme_object(new_env, "scheme_primitive_module");
  spark::Status_code status = spark::SUCCESS;
  if ((status = _add_constants(new_env)) != spark::SUCCESS)
    {
      MZ_GC_UNREG();
      return status;
    }
  if ((status = _add_procedures(new_env)) != spark::SUCCESS)
    {
      MZ_GC_UNREG();
      return status;
    }
  scheme_finish_primitive_module(new_env);
  scheme_protect_primitive_provide(new_env, 0);

  MZ_GC_UNREG();
  return spark::SUCCESS;
}

// exported function signatures
namespace spark_expat
{
  static Scheme_Object* open(int, Scheme_Object**);
  static Scheme_Object* close(int, Scheme_Object**);
  static Scheme_Object* set_start_element_handler(int, Scheme_Object**);
  static Scheme_Object* set_end_element_handler(int, Scheme_Object**);
  static Scheme_Object* set_element_handlers(int, Scheme_Object**);
  static Scheme_Object* set_character_data_handler(int, Scheme_Object**);
  static Scheme_Object* set_comment_handler(int, Scheme_Object**);
  static Scheme_Object* set_start_cdata_section_handler(int, Scheme_Object**);
  static Scheme_Object* set_end_cdata_section_handler(int, Scheme_Object**);
  static Scheme_Object* set_default_handler(int, Scheme_Object**);
  static Scheme_Object* set_external_entity_ref_handler(int, Scheme_Object**);
  static Scheme_Object* set_skipped_entity_handler(int, Scheme_Object**);
  // static Scheme_Object* set_unknown_encoding_handler(int, Scheme_Object**);
  static Scheme_Object* set_start_namespace_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_end_namespace_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_xml_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_start_doctype_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_end_doctype_decl_handler(int, Scheme_Object**);
  // static Scheme_Object* set_element_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_attlist_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_entity_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_unparsed_entity_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_notation_decl_handler(int, Scheme_Object**);
  static Scheme_Object* set_not_standalone_handler(int, Scheme_Object**);
  static Scheme_Object* parse(int, Scheme_Object**);
  static Scheme_Object* set_user_data(int, Scheme_Object**);
  static Scheme_Object* last_error(int, Scheme_Object**);
} // namespace spark_expat

spark::Status_code
_add_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-expat");
}

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_expat::open, "open", 3),
    new Procedure(spark_expat::close, "close", 1),
    new Procedure(spark_expat::set_start_element_handler, 
		  "set-start-element-handler!", 2),
    new Procedure(spark_expat::set_end_element_handler, 
		  "set-end-element-handler!", 2),
    new Procedure(spark_expat::set_element_handlers, 
		  "set-element-handlers!", 3),
    new Procedure(spark_expat::set_character_data_handler, 
		  "set-character-data-handler!", 2),
    new Procedure(spark_expat::set_comment_handler, 
		  "set-comment-handler!", 2),
    new Procedure(spark_expat::set_start_cdata_section_handler,
		  "set-start-cdata-section-handler!", 2),
    new Procedure(spark_expat::set_end_cdata_section_handler,
		  "set-end-cdata-section-handler!", 2),
    new Procedure(spark_expat::set_default_handler,
		  "set-default-handler!", 2),
    new Procedure(spark_expat::set_external_entity_ref_handler,
		  "set-external-entity-ref-handler!", 2),
    new Procedure(spark_expat::set_skipped_entity_handler,
		  "set-skipped-entity-handler!", 2),
    /* new Procedure(spark_expat::set_unknown_encoding_handler,
       "set-unknown-encoding-handler!", 2), */
    new Procedure(spark_expat::set_start_namespace_decl_handler,
		  "set-start-namespace-decl-handler!", 2),
    new Procedure(spark_expat::set_end_namespace_decl_handler,
		  "set-end-namespace-decl-handler!", 2),
    new Procedure(spark_expat::set_xml_decl_handler,
		  "set-xml-decl-handler!", 2),
    new Procedure(spark_expat::set_start_doctype_decl_handler,
		  "set-start-doctype-decl-handler!", 2),
    new Procedure(spark_expat::set_end_doctype_decl_handler,
		  "set-end-doctype-decl-handler!", 2),
    /*new Procedure(spark_expat::set_element_decl_handler,
      "set-element-decl-handler!", 2),*/
    new Procedure(spark_expat::set_attlist_decl_handler,
		  "set-attlist-decl-handler!", 2),
    new Procedure(spark_expat::set_entity_decl_handler,
		  "set-entity-decl-handler!", 2),
    new Procedure(spark_expat::set_unparsed_entity_decl_handler,
		  "set-unparsed-entity-decl-handler!", 2),
    new Procedure(spark_expat::set_notation_decl_handler,
		  "set-notation-decl-handler!", 2),
    new Procedure(spark_expat::set_not_standalone_handler,
		  "set-not-standalone-handler!", 2),
    new Procedure(spark_expat::parse, "parse", 2),
    new Procedure(spark_expat::set_user_data, "set-user-data!", 2),
    new Procedure(spark_expat::last_error, "last-error", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-expat");
}

// Exported Expat API

// Creates and returns an Expat XML parser.
// Takes 3 arguments:
// 1. Encoding name or null.
// 2. #t to create a parser with namespace processing.
// 3. A separator character. Valid only if namespace processing is turned on.
// Returns the ExpatParser handle on success.
// If the function fails, the returns value will be null.
Scheme_Object* 
spark_expat::open(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  // encoding name
  std::string encoding = "";
  if (argv[0] != scheme_null)
    {
      if (!SCHEME_CHAR_STRINGP(argv[0]))
	scheme_wrong_type("open", "string", 0, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[0]);
      encoding = SCHEME_BYTE_STR_VAL(str);
    }
  
  // check if namespace processing is turned on.
  bool namespace_processing = (argv[1] == scheme_true) ? true : false;

  // get the separator for namespace processing
  std::string separator = ".";
  if (argv[2] != scheme_null && namespace_processing)
    {
      if (!SCHEME_CHAR_STRINGP(argv[2]))
	scheme_wrong_type("open", "string", 2, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
      separator = SCHEME_BYTE_STR_VAL(str);
    }

  const XML_Char* enc = 0;
  if (encoding.length())
    enc = reinterpret_cast<const XML_Char*>(encoding.c_str());
  XML_Char sep = static_cast<XML_Char>(separator[0]);
  XML_Parser p;  
  if (namespace_processing)
    p = XML_ParserCreateNS(enc, sep);
  else
    p = XML_ParserCreate(enc);
  if (!p)
    {
      DEFAULT_RET_FINISH;
    }
  XML_SetCharacterDataHandler(p, _character_data_handler);
  XML_SetElementHandler(p, _start_element_handler,
			_end_element_handler);
  XML_SetCommentHandler(p, _comment_handler);
  XML_SetStartCdataSectionHandler(p, _start_cdata_section_handler);
  XML_SetEndCdataSectionHandler(p, _end_cdata_section_handler);
  XML_SetDefaultHandler(p, _default_handler);
  XML_SetExternalEntityRefHandler(p, _external_entity_ref_handler);
  XML_SetSkippedEntityHandler(p, _skipped_entity_handler);
  // XML_SetUnknownEncodingHandler(p, _unknown_encoding_handler);
  XML_SetStartNamespaceDeclHandler(p, _start_namespace_decl_handler);
  XML_SetEndNamespaceDeclHandler(p, _end_namespace_decl_handler);
  XML_SetXmlDeclHandler(p, _xml_decl_handler);
  XML_SetStartDoctypeDeclHandler(p, _start_doctype_decl_handler);
  XML_SetEndDoctypeDeclHandler(p, _end_doctype_decl_handler);
  // XML_SetElementDeclHandler(p, _element_decl_handler);
  XML_SetAttlistDeclHandler(p, _attlist_decl_handler);
  XML_SetEntityDeclHandler(p, _entity_decl_handler);
  XML_SetUnparsedEntityDeclHandler(p, _unparsed_entity_decl_handler);
  XML_SetNotationDeclHandler(p, _notation_decl_handler);
  XML_SetNotStandaloneHandler(p, _not_standalone_handler);
  Expat_xml_parser* xml_parser = new Expat_xml_parser(p);
  {
    Scheme_Object* tag = 0;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, tag);
    MZ_GC_REG();
    tag = scheme_make_integer(EXPAT_XML_PARSER_TAG);
    _ret_ = scheme_make_cptr(xml_parser, tag);
    MZ_GC_UNREG();
  }

  DEFAULT_RET_FINISH;
}

// Deletes an ExpatXmlParser and the resources held by it.
// Takes a handle to an opne Expat_xml_parser object as the
// only argument.
Scheme_Object*
spark_expat::close(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser)
    {
      delete parser;
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

// Sets the start element handler callback.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the start element callback.
// This function should have the signature:
// (define (start user-data element attributes))
Scheme_Object*
spark_expat::set_start_element_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->start_element_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Sets the end element handler callback.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the end element callback.
// This function should have the signature:
// (define (end user-data element attributes))
Scheme_Object*
spark_expat::set_end_element_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->end_element_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Sets the expat element handler callback.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the start element callback.
// This function should have the signature:
// (define (start user-data element attributes))
// 3. Scheme function object that will act as the end element callback.
// This function should have the signature:
// (define (end user-data element))
Scheme_Object*
spark_expat::set_element_handlers(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->start_element_handler = argv[1];
      parser->end_element_handler = argv[2];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Sets the text handler for the parser.
// 1. Expat_xml_parser object
// 2. A function to act as the character data handler callback.
// Signature of this function is: (define (cdata user-data text))
Scheme_Object*
spark_expat::set_character_data_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->character_data_handler = argv[1];
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

// Sets the comment handler callback.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the comment callback.
// This function should have the signature:
// (define (comment user-data char-data))
Scheme_Object*
spark_expat::set_comment_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->comment_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Set a handler that gets called at the beginning of a CDATA section.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data))
Scheme_Object* 
spark_expat::set_start_cdata_section_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->start_cdata_section_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Set a handler that gets called at the end of a CDATA section.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data))
Scheme_Object* 
spark_expat::set_end_cdata_section_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->end_cdata_section_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Sets a handler for any characters in the document which wouldn't 
// otherwise be handled.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data string))
Scheme_Object* 
spark_expat::set_default_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->default_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Sets an external entity handler.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data context base system-id public-id))
// This callback must return scheme_null on failure, or any other
// value on success.
Scheme_Object* 
spark_expat::set_external_entity_ref_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->external_entity_ref_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Sets an skipped entity handler.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data entity-name is-parameter-entity))
Scheme_Object* 
spark_expat::set_skipped_entity_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->skipped_entity_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

/*
  Scheme_Object* 
  spark_expat::set_unknown_encoding_handler(int argc, Scheme_Object** argv)
  {
  DEFAULT_RET_INIT;
  
  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
  argv,
  0);
  if (parser->parser)
  {
  parser->unknown_encoding_handler = argv[1];
  _ret_ = scheme_true;
  }
  DEFAULT_RET_FINISH;
  }
*/

// Set a handler to be called when a namespace is declared. 
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data prefix uri))
Scheme_Object* 
spark_expat::set_start_namespace_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->start_namespace_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Set a handler to be called when leaving the scope of a 
// namespace declaration.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data prefix))
Scheme_Object* 
spark_expat::set_end_namespace_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->end_namespace_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Sets a handler that is called for XML declarations and also 
// for text declarations discovered in external entities. 
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data version encoding is-standalone))
Scheme_Object* 
spark_expat::set_xml_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->xml_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Set a handler that is called at the start of a DOCTYPE declaration,
// before any external or internal subset is parsed.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data doctype-name sysid pubid has-internal-subset))
Scheme_Object* 
spark_expat::set_start_doctype_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->start_doctype_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Set a handler that is called at the end of a DOCTYPE declaration, 
// after parsing any external subset.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data))
Scheme_Object* 
spark_expat::set_end_doctype_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->end_doctype_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

/*
Scheme_Object* 
spark_expat::set_element_decl_handler(int argc, Scheme_Object** argv)
{
DEFAULT_RET_INIT;

Expat_xml_parser* parser = _scheme_object_to_expat(argc,
argv,
0);
if (parser->parser)
{
  parser->element_decl_handler = argv[1];
  _ret_ = scheme_true;
}
 DEFAULT_RET_FINISH;
}
*/

// Set a handler for attlist declarations in the DTD.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data el-name att-name att-type dflt is-required))
Scheme_Object* 
spark_expat::set_attlist_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->attlist_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Sets a handler that will be called for all entity declarations. 
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data entity-name is-parameter-entity
//                  value base system-id public-id notation-name))
Scheme_Object* 
spark_expat::set_entity_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->entity_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Set a handler that receives declarations of unparsed entities. 
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data entity-name base system-id public-id
//         notation-name))
Scheme_Object* 
spark_expat::set_unparsed_entity_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->unparsed_entity_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Set a handler that receives notation declarations.
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data notation-name base system-id public-id))
Scheme_Object* 
spark_expat::set_notation_decl_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->notation_decl_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Set a handler that is called if the document is not "standalone". 
// Arguments:
// 1. Expat_xml_parser object
// 2. Scheme function object that will act as the callback.
// This function should have the signature:
// (define (handler user-data))
// This function must return scheme_null on error, or any other value
// on success
Scheme_Object* 
spark_expat::set_not_standalone_handler(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      parser->not_standalone_handler = argv[1];
      _ret_ = scheme_true;
    }
  DEFAULT_RET_FINISH;
}

// Executes the parser.
// 1. Expat_xml_parser object
// 2. String to parse. If the document has ended, this must be null.
// Returns true on success.
Scheme_Object*
spark_expat::parse(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      std::string xml = "";
      if (argv[1] != scheme_null)
	{
	  if (!SCHEME_CHAR_STRINGP(argv[1]))
	    scheme_wrong_type("parse", "string", 1, argc, argv);      
	  Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
	  xml = SCHEME_BYTE_STR_VAL(str);
	}
      int len = static_cast<int>(xml.length());
      int is_final = (len > 0) ? 0 : 1;
      XML_Status status = XML_Parse(parser->parser,
				    xml.c_str(),
				    len, is_final);
      if (status == XML_STATUS_OK)
	_ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

// Sets the user-data for the parser. This enables passing values
// between handlers without using globals.
Scheme_Object* 
spark_expat::set_user_data(int argc, Scheme_Object** argv)
{
 DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  parser->user_data = argv[1];
  _ret_ = scheme_true;

  DEFAULT_RET_FINISH;
}

// Returns a description of the last error that this
// parser object generated.
Scheme_Object* 
spark_expat::last_error(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Expat_xml_parser* parser = _scheme_object_to_expat(argc,
						     argv,
						     0);
  if (parser->parser)
    {
      XML_Error error = XML_GetErrorCode(parser->parser);
      const char* err_str = 
	reinterpret_cast<const char*>(XML_ErrorString(error));
      _ret_ = scheme_make_utf8_string(err_str);
    }
  DEFAULT_RET_FINISH;							  
}

Expat_xml_parser* 
_scheme_object_to_expat(int argc, 
			Scheme_Object** argv,
			int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_scheme_object_to_expat", "cptr", 
			idx, argc, argv);
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_scheme_object_to_expat", "tag", 
			idx, argc, argv);
      return 0;
    }  
  Expat_tag tag = static_cast<Expat_tag>(i);
  if (tag != EXPAT_XML_PARSER_TAG)
    {
      scheme_wrong_type("_scheme_object_to_expat", "tag", 
			idx, argc, argv);
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(argv[idx]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_expat", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<Expat_xml_parser*>(p);
}

// The real start element callback.
// Calls the scheme function object with the following
// arguments:
// 1. user-data object
// 2. A string that represents the element name.
// 3. A hash-table that contains the element attributes.
void 
XMLCALL _start_element_handler(void* user_data, 
		       const char* elem,
		       const char** attrs)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* start_element_handler = parser->start_element_handler;
      if (start_element_handler != scheme_null)
	{
	  Scheme_Object* str_elem = 0;
	  Scheme_Hash_Table* attrs_ht = 0;
	  MZ_GC_DECL_REG(2);
	  MZ_GC_VAR_IN_REG(0, str_elem);
	  MZ_GC_VAR_IN_REG(1, attrs_ht);
	  MZ_GC_REG();

	  str_elem = scheme_make_utf8_string(elem);
	  attrs_ht = scheme_make_hash_table_equal();

	  for (int i = 0; attrs[i]; i += 2) 
	    {
	      Scheme_Object* key = 0;
	      Scheme_Object* value = 0;
	      MZ_GC_DECL_REG(2);
	      MZ_GC_VAR_IN_REG(0, key);
	      MZ_GC_VAR_IN_REG(1, value);
	      MZ_GC_REG();

	      key = scheme_make_utf8_string(attrs[i]);
	      value = scheme_make_utf8_string(attrs[i + 1]);
	      scheme_hash_set(attrs_ht, key, value);

	      MZ_GC_UNREG();
	    }

	  const int arg_count = 3;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_elem;
	  args[2] = reinterpret_cast<Scheme_Object*>(attrs_ht);
	  scheme_apply(start_element_handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

// The real end element handler.
// Executes the scheme end-element-handler function object
// with the user-data and element-name as arguments.
void
XMLCALL _end_element_handler(void* user_data, const char* elem)				   
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* end_element_handler = parser->end_element_handler;
      if (end_element_handler != scheme_null)
	{
	  Scheme_Object* str_elem = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, str_elem);
	  MZ_GC_REG();

	  str_elem = scheme_make_utf8_string(elem);

	  const int arg_count = 2;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_elem;
	  scheme_apply(end_element_handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

// The real character-data-handler.
// Executes the scheme character-data-handler function
// with user-data and character data as arguments.
void 
XMLCALL _character_data_handler(void* user_data, 
			const char* s, 
			int len)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* character_data_handler = parser->character_data_handler;
      if (character_data_handler != scheme_null)
	{	  
	  Scheme_Object* str = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, str);
	  MZ_GC_REG();

	  std::string ascii_str(s, len);
	  str = scheme_make_utf8_string(ascii_str.c_str());

	  const int arg_count = 2;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str;
	  scheme_apply(character_data_handler, arg_count, args);
	  
	  MZ_GC_UNREG();	 
	}
    }
}

void 
XMLCALL _comment_handler(void* user_data, const char* data)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* comment_handler = parser->comment_handler;
      if (comment_handler != scheme_null)
	{	  
	  Scheme_Object* str = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, str);
	  MZ_GC_REG();

	  str = scheme_make_utf8_string(data);

	  const int arg_count = 2;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str;
	  scheme_apply(comment_handler, arg_count, args);
	  
	  MZ_GC_UNREG();	 
	}
    }
}

void 
XMLCALL _start_cdata_section_handler(void* user_data)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->start_cdata_section_handler;
      if (handler != scheme_null)
	{	  
	  const int arg_count = 1;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  scheme_apply(handler, arg_count, args);
	}
    }
}

void 
XMLCALL _end_cdata_section_handler(void* user_data)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->end_cdata_section_handler;
      if (handler != scheme_null)
	{	  
	  const int arg_count = 1;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  scheme_apply(handler, arg_count, args);
	}
    }
}

void 
XMLCALL _default_handler(void* user_data, 
			 const XML_Char* data, 
			 int len)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->default_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, str);
	  MZ_GC_REG();

	  std::string ascii_str(data, len);
	  str = scheme_make_utf8_string(ascii_str.c_str());

	  const int arg_count = 2;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

int
XMLCALL _external_entity_ref_handler(XML_Parser p,
				     const XML_Char* context,
				     const XML_Char* base,
				     const XML_Char* system_id,
				     const XML_Char* public_id)
{
  Expat_xml_parser* parser = 
    reinterpret_cast<Expat_xml_parser*>(XML_GetUserData(p));
  if (parser)
    {
      Scheme_Object* handler = parser->external_entity_ref_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_context = 0;
	  Scheme_Object* str_base = 0;
	  Scheme_Object* str_system_id = 0;
	  Scheme_Object* str_public_id = 0;
	  MZ_GC_DECL_REG(4);
	  MZ_GC_VAR_IN_REG(0, str_context);
	  MZ_GC_VAR_IN_REG(1, str_base);
	  MZ_GC_VAR_IN_REG(2, str_system_id);
	  MZ_GC_VAR_IN_REG(3, str_public_id);
	  MZ_GC_REG();

	  str_context = scheme_make_utf8_string(context);
	  str_base = scheme_make_utf8_string(base);
	  str_system_id = scheme_make_utf8_string(system_id);
	  str_public_id = scheme_make_utf8_string(public_id);

	  const int arg_count = 5;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_context;
	  args[2] = str_base;
	  args[3] = str_system_id;
	  args[4] = str_public_id;
	  Scheme_Object* ret = scheme_apply(handler, arg_count, args);
	    

	  MZ_GC_UNREG();

	  if (ret == scheme_null)
	    return XML_STATUS_ERROR;
	}
    }
  return XML_STATUS_OK;
}

void 
XMLCALL _skipped_entity_handler(void* user_data,
				const XML_Char* entity_name,
				int is_parameter_entity)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->skipped_entity_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, str);
	  MZ_GC_REG();

	  str = scheme_make_utf8_string(entity_name);

	  const int arg_count = 3;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str;
	  args[2] = (is_parameter_entity > 0) ? scheme_true : scheme_false;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

/*
void 
XMLCALL _unknown_encoding_handler(void* encodingHandlerData,
const XML_Char* name,
XML_Encoding* info)
{
}
*/

void 
XMLCALL _start_namespace_decl_handler(void* user_data,
				      const XML_Char* prefix,
				      const XML_Char* uri)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->start_namespace_decl_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_prefix = 0;
	  Scheme_Object* str_uri = 0;
	  MZ_GC_DECL_REG(2);
	  MZ_GC_VAR_IN_REG(0, str_prefix);
	  MZ_GC_VAR_IN_REG(1, str_uri);
	  MZ_GC_REG();

	  str_prefix = scheme_make_utf8_string(prefix);
	  str_uri = scheme_make_utf8_string(uri);

	  const int arg_count = 3;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_prefix;
	  args[2] = str_uri;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

void 
XMLCALL _end_namespace_decl_handler(void* user_data,
				    const XML_Char* prefix)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->end_namespace_decl_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_prefix = 0;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, str_prefix);
	  MZ_GC_REG();

	  str_prefix = scheme_make_utf8_string(prefix);

	  const int arg_count = 2;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_prefix;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

void 
XMLCALL _xml_decl_handler(void* user_data,
			  const XML_Char* version,
			  const XML_Char* encoding,
			  int standalone)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->xml_decl_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_version = 0;
	  Scheme_Object* str_encoding = 0;
	  MZ_GC_DECL_REG(2);
	  MZ_GC_VAR_IN_REG(0, str_version);
	  MZ_GC_VAR_IN_REG(1, str_encoding);
	  MZ_GC_REG();

	  str_version = scheme_make_utf8_string(version);
	  str_encoding = scheme_make_utf8_string(encoding);

	  const int arg_count = 4;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_version;
	  args[2] = str_encoding;
	  args[3] = scheme_make_integer_value(standalone);
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

void 
XMLCALL _start_doctype_decl_handler(void* user_data,
				    const XML_Char* doctype_name,
				    const XML_Char* sysid,
				    const XML_Char* pubid,
				    int has_internal_subset)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->start_doctype_decl_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_doctype_name = 0;
	  Scheme_Object* str_sysid = 0;
	  Scheme_Object* str_pubid = 0;
	  MZ_GC_DECL_REG(3);
	  MZ_GC_VAR_IN_REG(0, str_doctype_name);
	  MZ_GC_VAR_IN_REG(1, str_sysid);
	  MZ_GC_VAR_IN_REG(2, str_pubid);
	  MZ_GC_REG();

	  str_doctype_name = scheme_make_utf8_string(doctype_name);
	  str_sysid = scheme_make_utf8_string(sysid);
	  str_pubid = scheme_make_utf8_string(pubid);

	  const int arg_count = 5;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_doctype_name;
	  args[2] = str_sysid;
	  args[3] = str_pubid;
	  args[4] = (has_internal_subset > 0) ? scheme_true : scheme_false;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

void 
XMLCALL _end_doctype_decl_handler(void* user_data)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->end_doctype_decl_handler;
      if (handler != scheme_null)
	{	  
	  const int arg_count = 1;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  scheme_apply(handler, arg_count, args);
	}
    }
}

/*
void 
XMLCALL _element_decl_handler(void* user_data,
const XML_Char* name,
XML_Content* model)
{
}
*/

void 
XMLCALL _attlist_decl_handler(void* user_data,
			      const XML_Char *elname,
			      const XML_Char *attname,
			      const XML_Char *att_type,
			      const XML_Char *dflt,
			      int isrequired)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->attlist_decl_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_elname = 0;
	  Scheme_Object* str_attname = 0;
	  Scheme_Object* str_att_type = 0;
	  Scheme_Object* str_dflt = 0;
	  MZ_GC_DECL_REG(4);
	  MZ_GC_VAR_IN_REG(0, str_elname);
	  MZ_GC_VAR_IN_REG(1, str_attname);
	  MZ_GC_VAR_IN_REG(2, str_att_type);
	  MZ_GC_VAR_IN_REG(3, str_dflt);
	  MZ_GC_REG();

	  str_elname = scheme_make_utf8_string(elname);
	  str_attname = scheme_make_utf8_string(attname);
	  str_att_type = scheme_make_utf8_string(att_type);
	  str_dflt = scheme_make_utf8_string(dflt);

	  const int arg_count = 6;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_elname;
	  args[2] = str_attname;
	  args[3] = str_att_type;
	  args[4] = str_dflt;
	  args[5] = (isrequired > 0) ? scheme_true : scheme_false;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

void 
XMLCALL _entity_decl_handler(void* user_data,
			     const XML_Char* entity_name,
			     int is_parameter_entity,
			     const XML_Char* value,
			     int value_length, 
			     const XML_Char* base,
			     const XML_Char* system_id,
			     const XML_Char* public_id,
			     const XML_Char* notation_name)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->entity_decl_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_entity_name = 0;
	  Scheme_Object* str_value = 0;
	  Scheme_Object* str_base = 0;
	  Scheme_Object* str_system_id = 0;
	  Scheme_Object* str_public_id = 0;
	  Scheme_Object* str_notation_name = 0;
	  MZ_GC_DECL_REG(6);
	  MZ_GC_VAR_IN_REG(0, str_entity_name);
	  MZ_GC_VAR_IN_REG(1, str_value);
	  MZ_GC_VAR_IN_REG(2, str_base);
	  MZ_GC_VAR_IN_REG(3, str_system_id);
	  MZ_GC_VAR_IN_REG(4, str_public_id);
	  MZ_GC_VAR_IN_REG(5, str_notation_name);
	  MZ_GC_REG();

	  str_entity_name = scheme_make_utf8_string(entity_name);
	  std::string tmp(value, value_length);
	  str_value = scheme_make_utf8_string(tmp.c_str());
	  str_base = scheme_make_utf8_string(base);
	  str_system_id = scheme_make_utf8_string(system_id);
	  str_public_id = scheme_make_utf8_string(public_id);
	  str_notation_name = scheme_make_utf8_string(notation_name);

	  const int arg_count = 8;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_entity_name;
	  args[2] = (is_parameter_entity > 0) ? scheme_true : scheme_false;
	  args[3] = str_value;
	  args[4] = str_base;
	  args[5] = str_system_id;
	  args[6] = str_public_id;
	  args[7] = str_notation_name;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

void 
XMLCALL _unparsed_entity_decl_handler(void* user_data,
				      const XML_Char* entity_name, 
				      const XML_Char* base,
				      const XML_Char* system_id,
				      const XML_Char* public_id,
				      const XML_Char* notation_name)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->unparsed_entity_decl_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_entity_name = 0;
	  Scheme_Object* str_base = 0;
	  Scheme_Object* str_system_id = 0;
	  Scheme_Object* str_public_id = 0;
	  Scheme_Object* str_notation_name = 0;
	  MZ_GC_DECL_REG(5);
	  MZ_GC_VAR_IN_REG(0, str_entity_name);
	  MZ_GC_VAR_IN_REG(1, str_base);
	  MZ_GC_VAR_IN_REG(2, str_system_id);
	  MZ_GC_VAR_IN_REG(3, str_public_id);
	  MZ_GC_VAR_IN_REG(4, str_notation_name);
	  MZ_GC_REG();

	  str_entity_name = scheme_make_utf8_string(entity_name);
	  str_base = scheme_make_utf8_string(base);
	  str_system_id = scheme_make_utf8_string(system_id);
	  str_public_id = scheme_make_utf8_string(public_id);
	  str_notation_name = scheme_make_utf8_string(notation_name);

	  const int arg_count = 6;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_entity_name;
	  args[2] = str_base;
	  args[3] = str_system_id;
	  args[4] = str_public_id;
	  args[5] = str_notation_name;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

void 
XMLCALL _notation_decl_handler(void* user_data, 
			       const XML_Char* notation_name,
			       const XML_Char* base,
			       const XML_Char* system_id,
			       const XML_Char* public_id)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->notation_decl_handler;
      if (handler != scheme_null)
	{	  
	  Scheme_Object* str_notation_name = 0;
	  Scheme_Object* str_base = 0;
	  Scheme_Object* str_system_id = 0;
	  Scheme_Object* str_public_id = 0;
	  MZ_GC_DECL_REG(4);
	  MZ_GC_VAR_IN_REG(0, str_notation_name);
	  MZ_GC_VAR_IN_REG(1, str_base);
	  MZ_GC_VAR_IN_REG(2, str_system_id);
	  MZ_GC_VAR_IN_REG(3, str_public_id);
	  MZ_GC_REG();

	  str_base = scheme_make_utf8_string(base);
	  str_system_id = scheme_make_utf8_string(system_id);
	  str_public_id = scheme_make_utf8_string(public_id);
	  str_notation_name = scheme_make_utf8_string(notation_name);

	  const int arg_count = 5;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  args[1] = str_notation_name;
	  args[2] = str_base;
	  args[3] = str_system_id;
	  args[4] = str_public_id;
	  scheme_apply(handler, arg_count, args);

	  MZ_GC_UNREG();
	}
    }
}

int
XMLCALL _not_standalone_handler(void* user_data)
{
  Expat_xml_parser* parser = reinterpret_cast<Expat_xml_parser*>(user_data);
  if (parser)
    {
      Scheme_Object* handler = parser->not_standalone_handler;
      if (handler != scheme_null)
	{	  
	  const int arg_count = 1;
	  Scheme_Object* args[arg_count];
	  args[0] = parser->user_data;
	  if (scheme_apply(handler, arg_count, args) == scheme_null)
	    return XML_STATUS_ERROR;
	  return XML_STATUS_OK;
	}
    }
  return XML_STATUS_ERROR;
}

