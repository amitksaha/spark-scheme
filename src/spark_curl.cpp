// MzScheme inetrface to the Curl API.
// Copyright (C) 2008  Vijay Mathew Pandyalakal
 
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

#include <vector>
#include <curl/curl.h>
#include "spark.h"
#include "spark_curl.h"
using namespace spark_curl;

static const char* MODULE_NAME = "#%spark-curl";

enum Curl_tag
  {
    EASY_CURL_TAG,
    MULTI_CURL_TAG
  }; // enum Curl_tag

typedef std::vector<curl_httppost*> Httppost_ptr_vec;
typedef std::map<curl_httppost*, curl_httppost*> Httppost_ptr_map;
typedef std::vector<curl_slist*> Slist_ptr_vec;

struct Curl
{
  CURL* curl;
  spark::Int_so_map callbacks;
  spark::Int_so_map callbacks_data;

  Curl(CURL* c = 0)
    : curl(c)
  { 
    _init_callbacks();
  }

  ~Curl()
  {
    size_t s = _httpposts.size();
    for (size_t i=0; i<s; ++i)
      {
	curl_httppost* p = _httpposts[i];
	if (p)
	  curl_formfree(p);
      }
    s = _slists.size();
    for (size_t i=0; i<s; ++i)
      {
	curl_slist* p = _slists[i];
	if (p)
	  curl_slist_free_all(p);
      }
  }

  size_t
  add_httppost(curl_httppost* p)
  {
    _httpposts.push_back(p);
    return (_httpposts.size() - 1);
  }

  curl_httppost*
  get_httppost(size_t index)
  {
    if (index >= _httpposts.size())
      return 0;
    return _httpposts[index];
  }

  void
  add_slist(curl_slist* slist)
  {
    _slists.push_back(slist);
  }

  void
  set_httppost_last(curl_httppost* p, curl_httppost* last)
  {
    _lasts[p] = last;
  }

  curl_httppost*
  get_httppost_last(curl_httppost* p)
  {
    return _lasts[p];
  }

private:
  void 
  _init_callbacks()
  {
    callbacks[CURLOPT_WRITEFUNCTION] = scheme_null;
    callbacks[CURLOPT_READFUNCTION] = scheme_null;
    callbacks[CURLOPT_IOCTLFUNCTION] = scheme_null;
    callbacks[CURLOPT_SEEKFUNCTION] = scheme_null;
    callbacks[CURLOPT_SOCKOPTFUNCTION] = scheme_null;
    callbacks[CURLOPT_OPENSOCKETFUNCTION] = scheme_null;
    callbacks[CURLOPT_PROGRESSFUNCTION] = scheme_null;
    callbacks[CURLOPT_HEADERFUNCTION] = scheme_null;
    callbacks[CURLOPT_DEBUGFUNCTION] = scheme_null;
    callbacks[CURLOPT_SSL_CTX_FUNCTION] = scheme_null;
    callbacks[CURLOPT_CONV_TO_NETWORK_FUNCTION] = scheme_null;
    callbacks[CURLOPT_CONV_FROM_NETWORK_FUNCTION] = scheme_null;
    callbacks[CURLOPT_CONV_FROM_UTF8_FUNCTION] = scheme_null;
    // data
    callbacks_data[CURLOPT_WRITEDATA] = scheme_null;
    callbacks_data[CURLOPT_READDATA] = scheme_null;
    callbacks_data[CURLOPT_HEADERDATA] = scheme_null;
    callbacks_data[CURLOPT_IOCTLDATA] = scheme_null;
    callbacks_data[CURLOPT_SEEKDATA] = scheme_null;
    callbacks_data[CURLOPT_SOCKOPTDATA] = scheme_null;
    callbacks_data[CURLOPT_OPENSOCKETDATA] = scheme_null;
    callbacks_data[CURLOPT_PROGRESSDATA] = scheme_null;
    callbacks_data[CURLOPT_DEBUGDATA] = scheme_null;
    callbacks_data[CURLOPT_SSL_CTX_DATA] = scheme_null;
  }
private:
  Httppost_ptr_vec _httpposts;
  Httppost_ptr_map _lasts;
  Slist_ptr_vec _slists;
}; // struct Curl

static spark::Status_code _add_constants(Scheme_Env* env);
static spark::Status_code _add_procedures(Scheme_Env* env);

static bool _init_default_functions(CURL* curl);
static Curl* _scheme_object_to_curl_wrapper(int argc,
					    Scheme_Object** argv,
					    int idx);
static CURL* _scheme_object_to_curl(int argc, 
				    Scheme_Object** argv,
				    int idx);	
static CURLM* _scheme_object_to_multi_curl(int argc, 
					   Scheme_Object** argv,
					   int idx);	
static Scheme_Object* _long_to_auth_list(long v);
static curl_slist* _curl_slist_from_scheme_list(Scheme_Object* lst);

// low-level callbacks
static size_t _generic_curl_read_callback(void* ptr, 
					  size_t size,
					  size_t nmemb, 
					  void* stream);
static size_t _generic_curl_write_callback(void* ptr, 
					   size_t size,
					   size_t nmemb, 
					   void* stream);
static size_t _generic_curl_header_callback(void* ptr, 
					    size_t size,
					    size_t nmemb, 
					    void* stream);
static curlioerr _generic_curl_ioctl_callback(CURL* handle, 
					      int cmd, 
					      void* p);
static int _generic_curl_seek_callback(void* instream, 
				       curl_off_t offset,
				       int origin);
static size_t _generic_curl_sockopt_callback(void* clientp,
					     curl_socket_t curlfd,
					     curlsocktype purpose);
static size_t _generic_curl_opensocket_callback(void* clientp,
						curlsocktype purpose,
						struct curl_sockaddr* address);
static int _generic_curl_progress_callback(void* clientp,
					   double dltotal,
					   double dlnow,
					   double ultotal,
					   double ulnow);
static int _generic_curl_debug_callback(CURL* handle,
					curl_infotype ci,
					char* s,
					size_t sz,
					void* p);
static CURLcode _generic_curl_ssl_ctx_callback(CURL* handle,
					       void* sslctx,
					       void* parm);
/*
static CURLcode _generic_curl_conv_to_network_callback(char* ptr, size_t len);
static CURLcode _generic_curl_conv_from_network_callback(char* ptr, 
							 size_t len);
static CURLcode _generic_curl_conv_from_utf8_callback(char* ptr, 
						      size_t len);
*/


spark::Status_code
spark_curl::initialize(Scheme_Env* env)
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
namespace spark_curl
{
  static Scheme_Object* curl_global_init(int, Scheme_Object**);
  static Scheme_Object* curl_global_cleanup(int, Scheme_Object**);
  static Scheme_Object* curl_easy_init(int, Scheme_Object**);
  static Scheme_Object* curl_easy_cleanup(int, Scheme_Object**);
  static Scheme_Object* curl_easy_duphandle(int, Scheme_Object**);
  static Scheme_Object* curl_easy_getinfo(int, Scheme_Object**);
  static Scheme_Object* curl_easy_perform(int, Scheme_Object**);
  static Scheme_Object* curl_easy_reset(int, Scheme_Object**);
  static Scheme_Object* curl_easy_setopt(int, Scheme_Object**);
  static Scheme_Object* curl_easy_strerror(int, Scheme_Object**);
  static Scheme_Object* curl_easy_unescape(int, Scheme_Object**);
  static Scheme_Object* curl_multi_init(int, Scheme_Object**);
  static Scheme_Object* curl_multi_cleanup(int, Scheme_Object**);
  static Scheme_Object* curl_multi_perform(int, Scheme_Object**);
  static Scheme_Object* curl_multi_add_handle(int, Scheme_Object**);
  static Scheme_Object* curl_multi_remove_handle(int, Scheme_Object**);
  static Scheme_Object* curl_multi_timeout(int, Scheme_Object**);
  static Scheme_Object* curl_multi_strerror(int, Scheme_Object**);
  static Scheme_Object* curl_formadd(int, Scheme_Object**);
} // namespace spark_curl

spark::Status_code
_add_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    // global init options
    Constant("CURL-GLOBAL-ALL", CURL_GLOBAL_ALL),
    Constant("CURL-GLOBAL-SSL", CURL_GLOBAL_SSL),
    Constant("CURL-GLOBAL-WIN32", CURL_GLOBAL_WIN32),
    Constant("CURL-GLOBAL-NOTHING", CURL_GLOBAL_NOTHING),
    // curl info
    Constant("CURLINFO-EFFECTIVE-URL", CURLINFO_EFFECTIVE_URL),
    Constant("CURLINFO-RESPONSE-CODE", CURLINFO_RESPONSE_CODE),
    Constant("CURLINFO-HTTP-CONNECTCODE", CURLINFO_HTTP_CONNECTCODE),
    Constant("CURLINFO-FILETIME", CURLINFO_FILETIME),
    Constant("CURLINFO-TOTAL-TIME", CURLINFO_TOTAL_TIME),
    Constant("CURLINFO-NAMELOOKUP-TIME", CURLINFO_NAMELOOKUP_TIME),
    Constant("CURLINFO-CONNECT-TIME", CURLINFO_CONNECT_TIME),
    Constant("CURLINFO-PRETRANSFER-TIME", CURLINFO_PRETRANSFER_TIME),
    Constant("CURLINFO-STARTTRANSFER-TIME", CURLINFO_STARTTRANSFER_TIME),
    Constant("CURLINFO-REDIRECT-TIME", CURLINFO_REDIRECT_TIME),
    Constant("CURLINFO-REDIRECT-COUNT", CURLINFO_REDIRECT_COUNT),
    Constant("CURLINFO-SIZE-UPLOAD", CURLINFO_SIZE_UPLOAD),
    Constant("CURLINFO-SIZE-DOWNLOAD", CURLINFO_SIZE_DOWNLOAD),
    Constant("CURLINFO-SPEED-UPLOAD", CURLINFO_SPEED_UPLOAD),
    Constant("CURLINFO-SPEED-DOWNLOAD", CURLINFO_SPEED_DOWNLOAD),
    Constant("CURLINFO-HEADER-SIZE", CURLINFO_HEADER_SIZE),
    Constant("CURLINFO-REQUEST-SIZE", CURLINFO_REQUEST_SIZE),
    Constant("CURLINFO-SSL-VERIFYRESULT", CURLINFO_SSL_VERIFYRESULT),
    Constant("CURLINFO-SSL-ENGINES", CURLINFO_SSL_ENGINES),
    Constant("CURLINFO-CONTENT-LENGTH-DOWNLOAD", 
	     CURLINFO_CONTENT_LENGTH_DOWNLOAD),
    Constant("CURLINFO-CONTENT-LENGTH-UPLOAD", 
	     CURLINFO_CONTENT_LENGTH_UPLOAD),
    Constant("CURLINFO-CONTENT-TYPE", CURLINFO_CONTENT_TYPE),
    Constant("CURLINFO-PRIVATE", CURLINFO_PRIVATE),
    Constant("CURLINFO-HTTPAUTH-AVAIL", CURLINFO_HTTPAUTH_AVAIL),
    Constant("CURLINFO-PROXYAUTH-AVAIL", CURLINFO_PROXYAUTH_AVAIL),
    Constant("CURLINFO-OS-ERRNO", CURLINFO_OS_ERRNO),
    Constant("CURLINFO-NUM-CONNECTS", CURLINFO_NUM_CONNECTS),
    Constant("CURLINFO-COOKIELIST", CURLINFO_COOKIELIST),
    Constant("CURLINFO-LASTSOCKET", CURLINFO_LASTSOCKET),
    Constant("CURLINFO-FTP-ENTRY-PATH", CURLINFO_FTP_ENTRY_PATH),
    // opts
    Constant("CURLOPT-VERBOSE", CURLOPT_VERBOSE),
    Constant("CURLOPT-HEADER", CURLOPT_HEADER),
    Constant("CURLOPT-NOPROGRESS", CURLOPT_NOPROGRESS),
    Constant("CURLOPT-NOSIGNAL", CURLOPT_NOSIGNAL),
    Constant("CURLOPT-WRITEFUNCTION", CURLOPT_WRITEFUNCTION),
    Constant("CURLOPT-READFUNCTION", CURLOPT_READFUNCTION),
    Constant("CURLOPT-IOCTLFUNCTION", CURLOPT_IOCTLFUNCTION),
    Constant("CURLOPT-SEEKFUNCTION", CURLOPT_SEEKFUNCTION),
    Constant("CURLOPT-SOCKOPTFUNCTION", CURLOPT_SOCKOPTFUNCTION),
    Constant("CURLOPT-OPENSOCKETFUNCTION", CURLOPT_OPENSOCKETFUNCTION),
    Constant("CURLOPT-PROGRESSFUNCTION", CURLOPT_PROGRESSFUNCTION),
    Constant("CURLOPT-HEADERFUNCTION", CURLOPT_HEADERFUNCTION),
    Constant("CURLOPT-DEBUGFUNCTION", CURLOPT_DEBUGFUNCTION),
    Constant("CURLOPT-SSL-CTX-FUNCTION", CURLOPT_SSL_CTX_FUNCTION),
    Constant("CURLOPT-CONV-TO-NETWORK-FUNCTION", CURLOPT_CONV_TO_NETWORK_FUNCTION),
    Constant("CURLOPT-CONV-FROM-NETWORK-FUNCTION", CURLOPT_CONV_FROM_NETWORK_FUNCTION),
    Constant("CURLOPT-CONV-FROM-UTF8-FUNCTION", CURLOPT_CONV_FROM_UTF8_FUNCTION),
    Constant("CURLOPT-WRITEDATA", CURLOPT_WRITEDATA),
    Constant("CURLOPT-READDATA", CURLOPT_READDATA),
    Constant("CURLOPT-HEADERDATA", CURLOPT_HEADERDATA),
    Constant("CURLOPT-IOCTLDATA", CURLOPT_IOCTLDATA),
    Constant("CURLOPT-SEEKDATA", CURLOPT_SEEKDATA),
    Constant("CURLOPT-SOCKOPTDATA", CURLOPT_SOCKOPTDATA),
    Constant("CURLOPT-OPENSOCKETDATA", CURLOPT_OPENSOCKETDATA),
    Constant("CURLOPT-PROGRESSDATA", CURLOPT_PROGRESSDATA),
    Constant("CURLOPT-DEBUGDATA", CURLOPT_DEBUGDATA),
    Constant("CURLOPT-SSL-CTX-DATA", CURLOPT_SSL_CTX_DATA),
    Constant("CURLOPT-WRITEHEADER", CURLOPT_WRITEHEADER),
    Constant("CURLOPT-ERRORBUFFER", CURLOPT_ERRORBUFFER),
    Constant("CURLOPT-STDERR", CURLOPT_STDERR),
    Constant("CURLOPT-FAILONERROR", CURLOPT_FAILONERROR),
    Constant("CURLOPT-URL", CURLOPT_URL),
    Constant("CURLOPT-PROXY", CURLOPT_PROXY),
    Constant("CURLOPT-PROXYPORT", CURLOPT_PROXYPORT),
    Constant("CURLOPT-PROXYTYPE", CURLOPT_PROXYTYPE),
    Constant("CURLOPT-HTTPPROXYTUNNEL", CURLOPT_HTTPPROXYTUNNEL),
    Constant("CURLOPT-INTERFACE", CURLOPT_INTERFACE),
    Constant("CURLOPT-LOCALPORT", CURLOPT_LOCALPORT),
    Constant("CURLOPT-LOCALPORTRANGE", CURLOPT_LOCALPORTRANGE),
    Constant("CURLOPT-DNS-CACHE-TIMEOUT", CURLOPT_DNS_CACHE_TIMEOUT),
    Constant("CURLOPT-DNS-USE-GLOBAL-CACHE", CURLOPT_DNS_USE_GLOBAL_CACHE),
    Constant("CURLOPT-BUFFERSIZE", CURLOPT_BUFFERSIZE),
    Constant("CURLOPT-PORT", CURLOPT_PORT),
    Constant("CURLOPT-TCP_NODELAY", CURLOPT_TCP_NODELAY),
    Constant("CURLOPT-NETRC", CURLOPT_NETRC),
    Constant("CURLOPT-NETRC-FILE", CURLOPT_NETRC_FILE),
    Constant("CURLOPT-USERPWD", CURLOPT_USERPWD),
    Constant("CURLOPT-PROXYUSERPWD", CURLOPT_PROXYUSERPWD),
    Constant("CURLOPT-HTTPAUTH", CURLOPT_HTTPAUTH),
    Constant("CURLOPT-PROXYAUTH", CURLOPT_PROXYAUTH),
    Constant("CURLOPT-AUTOREFERER", CURLOPT_AUTOREFERER),
    Constant("CURLOPT-ENCODING", CURLOPT_ENCODING),
    Constant("CURLOPT-FOLLOWLOCATION", CURLOPT_FOLLOWLOCATION),
    Constant("CURLOPT-UNRESTRICTED-AUTH", CURLOPT_UNRESTRICTED_AUTH),
    Constant("CURLOPT-MAXREDIRS", CURLOPT_MAXREDIRS),
    Constant("CURLOPT-POST301", CURLOPT_POST301),
    Constant("CURLOPT-PUT", CURLOPT_PUT),
    Constant("CURLOPT-POST", CURLOPT_POST),
    Constant("CURLOPT-POSTFIELDS", CURLOPT_POSTFIELDS),
    Constant("CURLOPT-POSTFIELDSIZE", CURLOPT_POSTFIELDSIZE),
    Constant("CURLOPT-POSTFIELDSIZE-LARGE", CURLOPT_POSTFIELDSIZE_LARGE),
    Constant("CURLOPT-COPYPOSTFIELDS", CURLOPT_COPYPOSTFIELDS),
    Constant("CURLOPT-HTTPPOST", CURLOPT_HTTPPOST),
    Constant("CURLOPT-REFERER", CURLOPT_REFERER),
    Constant("CURLOPT-USERAGENT", CURLOPT_USERAGENT),
    Constant("CURLOPT-HTTPHEADER", CURLOPT_HTTPHEADER),
    Constant("CURLOPT-HTTP200ALIASES", CURLOPT_HTTP200ALIASES),
    Constant("CURLOPT-COOKIE", CURLOPT_COOKIE),
    Constant("CURLOPT-COOKIEFILE", CURLOPT_COOKIEFILE),
    Constant("CURLOPT-COOKIEJAR", CURLOPT_COOKIEJAR),
    Constant("CURLOPT-COOKIESESSION", CURLOPT_COOKIESESSION),
    Constant("CURLOPT-COOKIELIST", CURLOPT_COOKIELIST),
    Constant("CURLOPT-HTTPGET", CURLOPT_HTTPGET),
    Constant("CURLOPT-HTTP-VERSION", CURLOPT_HTTP_VERSION),
    Constant("CURLOPT-IGNORE-CONTENT-LENGTH", CURLOPT_IGNORE_CONTENT_LENGTH),
    Constant("CURLOPT-HTTP-CONTENT-DECODING", CURLOPT_HTTP_CONTENT_DECODING),
    Constant("CURLOPT-HTTP-TRANSFER-DECODING", CURLOPT_HTTP_TRANSFER_DECODING),
    Constant("CURLOPT-FTPPORT", CURLOPT_FTPPORT),
    Constant("CURLOPT-QUOTE", CURLOPT_QUOTE),
    Constant("CURLOPT-POSTQUOTE", CURLOPT_POSTQUOTE),
    Constant("CURLOPT-PREQUOTE", CURLOPT_PREQUOTE),
    Constant("CURLOPT-DIRLISTONLY", CURLOPT_DIRLISTONLY),
    Constant("CURLOPT-APPEND", CURLOPT_APPEND),
    Constant("CURLOPT-FTP-USE-EPRT", CURLOPT_FTP_USE_EPRT),
    Constant("CURLOPT-FTP-USE-EPSV", CURLOPT_FTP_USE_EPSV),
    Constant("CURLOPT-FTP-CREATE-MISSING-DIRS", CURLOPT_FTP_CREATE_MISSING_DIRS),
    Constant("CURLOPT-FTP-RESPONSE-TIMEOUT", CURLOPT_FTP_RESPONSE_TIMEOUT),
    Constant("CURLOPT-FTP-ALTERNATIVE-TO-USER", CURLOPT_FTP_ALTERNATIVE_TO_USER),
    Constant("CURLOPT-FTP-SKIP-PASV-IP", CURLOPT_FTP_SKIP_PASV_IP),
    Constant("CURLOPT-USE-SSL", CURLOPT_USE_SSL),
    Constant("CURLOPT-FTPSSLAUTH", CURLOPT_FTPSSLAUTH),
    Constant("CURLOPT-FTP-SSL-CCC", CURLOPT_FTP_SSL_CCC),
    Constant("CURLOPT-FTP-ACCOUNT", CURLOPT_FTP_ACCOUNT),
    Constant("CURLOPT-FTP-FILEMETHOD", CURLOPT_FTP_FILEMETHOD),
    Constant("CURLOPT-TRANSFERTEXT", CURLOPT_TRANSFERTEXT),
    Constant("CURLOPT-PROXY-TRANSFER-MODE", CURLOPT_PROXY_TRANSFER_MODE),
    Constant("CURLOPT-CRLF", CURLOPT_CRLF),
    Constant("CURLOPT-RANGE", CURLOPT_RANGE),
    Constant("CURLOPT-RESUME-FROM", CURLOPT_RESUME_FROM),
    Constant("CURLOPT-RESUME-FROM-LARGE", CURLOPT_RESUME_FROM_LARGE),
    Constant("CURLOPT-CUSTOMREQUEST", CURLOPT_CUSTOMREQUEST),
    Constant("CURLOPT-FILETIME", CURLOPT_FILETIME),
    Constant("CURLOPT-NOBODY", CURLOPT_NOBODY),
    Constant("CURLOPT-INFILESIZE", CURLOPT_INFILESIZE),
    Constant("CURLOPT-INFILESIZE-LARGE", CURLOPT_INFILESIZE_LARGE),
    Constant("CURLOPT-UPLOAD", CURLOPT_UPLOAD),
    Constant("CURLOPT-MAXFILESIZE", CURLOPT_MAXFILESIZE),
    Constant("CURLOPT-MAXFILESIZE-LARGE", CURLOPT_MAXFILESIZE_LARGE),
    Constant("CURLOPT-TIMECONDITION", CURLOPT_TIMECONDITION),
    Constant("CURLOPT-TIMEVALUE", CURLOPT_TIMEVALUE),
    Constant("CURLOPT-TIMEOUT", CURLOPT_TIMEOUT),
    Constant("CURLOPT-TIMEOUT-MS", CURLOPT_TIMEOUT_MS),
    Constant("CURLOPT-LOW-SPEED-LIMIT", CURLOPT_LOW_SPEED_LIMIT),
    Constant("CURLOPT-LOW-SPEED-TIME", CURLOPT_LOW_SPEED_TIME),
    Constant("CURLOPT-MAX-SEND-SPEED-LARGE", CURLOPT_MAX_SEND_SPEED_LARGE),
    Constant("CURLOPT-MAX-RECV-SPEED-LARGE", CURLOPT_MAX_RECV_SPEED_LARGE),
    Constant("CURLOPT-MAXCONNECTS", CURLOPT_MAXCONNECTS),
    Constant("CURLOPT-CLOSEPOLICY", CURLOPT_CLOSEPOLICY),
    Constant("CURLOPT-FRESH-CONNECT", CURLOPT_FRESH_CONNECT),
    Constant("CURLOPT-FORBID-REUSE", CURLOPT_FORBID_REUSE),
    Constant("CURLOPT-CONNECTTIMEOUT", CURLOPT_CONNECTTIMEOUT),
    Constant("CURLOPT-CONNECTTIMEOUT-MS", CURLOPT_CONNECTTIMEOUT_MS),
    Constant("CURLOPT-IPRESOLVE", CURLOPT_IPRESOLVE),
    Constant("CURLOPT-CONNECT-ONLY", CURLOPT_CONNECT_ONLY),
    Constant("CURLOPT-SSLCERT", CURLOPT_SSLCERT),
    Constant("CURLOPT-SSLCERTTYPE", CURLOPT_SSLCERTTYPE),
    Constant("CURLOPT-SSLKEY", CURLOPT_SSLKEY),
    Constant("CURLOPT-SSLKEYTYPE", CURLOPT_SSLKEYTYPE),
    Constant("CURLOPT-KEYPASSWD", CURLOPT_KEYPASSWD),
    Constant("CURLOPT-SSLENGINE", CURLOPT_SSLENGINE),
    Constant("CURLOPT-SSLENGINE-DEFAULT", CURLOPT_SSLENGINE_DEFAULT),
    Constant("CURLOPT-SSLVERSION", CURLOPT_SSLVERSION),
    Constant("CURLOPT-SSL-VERIFYPEER", CURLOPT_SSL_VERIFYPEER),
    Constant("CURLOPT-CAINFO", CURLOPT_CAINFO),
    Constant("CURLOPT-CAPATH", CURLOPT_CAPATH),
    Constant("CURLOPT-RANDOM-FILE", CURLOPT_RANDOM_FILE),
    Constant("CURLOPT-EGDSOCKET", CURLOPT_EGDSOCKET),
    Constant("CURLOPT-SSL-VERIFYHOST", CURLOPT_SSL_VERIFYHOST),
    Constant("CURLOPT-SSL-CIPHER-LIST", CURLOPT_SSL_CIPHER_LIST),
    Constant("CURLOPT-SSL-SESSIONID-CACHE", CURLOPT_SSL_SESSIONID_CACHE),
    Constant("CURLOPT-KRBLEVEL", CURLOPT_KRBLEVEL),
    Constant("CURLOPT-SSH-AUTH-TYPES", CURLOPT_SSH_AUTH_TYPES),
    Constant("CURLOPT-SSH-HOST-PUBLIC-KEY-MD5", CURLOPT_SSH_HOST_PUBLIC_KEY_MD5),
    Constant("CURLOPT-SSH-PUBLIC-KEYFILE", CURLOPT_SSH_PUBLIC_KEYFILE),
    Constant("CURLOPT-SSH-PRIVATE-KEYFILE", CURLOPT_SSH_PRIVATE_KEYFILE),
    Constant("CURLOPT-PRIVATE", CURLOPT_PRIVATE),
    Constant("CURLOPT-SHARE", CURLOPT_SHARE),
    Constant("CURLOPT-NEW-FILE-PERMS", CURLOPT_NEW_FILE_PERMS),
    Constant("CURLOPT-NEW-DIRECTORY-PERMS", CURLOPT_NEW_DIRECTORY_PERMS),
    Constant("CURLOPT-TELNETOPTIONS", CURLOPT_TELNETOPTIONS),
    Constant("CURLOPT-WRITEINFO", CURLOPT_WRITEINFO),
    Constant("CURLOPT-LASTENTRY", CURLOPT_LASTENTRY),    
    // curl_infotypes
    Constant("CURLINFO-TEXT", CURLINFO_TEXT),    
    Constant("CURLINFO-HEADER-IN", CURLINFO_HEADER_IN),    
    Constant("CURLINFO-HEADER-OUT", CURLINFO_HEADER_OUT),  
    Constant("CURLINFO-DATA-IN", CURLINFO_DATA_IN),    
    Constant("CURLINFO-DATA-OUT", CURLINFO_DATA_OUT),  
    // proxy types  
    Constant("CURLPROXY-HTTP", CURLPROXY_HTTP),
    Constant("CURLPROXY-SOCKS4", CURLPROXY_SOCKS4),
    Constant("CURLPROXY-SOCKS5", CURLPROXY_SOCKS5),
    Constant("CURLPROXY-SOCKS4A", CURLPROXY_SOCKS4A),
    Constant("CURLPROXY-SOCKS5-HOSTNAME", CURLPROXY_SOCKS5_HOSTNAME),
    // netrc options
    Constant("CURL-NETRC-OPTIONAL", CURL_NETRC_OPTIONAL),
    Constant("CURL-NETRC-IGNORED", CURL_NETRC_IGNORED),
    Constant("CURL-NETRC-REQUIRED", CURL_NETRC_REQUIRED),
    // HTTP Auths
    Constant("CURLAUTH-BASIC", CURLAUTH_BASIC),
    Constant("CURLAUTH-DIGEST", CURLAUTH_DIGEST),
    Constant("CURLAUTH-GSSNEGOTIATE", CURLAUTH_GSSNEGOTIATE),
    Constant("CURLAUTH-NTLM", CURLAUTH_NTLM),
    Constant("CURLAUTH-ANY", CURLAUTH_ANY),
    Constant("CURLAUTH-ANYSAFE", CURLAUTH_ANYSAFE),
    // HTTP version
    Constant("CURL-HTTP-VERSION-NONE", CURL_HTTP_VERSION_NONE),
    Constant("CURL-HTTP-VERSION-1-0", CURL_HTTP_VERSION_1_0),
    Constant("CURL-HTTP-VERSION-1-1", CURL_HTTP_VERSION_1_1),
    // SSL for FTP
    Constant("CURLUSESSL-NONE", CURLUSESSL_NONE),
    Constant("CURLUSESSL-TRY", CURLUSESSL_TRY),
    Constant("CURLUSESSL-CONTROL", CURLUSESSL_CONTROL),
    Constant("CURLUSESSL-ALL", CURLUSESSL_ALL),
    // FTP SSL Auth
    Constant("CURLFTPAUTH-DEFAULT", CURLFTPAUTH_DEFAULT),
    Constant("CURLFTPAUTH-SSL", CURLFTPAUTH_SSL),
    Constant("CURLFTPAUTH-TLS", CURLFTPAUTH_TLS),
    // FTP CCC Options
    Constant("CURLFTPSSL-CCC-NONE", CURLFTPSSL_CCC_NONE),
    Constant("CURLFTPSSL-CCC-PASSIVE", CURLFTPSSL_CCC_PASSIVE),
    Constant("CURLFTPSSL-CCC-ACTIVE", CURLFTPSSL_CCC_ACTIVE),
    // FTP File methods
    Constant("CURLFTPMETHOD-MULTICWD", CURLFTPMETHOD_MULTICWD),
    Constant("CURLFTPMETHOD-NOCWD", CURLFTPMETHOD_NOCWD),
    Constant("CURLFTPMETHOD-SINGLECWD", CURLFTPMETHOD_SINGLECWD),
    // HTTP/FTP Time condition
    Constant("CURL-TIMECOND-IFMODSINCE", CURL_TIMECOND_IFMODSINCE),
    Constant("CURL-TIMECOND-IFUNMODSINCE", CURL_TIMECOND_IFUNMODSINCE),
    // IP-resolve options
    Constant("CURL-IPRESOLVE-WHATEVER", CURL_IPRESOLVE_WHATEVER),
    Constant("CURL-IPRESOLVE-V4", CURL_IPRESOLVE_V4),
    Constant("CURL-IPRESOLVE-V6", CURL_IPRESOLVE_V6),
    // SSL Versions
    Constant("CURL-SSLVERSION-DEFAULT", CURL_SSLVERSION_DEFAULT),
    Constant("CURL-SSLVERSION-TLSv1", CURL_SSLVERSION_TLSv1),
    Constant("CURL-SSLVERSION-SSLv2", CURL_SSLVERSION_SSLv2),
    Constant("CURL-SSLVERSION-SSLv3", CURL_SSLVERSION_SSLv3),
    // SSH Auth types
    Constant("CURLSSH-AUTH-PUBLICKEY", CURLSSH_AUTH_PUBLICKEY),
    Constant("CURLSSH-AUTH-PASSWORD", CURLSSH_AUTH_PASSWORD),
    Constant("CURLSSH-AUTH-HOST", CURLSSH_AUTH_HOST),
    Constant("CURLSSH-AUTH-KEYBOARD", CURLSSH_AUTH_KEYBOARD),
    Constant("CURLSSH-AUTH-ANY", CURLSSH_AUTH_ANY),
    // curl forms
    Constant("CURLFORM-COPYNAME", CURLFORM_COPYNAME),
    Constant("CURLFORM-PTRNAME", CURLFORM_PTRNAME),
    Constant("CURLFORM-COPYCONTENTS", CURLFORM_COPYCONTENTS),
    Constant("CURLFORM-PTRCONTENTS", CURLFORM_PTRCONTENTS),
    Constant("CURLFORM-CONTENTSLENGTH", CURLFORM_CONTENTSLENGTH),
    Constant("CURLFORM-FILECONTENT", CURLFORM_FILECONTENT),
    Constant("CURLFORM-FILE", CURLFORM_FILE),
    Constant("CURLFORM-CONTENTTYPE", CURLFORM_CONTENTTYPE),
    Constant("CURLFORM-FILENAME", CURLFORM_FILENAME),
    Constant("CURLFORM-BUFFERPTR", CURLFORM_BUFFER),
    Constant("CURLFORM-BUFFERLENGTH", CURLFORM_BUFFERLENGTH),
    Constant("CURLFORM-ARRAY", CURLFORM_ARRAY),
    Constant("CURLFORM-CONTENTHEADER", CURLFORM_CONTENTHEADER),
    Constant("CURLFORM-END", CURLFORM_END),

    Constant("", 0),
  };
  return add_constants(env, constants, "spark-curl");
}

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_curl::curl_global_init, 
		  "curl-global-init", 0, 1),
    new Procedure(spark_curl::curl_global_cleanup, 
		  "curl-global-cleanup", 0),
    new Procedure(spark_curl::curl_easy_init, 
		  "curl-easy-init", 0),
    new Procedure(spark_curl::curl_easy_cleanup, 
		  "curl-easy-cleanup", 1),
    new Procedure(spark_curl::curl_easy_duphandle, 
		  "curl-easy-duphandle", 1),
    new Procedure(spark_curl::curl_easy_perform, 
		  "curl-easy-perform", 1),
    new Procedure(spark_curl::curl_easy_getinfo, 
		  "curl-easy-getinfo", 2),
    new Procedure(spark_curl::curl_easy_reset, 
		  "curl-easy-reset", 1),
    new Procedure(spark_curl::curl_easy_setopt, 
		  "curl-easy-setopt", 3),
    new Procedure(spark_curl::curl_easy_strerror, 
		  "curl-easy-strerror", 0),
    new Procedure(spark_curl::curl_easy_unescape, 
		  "curl-easy-unescape", 2),
    new Procedure(spark_curl::curl_formadd, 
		  "curl-formadd", 2, -1),
    new Procedure(spark_curl::curl_multi_init, 
		  "curl-multi-init", 0),
    new Procedure(spark_curl::curl_multi_cleanup, 
		  "curl-multi-cleanup", 1),
    new Procedure(spark_curl::curl_multi_add_handle, 
		  "curl-multi-add-handle", 2),
    new Procedure(spark_curl::curl_multi_remove_handle, 
		  "curl-multi-remove-handle", 2),
    new Procedure(spark_curl::curl_multi_perform, 
		  "curl-multi-perform", 1),
    new Procedure(spark_curl::curl_multi_strerror, 
		  "curl-multi-strerror", 0),
    new Procedure(spark_curl::curl_multi_timeout, 
		  "curl-multi-timeout", 1),
    0
  };
  return spark::add_procedures(env, procedures, "spark-curl");
}

static CURLcode _last_error = CURLE_OK;
static CURLMcode _last_m_error = CURLM_OK;

// Exported Curl API

Scheme_Object* 
spark_curl::curl_global_init(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  int flags = CURL_GLOBAL_ALL;
  if (argc == 1)
    spark::Utils::int_from_scheme_long(argv[0], flags);
  _last_error = ::curl_global_init(flags);
  if (_last_error == CURLE_OK)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_global_cleanup(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  ::curl_global_cleanup();
  _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_init(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  CURL* curl = ::curl_easy_init();
  if (curl)
    {
      if (!_init_default_functions(curl))
	{
	  ::curl_easy_cleanup(curl);
	  DEFAULT_RET_FINISH;
	}
      Curl* c = new Curl(curl);
      Curl_tag t = EASY_CURL_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	_ret_ = scheme_make_cptr(c, tag);
	MZ_GC_UNREG();
      }
    }
  else
    _last_error = CURLE_FAILED_INIT;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_cleanup(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  Curl* curl = _scheme_object_to_curl_wrapper(argc, argv, 0);
  if (curl)
    {
      if (curl->curl)
	::curl_easy_cleanup(curl->curl);
      delete curl;
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_duphandle(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  CURL* curl = _scheme_object_to_curl(argc, argv, 0);
  if (curl)
    {
      CURL* curl_dup = ::curl_easy_duphandle(curl);
      if (curl_dup)
	{
	  Curl_tag t = EASY_CURL_TAG;
	  {
	    Scheme_Object* tag = 0;
	    MZ_GC_DECL_REG(1);
	    MZ_GC_VAR_IN_REG(0, tag);
	    MZ_GC_REG();
	    tag = scheme_make_integer(t);
	    _ret_ = scheme_make_cptr(curl_dup, tag);
	    MZ_GC_UNREG();
	  }
	}
    }
  else
    _last_error = CURLE_FAILED_INIT;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_getinfo(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  CURL* curl = _scheme_object_to_curl(argc, argv, 0);
  if (curl)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      CURLINFO info = static_cast<CURLINFO>(i);
      switch (info)
	{
	case CURLINFO_EFFECTIVE_URL:
	case CURLINFO_CONTENT_TYPE:
	case CURLINFO_PRIVATE:
	case CURLINFO_FTP_ENTRY_PATH:
	  {
	    char* s = 0;
	    _last_error = ::curl_easy_getinfo(curl, info, s);
	    if (_last_error == CURLE_OK && s)
	      _ret_ = scheme_make_utf8_string(s);
	    break;
	  }
	case CURLINFO_RESPONSE_CODE:
	case CURLINFO_HTTP_CONNECTCODE:
	case CURLINFO_FILETIME:
	case CURLINFO_REDIRECT_COUNT:
	case CURLINFO_HEADER_SIZE:
	case CURLINFO_HTTPAUTH_AVAIL:
	case CURLINFO_PROXYAUTH_AVAIL:
	case CURLINFO_OS_ERRNO:
	case CURLINFO_NUM_CONNECTS:
	case CURLINFO_LASTSOCKET:
	  {
	    long v = 0;
	    _last_error = ::curl_easy_getinfo(curl, info, &v);
	    if (_last_error == CURLE_OK)
	      {
		if (info == CURLINFO_HTTPAUTH_AVAIL
		    || info == CURLINFO_PROXYAUTH_AVAIL)
		  _ret_ = _long_to_auth_list(v);
		else
		  _ret_ = scheme_make_integer_value(v);
		break;
	      }	
	  }	    
	case CURLINFO_TOTAL_TIME:
	case CURLINFO_NAMELOOKUP_TIME:
	case CURLINFO_CONNECT_TIME:
	case CURLINFO_PRETRANSFER_TIME:
	case CURLINFO_STARTTRANSFER_TIME:
	case CURLINFO_REDIRECT_TIME:
	case CURLINFO_SIZE_UPLOAD:
	case CURLINFO_SIZE_DOWNLOAD:
	case CURLINFO_SPEED_UPLOAD:
	case CURLINFO_SPEED_DOWNLOAD:
	case CURLINFO_CONTENT_LENGTH_UPLOAD:
	case CURLINFO_CONTENT_LENGTH_DOWNLOAD:
	  {
	    double d = 0.0f;
	    _last_error = ::curl_easy_getinfo(curl, info, &d);
	    if (_last_error == CURLE_OK)
	      _ret_ = scheme_make_float(static_cast<float>(d));
	    break;
	  }
	case CURLINFO_SSL_ENGINES:
	case CURLINFO_COOKIELIST:
	  {
	    curl_slist* csl = 0;
	    _last_error = ::curl_easy_getinfo(curl, info, &csl);
	    if (_last_error == CURLE_OK)
	      {
		std::vector<std::string> svec;
		curl_slist* tmp = csl;
		while (csl)
		  {
		    if (csl->data)
		      svec.push_back(csl->data);
		    csl = csl->next;
		  }
		size_t sz = svec.size();
		if (sz > 0)
		  {
		    Scheme_Object** elems = new Scheme_Object*[sz];
		    for (size_t i=0; i<sz; ++i)
		      {
			Scheme_Object* obj = NULL;
			MZ_GC_DECL_REG(1);
			MZ_GC_VAR_IN_REG(0, obj);
			MZ_GC_REG();
			obj = scheme_make_utf8_string(svec[i].c_str());
			elems[i] = obj;
			MZ_GC_UNREG();
		      }
		    _ret_ = scheme_build_list(sz, elems);      
		    delete[] elems;
		  }
		csl = tmp;
		::curl_slist_free_all(csl);
	      }
	    break;
	  }
	default:
	  scheme_signal_error("Invalid CURLINFO.");
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_perform(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  CURL* curl = _scheme_object_to_curl(argc, argv, 0);
  if (curl)
    {
      if ((_last_error = ::curl_easy_perform(curl)) == CURLE_OK)
	_ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_reset(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  CURL* curl = _scheme_object_to_curl(argc, argv, 0);
  if (curl)
    {
      ::curl_easy_reset(curl);
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_setopt(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  Curl* curl_wrapper = _scheme_object_to_curl_wrapper(argc, argv, 0);
  if (curl_wrapper)
    {
      CURL* curl = curl_wrapper->curl;
      if (!curl)
	{
	  DEFAULT_RET_FINISH;
	}
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      CURLoption opt = static_cast<CURLoption>(i);
      switch (opt)
	{
	case CURLOPT_VERBOSE:
	case CURLOPT_HEADER:
	case CURLOPT_NOPROGRESS:
	case CURLOPT_FAILONERROR:
	case CURLOPT_HTTPPROXYTUNNEL:
	case CURLOPT_TCP_NODELAY:
	case CURLOPT_AUTOREFERER:
	case CURLOPT_FOLLOWLOCATION:
	case CURLOPT_UNRESTRICTED_AUTH:
	case CURLOPT_POST301:
	case CURLOPT_POST:
	case CURLOPT_PUT:
	case CURLOPT_COOKIESESSION:
	case CURLOPT_IGNORE_CONTENT_LENGTH:
	case CURLOPT_HTTP_CONTENT_DECODING:
	case CURLOPT_HTTP_TRANSFER_DECODING:
	case CURLOPT_DIRLISTONLY:
	case CURLOPT_APPEND:
	case CURLOPT_FTP_USE_EPRT:
	case CURLOPT_FTP_USE_EPSV:
	case CURLOPT_FTP_CREATE_MISSING_DIRS:
	case CURLOPT_FTP_SKIP_PASV_IP:
	case CURLOPT_TRANSFERTEXT:
	case CURLOPT_PROXY_TRANSFER_MODE:
	case CURLOPT_CRLF:
	case CURLOPT_NOBODY:
	case CURLOPT_UPLOAD:
	case CURLOPT_FRESH_CONNECT:
	case CURLOPT_FORBID_REUSE:
	case CURLOPT_CONNECT_ONLY:
	case CURLOPT_SSLENGINE_DEFAULT:
	case CURLOPT_SSL_VERIFYPEER:
	case CURLOPT_SSL_VERIFYHOST:
	case CURLOPT_SSL_SESSIONID_CACHE:
	case CURLOPT_NOSIGNAL:
	case CURLOPT_HTTPGET:
	case CURLOPT_FILETIME:
	  {
	    int p = argv[2] == scheme_true ? 1 : 0;
	    if ((_last_error = ::curl_easy_setopt(curl, opt, p)) == CURLE_OK)
	      _ret_ = scheme_true;
	    break;
	  }
	case CURLOPT_WRITEFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						  _generic_curl_write_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_WRITEDATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_READFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						  _generic_curl_read_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_READDATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_HEADERFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						  _generic_curl_header_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_HEADERDATA,
						 reinterpret_cast<void*>(curl_wrapper));	
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_IOCTLFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						   _generic_curl_ioctl_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_IOCTLDATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_SEEKFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						  _generic_curl_seek_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_SEEKDATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_SOCKOPTFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						  _generic_curl_sockopt_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_SOCKOPTDATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_OPENSOCKETFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						 _generic_curl_opensocket_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_OPENSOCKETDATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_PROGRESSFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						  _generic_curl_progress_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_PROGRESSDATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_DEBUGFUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						  _generic_curl_debug_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_DEBUGDATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;
	  }
	case CURLOPT_SSL_CTX_FUNCTION:
	  {
	    if ((_last_error = ::curl_easy_setopt(curl, opt, 
						  _generic_curl_ssl_ctx_callback))
		== CURLE_OK)
	      {
		_last_error = ::curl_easy_setopt(curl, CURLOPT_SSL_CTX_DATA,
						 reinterpret_cast<void*>(curl_wrapper));
		if (_last_error == CURLE_OK)
		  {
		    curl_wrapper->callbacks[opt] = argv[2];
		    _ret_ = scheme_true;
		  }
	      }
	    break;	   
	  }
	case CURLOPT_CONV_TO_NETWORK_FUNCTION:
	  {
	    /*
	      if ((_last_error = ::curl_easy_setopt(curl, opt, 
	      _generic_curl_conv_to_network_callback))
	      == CURLE_OK)
	      {
	      curl_wrapper->callbacks[opt] = argv[1];
	      _ret_ = scheme_true;
	      }
	    */
	    break;	   	   
	  }
	case CURLOPT_CONV_FROM_NETWORK_FUNCTION:
	  {
	    /*
	      if ((_last_error = ::curl_easy_setopt(curl, opt, 
	      _generic_curl_conv_from_network_callback))
	      == CURLE_OK)
	      {
	      curl_wrapper->callbacks[opt] = argv[1];
	      _ret_ = scheme_true;
	      }
	    */
	    break;	   	   
	  }
	case CURLOPT_CONV_FROM_UTF8_FUNCTION:
	  {
	    /*
	      if ((_last_error = ::curl_easy_setopt(curl, opt, 
	      _generic_curl_conv_from_utf8_callback))
	      == CURLE_OK)
	      {
	      curl_wrapper->callbacks[opt] = argv[1];
	      _ret_ = scheme_true;
	      }
	    */
	    break;	   	   
	  }
	case CURLOPT_WRITEDATA:
	case CURLOPT_READDATA:
	case CURLOPT_HEADERDATA:
	case CURLOPT_IOCTLDATA:
	case CURLOPT_SEEKDATA:
	case CURLOPT_SOCKOPTDATA:
	case CURLOPT_OPENSOCKETDATA:
	case CURLOPT_PROGRESSDATA:
	case CURLOPT_DEBUGDATA:
	case CURLOPT_SSL_CTX_DATA:
	  {
	    curl_wrapper->callbacks_data[opt] = argv[2];
	    _ret_ = scheme_true;
	    break;
	  }
	  // ignore for now
	case CURLOPT_ERRORBUFFER:
	  {
	    break;
	  }
	  // ignore for now
	case CURLOPT_STDERR:
	  {
	    break;
	  }
	case CURLOPT_URL:
	case CURLOPT_PROXY:
	case CURLOPT_INTERFACE:
	case CURLOPT_NETRC_FILE:
	case CURLOPT_USERPWD:
	case CURLOPT_PROXYUSERPWD:
	case CURLOPT_ENCODING:
	case CURLOPT_COPYPOSTFIELDS:
	case CURLOPT_REFERER:
	case CURLOPT_USERAGENT:
	case CURLOPT_COOKIE:
	case CURLOPT_COOKIEFILE:
	case CURLOPT_COOKIEJAR:
	case CURLOPT_COOKIELIST:
	case CURLOPT_FTPPORT:
	case CURLOPT_FTP_ALTERNATIVE_TO_USER:
	case CURLOPT_FTP_ACCOUNT:
	case CURLOPT_RANGE:
	case CURLOPT_CUSTOMREQUEST:
	case CURLOPT_SSLCERT:
	case CURLOPT_SSLCERTTYPE:
	case CURLOPT_SSLKEY:
	case CURLOPT_SSLKEYTYPE:
	case CURLOPT_KEYPASSWD:
	case CURLOPT_SSLENGINE:
	case CURLOPT_CAINFO:
	case CURLOPT_CAPATH:
	case CURLOPT_RANDOM_FILE:
	case CURLOPT_EGDSOCKET:
	case CURLOPT_SSL_CIPHER_LIST:
	case CURLOPT_KRBLEVEL:
	case CURLOPT_SSH_HOST_PUBLIC_KEY_MD5:
	case CURLOPT_SSH_PUBLIC_KEYFILE:
	case CURLOPT_SSH_PRIVATE_KEYFILE:
	  {
	    std::string p;
	    if (argv[2] != scheme_null)
	      {
		if (!SCHEME_CHAR_STRINGP(argv[2]))
		  scheme_wrong_type("curl-easy-setopt", "string", 2, argc, argv);
		Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
		p = SCHEME_BYTE_STR_VAL(str);
	      }
	    if ((_last_error = ::curl_easy_setopt(curl, opt, p.c_str())) == CURLE_OK)
	      _ret_ = scheme_true;
	    break;
	  }
	case CURLOPT_PROXYPORT:
	case CURLOPT_PROXYTYPE:
	case CURLOPT_LOCALPORT:
	case CURLOPT_LOCALPORTRANGE:
	case CURLOPT_DNS_CACHE_TIMEOUT:
	case CURLOPT_BUFFERSIZE:
	case CURLOPT_PORT:
	case CURLOPT_NETRC:
	case CURLOPT_HTTPAUTH:
	case CURLOPT_PROXYAUTH:
	case CURLOPT_MAXREDIRS:
	case CURLOPT_POSTFIELDSIZE:
	case CURLOPT_HTTP_VERSION:
	case CURLOPT_FTP_RESPONSE_TIMEOUT:
	case CURLOPT_USE_SSL:
	case CURLOPT_FTPSSLAUTH:
	case CURLOPT_FTP_SSL_CCC:
	case CURLOPT_FTP_FILEMETHOD:
	case CURLOPT_RESUME_FROM:
	case CURLOPT_INFILESIZE:
	case CURLOPT_MAXFILESIZE:
	case CURLOPT_TIMEVALUE:
	case CURLOPT_TIMECONDITION:
	case CURLOPT_TIMEOUT:
	case CURLOPT_TIMEOUT_MS:
	case CURLOPT_LOW_SPEED_LIMIT:
	case CURLOPT_LOW_SPEED_TIME:
	case CURLOPT_MAXCONNECTS:
	case CURLOPT_CONNECTTIMEOUT:
	case CURLOPT_CONNECTTIMEOUT_MS:
	case CURLOPT_IPRESOLVE:
	case CURLOPT_SSLVERSION:
	case CURLOPT_NEW_FILE_PERMS:
	case CURLOPT_NEW_DIRECTORY_PERMS:
	  {
	    long p = 0;
	    spark::Utils::long_from_scheme_long(argv[2], p);
	    if ((_last_error = ::curl_easy_setopt(curl, opt, p)) == CURLE_OK)
	      _ret_ = scheme_true;
	    break;
	  }
	case CURLOPT_DNS_USE_GLOBAL_CACHE:
	  {
	    // obsolete
	    break;
	  }
	case CURLOPT_POSTFIELDS:
	  {
	    if (!SCHEME_CHAR_STRINGP(argv[2]))
	      scheme_wrong_type("curl-easy-setopt", "string", 2, argc, argv);
	    Scheme_Object* str = scheme_char_string_to_byte_string(argv[2]);
	    void* p = reinterpret_cast<void*>(SCHEME_BYTE_STR_VAL(str));
	    if ((_last_error = ::curl_easy_setopt(curl, opt, p)) == CURLE_OK)
	      _ret_ = scheme_true;
	    break;
	  }
	case CURLOPT_POSTFIELDSIZE_LARGE:
	case CURLOPT_RESUME_FROM_LARGE:
	case CURLOPT_INFILESIZE_LARGE:
	case CURLOPT_MAXFILESIZE_LARGE:
	case CURLOPT_MAX_SEND_SPEED_LARGE:
	case CURLOPT_MAX_RECV_SPEED_LARGE:
	  {
	    long p = 0;
	    spark::Utils::long_from_scheme_long(argv[2], p);
	    if ((_last_error = 
		 ::curl_easy_setopt(curl, opt, static_cast<curl_off_t>(p)))
		== CURLE_OK)
	      _ret_ = scheme_true;
	    break;
	  }
	case CURLOPT_HTTPPOST:
	  {
	    // HTTPOST data is stored in the Curl wrapper object.
	    // The argument will be an index to the vector of curl_httppost
	    // pointers.
	    int index = 0;
	    spark::Utils::int_from_scheme_long(argv[2], index);
	    curl_httppost* post = 
	      curl_wrapper->get_httppost(static_cast<size_t>(index));
	    if (post)
	      {
		if ((_last_error = ::curl_easy_setopt(curl, opt, post))
		    == CURLE_OK)
		  _ret_ = scheme_true;
	      }
	    break;
	  }
	case CURLOPT_HTTPHEADER:
	case CURLOPT_HTTP200ALIASES:
	case CURLOPT_QUOTE:
	case CURLOPT_POSTQUOTE:
	case CURLOPT_PREQUOTE:
	case CURLOPT_TELNETOPTIONS:
	  {
	    curl_slist* slist = _curl_slist_from_scheme_list(argv[2]);
	    if (slist)
	      {
		curl_wrapper->add_slist(slist);
		_last_error = ::curl_easy_setopt(curl, opt, slist);
	      }
	    else
	      _last_error = ::curl_easy_setopt(curl, opt, 0);
	    if (_last_error == CURLE_OK)
	      _ret_ = scheme_true;
	    break;
	  }
	case CURLOPT_SSH_AUTH_TYPES:
	  {
	    int f = spark::Utils::flag_from_list(argv[2]);
	    if ((_last_error = ::curl_easy_setopt(curl, opt, f))
		== CURLE_OK)
	      _ret_ = scheme_true;
	    break;
	  }
	case CURLOPT_PRIVATE:
	  {
	    // TODO: to be implemented
	    break;
	  }
	case CURLOPT_SHARE:
	  {
	    // TODO: to be implemented
	    break;
	  }
	case CURLOPT_CLOSEPOLICY:
	  {
	    // obsolete
	    break;
	  }
	case CURLOPT_WRITEINFO:
	case CURLOPT_LASTENTRY:
	default:
	  break;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_strerror(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  int i = 0;
  spark::Utils::int_from_scheme_long(argv[0], i);
  CURLcode c = static_cast<CURLcode>(i);
  const char* s = ::curl_easy_strerror(c);
  if (s)
    _ret_ = scheme_make_utf8_string(s);
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_easy_unescape(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  CURL* curl = _scheme_object_to_curl(argc, argv, 0);
  if (curl)
    {
      if (!SCHEME_CHAR_STRINGP(argv[1]))
	scheme_wrong_type("curl-easy-unescape", "string", 1, argc, argv);
      Scheme_Object* str = scheme_char_string_to_byte_string(argv[1]);
      std::string p = SCHEME_BYTE_STR_VAL(str);
      int len = 0;
      char* s = ::curl_easy_unescape(curl, p.c_str(), 
				     p.length(), &len);
      if (s)
	{
	  _ret_ = scheme_make_utf8_string(s);
	  curl_free(s);
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_formadd(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  Curl* curl_wrapper = _scheme_object_to_curl_wrapper(argc, argv, 0);
  if (curl_wrapper)
    {
      int i = 0;
      spark::Utils::int_from_scheme_long(argv[1], i);
      curl_httppost* post = curl_wrapper->get_httppost(static_cast<size_t>(i));
      curl_httppost* last = 0;
      bool post_exists = true;
      if (post)
	last = curl_wrapper->get_httppost_last(post);
      else
	post_exists = false;
      for (int i=2; i<argc; ++i)
	{
	  int t = 0;
	  spark::Utils::int_from_scheme_long(argv[i], t);
	  if (t == CURLFORM_END)
	    {
	      CURLFORMcode c = ::curl_formadd(&post, &last, CURLFORM_END);
	      if (c != CURL_FORMADD_OK)
		{
		  DEFAULT_RET_FINISH;
		}
	      break;
	    }
	  else
	    {
	      ++i;
	      if (i >= argc)
		{
		  scheme_signal_error("curlform:value pair mismatch.");
		  DEFAULT_RET_FINISH;
		}
	      if (!SCHEME_CHAR_STRINGP(argv[i]))
		scheme_wrong_type("curl-formadd", "string", i, argc, argv);
	      Scheme_Object* str = scheme_char_string_to_byte_string(argv[i]);
	      std::string p = SCHEME_BYTE_STR_VAL(str);
	      CURLFORMcode c = ::curl_formadd(&post, &last, t, p.c_str());
	      if (c != CURL_FORMADD_OK)
		{
		  DEFAULT_RET_FINISH;
		}
	    }
	}
      if (!post_exists)
	curl_wrapper->add_httppost(post);
      curl_wrapper->set_httppost_last(post, last);
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

// multi interface

Scheme_Object* 
spark_curl::curl_multi_init(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  CURLM* curlm = ::curl_multi_init();
  if (curlm)
    {
      Curl_tag t = MULTI_CURL_TAG;
      {
	Scheme_Object* tag = 0;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, tag);
	MZ_GC_REG();
	tag = scheme_make_integer(t);
	_ret_ = scheme_make_cptr(curlm, tag);
	MZ_GC_UNREG();
      }
    }
  else
    _last_error = CURLE_FAILED_INIT;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_multi_cleanup(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  CURLM* curlm = _scheme_object_to_multi_curl(argc, argv, 0);
  if (curlm)
    {
      ::curl_multi_cleanup(curlm);
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_multi_add_handle(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  CURLM* curlm = _scheme_object_to_multi_curl(argc, argv, 0);
  CURL* curl = _scheme_object_to_curl(argc, argv, 1);
  if (curlm && curl)
    {
      _last_m_error = ::curl_multi_add_handle(curlm, curl);
      if (_last_m_error == CURLM_OK)
	_ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_multi_remove_handle(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  CURLM* curlm = _scheme_object_to_multi_curl(argc, argv, 0);
  CURL* curl = _scheme_object_to_curl(argc, argv, 1);
  if (curlm && curl)
    {
      _last_m_error = ::curl_multi_remove_handle(curlm, curl);
      if (_last_m_error == CURLM_OK)
	_ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_multi_perform(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  CURLM* curlm = _scheme_object_to_multi_curl(argc, argv, 0);
  if (curlm)
    {
      int n = 0;
      _last_m_error = ::curl_multi_perform(curlm, &n);
      if (_last_m_error == CURLM_OK)
	_ret_ = scheme_make_integer(n);
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_multi_strerror(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  const char* s = ::curl_multi_strerror(_last_m_error);
  if (s)
    _ret_ = scheme_make_utf8_string(s);
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_curl::curl_multi_timeout(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
    
  CURLM* curlm = _scheme_object_to_multi_curl(argc, argv, 0);
  if (curlm)
    {
      long t = 0;
      _last_m_error = ::curl_multi_timeout(curlm, &t);
      if (_last_m_error == CURLM_OK)
	_ret_ = scheme_make_integer(t);
    }
  
  DEFAULT_RET_FINISH;
}

CURLM* 
_scheme_object_to_multi_curl(int argc, 
			     Scheme_Object** argv,
			     int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_scheme_object_to_multi_curl", "cptr", 
			idx, argc, argv);
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_scheme_object_to_multi_curl", "tag", 
			idx, argc, argv);
      return 0;
    }  
  Curl_tag tag = static_cast<Curl_tag>(i);
  if (tag != MULTI_CURL_TAG)
    {
      scheme_wrong_type("_scheme_object_to_multi_curl", "tag", 
			idx, argc, argv);
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(argv[idx]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_multi_curl", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<CURLM*>(p);
}

Curl* 
_scheme_object_to_curl_wrapper(int argc, 
			       Scheme_Object** argv,
			       int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_scheme_object_to_curl", "cptr", 
			idx, argc, argv);
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_scheme_object_to_curl", "tag", 
			idx, argc, argv);
      return 0;
    }  
  Curl_tag tag = static_cast<Curl_tag>(i);
  if (tag != EASY_CURL_TAG)
    {
      scheme_wrong_type("_scheme_object_to_curl", "tag", 
			idx, argc, argv);
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(argv[idx]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_curl", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<Curl*>(p);
}

CURL* 
_scheme_object_to_curl(int argc, 
		       Scheme_Object** argv,
		       int idx)
{
  Curl* c = _scheme_object_to_curl_wrapper(argc, argv, idx);
  if (c)
    return c->curl;
  return 0;
}

Scheme_Object*
_long_to_auth_list(long v)
{
  std::vector<int> ivec;
  if (v & CURLAUTH_BASIC == CURLAUTH_BASIC)
    ivec.push_back(CURLAUTH_BASIC);
  if (v & CURLAUTH_DIGEST == CURLAUTH_DIGEST)
    ivec.push_back(CURLAUTH_DIGEST);
  if (v & CURLAUTH_GSSNEGOTIATE == CURLAUTH_GSSNEGOTIATE)
    ivec.push_back(CURLAUTH_GSSNEGOTIATE);
  if (v & CURLAUTH_NTLM == CURLAUTH_NTLM)
    ivec.push_back(CURLAUTH_NTLM);
  if (v & CURLAUTH_ANY == CURLAUTH_ANY)
    ivec.push_back(CURLAUTH_ANY);
  if (v & CURLAUTH_ANYSAFE == CURLAUTH_ANYSAFE)
    ivec.push_back(CURLAUTH_ANYSAFE);
  size_t sz = ivec.size();
  if (sz > 0)
    {
      Scheme_Object** elems = new Scheme_Object*[sz];
      for (size_t i=0; i<sz; ++i)
	{
	  Scheme_Object* obj = NULL;
	  MZ_GC_DECL_REG(1);
	  MZ_GC_VAR_IN_REG(0, obj);
	  MZ_GC_REG();
	  obj = scheme_make_integer_value(ivec[i]);
	  elems[i] = obj;
	  MZ_GC_UNREG();
	}
      Scheme_Object* r = scheme_build_list(sz, elems);      
      delete[] elems;
      return r;
    }
  return scheme_null;
}

size_t 
_generic_curl_read_callback(void* ptr, 
			    size_t size,
			    size_t nmemb, 
			    void* param)
{
  if (!param)
    return static_cast<size_t>(CURLE_WRITE_ERROR);
  Curl* curl_wrapper = reinterpret_cast<Curl*>(param);
  Scheme_Object* f = curl_wrapper->callbacks[CURLOPT_READFUNCTION];
  size_t actual_bytes = size * nmemb;
  if (f == scheme_null)
    return actual_bytes;
  {
    Scheme_Object* str = NULL;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, str);
    MZ_GC_REG();
    if (ptr)
      str = scheme_make_sized_byte_string(reinterpret_cast<char*>(ptr),
					  actual_bytes,
					  1);
    else
      str = scheme_null;
    const int arg_count = 2;
    Scheme_Object* args[arg_count];
    args[0] = str;
    args[1] = curl_wrapper->callbacks_data[CURLOPT_READDATA];
    scheme_apply(f, arg_count, args);
    MZ_GC_UNREG();
  }
  return actual_bytes;
}

size_t 
_generic_curl_write_callback(void* ptr, 
			     size_t size,
			     size_t nmemb, 
			     void* param)
{
  if (!param)
    return static_cast<size_t>(CURLE_WRITE_ERROR);
  Curl* curl_wrapper = reinterpret_cast<Curl*>(param);
  Scheme_Object* f = curl_wrapper->callbacks[CURLOPT_WRITEFUNCTION];
  size_t actual_bytes = size * nmemb;
  if (f == scheme_null)
    return actual_bytes;
  {
    Scheme_Object* str = NULL;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, str);
    MZ_GC_REG();
    if (ptr)
      str = scheme_make_sized_byte_string(reinterpret_cast<char*>(ptr),
					  actual_bytes,
					  1);
    else
      str = scheme_null;
    const int arg_count = 2;
    Scheme_Object* args[arg_count];
    args[0] = str;
    args[1] = curl_wrapper->callbacks_data[CURLOPT_WRITEDATA];
    scheme_apply(f, arg_count, args);
    MZ_GC_UNREG();
  }
  return actual_bytes;
}

size_t 
_generic_curl_header_callback(void* ptr, size_t size,
			      size_t nmemb, void* param)
{
  if (!param)
    return static_cast<size_t>(CURLE_WRITE_ERROR);
  Curl* curl_wrapper = reinterpret_cast<Curl*>(param);
  Scheme_Object* f = curl_wrapper->callbacks[CURLOPT_HEADERFUNCTION];
  size_t actual_bytes = size * nmemb;
  if (f == scheme_null)
    return actual_bytes;
  {
    Scheme_Object* str = NULL;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, str);
    MZ_GC_REG();
    if (ptr)
      str = scheme_make_sized_byte_string(reinterpret_cast<char*>(ptr),
					  actual_bytes,
					  1);
    else
      str = scheme_null;
    const int arg_count = 2;
    Scheme_Object* args[arg_count];
    args[0] = str;
    args[1] = curl_wrapper->callbacks_data[CURLOPT_HEADERDATA];
    scheme_apply(f, arg_count, args);
    MZ_GC_UNREG();
  }
  return actual_bytes;
}

curlioerr 
_generic_curl_ioctl_callback(CURL* handle, 
			     int cmd, 
			     void* param)
{
  if (!param)
    return CURLIOE_FAILRESTART;
  Curl* curl_wrapper = reinterpret_cast<Curl*>(param);
  Scheme_Object* f = curl_wrapper->callbacks[CURLOPT_IOCTLFUNCTION];
  if (f == scheme_null)
    return CURLIOE_OK;
  {
    const int arg_count = 2;
    Scheme_Object* args[arg_count];
    args[0] = NULL;
    args[1] = NULL;
    MZ_GC_DECL_REG(1);
    MZ_GC_VAR_IN_REG(0, args[0]);
    MZ_GC_REG();
    args[0] = scheme_make_integer(cmd);
    args[1] = curl_wrapper->callbacks_data[CURLOPT_IOCTLDATA];
    scheme_apply(f, arg_count, args);
    MZ_GC_UNREG();
  }
  return CURLIOE_OK;
}

int 
_generic_curl_seek_callback(void* /*instream*/, 
			    curl_off_t /*offset*/,
			    int /*origin*/)
{
  // TODO: Implement.
  return 1;
}

size_t 
_generic_curl_sockopt_callback(void* /*clientp*/,
			       curl_socket_t /*curlfd*/,
			       curlsocktype /*purpose*/)
{
  // TODO: Implement.
  return 1;
}

size_t 
_generic_curl_opensocket_callback(void* /*clientp*/,
				  curlsocktype /*purpose*/,
				  struct curl_sockaddr* /*address*/)
{
  // TODO: Implement.
  return 1;
}

int
_generic_curl_progress_callback(void* param,
				double dltotal,
				double dlnow,
				double ultotal,
				double ulnow)
{
  if (!param)
    return 1;
  Curl* curl_wrapper = reinterpret_cast<Curl*>(param);
  Scheme_Object* f = curl_wrapper->callbacks[CURLOPT_PROGRESSFUNCTION];
  if (f == scheme_null)
    return 0;
  {
    const int arg_count = 5;
    Scheme_Object* args[arg_count];
    args[0] = NULL;
    args[1] = NULL;
    args[2] = NULL;
    args[3] = NULL;
    args[4] = NULL;
    MZ_GC_DECL_REG(4);
    MZ_GC_VAR_IN_REG(0, args[1]);
    MZ_GC_VAR_IN_REG(1, args[2]);
    MZ_GC_VAR_IN_REG(2, args[3]);
    MZ_GC_VAR_IN_REG(3, args[4]);
    MZ_GC_REG();
    args[0] = curl_wrapper->callbacks_data[CURLOPT_PROGRESSDATA];
    args[1] = scheme_make_double(dltotal);
    args[2] = scheme_make_double(dlnow);
    args[3] = scheme_make_double(ultotal);
    args[4] = scheme_make_double(ulnow);
    scheme_apply(f, arg_count, args);
    MZ_GC_UNREG();
  }
  return 0;
}

int
_generic_curl_debug_callback(CURL* handle,
			     curl_infotype ci,
			     char* s,
			     size_t sz,
			     void* param)
{
  if (!param)
    return 1;
  Curl* curl_wrapper = reinterpret_cast<Curl*>(param);
  Scheme_Object* f = curl_wrapper->callbacks[CURLOPT_DEBUGFUNCTION];
  if (f == scheme_null)
    return 0;
  {
    const int arg_count = 3;
    Scheme_Object* args[arg_count];
    args[0] = NULL;
    args[1] = NULL;
    args[2] = NULL;
    MZ_GC_DECL_REG(2);
    MZ_GC_VAR_IN_REG(0, args[0]);
    MZ_GC_VAR_IN_REG(1, args[1]);
    MZ_GC_REG();
    args[0] = scheme_make_integer(static_cast<int>(ci));
    args[1] = scheme_make_sized_byte_string(s, sz, 1);
    args[2] = curl_wrapper->callbacks_data[CURLOPT_DEBUGDATA];
    scheme_apply(f, arg_count, args);
    MZ_GC_UNREG();
  }
  return 0;
}

CURLcode
_generic_curl_ssl_ctx_callback(CURL* handle,
			       void* sslctx,
			       void* parm)
{
  // TODO: Implement.
  return CURLE_OK;
}

/*
CURLcode
_generic_curl_conv_to_network_callback(char* ptr, size_t len)
{
return CURLE_OK;
}

CURLcode
_generic_curl_conv_from_network_callback(char* ptr, size_t len)
{
  return CURLE_OK;
}

CURLcode
_generic_curl_conv_from_utf8_callback(char* ptr, size_t len)
{
  return CURLE_OK;
}
*/

curl_slist* 
_curl_slist_from_scheme_list(Scheme_Object* list)
{
  if (!list || list == scheme_null)
    return 0;
  if (!SCHEME_LISTP(list))
    return 0;
  curl_slist* slist = 0;
  Scheme_Object* elem = SCHEME_CAR(list);
  Scheme_Object* rest = SCHEME_CDR(list);
  while (elem)
    {
      if (elem != scheme_null)
	{
	  if (SCHEME_CHAR_STRINGP(elem))
	    {
	      Scheme_Object* str = scheme_char_string_to_byte_string(elem);
	      std::string p = SCHEME_BYTE_STR_VAL(str);
	      slist = curl_slist_append(slist, p.c_str());
	    }
	}
      if (rest)
	{
	  if (rest == scheme_null)
	    break;
	  elem = SCHEME_CAR(rest);
	  rest = SCHEME_CDR(rest);
	}
      else
	break;
    }
  return slist;
}

bool 
_init_default_functions(CURL* curl)
{
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_READFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_HEADERFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_IOCTLFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_SEEKFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_SOCKOPTFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_OPENSOCKETFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_PROGRESSFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_DEBUGFUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, CURLOPT_SSL_CTX_FUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, 
					CURLOPT_CONV_TO_NETWORK_FUNCTION, 0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, 
					CURLOPT_CONV_FROM_NETWORK_FUNCTION, 
					0))
      != CURLE_OK)
    return false;
  if ((_last_error = ::curl_easy_setopt(curl, 
					CURLOPT_CONV_FROM_UTF8_FUNCTION, 
					0))
      != CURLE_OK)
    return false;
  return true;
}

