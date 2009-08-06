// MzScheme inetrface to the Pthreads API.
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

#include <unistd.h>
#include "spark.h"
#include "spark_pthread.h"
using namespace spark_pthread;

static const char* MODULE_NAME = "#%spark-pthread";

struct Callback_info
{
  Scheme_Object* callback;
  Scheme_Object* callback_param;
  
  Callback_info()
    : callback(scheme_null),
      callback_param(scheme_null)
  { }

  Callback_info(Scheme_Object* cb, Scheme_Object* cb_p)
    : callback(cb),
      callback_param(cb_p)
  { }
};

enum Pthread_tag
  {
    PTHREAD_ATTR_TAG,
    PTHREAD_COND_TAG,
    PTHREAD_MUTEX_TAG
  };

static Scheme_Env* _env;

static spark::Status_code _add_constants(Scheme_Env* env);
static spark::Status_code _add_procedures(Scheme_Env* env);
static pthread_t _scheme_object_to_pthread(Scheme_Object* obj);
static pthread_attr_t* _scheme_object_to_attr(int argc, 
					      Scheme_Object** argv,
					      int idx);
static pthread_mutex_t* _scheme_object_to_mutex(int argc, 
						Scheme_Object** argv,
						int idx);
static pthread_cond_t* _scheme_object_to_cond(int argc, 
					      Scheme_Object** argv,
					      int idx);
static void* _generic_pthread_callback(void* p);
	 
spark::Status_code
spark_pthread::initialize(Scheme_Env* env)
{
  _env = env;
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
namespace spark_pthread
{
  static Scheme_Object* create(int, Scheme_Object**);
  static Scheme_Object* exit(int, Scheme_Object**);
  static Scheme_Object* self(int, Scheme_Object**);
  static Scheme_Object* join(int, Scheme_Object**);
  static Scheme_Object* detach(int, Scheme_Object**);
  static Scheme_Object* attr_init(int, Scheme_Object**);
  static Scheme_Object* attr_destroy(int, Scheme_Object**);
  static Scheme_Object* attr_setdetachstate(int, Scheme_Object**);
  static Scheme_Object* attr_getdetachstate(int, Scheme_Object**);
  static Scheme_Object* attr_setschedpolicy(int, Scheme_Object**);
  static Scheme_Object* attr_getschedpolicy(int, Scheme_Object**);
  static Scheme_Object* attr_setschedparam(int, Scheme_Object**);
  static Scheme_Object* attr_getschedparam(int, Scheme_Object**);
  static Scheme_Object* attr_setinheritsched(int, Scheme_Object**);
  static Scheme_Object* attr_getinheritsched(int, Scheme_Object**);
  static Scheme_Object* attr_setscope(int, Scheme_Object**);
  static Scheme_Object* attr_getscope(int, Scheme_Object**);
  static Scheme_Object* setschedparam(int, Scheme_Object**);
  static Scheme_Object* getschedparam(int, Scheme_Object**);
  static Scheme_Object* cancel(int, Scheme_Object**);
  static Scheme_Object* setcancelstate(int, Scheme_Object**);
  static Scheme_Object* setcanceltype(int, Scheme_Object**);
  static Scheme_Object* testcancel(int, Scheme_Object**);
  static Scheme_Object* cond_init(int, Scheme_Object**);
  static Scheme_Object* cond_signal(int, Scheme_Object**);
  static Scheme_Object* cond_broadcast(int, Scheme_Object**);
  static Scheme_Object* cond_wait(int, Scheme_Object**);
  static Scheme_Object* cond_timedwait(int, Scheme_Object**);
  static Scheme_Object* cond_destroy(int, Scheme_Object**);
  static Scheme_Object* equal(int, Scheme_Object**);
  static Scheme_Object* mutex_init(int, Scheme_Object**);
  static Scheme_Object* mutex_lock(int, Scheme_Object**);
  static Scheme_Object* mutex_trylock(int, Scheme_Object**);
  static Scheme_Object* mutex_unlock(int, Scheme_Object**);
  static Scheme_Object* mutex_destroy(int, Scheme_Object**);
  // sleep
  static Scheme_Object* sleep(int, Scheme_Object**);
  static Scheme_Object* usleep(int, Scheme_Object**);
  static Scheme_Object* nanosleep(int, Scheme_Object**);
  // timer
  static Scheme_Object* timer(int, Scheme_Object**);
} // namespace spark_sqlite

spark::Status_code
_add_constants(Scheme_Env* env)
{
  using spark::Constant;
  Constant constants[] = { 
    Constant("PTHREAD-CREATE-JOINABLE", PTHREAD_CREATE_JOINABLE),
    Constant("PTHREAD-CREATE-DETACHED", PTHREAD_CREATE_DETACHED),
    Constant("SCHED-OTHER", SCHED_OTHER),
    Constant("SCHED-RR", SCHED_RR),
    Constant("SCHED-FIFO", SCHED_FIFO),
    Constant("PTHREAD-EXPLICIT-SCHED", PTHREAD_EXPLICIT_SCHED),
    Constant("PTHREAD-INHERIT-SCHED", PTHREAD_INHERIT_SCHED),
    Constant("PTHREAD-SCOPE-SYSTEM", PTHREAD_SCOPE_SYSTEM),
    Constant("PTHREAD-SCOPE-PROCESS", PTHREAD_SCOPE_PROCESS),
    Constant("", 0)
  };
  return add_constants(env, constants, "spark-pthread");
}

spark::Status_code
_add_procedures(Scheme_Env* env)
{
  using spark::Procedure;
  Procedure* procedures[] = { 
    new Procedure(spark_pthread::create, 
		  "pthread-create", 3),
    new Procedure(spark_pthread::exit, 
		  "pthread-exit", 1),
    new Procedure(spark_pthread::self, 
		  "pthread-self", 1),
    new Procedure(spark_pthread::join, 
		  "pthread-join", 1),
    new Procedure(spark_pthread::detach, 
		  "pthread-detach", 1),
    new Procedure(spark_pthread::attr_init, 
		  "pthread-attr-init", 0),
    new Procedure(spark_pthread::attr_destroy, 
		  "pthread-attr-destroy", 1),
    new Procedure(spark_pthread::attr_setdetachstate, 
		  "pthread-attr-setdetachstate", 2),
    new Procedure(spark_pthread::attr_getdetachstate, 
		  "pthread-attr-getdetachstate", 1),
    new Procedure(spark_pthread::attr_setschedpolicy, 
		  "pthread-attr-setschedpolicy", 2),
    new Procedure(spark_pthread::attr_getschedpolicy, 
		  "pthread-attr-getschedpolicy", 1),
    new Procedure(spark_pthread::attr_setschedparam, 
		  "pthread-attr-setschedparam", 2),
    new Procedure(spark_pthread::attr_getschedparam, 
		  "pthread-attr-getschedparam", 1),
    new Procedure(spark_pthread::attr_setinheritsched, 
		  "pthread-attr-setinheritsched", 2),
    new Procedure(spark_pthread::attr_getinheritsched, 
		  "pthread-attr-getinheritsched", 1),
    new Procedure(spark_pthread::attr_setscope, 
		  "pthread-attr-setscope", 2),
    new Procedure(spark_pthread::attr_getscope, 
		  "pthread-attr-getscope", 1),
    new Procedure(spark_pthread::setschedparam, 
		  "pthread-setschedparam", 3),
    new Procedure(spark_pthread::getschedparam, 
		  "pthread-getschedparam", 1),
    new Procedure(spark_pthread::cancel, 
		  "pthread-cancel", 1),
    new Procedure(spark_pthread::setcancelstate, 
		  "pthread-setcancelstate", 1),
    new Procedure(spark_pthread::setcanceltype, 
		  "pthread-setcanceltype", 1),
    new Procedure(spark_pthread::testcancel, 
		  "pthread-testcancel", 0),
    new Procedure(spark_pthread::cond_init, 
		  "pthread-cond-init", 1),
    new Procedure(spark_pthread::cond_signal, 
		  "pthread-cond-signal", 1),
    new Procedure(spark_pthread::cond_broadcast, 
		  "pthread-cond-broadcast", 1),
    new Procedure(spark_pthread::cond_wait, 
		  "pthread-cond-wait", 2),
    new Procedure(spark_pthread::cond_timedwait, 
		  "pthread-cond-timedwait", 3),
    new Procedure(spark_pthread::cond_destroy, 
		  "pthread-cond-destroy", 1),
    new Procedure(spark_pthread::equal, 
		  "pthread-equal", 2),
    new Procedure(spark_pthread::mutex_init, 
		  "pthread-mutex-init", 1),
    new Procedure(spark_pthread::mutex_lock, 
		  "pthread-mutex-lock", 1),
    new Procedure(spark_pthread::mutex_trylock, 
		  "pthread-mutex-trylock", 1),
    new Procedure(spark_pthread::mutex_unlock, 
		  "pthread-mutex-unlock", 1),
    new Procedure(spark_pthread::mutex_destroy, 
		  "pthread-mutex-destroy", 1),
    new Procedure(spark_pthread::sleep, 
		  "pthread-sleep", 1),
    new Procedure(spark_pthread::usleep, 
		  "pthread-usleep", 1),
    new Procedure(spark_pthread::nanosleep, 
		  "pthread-nanosleep", 1),
    // timer
    new Procedure(spark_pthread::timer,
		  "pthread-timer", 2),
    0
  };
  return spark::add_procedures(env, procedures, "spark-pthread");
}

// Exported pthreads API

Scheme_Object* 
spark_pthread::create(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = 0;
  if (argv[0] != scheme_null)
    attr = _scheme_object_to_attr(argc, argv, 0);
  Callback_info* ci = new Callback_info(argv[1], argv[2]);
  pthread_t pt = 0;
  if (pthread_create(&pt, attr, _generic_pthread_callback,
		     reinterpret_cast<void*>(ci)) == 0)
    {
      _ret_ = scheme_make_integer(static_cast<int>(pt));
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::exit(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_exit(reinterpret_cast<void*>(argv[0]));

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::self(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  _ret_ = scheme_make_integer(pthread_self());

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::join(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  void* retval = 0;
  pthread_t pt = _scheme_object_to_pthread(argv[0]);
  if (pthread_join(pt, &retval) == 0)
    _ret_ = reinterpret_cast<Scheme_Object*>(retval);
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::detach(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_t pt = _scheme_object_to_pthread(argv[0]);
  if (pthread_detach(pt) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_init(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = new pthread_attr_t;
  if (pthread_attr_init(attr) == 0)
    {
      Scheme_Object* tag = 0;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, tag);
      MZ_GC_REG();
      tag = scheme_make_integer(PTHREAD_ATTR_TAG);
      MZ_GC_UNREG();
      _ret_ = scheme_make_cptr(attr, tag);
    }
  else
    delete attr;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_destroy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      if (pthread_attr_destroy(attr) == 0)
	{
	  delete attr;
	  _ret_ = scheme_true;
	}
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_setdetachstate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      spark::Utils::int_from_scheme_long(argv[1], v);
      if (pthread_attr_setdetachstate(attr, v) == 0)
	_ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_getdetachstate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      if (pthread_attr_getdetachstate(attr, &v) == 0)
	_ret_ = scheme_make_integer(v);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_setschedpolicy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      spark::Utils::int_from_scheme_long(argv[1], v);
      if (pthread_attr_setschedpolicy(attr, v) == 0)
	_ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_getschedpolicy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      if (pthread_attr_getschedpolicy(attr, &v) == 0)
	_ret_ = scheme_make_integer(v);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_setschedparam(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      spark::Utils::int_from_scheme_long(argv[1], v);
      sched_param sp;
      sp.__sched_priority = v;
      if (pthread_attr_setschedparam(attr, &sp) == 0)
	_ret_ = scheme_true;
    }  
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_getschedparam(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      sched_param sp;
      if (pthread_attr_getschedparam(attr, &sp) == 0)
	_ret_ = scheme_make_integer(sp.__sched_priority);
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_setinheritsched(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      spark::Utils::int_from_scheme_long(argv[1], v);
      if (pthread_attr_setinheritsched(attr, v) == 0)
	_ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_getinheritsched(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      if (pthread_attr_getinheritsched(attr, &v) == 0)
	_ret_ = scheme_make_integer(v);
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_setscope(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      spark::Utils::int_from_scheme_long(argv[1], v);
      if (pthread_attr_setscope(attr, v) == 0)
	_ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::attr_getscope(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_attr_t* attr = _scheme_object_to_attr(argc, argv, 0);
  if (attr)
    {
      int v = 0;
      if (pthread_attr_getscope(attr, &v) == 0)
	_ret_ = scheme_make_integer(v);
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::setschedparam(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_t pt = _scheme_object_to_pthread(argv[0]);
  int policy = 0;
  spark::Utils::int_from_scheme_long(argv[1], policy);
  int v = 0;  
  spark::Utils::int_from_scheme_long(argv[2], v);
  sched_param sp;
  sp.__sched_priority = v;
  if (pthread_setschedparam(pt, policy, &sp) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::getschedparam(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_t pt = _scheme_object_to_pthread(argv[0]);

  int policy = 0;
  sched_param sp;
  if (pthread_getschedparam(pt, &policy, &sp) == 0)
    {
      Scheme_Object* elems[2];
      elems[0] = NULL;
      elems[1] = NULL;
      MZ_GC_DECL_REG(2);
      MZ_GC_VAR_IN_REG(0, elems[0]);
      MZ_GC_VAR_IN_REG(1, elems[1]);
      MZ_GC_REG();
      elems[0] = scheme_make_integer(policy);
      elems[1] = scheme_make_integer(sp.__sched_priority);
      MZ_GC_UNREG();
      _ret_ = scheme_build_list(2, elems);      
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::cancel(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_t pt = _scheme_object_to_pthread(argv[0]);
  if (pthread_cancel(pt) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::setcancelstate(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int state = 0;
  spark::Utils::int_from_scheme_long(argv[0], state);
  int old_state = 0;
  if (pthread_setcancelstate(state, &old_state) == 0)
    _ret_ = scheme_make_integer(old_state);
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::setcanceltype(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int type = 0;
  spark::Utils::int_from_scheme_long(argv[0], type);
  int old_type = 0;
  if (pthread_setcanceltype(type, &old_type) == 0)
    _ret_ = scheme_make_integer(old_type);
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::testcancel(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_testcancel();
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::cond_init(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_cond_t* cond = new pthread_cond_t;
  if (pthread_cond_init(cond, 0) == 0)
    {
      Scheme_Object* tag = 0;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, tag);
      MZ_GC_REG();
      tag = scheme_make_integer(PTHREAD_COND_TAG);
      MZ_GC_UNREG();
      _ret_ = scheme_make_cptr(cond, tag);    
    }
  else
    delete cond;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::cond_signal(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  pthread_cond_t* cond = _scheme_object_to_cond(argc, argv, 0);
  if (pthread_cond_signal(cond) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::cond_broadcast(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_cond_t* cond = _scheme_object_to_cond(argc, argv, 0);
  if (pthread_cond_broadcast(cond) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::cond_wait(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_cond_t* cond = _scheme_object_to_cond(argc, argv, 0);
  pthread_mutex_t* mutex = _scheme_object_to_mutex(argc, argv, 1);
  if (pthread_cond_wait(cond, mutex) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::cond_timedwait(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  if (!SCHEME_LISTP(argv[2]))
    scheme_wrong_type("cond-timedwait", "list", 2, argc, argv);
  pthread_cond_t* cond = _scheme_object_to_cond(argc, argv, 0);
  pthread_mutex_t* mutex = _scheme_object_to_mutex(argc, argv, 1);
  int sec = 0;
  int nsec = 0;
  Scheme_Object* obj = SCHEME_CAR(argv[2]);
  if (obj != scheme_null)
    spark::Utils::int_from_scheme_long(obj, sec);
  obj = SCHEME_CDR(argv[2]);
  if (obj != scheme_null)
    {
      obj = SCHEME_CAR(obj);
      if (obj != scheme_null)
	spark::Utils::int_from_scheme_long(obj, nsec);
    }
  timespec ts;
  ts.tv_sec = static_cast<__time_t>(sec);
  ts.tv_nsec = static_cast<long int>(nsec);
  if (pthread_cond_timedwait(cond, mutex, &ts) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::cond_destroy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  pthread_cond_t* cond = _scheme_object_to_cond(argc, argv, 0);
  if (pthread_cond_destroy(cond) == 0)
    {
      delete cond;
      _ret_ = scheme_true;
    }

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::equal(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_t pt01 = _scheme_object_to_pthread(argv[0]);
  pthread_t pt02 = _scheme_object_to_pthread(argv[1]);
  if (pthread_equal(pt01, pt02))
    _ret_ = scheme_true;
  else
    _ret_ = scheme_false;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::mutex_init(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;
  
  pthread_mutex_t* mutex = new pthread_mutex_t;
  if (pthread_mutex_init(mutex, 0) == 0)
    {
      Scheme_Object* tag = 0;
      MZ_GC_DECL_REG(1);
      MZ_GC_VAR_IN_REG(0, tag);
      MZ_GC_REG();
      tag = scheme_make_integer(PTHREAD_MUTEX_TAG);
      MZ_GC_UNREG();
      _ret_ = scheme_make_cptr(mutex, tag);
    }
  else
    delete mutex;

  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::mutex_lock(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_mutex_t* mutex = _scheme_object_to_mutex(argc, argv, 0);
  
  if (pthread_mutex_lock(mutex) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::mutex_trylock(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_mutex_t* mutex = _scheme_object_to_mutex(argc, argv, 0);
  
  if (pthread_mutex_trylock(mutex) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::mutex_unlock(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_mutex_t* mutex = _scheme_object_to_mutex(argc, argv, 0);
  
  if (pthread_mutex_unlock(mutex) == 0)
    _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::mutex_destroy(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  pthread_mutex_t* mutex = _scheme_object_to_mutex(argc, argv, 0);
    
  if (pthread_mutex_destroy(mutex) == 0)
    {
      delete mutex;
      _ret_ = scheme_true;
    }
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::sleep(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  int secs = 0;
  spark::Utils::int_from_scheme_long(argv[0], secs);
  ::sleep(secs);
  _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::usleep(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  long microsecs = 0;
  spark::Utils::long_from_scheme_long(argv[0], microsecs);
  ::usleep(microsecs);
  _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

Scheme_Object* 
spark_pthread::nanosleep(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  long microsecs = 0;
  spark::Utils::long_from_scheme_long(argv[0], microsecs);
  int secs = microsecs / 1000000;
  timespec ts;
  ts.tv_sec = secs;
  ts.tv_nsec = (microsecs % 1000000) * 1000;
  ::nanosleep(&ts, 0);
  _ret_ = scheme_true;
  
  DEFAULT_RET_FINISH;
}

static void*
_timer_callback(void* p)
{
  long* v = reinterpret_cast<long*>(p);
  ::usleep(*v);
  return 0;
}

Scheme_Object* 
spark_pthread::timer(int argc, Scheme_Object** argv)
{
  DEFAULT_RET_INIT;

  long millisecs = 0;
  spark::Utils::long_from_scheme_long(argv[0], millisecs);
  long microsecs = millisecs * 1000;
  pthread_t pt;
  if (pthread_create(&pt, 0, _timer_callback,
		     reinterpret_cast<void*>(&microsecs)) == 0) 
    {
      pthread_join(pt, 0);
      if (argv[1] != scheme_null)
	scheme_apply(argv[1], 0, 0);
      _ret_ = scheme_true;
    }
  else
    _ret_ = scheme_false;
  
  DEFAULT_RET_FINISH;
}

pthread_t
_scheme_object_to_pthread(Scheme_Object* obj)
{
  int pt = 0;
  spark::Utils::int_from_scheme_long(obj, pt);
  return static_cast<pthread_t>(pt);
}

pthread_attr_t* 
_scheme_object_to_attr(int argc, 
		       Scheme_Object** argv,
		       int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_scheme_object_to_attr", "cptr", 
			idx, argc, argv);
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_scheme_object_to_attr", "tag", 
			idx, argc, argv);
      return 0;
    }  
  Pthread_tag tag = static_cast<Pthread_tag>(i);
  if (tag != PTHREAD_ATTR_TAG)
    {
      scheme_wrong_type("_scheme_object_to_attr", "tag", 
			idx, argc, argv);
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(argv[0]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_attr", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<pthread_attr_t*>(p);
}

pthread_cond_t* 
_scheme_object_to_cond(int argc, 
		       Scheme_Object** argv,
		       int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_scheme_object_to_cond", "cptr", 
			idx, argc, argv);
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_scheme_object_to_cond", "tag", 
			idx, argc, argv);
      return 0;
    }  
  Pthread_tag tag = static_cast<Pthread_tag>(i);
  if (tag != PTHREAD_COND_TAG)
    {
      scheme_wrong_type("_scheme_object_to_cond", "tag", 
			idx, argc, argv);
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(argv[0]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_cond", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<pthread_cond_t*>(p);
}

pthread_mutex_t* 
_scheme_object_to_mutex(int argc, 
			Scheme_Object** argv,
			int idx)
{
  if (!SCHEME_CPTRP(argv[idx]))
    {
      scheme_wrong_type("_scheme_object_to_mutex", "cptr", 
			idx, argc, argv);
      return 0;
    }  
  Scheme_Object* tag_obj = SCHEME_CPTR_TYPE(argv[idx]);
  int i = 0;
  if (!spark::Utils::int_from_scheme_long(tag_obj, i))
    {
      scheme_wrong_type("_scheme_object_to_mutex", "tag", 
			idx, argc, argv);
      return 0;
    }  
  Pthread_tag tag = static_cast<Pthread_tag>(i);
  if (tag != PTHREAD_MUTEX_TAG)
    {
      scheme_wrong_type("_scheme_object_to_mutex", "tag", 
			idx, argc, argv);
      return 0;
    }
  void* p = SCHEME_CPTR_VAL(argv[0]);
  if (!p)
    {
      scheme_wrong_type("_scheme_object_to_mutex", "cptr-val", 
			idx, argc, argv);
      return 0;
    }
  return reinterpret_cast<pthread_mutex_t*>(p);
}

void* 
_generic_pthread_callback(void* p)
{
  Callback_info* ci = reinterpret_cast<Callback_info*>(p);  
  if (ci->callback != scheme_null)
    {
      const int arg_count = 1;
      Scheme_Object* args[arg_count];
      args[0] = ci->callback_param;
      scheme_apply(ci->callback, arg_count, args);
    }
  delete ci;
  return 0;
}

