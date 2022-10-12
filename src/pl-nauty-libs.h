#ifndef PL_NAUTY_LIBS_H
#define PL_NAUTY_LIBS_H

/*
 * **********************************************
 * these constants determine the behaviour of the 
 * get graph / unify graph functions in case of 
 * error. By default these functions throw a prolog
 * exception, which can be suppressed by setting
 * these constants to false. 
 * 
 * default is true and if false then PL_clear_exception
 * will be called before/after calling pl_get_graph_ex
 * and pl_unify_graph_ex. in some cases the exceptions
 * will not be raised to begin with.
 * 
 * TODO: currently unimplemented, add this feature.
 * **********************************************
 */
#ifndef PL_NAUTY_GET_EX
 #define PL_NAUTY_GET_EX TRUE 
#endif

#ifndef PL_NAUTY_UNIFY_EX
 #define PL_NAUTY_UNIFY_EX TRUE
#endif

// #if PL_NAUTY_GET_EX
//  #define PL_NAUTY_RAISE_GET(Error) Error ;
// #else
//  #define PL_NAUTY_RAISE_GET(Error) 
// #endif

/**
 * **********************************************
 * libraries relating to to nauty's graph* struct
 * mainly to convert back and forth from graph*
 * to term_t and some operations to e.g, compare
 * graphs.
 * **********************************************
 */
#include "pl-nauty-graph.h"

/**
 * **********************************************
 * libraries with auxiliary swipl functions, 
 * mainly used to convert various arrays to lists
 * and vice versa
 * **********************************************
 */
#include "pl-nauty-lists.h"

#endif