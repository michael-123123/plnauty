#ifndef PL_NAUTY_H
#define PL_NAUTY_H
//

/*
 * **********************************************
 * swipl types and functions etc...
 * **********************************************
 */
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

/*
 * **********************************************
 * c miscellenia
 * **********************************************
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

/*
 * **********************************************
 * nauty headers (nauty, densenauty, graph2string
 * etc...)
 * **********************************************
 */
#include <nauty.h>
#include <gtools.h>
#include <naututil.h>
#include <nautinv.h>

/*
 * **********************************************
 * the implementation
 * **********************************************
 */
#include "pl-nauty-flags.h"
#include "pl-nauty-error.h"
#include "pl-nauty-compat.h"
#include "pl-nauty-libs.h"
#include "pl-densenauty-opts.h"
#include "pl-densenauty.h"

/*
 * **********************************************
 * *****       commonly used macros        ******
 * **********************************************
 */

/*
 * **********************************************
 * >                   NULL                     <
 * **********************************************
 */
#ifndef NULL
#define NULL ((void*) 0)
#endif 

/*
 * **********************************************
 * >                 TRUE/FALSE                 <
 * **********************************************
 */
#ifndef TRUE
#define FALSE (0)
#define TRUE  (1)
#endif 

/*
 * **********************************************
 * >                  MIN/MAX                   <
 * **********************************************
 */
#ifndef MIN
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#endif 

#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif


/*
 * **********************************************
 * >                 DEBUG MODE                 <
 * **********************************************
 */

/*
 * **********************************************
 * if defined then debugging stuff will be 
 * compiled. usually passed through make
 * **********************************************
 */
// #define PL_NAUTY_DEBUG


//
#endif