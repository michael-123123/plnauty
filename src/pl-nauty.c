#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <nauty.h>
#include <gtools.h>
#include <naututil.h>
#include <nautinv.h>

#include "pl-nauty.h"




//=============================================================================
static const PL_extension predicates[] = {
	//
	//  { "name", arity, function, PL_FA_<flags> },
	//
	{ "$densenauty_initialize", 0, (void*) pl_densenauty_init, 0},
	{ "$densenauty"           , 8, (void*) pl_densenauty     , 0},
	{ "$isomorphic"           , 6, (void*) pl_isomorphic     , 0},
	{ "$graph_convert"        , 5, (void*) pl_graph_convert  , 0},
#ifdef PL_NAUTY_DEBUG
	{ "pl_densenauty_debug_opts",       4, (void*) pl_densenauty_debug_opts, 0},
	{ "pl_nauty_graph_convert_self",    3, (void*) pl_graph_convert_self,    0},
	{ "pl_nauty_graph_convert_debug",   5, (void*) pl_graph_convert_debug,   0},
#endif
	{ NULL     , 0, NULL              , 0 }    // terminating line
};

//-----------------------------------------------------------------------------
install_t install()
{
	PL_register_extensions(predicates);	/* This is the only PL_ call allowed */
	/* before PL_initialise().  It */
	/* ensures the foreign predicates */
	/* are available before loading */
	/* Prolog code */
}