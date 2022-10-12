#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "pl-nauty-flags.h"
#include "pl-nauty-compat.h"

/*
 * **********************************************
 * same as functor/3 for compound terms only.
 * included in swipl7 and above.
 * **********************************************
 */
#if !HAVE_SWIPL7
int 
PL_get_compound_name_arity(term_t t, atom_t *name, int *arity)
{
	if(!PL_is_compound(t))
		return FALSE ;
	
	int* a = arity ;
	return PL_get_name_arity(t, name, a) ;
}
#endif
