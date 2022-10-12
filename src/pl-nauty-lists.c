#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdlib.h>

#include "pl-nauty-error.h"
#include "pl-nauty-lists.h"

/*
 * **********************************************
 * list_to_array (get)
 * **********************************************
 */
int
pl_get_list_int_array_ex(term_t list, int length, int plus, int *array)
{
	term_t head = PL_new_term_ref() ;
	term_t tail = PL_copy_term_ref(list) ;
	
	int i ;
	for(i = 0 ; i < length ; i++) {
		if(!PL_get_list_ex(tail, head, tail))
			return FALSE ;
		
		if(!PL_get_integer_ex(head, array+i))
			return FALSE ;
		
		array[i] += plus ;
	}
	
	if(!PL_get_nil_ex(tail))
		return FALSE ;
	
	return TRUE ;
}

/*
 * **********************************************
 * array_to_list (unify)
 * **********************************************
 */
int 
pl_unify_list_int_array_ex(term_t list, int length, int plus, int *array)
{
	term_t head = PL_new_term_ref() ;
	term_t tail = PL_copy_term_ref(list) ;
	
	int i ;
	for(i = 0 ; i < length ; i++) {
		head = PL_new_term_ref() ;
		if(!PL_unify_list(tail, head, tail)) {
			PL_UNIFIABLE_ERROR("pair", tail) ;
			return FALSE ;
		}
		
		if(!PL_unify_integer(head, array[i]+plus)) {
			PL_UNIFIABLE_ERROR("integer", head) ;
			return FALSE ;
		}
	}
	
	if(!PL_unify_nil(tail)) {
		PL_UNIFIABLE_ERROR("[]", tail) ;
		return FALSE ;
	}
	
	return TRUE ;
}
