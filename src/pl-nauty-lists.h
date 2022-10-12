#ifndef PL_NAUTY_LISTS_H
#define PL_NAUTY_LISTS_H

/**
 * **********************************************
 * int pl_get_list_int_array_ex(
 * 	term_t list, 
 * 	int length, 
 * 	int plus, 
 * 	int *array
 * )
 * 
 * convert a prolog integer list from term_t to 
 * integer array. it is assumed that the length 
 * of the list is known in advance and that array
 * is allocated by the caller.
 * 
 * @param list list to copy
 * @param length the length of the list
 * @param plus a constant that is added to all values 
 *             in array during copying.
 * @param array result of copy
 * **********************************************
 */
int pl_get_list_int_array_ex(term_t, int, int, int*) ;

/**
 * **********************************************
 * int pl_unify_list_int_array_ex(
 * 	term_t list, 
 * 	int length, 
 * 	int plus, 
 * 	int *array
 * )
 * 
 * convert a c integer array to a prolog list 
 * using unification. it is assumed that the length 
 * of the array is known in advance and that array
 * is allocated by the caller.
 * 
 * @param list resulting array 
 * @param length length of list 
 * @param plus a constant that is added to all values 
 *             in list during copying.
 * @param array array to copy from.
 * **********************************************
 */
int pl_unify_list_int_array_ex(term_t, int, int, int*) ;

#endif