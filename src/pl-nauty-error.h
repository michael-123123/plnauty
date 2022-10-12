#ifndef PL_NAUTY_ERROR_H
#define PL_NAUTY_ERROR_H

/*
 * **********************************************
 * stringify and macro expand stringify taken 
 * from the gcc manual. 
 * **********************************************
 */
#define xstr(a) str(a)
#define str(a) #a

/*
 * **********************************************
 * will call PL_type_error with unifiable(cause)
 * and culprit. this macro is used by the unify
 * functions to raise an exception.
 * **********************************************
 */
#define PL_UNIFIABLE_ERROR(cause, culprit) PL_type_error( "unifiable(" cause ")" , culprit)

/*
 * **********************************************
 * will raise a PL_UNIFIABLE_ERROR and return FALSE
 * **********************************************
 */
#define PL_UNIFIABLE_ERROR_RET(cause, culprit) 				\
	do {								\
		PL_type_error( "unifiable(" cause ")" , culprit) ; 	\
		return FALSE ; 						\
	} while(0) ;

/*
 * **********************************************
 * will call PL_type_error with put(cause)
 * and culprit. this macro is used by the PL_put_*
 * functions to raise an exception.
 * **********************************************
 */
#define PL_PUT_ERROR(cause, culprit) PL_type_error( "put(" cause ")" , culprit)

/*
 * **********************************************
 * will raise a PL_UNIFIABLE_ERROR and return FALSE
 * **********************************************
 */
#define PL_PUT_ERROR_RET(cause, culprit) 			\
	do {							\
		PL_type_error( "put(" cause ")" , culprit) ; 	\
		return FALSE ; 					\
	} while(0) ;

#endif