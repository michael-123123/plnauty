#ifndef PL_NAUTY_COMPAT_H
#define PL_NAUTY_COMPAT_H

/*
 * **********************************************
 * these are prolog version constants usually 
 * passed by the build environment via make
 * 
 * it is enough for make to pass only PLVERSION 
 * (which is obtained by swipl --dump-runtime-variables)
 * 
 * PLVERSION is the swipl version in long format
 * HAVE_SWIPL7 <=> PLVERSION > 70000 (swipl ver. 7 and above)
 * HAVE_SWIPL6 <=> PLVERSION > 60000 (swipl ver. 6 and above)
 * **********************************************
 */
#ifndef PLVERSION
 #define PLVERSION 0
#endif

#ifndef HAVE_SWIPL7
 #if (PLVERSION >= 70000)
  #define HAVE_SWIPL7 TRUE
 #else
  #define HAVE_SWIPL7 FALSE 
 #endif
#endif

#ifndef HAVE_SWIPL6
 #if (PLVERSION >= 60000)
  #define HAVE_SWIPL6 TRUE
 #else
  #define HAVE_SWIPL6 FALSE 
 #endif
#endif

/**
 * **********************************************
 * int PL_get_compound_name_arity(
 * 	term_t +term, 
 * 	atom_t* name, 
 * 	size_t* arity
 * )
 * 
 * if term is a compound term then name and arity
 * are assigned the values of its name and arity.
 * 
 * this is used during the options parsing portion
 * of pl-densenauty. if you're running swipl6 or 
 * earlier you need this function. 
 * 
 * @param term the term to test
 * @param name term's name 
 * @param arity term's arity
 * **********************************************
 */
#if !HAVE_SWIPL7
int PL_get_compound_name_arity(term_t, atom_t*, int*) ;
#endif

#endif