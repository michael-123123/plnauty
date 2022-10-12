#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdlib.h>

#include <nauty.h>
#include <gtools.h>
#include <naututil.h>
#include <nautinv.h>

#include "pl-nauty-flags.h"
#include "pl-nauty-libs.h"
#include "pl-densenauty-opts.h"

/*
 * **********************************************
 * 
 * **********************************************
 */
// + + + + -
foreign_t
pl_graph_convert(term_t N, term_t InFmt, term_t In, term_t OutFmt, term_t Out)
{
	int n, m, i ;
	if(!PL_get_integer_ex(N, &n))
		PL_fail ;
	
	flag_t infmt ;
	if(!pl_get_densenauty_fmt_ex(InFmt, &infmt))
		PL_fail ;
	
	flag_t outfmt ;
	if(!pl_get_densenauty_fmt_ex(OutFmt, &outfmt))
		PL_fail ;
	
	m = SETWORDSNEEDED(n);
	nauty_check(WORDSIZE, m, n, NAUTYVERSIONID) ;
	
	DYNALLSTAT(graph,gconv,gconv_sz);	
	DYNALLOC2(graph,gconv,gconv_sz,m,n,"malloc");
	
	if(!pl_get_graph_ex(In, n, m, infmt, gconv))
		PL_fail ;
	
	if(!pl_unify_graph_ex(Out, n, m, outfmt, gconv))
		PL_fail ;
	
	PL_succeed ;
}

/*
 * **********************************************
 * this implementation is taken from nauty's 
 * manual page 46 (the last densenauty call 
 * example).
 * **********************************************
 */
boolean equal_graphs(int n, int m, graph* g1, graph* g2)
{
	size_t k ;
	for(k = 0 ; k < m*(size_t)n ; ++k) 
		if(g1[k] != g2[k])
			return FALSE ;
		return TRUE ;
}

/*
 * **********************************************
 * 
 * **********************************************
 */
int
vertex_coloring_to_lab_ptn(int n, int k, int* col, int* lab, int* ptn)
{
	int i, j, c, curr ;
	
	c = -1 ;
	if(k >= 0)
		c = k ;
	else {
		for(i = 0 ; i < n ; i++)
			c = (col[i] > c ? col[i] : c) ;
	}
	c++ ;
	
// 	Sprintf("c = %d\n", c) ;
	curr = 0 ;
	for(i = 0 ; i < c ; i++) {
		
		for(j = 0 ; j < n ; j++) {
// 			Sprintf("%d, %d, %d\n", i, j, col[j]) ;
			if( col[j] == i ) {
				lab[curr] = j ;
				ptn[curr] = 1 ;
				curr++ ;
			}
		}
// 		Sprintf("\n") ;
		
		if(curr > 0)
			ptn[curr-1] = 0 ;
	}
	
// 	Sprintf("\ngot coloring : ") ;
// 	for(i = 0 ; i < n ; i++)
// 		Sprintf("%d ", col[i]) ;
// 	Sprintf("\n") ;
// 	
// 	Sprintf("converted coloring to:\n") ;
// 	Sprintf("lab : ") ;
// 	for(i = 0 ; i < n ; i++) {
// 		Sprintf("%d ", lab[i]) ;
// 	}
// 	Sprintf("\n") ;
// 	
// 	Sprintf("ptn : ") ;
// 	for(i = 0 ; i < n ; i++) {
// 		Sprintf("%d ", ptn[i]) ;
// 	}
// 	Sprintf("\n") ;
	
	
	
	return TRUE ;
}

/*
 * **********************************************
 * sanity check for pl_get_graph_ex and pl_unify_graph_ex
 * takes an adj matrix and copies it to a fresh 
 * adj matrix using pl_get_graph_ex and 
 * pl_unify_graph_ex. should not be generally
 * built. controlled using PL_NAUTY_DEBUG
 * **********************************************
 */
#ifdef PL_NAUTY_DEBUG
foreign_t
pl_graph_convert_self(term_t N, term_t In, term_t Out)
{	
	int n, m, i ;
	if(!PL_get_integer_ex(N, &n))
		PL_fail ;
	
	m = SETWORDSNEEDED(n);
	nauty_check(WORDSIZE, m, n, NAUTYVERSIONID) ;
	
	DYNALLSTAT(graph,gconv,gconv_sz);	
	DYNALLOC2(graph,gconv,gconv_sz,m,n,"malloc");
	
	if(!pl_get_graph_ex(In, n, m, PL_GRAPH_FMT_MATRIX, gconv))
		PL_fail ;
	
	if(!pl_unify_graph_ex(Out, n, m, PL_GRAPH_FMT_MATRIX, gconv))
		PL_fail ;
	
	PL_succeed ;
}
#endif

#ifdef PL_NAUTY_DEBUG
foreign_t
pl_graph_convert_debug(term_t N, term_t InFmt, term_t In, term_t OutFmt, term_t Out)
{
	int n, m, i ;
	if(!PL_get_integer_ex(N, &n))
		PL_fail ;
	
	flag_t infmt, outfmt ;
	if(!PL_get_long_ex(InFmt, &infmt))
		PL_fail ;
	if(!PL_get_long_ex(OutFmt, &outfmt))
		PL_fail ;
	
	m = SETWORDSNEEDED(n);
	nauty_check(WORDSIZE, m, n, NAUTYVERSIONID) ;
	
	DYNALLSTAT(graph,gconv,gconv_sz);	
	DYNALLOC2(graph,gconv,gconv_sz,m,n,"malloc");
	
	if(!pl_get_graph_ex(In, n, m, infmt, gconv))
		PL_fail ;
	
	if(!pl_unify_graph_ex(Out, n, m, outfmt, gconv))
		PL_fail ;
	
	PL_succeed ;
}
#endif
