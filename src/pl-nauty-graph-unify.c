#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <nauty.h>
#include <gtools.h>
#include <naututil.h>
#include <nautinv.h>

#include "pl-nauty-flags.h"
#include "pl-nauty-error.h"
#include "pl-nauty-graph-unify.h"

/*
 * **********************************************
 * graph* -> term_t
 * **********************************************
 */
int
pl_unify_graph_ex(term_t Graph, int n, int m, flag_t fmt, graph* g)
{
	term_t Fmt ;
	
	switch (fmt) {
		case PL_GRAPH_FMT_MATRIX   : return pl_unify_graph_adj_mat_ex(Graph, n, m, g   ) ;
		case PL_GRAPH_FMT_ADJ_LIST : return pl_unify_graph_adj_lists_ex(Graph, n, m, g ) ;
		case PL_GRAPH_FMT_EDGE_LIST: return pl_unify_graph_edge_lists_ex(Graph, n, m, g) ;
		case PL_GRAPH_FMT_UPPER    : return pl_unify_graph_upper_ex(Graph, n, m, g     ) ;
		case PL_GRAPH_FMT_FLAT     : return pl_unify_graph_flat_ex(Graph, n, m, g      ) ;
		case PL_GRAPH_FMT_CHAR2    : return pl_unify_graph_char2_ex(Graph, n, m, g     ) ;
		case PL_GRAPH_FMT_CHAR16   : return pl_unify_graph_char16_ex(Graph, n, m, g    ) ;
		case PL_GRAPH_FMT_G6_CODES : return pl_unify_graph_g6codes_ex(Graph, n, m, g   ) ;
		case PL_GRAPH_FMT_G6_CHARS : return pl_unify_graph_g6chars_ex(Graph, n, m, g   ) ;
		case PL_GRAPH_FMT_G6_ATOM  : return pl_unify_graph_g6atom_ex(Graph, n, m, g    ) ;
		case PL_GRAPH_FMT_G6_STRING: return pl_unify_graph_g6string_ex(Graph, n, m, g  ) ;
		
		default:
			Fmt = PL_new_term_ref() ;
			if(!PL_put_integer(Fmt, fmt))
				return FALSE ;
			
			PL_type_error("graph fmt (in pl_unify_graph_ex)", Fmt) ;
			return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_MATRIX
 * **********************************************
 */
int
pl_unify_graph_adj_mat_ex(term_t Graph, int n, int m, graph *g)
{
	int i, j, c ;
	term_t mat, row, cell ;
	set *gi ;
	
	mat = PL_copy_term_ref(Graph) ;
	row = PL_new_term_ref() ;
	cell = PL_new_term_ref() ;
	for(i = 0 ; i < n ; i++) {
		gi = GRAPHROW(g, i, m) ;
		
		row = PL_new_term_ref() ;
		if(!PL_unify_list(mat, row, mat)) {
			PL_UNIFIABLE_ERROR("pair", mat) ;
			return FALSE ;
		}
		
		for(j = 0 ; j < n ; j++) {
			cell = PL_new_term_ref() ;
			if(!PL_unify_list(row, cell, row)) {
				PL_UNIFIABLE_ERROR("pair", row) ;
				return FALSE ;
			}
			
			c = ISELEMENT(gi, j) ? 1 : 0 ;
			if(!PL_unify_integer(cell, c)) {
				PL_UNIFIABLE_ERROR("integer", cell) ;
				return FALSE ;
			}
		}
		
		if(!PL_unify_nil(row)) {
			PL_UNIFIABLE_ERROR("[]", row) ;
			return FALSE ;
		}
	}
	
	if(!PL_unify_nil(mat)) {
		PL_UNIFIABLE_ERROR("[]", mat) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_ADJ_LIST
 * **********************************************
 */
int
pl_unify_graph_adj_lists_ex(term_t Graph, int n, int m, graph *g)
{
	functor_t minus_f = PL_new_functor(PL_new_atom("-"), 2) ;
	
	int i, j, c ;
	term_t head, tail, vert, adj, x ;
	set *gi ;
	
	head = PL_new_term_ref() ;
	tail = PL_new_term_ref() ;
	if(!PL_put_nil(tail)) {
		PL_PUT_ERROR("[]", tail) ;
		return FALSE ;
	}
	
	vert = PL_new_term_ref() ;
	adj = PL_new_term_ref() ;
	x = PL_new_term_ref() ;
	if(!PL_put_nil(adj)) {
		PL_PUT_ERROR("[]", adj) ;
		return FALSE ;
	}
	
	for(i = n-1 ; i >=0 ; i--) {
		gi = GRAPHROW(g, i, m) ;
		
		adj = PL_new_term_ref() ;
		if(!PL_put_nil(adj)) {
			PL_PUT_ERROR("[]", adj) ;
			return FALSE ;
		}
		
		for(j = n-1 ; j > i ; j--) {
			if(ISELEMENT(gi, j)) {
				x = PL_new_term_ref() ;
				if(!PL_put_integer(x, j+1)) {
					PL_PUT_ERROR("integer", x) ;
					return FALSE ;
				}
				
				if(!PL_cons_list(adj, x, adj)) {
					PL_PUT_ERROR("cons", adj) ;
					return FALSE ;
				}
			}
		}
		
		vert = PL_new_term_ref() ;
		if(!PL_put_integer(vert, i+1)) {
			PL_PUT_ERROR("integer", vert) ;
			return FALSE ;
		}
		
		head = PL_new_term_ref() ;
		if(!PL_cons_functor(head, minus_f, vert, adj)) {
			PL_PUT_ERROR("v-adjv", head) ;
			return FALSE ;
		}
		
		if(!PL_cons_list(tail, head, tail)) {
			PL_PUT_ERROR("cons", tail) ;
			return FALSE ;
		}
	}
	
	if(!PL_unify(Graph, tail)) {
		PL_UNIFIABLE_ERROR("edge_list", Graph) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_EDGE_LIST
 * **********************************************
 */
int
pl_unify_graph_edge_lists_ex(term_t Graph, int n, int m, graph *g)
{
	functor_t minus_f = PL_new_functor(PL_new_atom("-"), 2) ;
	
	int i, j, c ;
	term_t head, tail, u, v ;
	set *gi ;
	
	head = PL_new_term_ref() ;
	tail = PL_new_term_ref() ;
	if(!PL_put_nil(tail)) {
		PL_PUT_ERROR("[]", tail) ;
		return FALSE ;
	}
	
	v = PL_new_term_ref() ;
	u = PL_new_term_ref() ;
	
	for(i = n-1 ; i >= 0 ; i--) {
		gi = GRAPHROW(g, i, m) ;
		
		if(!PL_put_integer(v, i+1)) {
			PL_PUT_ERROR("integer", v) ;
			return FALSE ;
		}
		
		for(j = n-1 ; j > i ; j--) {
			if(ISELEMENT(gi, j)) {
				u = PL_new_term_ref() ;
				if(!PL_put_integer(u, j+1)) {
					PL_PUT_ERROR("integer", u) ;
					return FALSE ;
				}
				
				head = PL_new_term_ref() ;
				if(!PL_cons_functor(head, minus_f, v, u)) {
					PL_PUT_ERROR("v-u", head) ;
					return FALSE ;
				}
				
				if(!PL_cons_list(tail, head, tail)) {
					PL_PUT_ERROR("cons", tail) ;
					return FALSE ;
				}
			}
		}
	}
	
	if(!PL_unify(Graph, tail)) {
		PL_UNIFIABLE_ERROR("edge_list", Graph) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_UPPER
 * **********************************************
 */
int
pl_unify_graph_upper_ex(term_t Graph, int n, int m, graph *g)
{
	int i, j, c ;
	term_t mat, row, cell ;
	set *gi ;
	
	mat = PL_copy_term_ref(Graph) ;
	row = PL_new_term_ref() ;
	cell = PL_new_term_ref() ;
	for(i = 0 ; i < n ; i++) {
		gi = GRAPHROW(g, i, m) ;
		
		row = PL_new_term_ref() ;
		if(!PL_unify_list(mat, row, mat)) {
			PL_UNIFIABLE_ERROR("pair", mat) ;
			return FALSE ;
		}
		
		for(j = i+1 ; j < n ; j++) {
			cell = PL_new_term_ref() ;
			if(!PL_unify_list(row, cell, row)) {
				PL_UNIFIABLE_ERROR("pair", row) ;
				return FALSE ;
			}
			
			c = ISELEMENT(gi, j) ? 1 : 0 ;
			if(!PL_unify_integer(cell, c)) {
				PL_UNIFIABLE_ERROR("integer", cell) ;
				return FALSE ;
			}
		}
		
		if(!PL_unify_nil(row)) {
			PL_UNIFIABLE_ERROR("[]", row) ;
			return FALSE ;
		}
	}
	
	if(!PL_unify_nil(mat)) {
		PL_UNIFIABLE_ERROR("[]", mat) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_FLAT
 * **********************************************
 */
int
pl_unify_graph_flat_ex(term_t Graph, int n, int m, graph *g)
{
	int i, j, c ;
	term_t mat, row, cell ;
	set *gi ;
	
	mat = PL_copy_term_ref(Graph) ;
	cell = PL_new_term_ref() ;
	for(i = 0 ; i < n ; i++) {
		gi = GRAPHROW(g, i, m) ;
		
		for(j = 0 ; j < n ; j++) {
			cell = PL_new_term_ref() ;
			if(!PL_unify_list(mat, cell, mat)) {
				PL_UNIFIABLE_ERROR("pair", mat) ;
				return FALSE ;
			}
			
			c = ISELEMENT(gi, j) ? 1 : 0 ;
			if(!PL_unify_integer(cell, c)) {
				PL_UNIFIABLE_ERROR("integer", cell) ;
				return FALSE ;
			}
		}
	}
	
	if(!PL_unify_nil(mat)) {
		PL_UNIFIABLE_ERROR("[]", mat) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_CHAR*
 * **********************************************
 */
int
pl_unify_graph_char(term_t Graph, int n, int m, graph* g, int radix)
{
	static const char* hex = "0123456789abcdef" ;
	
	int i, j, c, nchoose2 ;
	set *gi ;
	DYNALLSTAT(char,chr,chr_sz);
	DYNALLSTAT(char,xchr,xchr_sz);
	char *p ;
	
	nchoose2 = 1 + n*(n-1) / 2 ;
	DYNALLOC1(char,chr,chr_sz,nchoose2,"malloc"); 
	if(chr == NULL) {
		PL_resource_error("malloc/unify_graph_char") ;
		return FALSE ;
	}
	
	p = chr ;		
	for(i = 0 ; i < n ; i++) {
		gi = GRAPHROW(g, i, m) ;
		for(j = i+1 ; j < n ; j++) 
			*p++ = (char) (ISELEMENT(gi, j) ? 1 : 0) ;
	}
	*p++ = '\0' ;
	
	term_t tchr = PL_new_term_ref() ;
	if(radix == 16) {
		DYNALLOC1(char,xchr,xchr_sz,4 + ((nchoose2-1) / 4) ,"malloc"); 
		xchr[0] = '0' ;
		xchr[1] = 'x' ;
		
		int mod = (nchoose2-1) % 4 ;
		switch(mod) {
			case 0:
				break ;
			case 1:
				xchr[2] = hex[chr[0]] ;
				break ;
			case 2:
				xchr[2] = hex[2*chr[0] + chr[1]] ;
				break ;
			case 3:
				xchr[2] = hex[4*chr[0] + 2*chr[1] + chr[2]] ;
				break ;
		}
		
		j = mod == 0 ? 2 : 3 ;
		for(i = mod ; i < nchoose2-1 ; i+=4)
			xchr[j++] = hex[8*chr[i] + 4*chr[i+1] + 2*chr[i+2] + chr[i+3]] ;
		xchr[j] = '\0' ;
		
		if(!PL_put_atom_chars(tchr, xchr)) {
			PL_PUT_ERROR("atom_chars", tchr) ;
			return FALSE ;
		}
		
	} else {
		for(i = 0 ; i < nchoose2-1 ; i++)
			chr[i] = chr[i] + '0' ;
		chr[i] = '\0' ;
		
		
		if(!PL_put_atom_chars(tchr, chr)) {
			PL_PUT_ERROR("atom_chars", tchr) ;
			return FALSE ;
		}
	}
	
	// free(chr) ;
	if(!PL_unify(tchr, Graph)) {
		PL_UNIFIABLE_ERROR("characteristice upper diagonal", Graph) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_CHAR2
 * **********************************************
 */
int
pl_unify_graph_char2_ex(term_t Graph, int n, int m, graph *g)
{
	return pl_unify_graph_char(Graph, n, m, g, 2) ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_CHAR16
 * **********************************************
 */
int
pl_unify_graph_char16_ex(term_t Graph, int n, int m, graph *g)
{
	return pl_unify_graph_char(Graph, n, m, g, 16) ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_G6_CODES
 * **********************************************
 */
int
pl_unify_graph_g6codes_ex(term_t Graph, int n, int m, graph *g)
{
	term_t Fmt ;
	char *p, *gstr ;
	p = gstr = ntog6(g, m, n) ;
	if(gstr == NULL) 
		return FALSE ;
	
	while(*p++ != '\n') { }
	*--p = '\0' ;
	
	term_t result = PL_new_term_ref() ;
	if(!PL_put_list_codes(result, gstr)) {
		PL_PUT_ERROR("list_codes", result) ;
		return FALSE ;
	}
	
	if(!PL_unify(result, Graph)) {
		PL_UNIFIABLE_ERROR("put(result_codes)", Graph) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_G6_CHARS
 * **********************************************
 */
int
pl_unify_graph_g6chars_ex(term_t Graph, int n, int m, graph *g)
{
	term_t Fmt ;
	char *p, *gstr ;
	p = gstr = ntog6(g, m, n) ;
	if(gstr == NULL) 
		return FALSE ;
	
	while(*p++ != '\n') { }
	*--p = '\0' ;
	
	term_t result = PL_new_term_ref() ;
	if(!PL_put_list_chars(result, gstr)) {
		PL_PUT_ERROR("list_chars", result) ;
		return FALSE ;
	}
	
	if(!PL_unify(result, Graph)) {
		PL_UNIFIABLE_ERROR("result_chars", Graph) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_G6_ATOM
 * **********************************************
 */
int
pl_unify_graph_g6atom_ex(term_t Graph, int n, int m, graph *g)
{
	term_t Fmt ;
	char *p, *gstr ;
	p = gstr = ntog6(g, m, n) ;
	if(gstr == NULL) 
		return FALSE ;
	
	while(*p++ != '\n') { }
	*--p = '\0' ;
	
	term_t result = PL_new_term_ref() ;
	if(!PL_put_atom_chars(result, gstr)) {
		PL_PUT_ERROR("atom", result) ;
		return FALSE ;
	}
	
	if(!PL_unify(result, Graph)) {
		PL_UNIFIABLE_ERROR("result_atom", Graph) ;
		return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * graph* -> PL_GRAPH_FMT_G6_STRING
 * **********************************************
 */
int
pl_unify_graph_g6string_ex(term_t Graph, int n, int m, graph *g)
{
	term_t Fmt ;
	char *p, *gstr ;
	p = gstr = ntog6(g, m, n) ;
	if(gstr == NULL) 
		return FALSE ;
	
	while(*p++ != '\n') { }
	*--p = '\0' ;
	
	term_t result = PL_new_term_ref() ;
	if(!PL_put_string_chars(result, gstr)) {
		PL_PUT_ERROR("string", result) ;
		return FALSE ;
	}
	
	if(!PL_unify(result, Graph)) {
		PL_UNIFIABLE_ERROR("result_string", Graph) ;
		return FALSE ;
	}
	
	return TRUE ;
}
