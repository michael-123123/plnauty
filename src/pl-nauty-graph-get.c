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
#include "pl-nauty-graph-get.h"

/*
 * **********************************************
 * term_t -> graph*
 * **********************************************
 */
int 
pl_get_graph_ex(term_t Graph, int n, int m, flag_t fmt, graph* g)
{
	term_t Fmt ;
	
	switch (fmt) {
		case PL_GRAPH_FMT_MATRIX   : return pl_get_graph_adj_mat_ex    (Graph, n, m, g) ;
		case PL_GRAPH_FMT_ADJ_LIST : return pl_get_graph_adj_lists_ex  (Graph, n, m, g) ;
		case PL_GRAPH_FMT_EDGE_LIST: return pl_get_graph_edge_lists_ex (Graph, n, m, g) ;
		case PL_GRAPH_FMT_UPPER    : return pl_get_graph_upper_ex      (Graph, n, m, g) ;
		case PL_GRAPH_FMT_FLAT     : return pl_get_graph_flat_ex       (Graph, n, m, g) ;
		case PL_GRAPH_FMT_CHAR2    : return pl_get_graph_char2_ex      (Graph, n, m, g) ;
		case PL_GRAPH_FMT_CHAR16   : return pl_get_graph_char16_ex     (Graph, n, m, g) ;
		case PL_GRAPH_FMT_G6_CODES : return pl_get_graph_g6codes_ex    (Graph, n, m, g) ;
		case PL_GRAPH_FMT_G6_CHARS : return pl_get_graph_g6chars_ex    (Graph, n, m, g) ;
		case PL_GRAPH_FMT_G6_ATOM  : return pl_get_graph_g6atom_ex     (Graph, n, m, g) ;
		case PL_GRAPH_FMT_G6_STRING: return pl_get_graph_g6string_ex   (Graph, n, m, g) ;
		
		default:
			Fmt = PL_new_term_ref() ;
			if(!PL_put_integer(Fmt, fmt))
				return FALSE ;
			
			PL_type_error("graph fmt (in pl_get_graph_ex)", Fmt) ;
			return FALSE ;
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_MATRIX -> graph*
 * **********************************************
 */
int
pl_get_graph_adj_mat_ex(term_t Graph, int n, int m, graph *g)
{
	int c,i,j ;
	term_t mat, row, cell ;
	
	EMPTYGRAPH(g,m,n) ;
	
	row = PL_new_term_ref() ;
	cell = PL_new_term_ref() ;
	mat = PL_copy_term_ref(Graph) ;
	for(i = 0 ; i < n ; i++) {
		if(!PL_get_list_ex(mat, row, mat))
			return FALSE ;
		
		for(j = 0 ; j < n ; j++) {
			if(!PL_get_list_ex(row, cell ,row))
				return FALSE ;
			
			if(!PL_get_integer_ex(cell, &c))
				return FALSE ;
			
			// if(i < j && c == 1)
			// 	ADDONEEDGE(g, i, j, m) ;
			
// 			if(c == 1)
// 				ADDONEEDGE(g, i, j, m) ;
			
			if(c == 1)
				ADDONEARC(g, i, j, m) ;
		}
		
		if(!PL_get_nil_ex(row))
			return FALSE ;
	}
	
	if(!PL_get_nil_ex(mat))
		return FALSE ;
	
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_ADJ_LIST -> graph*
 * **********************************************
 */
int
pl_get_graph_adj_lists_ex(term_t Graph, int n, int m, graph *g)
{
	functor_t minus_f = PL_new_functor(PL_new_atom("-"), 2) ;
	
	int v, u ;
	term_t head, tail, V, AdjV, ahead, atail ;
	
	EMPTYGRAPH(g,m,n) ;
	
	head = PL_new_term_ref() ;
	tail = PL_copy_term_ref(Graph) ;
	
	V = PL_new_term_ref() ;
	AdjV = PL_new_term_ref() ;
	
	ahead = PL_new_term_ref() ;
	atail = PL_new_term_ref() ;
	
	while(PL_get_list_ex(tail, head, tail)) {
		
		if(!PL_is_functor(head, minus_f)) {
			PL_type_error("-/2", head) ;
			return FALSE ;
		}
		
		if(!PL_get_arg(1, head, V)) {
			PL_type_error("V-adjv", head) ;
			return FALSE ;
		}
		
		if(!PL_get_arg(2, head, AdjV)) {
			PL_type_error("v-ADJV", head) ;
			return FALSE ;
		}
		
		if(!PL_get_integer_ex(V, &v)) 
			return FALSE ;
		
		atail = PL_copy_term_ref(AdjV) ;
		while(PL_get_list_ex(atail, ahead, atail)) {
			if(!PL_get_integer_ex(ahead, &u))
				return FALSE ;
			
			ADDONEEDGE(g, v-1, u-1, m) ;
// 			ADDONEARC(g, v-1, u-1, m) ;
		}
		
		if(!PL_get_nil_ex(atail))
			return FALSE ;
	}
	
	if(!PL_get_nil_ex(tail))
		return FALSE ;
	
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_EDGE_LIST -> graph*
 * **********************************************
 */
int
pl_get_graph_edge_lists_ex(term_t Graph, int n, int m, graph *g)
{
	int u,v ;
	term_t head, tail, U, V;
	functor_t minus_f ; 
	
	minus_f = PL_new_functor(PL_new_atom("-"), 2) ;
	
	EMPTYGRAPH(g,m,n) ;
	
	U = PL_new_term_ref() ;
	V = PL_new_term_ref() ;
	
	head = PL_new_term_ref() ;
	tail = PL_copy_term_ref(Graph) ;
	
	while(PL_get_list_ex(tail, head, tail)) {
		
		
		if(!PL_is_functor(head, minus_f)) {
			PL_type_error("-/2", head) ;
			return FALSE ;
		}
		
		if(!PL_get_arg(1, head, U)) {
			PL_type_error("U-v", head) ;
		}
		
		if(!PL_get_arg(2, head, V)) {
			PL_type_error("u-V", head) ;
		}
		
		if(!PL_get_integer_ex(U, &u))
			return FALSE ;
		
		if(!PL_get_integer_ex(V, &v))
			return FALSE ;
		
		if(v < 1 || v > n) {
			PL_type_error("v:between(1,|V|)", V) ;
			return FALSE ;
		}
		
		if(u < 1 || u > n) {
			PL_type_error("u:between(1,|V|)", U) ;
			return FALSE ;
		}
		
		ADDONEEDGE(g, u-1, v-1, m) ;
// 		ADDONEARC(g, u-1, v-1, m) ;
	}
	
	if(!PL_get_nil_ex(tail))
		return FALSE ;
	
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_UPPER -> graph*
 * **********************************************
 */
int
pl_get_graph_upper_ex(term_t Graph, int n, int m, graph *g)
{
	int c,i,j ;
	term_t mat, row, cell ;
	
	EMPTYGRAPH(g,m,n) ;
	
	row = PL_new_term_ref() ;
	cell = PL_new_term_ref() ;
	mat = PL_copy_term_ref(Graph) ;
	for(i = 0 ; i < n ; i++) {
		if(!PL_get_list_ex(mat, row, mat))
			return FALSE ;
		
		for(j = i+1 ; j < n ; j++) {
			if(!PL_get_list_ex(row, cell ,row))
				return FALSE ;
			
			if(!PL_get_integer_ex(cell, &c))
				return FALSE ;
			
			// if(i < j && c == 1)
			// 	ADDONEEDGE(g, i, j, m) ;
			
// 			if(c == 1)
// 				ADDONEEDGE(g, i, j, m) ;
			
			if(c == 1)
				ADDONEARC(g, i, j, m) ;
			
		}
		
		if(!PL_get_nil_ex(row))
			return FALSE ;
	}
	
	if(!PL_get_nil_ex(mat))
		return FALSE ;
	
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_FLAT -> graph*
 * **********************************************
 */
int
pl_get_graph_flat_ex(term_t Graph, int n, int m, graph *g)
{
	int c,i,j ;
	term_t mat, row, cell ;
	
	EMPTYGRAPH(g,m,n) ;
	
	cell = PL_new_term_ref() ;
	mat = PL_copy_term_ref(Graph) ;
	for(i = 0 ; i < n ; i++) {
		
		for(j = i+1 ; j < n ; j++) {
			if(!PL_get_list_ex(mat, cell ,mat))
				return FALSE ;
			
			if(!PL_get_integer_ex(cell, &c))
				return FALSE ;
			
// // 			if(c == 1)
// // 				ADDONEEDGE(g, i, j, m) ;
			
			if(c == 1)
				ADDONEARC(g, i, j, m) ;
			
		}
	}
	
	if(!PL_get_nil_ex(mat))
		return FALSE ;
	
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_CHAR2 -> graph*
 * **********************************************
 */
int
pl_get_graph_char2_ex(term_t Graph, int n, int m, graph *g)
{	
	char* cs ;
	int c,i,j ;
	
	if(!PL_get_atom_chars(Graph, &cs)) {
		PL_UNIFIABLE_ERROR("atom", Graph) ;
		return FALSE ;
	}
	
	EMPTYGRAPH(g,m,n) ;
	
	for(i = 0 ; *cs != 0 && i < n ; i++) {
		for(j = i+1 ; *cs != 0 && j < n ; j++) {
			if(*cs == '1') {
				// ADDONEEDGE(g, i, j, m) ;
				ADDONEARC(g, i, j, m) ;
			} else if(*cs != '0') {
				PL_UNIFIABLE_ERROR("oneof([0,1])", Graph) ;
				return FALSE ;
			}
			cs++ ;
		}
	}
	
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_CHAR16 -> graph*
 * **********************************************
 */
int pl_get_graph_char16_ex(term_t Graph, int n, int m, graph *g)
{
	PL_instantiation_error(Graph) ;
	return FALSE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_G6_CODES -> graph*
 * **********************************************
 */
int
pl_get_graph_g6codes_ex(term_t Graph, int n, int m, graph *g)
{
	char *gstr ;
	if(!PL_get_chars(Graph, &gstr, CVT_LIST)) {
		PL_UNIFIABLE_ERROR("codes-list", Graph) ;
		return FALSE ;
	}
	
	stringtograph(gstr, g, m) ;
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_G6_CHARS -> graph*
 * **********************************************
 */
int
pl_get_graph_g6chars_ex(term_t Graph, int n, int m, graph *g)
{
	char *gstr ;
	if(!PL_get_chars(Graph, &gstr, CVT_LIST)) {
		PL_UNIFIABLE_ERROR("chars-list", Graph) ;
		return FALSE ;
	}
	
	stringtograph(gstr, g, m) ;
	return TRUE ;
}

// int
// pl_get_graph_g6chars_ex(term_t Graph, int n, int m, graph *g)
// {
// 	int i, c ;
// 	size_t length ;
// 	term_t head, tail, skip ;
// 	
// 	skip = PL_new_term_ref() ;
// 	if(PL_skip_list(Graph, skip, &length) != PL_LIST) {
// 		PL_type_error("list", Graph) ;
// 		return FALSE ;
// 	}
// 	
// 	char *gstr = (char*) malloc(sizeof(char) * length + 1) ;
// 	if(gstr == NULL) {
// 		PL_resource_error("malloc/graph->g6chars") ;
// 		return FALSE ;
// 	}
// 	char *p = gstr ;
// 	
// 	head = PL_new_term_ref() ;
// 	tail = PL_copy_term_ref(Graph) ;
// 	
// 	for(i = 0 ; i < length ; i++) {
// 		if(!PL_get_list_ex(tail, head, tail)) {
// 			free(gstr) ;
// 			return FALSE ;
// 		}
// 		
// 		if(!PL_get_char_ex(head, &c, FALSE)) {
// 			free(gstr) ;
// 			return FALSE ;
// 		}
// 		
// 		*p++ = c ;
// 	}
// 	
// 	stringtograph(gstr, g, m) ;
// 	free(gstr) ;
// 	return TRUE ;
// }

/*
 * **********************************************
 * PL_GRAPH_FMT_G6_ATOM -> graph*
 * **********************************************
 */
int
pl_get_graph_g6atom_ex(term_t Graph, int n, int m, graph *g)
{
	char * gstr ;
	if(!PL_get_chars(Graph, &gstr, CVT_ATOMIC)) {
		PL_UNIFIABLE_ERROR("atomic", Graph) ;
		return FALSE ;
	}
	
	stringtograph(gstr, g, m) ;
	return TRUE ;
}

/*
 * **********************************************
 * PL_GRAPH_FMT_G6_STRING -> graph*
 * **********************************************
 */
int
pl_get_graph_g6string_ex(term_t Graph, int n, int m, graph *g)
{
	char * gstr ;
	if(!PL_get_chars(Graph, &gstr, CVT_STRING)) {
		PL_UNIFIABLE_ERROR("string", Graph) ;
		return FALSE ;
	}
	
	stringtograph(gstr, g, m) ;
	return TRUE ;
}
