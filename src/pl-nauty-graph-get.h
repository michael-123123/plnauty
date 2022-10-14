#ifndef PL_NAUTY_GRAPH_GET_H
#define PL_NAUTY_GRAPH_GET_H
//


/*
 * **********************************************
 * given one of the supported prolog graph 
 * representations (see above) we'd like to convert
 * it to nauty's graph format
 * 
 * TODO: Documentation
 * **********************************************
 */
int pl_get_graph_ex(term_t, int, int, flag_t, graph*) ;
int pl_get_graph_adj_mat_ex(term_t, int, int, graph*) ;
int pl_get_graph_adj_lists_ex(term_t, int, int, graph*) ;
int pl_get_graph_edge_lists_ex(term_t, int, int, graph*) ;
int pl_get_graph_upper_ex(term_t, int, int, graph*) ;
int pl_get_graph_flat_ex(term_t, int, int, graph*) ;
int pl_get_graph_char2_ex(term_t, int, int, graph*) ;
int pl_get_graph_char16_ex(term_t, int, int, graph*) ;
int pl_get_graph_g6codes_ex(term_t, int, int, graph*) ;
int pl_get_graph_g6chars_ex(term_t, int, int, graph*) ;
int pl_get_graph_g6atom_ex(term_t, int, int, graph*) ;
int pl_get_graph_g6string_ex(term_t, int, int, graph*) ;
int pl_get_graph_d6atom_ex(term_t, int, int, graph*) ;
int pl_get_graph_di_edge_list_ex(term_t, int, int, graph*) ;
int pl_get_graph_perm_list_ex(term_t, int, int, graph*) ;
int pl_get_graph_perm_pairs_ex(term_t, int, int, graph*) ;
int pl_get_graph_sat_adj_matrix(term_t, int, int, graph*) ;

//
#endif
