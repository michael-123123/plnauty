#ifndef PL_NAUTY_GRAPH_CONV_H
#define PL_NAUTY_GRAPH_CONV_H


/**
 * **********************************************
 * foreign_t pl_graph_convert(
 * 	term_t +N,
 * 	term_t +InFmt,
 * 	term_t +In,
 * 	term_t +OutFmt,
 * 	term_t -Out
 * )
 * 
 * convert a graph from one representation to another
 * representation. Available representations include
 * among others: adjacency matrices and lists, edge
 * lists, graph6 and upper triangles of matrix.
 * 
 * @param N number of vertices
 * @param InFmt input format
 * @param In input graph
 * @param OutFmt desired output format
 * @param Out output graph
 * **********************************************
 */
foreign_t pl_graph_convert(term_t, term_t, term_t, term_t, term_t) ;

/**
 * **********************************************
 * boolean equal_graphs(int n, int m, graph* g1, graph* g2)
 * 
 * test if two graphs are equal (i.e, identical)
 * 
 * @param n number of vertices
 * @param m nauty's word bound parameter
 * @param g1 first graph
 * @param g2 second graph
 * **********************************************
 */
boolean equal_graphs(int, int, graph*, graph*) ;

/**
 * **********************************************
 * TODO: Documentation
 * 
 * @param n number of vertices
 * @param k number of colors (if -1 then the function 
 *          will extrapolate k from col)
 * @param col the coloring array col[i] = coloring 
 *            of vertex i
 * @param lab nauty's lab array
 * @param ptn nauty's ptn array
 * **********************************************
 */
int vertex_coloring_to_lab_ptn(int n, int k, int* col, int* lab, int* ptn) ;

/**
 * **********************************************
 * TODO: Documentation
 * **********************************************
 */
#ifdef PL_NAUTY_DEBUG
foreign_t pl_graph_convert_self(term_t, term_t, term_t) ;
#endif

#ifdef PL_NAUTY_DEBUG
foreign_t pl_graph_convert_debug(term_t, term_t, term_t, term_t, term_t) ;
#endif


#endif
