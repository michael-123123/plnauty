#ifndef PL_DENSENAUTY_H
#define PL_DENSENAUTY_H

/**
 * **********************************************
 * foreign_t pl_densenauty(
 * 	term_t N, 
 * 	term_t Graph, 
 * 	term_t Lab, 
 * 	term_t Ptn, 
 * 	term_t Perm, 
 * 	term_t Orbits, 
 * 	term_t Canonic, 
 * 	term_t Opts
 * )
 * 
 * convert a graph with n vertices to a nauty graph*
 * and call densenauty. convert the result back to 
 * prolog format. the result includes several parameters
 * which are part of the nauty call (see nauty user
 * guide to understand their meaning).
 * 
 * @param N number of vertices
 * @param Graph input graph
 * @param Lab lab array (labeling)
 * @param Ptn ptn array (partitioning)
 * @param Perm permutation from Graph to Canonic
 * @param Orbits orbits array
 * @param Canonic canonic graph representation of input
 * @param Opts options list
 * 
 * **********************************************
 * 
 * Options include:
 * 
 * - infmt(Format)
 * 	input format
 * 
 * - cgfmt(Format)
 * 	canonical (output) format
 * 
 * default for both is adjacency matrix
 * 
 * - get_lab(true/false)
 * 	get canonical labeling?
 * 
 * - get_ptn(true/false)
 * 	get canonical partition?
 * 
 * - get_perm(true/false)
 * 	get the permutation from Input to Canonic?
 * 
 * - get_orbits(true/false)
 * 	get the orbits of graph
 * 
 * - get_cg(true/false)
 * 	get the canonic representation
 * 
 * default for all these is true
 * 
 * - coloring(Lab, Ptn)
 * 	initial vertex coloring, omitted by default
 * 
 * - digraph(true/false)
 * 	is the graph directed? default false
 * 
 * several formats are avaialable though not all
 * formats are yet supported
 * 
 * - adj_matrix
 * - adj_lists
 * - edge_list
 * 
 * - upper_triangle
 * - upper_triangle_flat
 * - upper_triangle_char2
 * - upper_triangle_char16
 * 
 * - graph6_atom
 * - graph6_chars
 * - graph6_codes
 * - graph6_string
 * **********************************************
 */
foreign_t pl_densenauty(term_t, term_t, term_t, term_t, term_t, term_t, term_t, term_t) ;

/**
 * **********************************************
 * foreign_t pl_isomorphic(
 * 	term_t N,
 * 	term_t Graph1,
 * 	term_t Graph2,
 * 	term_t Perm,
 * 	term_t Canonic,
 * 	term_t Opts
 * )
 * 
 * test if two graphs are isomorphic, this can work
 * quicker than two calls to pl_densenauty.
 * 
 * currently Opts does not support any options
 * the graphs are assumed to be prolog int adj 
 * matrices representation. 
 * 
 * TODO: Options handling
 * 
 * @param N number of vertices
 * @param Graph1 first graph
 * @param Graph2 second graph
 * @param Perm if Graph1 isomorphic to Graph2 then
 *             Perm is unified with the permutation
 *             such that Perm(Graph1) = Graph2
 * @param Opts the options list
 * **********************************************
 */
foreign_t pl_isomorphic(term_t, term_t, term_t, term_t, term_t, term_t) ;

#endif