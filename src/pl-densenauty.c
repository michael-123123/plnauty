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
#include "pl-densenauty.h"

/*
 * **********************************************
 * pl_densenauty performs some book-keeping and 
 * then a low level call to densenauty
 * **********************************************
 */
foreign_t
pl_densenauty(term_t N, term_t Graph,
	      term_t Lab, term_t Ptn, term_t Perm, term_t Orbits,
	      term_t Canonic, 
	      term_t Opts)
{
	/*
	 * options management
	 * DONE digraph
	 * DONE inlab, inptn
	 * DONE cg
	 */
	term_t InLab = PL_new_term_ref() ;
	term_t InPtn = PL_new_term_ref() ;
	term_t Color = PL_new_term_ref() ;
	
	PL_DENSENAUTY_DEFAULTOPTS(pl_densenauty_opts) ;
	if(!pl_get_densenauty_opts_ex(Opts, &pl_densenauty_opts, InLab, InPtn, Color))
		PL_fail ;
	
	/*
	 * nauty vars
	 */
	DYNALLSTAT(graph,g,g_sz);
	DYNALLSTAT(int,inlab,inlab_sz);
	DYNALLSTAT(int,inptn,inptn_sz);
	DYNALLSTAT(int,lab,lab_sz);
	DYNALLSTAT(int,ptn,ptn_sz);
	DYNALLSTAT(int,perm,perm_sz);
	DYNALLSTAT(int,orbits,orbits_sz);
	DYNALLSTAT(graph,cg,cg_sz);
	DYNALLSTAT(int, col, col_sz) ;
	optionblk options ;
	
	if(pl_densenauty_opts.digraph) {
		DEFAULTOPTIONS_DIGRAPH(options1) ;
		options = options1 ;
	} else {
		DEFAULTOPTIONS_GRAPH(options2);
		options = options2 ;
	}
	
	statsblk stats;
	
	if(pl_densenauty_opts.get_cg)
		options.getcanon = TRUE ;
	
	/*
	 * run densenauty
	 */
	int n, m ;
	if(!PL_get_integer_ex(N, &n))
		PL_fail ;
	if(n <= 0) {
		PL_instantiation_error(N) ;
		PL_fail ;
	}
	
	m = SETWORDSNEEDED(n) ;
	nauty_check(WORDSIZE, m, n, NAUTYVERSIONID) ;
	
	DYNALLOC2(graph,g,g_sz,m,n,"malloc");
	DYNALLOC1(int,lab,lab_sz,n,"malloc");
	DYNALLOC1(int,perm,perm_sz,n,"malloc");
	DYNALLOC1(int,ptn,ptn_sz,n,"malloc");
	DYNALLOC1(int,orbits,orbits_sz,n,"malloc");
	
	if(pl_densenauty_opts.get_cg) {
		DYNALLOC2(graph,cg,cg_sz,m,n,"malloc");
	}
	
	if(pl_densenauty_opts.have_coloring) {
		DYNALLOC1(int,col,col_sz,n,"malloc");
	}
	
	if(pl_densenauty_opts.have_labptn) {
		if(!pl_get_list_int_array_ex(InLab, n, -1, lab) || 
		   !pl_get_list_int_array_ex(InPtn, n,  0, ptn) )
			PL_fail ;
		
		options.defaultptn = FALSE ;
	} else if( pl_densenauty_opts.have_coloring) {
		if(!pl_get_list_int_array_ex(Color, n, -1, col))
			PL_fail ;
		
		if(!vertex_coloring_to_lab_ptn(n, -1, col, lab, ptn)) {
			PL_resource_error("coloring->lab*ptn") ;
			PL_fail ;
		}
		
		options.defaultptn = FALSE ;
	}
	
	if(!pl_get_graph_ex(Graph, n, m, pl_densenauty_opts.infmt, g))
		PL_fail ;
	
	densenauty(g, lab, ptn, orbits, &options, &stats, m, n, cg) ;
	
	if(pl_densenauty_opts.get_lab && !pl_unify_list_int_array_ex(Lab, n, 1, lab))
		PL_fail ;
	
	if(pl_densenauty_opts.get_ptn && !pl_unify_list_int_array_ex(Ptn, n, 0, ptn))
		PL_fail ;
	
	if(pl_densenauty_opts.get_orbits && !pl_unify_list_int_array_ex(Orbits, n, 1, orbits))
		PL_fail ;
	
	if(pl_densenauty_opts.get_perm) {
		int i;
		for(i = 0 ; i < n ; i++)
			perm[lab[i]] = i ;
	
		if(!pl_unify_list_int_array_ex(Perm, n, 1, perm))
			PL_fail ;
	}

	if(pl_densenauty_opts.get_cg && !pl_unify_graph_ex(Canonic, n, m, pl_densenauty_opts.cgfmt, cg))
		PL_fail ;
	
	PL_succeed ;
}

/*
 * **********************************************
 * test if Graph1 ~ Graph2 under Perm
 * **********************************************
 */
foreign_t 
pl_isomorphic(term_t N, term_t Graph1, term_t Graph2, term_t Perm, term_t Canonic, term_t Opts)
{
	/*
	 * TODO flags handling
	 */
	PL_ISOMORPHIC_DEFAULTOPTS(pl_isomorphic_opts) ;
	if(!pl_get_isomorphic_opts_ex(Opts, &pl_isomorphic_opts))
		PL_fail ;
	
	// graph1 vars
	DYNALLSTAT(graph,g1,g1_sz);
	DYNALLSTAT(int,lab1,lab1_sz);
	DYNALLSTAT(graph,cg1,cg1_sz);
	// graph2 vars
	DYNALLSTAT(graph,g2,g2_sz);
	DYNALLSTAT(int,lab2,lab2_sz);
	DYNALLSTAT(graph,cg2,cg2_sz);
	// shared vars
	DYNALLSTAT(int,ptn,ptn_sz);
	DYNALLSTAT(int,orbits,orbits_sz);
	DYNALLSTAT(int,perm,perm_sz);
	DEFAULTOPTIONS_GRAPH(options);
	statsblk stats;
	
	int m, n, i ;
	
	options.getcanon = TRUE;
	
	if(!PL_get_integer_ex(N, &n) || n <= 0)
		PL_fail ;
	
	m = SETWORDSNEEDED(n);
	
	nauty_check(WORDSIZE,m,n,NAUTYVERSIONID);
	
	// alloc graph1
	DYNALLOC2(graph,g1,g1_sz,m,n,"malloc");
	DYNALLOC2(graph,cg1,cg1_sz,m,n,"malloc");
	DYNALLOC1(int,lab1,lab1_sz,n,"malloc");
	// alloc graph2
	DYNALLOC2(graph,g2,g2_sz,m,n,"malloc");
	DYNALLOC2(graph,cg2,cg2_sz,m,n,"malloc");
	DYNALLOC1(int,lab2,lab2_sz,n,"malloc");
	// alloc shared vars	
	DYNALLOC1(int,ptn,ptn_sz,n,"malloc");
	DYNALLOC1(int,orbits,orbits_sz,n,"malloc");
	DYNALLOC1(int,perm,perm_sz,n,"malloc");
	
	if(!pl_get_graph_ex(Graph1, n, m, pl_isomorphic_opts.fmt1, g1))
		PL_fail ;
	
	if(!pl_get_graph_ex(Graph2, n, m, pl_isomorphic_opts.fmt2, g2))
		PL_fail ;
	
	densenauty(g1,lab1,ptn,orbits,&options,&stats,m,n,cg1);
	densenauty(g2,lab2,ptn,orbits,&options,&stats,m,n,cg2);
	
	if(!equal_graphs(n, m, cg1, cg2))
		PL_fail ;

	if(pl_isomorphic_opts.get_perm) {
		for(i = 0 ; i < n ; i++)
			perm[lab2[i]] = lab1[i] ;
		
		if(!pl_unify_list_int_array_ex(Perm, n, 1, perm))
			PL_fail ;
	}
	
	if(pl_isomorphic_opts.get_cg) {
		if(!pl_unify_graph_ex(Canonic, n, m, pl_isomorphic_opts.cgfmt, cg1))
			PL_fail ;
	}
	
	PL_succeed ;
}