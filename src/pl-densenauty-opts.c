#include <SWI-Stream.h>
#include <SWI-Prolog.h>

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "pl-nauty-flags.h"
#include "pl-densenauty-opts.h"

/*
 * **********************************************
 * >  global variables (declared in pl-opts.h)  <
 * **********************************************
 */

/*
 * **********************************************
 *       densenauty predicate options 
 * **********************************************
 */
static bool_t densenauty_initialized = FALSE ;

atom_t infmt_a      = (atom_t) NULL ;
atom_t cgfmt_a      = (atom_t) NULL ;
atom_t get_lab_a    = (atom_t) NULL ;
atom_t get_ptn_a    = (atom_t) NULL ;
atom_t get_perm_a   = (atom_t) NULL ;
atom_t get_orbits_a = (atom_t) NULL ;
atom_t get_cg_a     = (atom_t) NULL ;
atom_t coloring_a   = (atom_t) NULL ;
atom_t digraph_a    = (atom_t) NULL ;
atom_t fmt1_a       = (atom_t) NULL ;
atom_t fmt2_a       = (atom_t) NULL ;

/*
 * **********************************************
 *        densenauty in/out formats
 * **********************************************
 * several formats are avaialable though not all
 * formats are yet supported
 * **********************************************
 */
atom_t adj_mat_a      = (atom_t) NULL ;
atom_t adj_lists_a    = (atom_t) NULL ;
atom_t edge_list_a    = (atom_t) NULL ;
atom_t upper_tri_a    = (atom_t) NULL ;
atom_t upper_flat_a   = (atom_t) NULL ;
atom_t upper_char2_a  = (atom_t) NULL ;
atom_t upper_char16_a = (atom_t) NULL ;
atom_t g6_atom_a      = (atom_t) NULL ;
atom_t g6_chars_a     = (atom_t) NULL ;
atom_t g6_codes_a     = (atom_t) NULL ;
atom_t g6_string_a    = (atom_t) NULL ;
atom_t d6_atom_a      = (atom_t) NULL ;
atom_t di_edge_list_a = (atom_t) NULL ;
atom_t perm_list_a    = (atom_t) NULL ;
atom_t perm_pairs_a   = (atom_t) NULL ;

/*
 * **********************************************
 * global options block
 * **********************************************
 */
pl_densenauty_optsblk pl_densenauty_opts ;
pl_isomorphic_optsblk pl_isomorphic_opts ;

/*
 * **********************************************
 * initialize the various global variables related
 * to densenauty calls. this should be done upon
 * initialization of the prolog module.
 * **********************************************
 */
foreign_t
pl_densenauty_init()
{
	if(densenauty_initialized) {
		PL_warning("$init_nauty: reinitialization should not occur") ;
		PL_succeed ;
	}
	
	/*
	 * initialize flag names
	 */
	infmt_a      = PL_new_atom("infmt")      ;
	cgfmt_a      = PL_new_atom("cgfmt")      ;
	get_lab_a    = PL_new_atom("get_lab")    ;
	get_ptn_a    = PL_new_atom("get_ptn")    ;
	get_perm_a   = PL_new_atom("get_perm")   ;
	get_orbits_a = PL_new_atom("get_orbits") ;
	get_cg_a     = PL_new_atom("get_cg")     ;
	coloring_a   = PL_new_atom("coloring")   ;
	digraph_a    = PL_new_atom("digraph")    ;
	fmt1_a       = PL_new_atom("fmt1")       ;
	fmt2_a       = PL_new_atom("fmt2")       ;
	
	/*
	 * initialize format names
	 */
	adj_mat_a      = PL_new_atom("adj_matrix")             ;
	adj_lists_a    = PL_new_atom("adj_lists")              ;
	edge_list_a    = PL_new_atom("edge_list")              ;
	upper_tri_a    = PL_new_atom("upper_triangle")         ;
	upper_flat_a   = PL_new_atom("upper_triangle_flat")    ;
	upper_char2_a  = PL_new_atom("upper_triangle_char2")   ;
	upper_char16_a = PL_new_atom("upper_triangle_char16")  ;
	g6_atom_a      = PL_new_atom("graph6_atom")            ;
	g6_chars_a     = PL_new_atom("graph6_chars")           ;
	g6_codes_a     = PL_new_atom("graph6_codes")           ;
	g6_string_a    = PL_new_atom("graph6_string")          ;
	d6_atom_a      = PL_new_atom("digraph6_atom")          ;
	di_edge_list_a = PL_new_atom("directed_edge_list")     ;
	perm_list_a    = PL_new_atom("permutation_list")       ;
	perm_pairs_a   = PL_new_atom("permutation_pairs_list") ;


	/*
	 * initialize option struct
	 * this should be done every nauty call not 
	 * just at initialization
	 */
	PL_DENSENAUTY_DEFAULTOPTS(pl_densenauty_opts) ;
	PL_ISOMORPHIC_DEFAULTOPTS(pl_isomorphic_opts) ;
	
	densenauty_initialized = TRUE ;
	PL_succeed ;
}

/*
 * **********************************************
 * parse nauty prolog options to an options struct
 * **********************************************
 */
int
pl_get_densenauty_opts_ex(term_t Opts, pl_densenauty_optsblk *opts, term_t InLab, term_t InPtn, term_t Coloring)
{
	term_t head = PL_new_term_ref() ;
	term_t tail = PL_copy_term_ref(Opts) ;
	
	atom_t opt ;
	int  arity ;
	
	term_t arg1 = PL_new_term_ref() ;
	term_t arg2 = PL_new_term_ref() ;
	
	int temp ;
	
	while(PL_get_list_ex(tail, head, tail)) {
		
		/*
		 * skip all non-options
		 */
		if(!PL_get_compound_name_arity(head, &opt, &arity))
			continue ;
		
		if(arity > 2)
			continue ;
		
		if(opt != infmt_a   && opt != cgfmt_a    && opt != get_lab_a    && 
		   opt != get_ptn_a && opt != get_perm_a && opt != get_orbits_a && 
		   opt != get_cg_a  && opt != coloring_a && opt != digraph_a    )
			continue ;
		
		/*
		 * throw an exception in case of 
		 * bad arity options
		 */
		if((opt != coloring_a && arity != 1) || (opt == coloring_a && arity < 1)) {
			PL_type_error("$densenauty: option has bad arity", head) ;
			return FALSE ;
		}
		
		/*
		 * get the option's parameters
		 */
		if(!PL_get_arg(1, head, arg1)) {
			PL_representation_error("opt has arity >= 1, arg/3 failed!") ;
			return FALSE ;
		}
		
		if(arity > 1 && !PL_get_arg(2, head, arg2)) {
			PL_representation_error("opt has arity >= 2, arg/3 failed!") ;
			return FALSE ;
		}
		
		
		/*
		 * set each options
		 */
		
		if(opt == infmt_a) {
			// Sprintf("nauty opt: from/1\n") ;
			
			if(!pl_get_densenauty_fmt_ex(arg1, &opts->infmt))
				return FALSE ;

		} else if(opt == cgfmt_a) {
			// Sprintf("nauty opt: to/1\n") ;
			
			if(!pl_get_densenauty_fmt_ex(arg1, &opts->cgfmt))
				return FALSE ;
			
		} else if(opt == get_lab_a) {
			// Sprintf("nauty opt: get_lab/1\n") ;
			
			if(!PL_get_bool_ex(arg1, &opts->get_lab))
				return FALSE ;
			
		} else if(opt == get_ptn_a) {
			// Sprintf("nauty opt: get_ptn/1\n") ;
			
			if(!PL_get_bool_ex(arg1, &opts->get_ptn))
				return FALSE ;
			
		} else if(opt == get_perm_a) {
			// Sprintf("nauty opt: get_perm/1\n") ;
			
			if(!PL_get_bool_ex(arg1, &opts->get_perm))
				return FALSE ;
			
		} else if(opt == get_orbits_a) {
			// Sprintf("nauty opt: get_orbits/1\n") ;
			
			if(!PL_get_bool_ex(arg1, &opts->get_orbits))
				return FALSE ;
			
		} else if(opt == get_cg_a) {
			// Sprintf("nauty opt: get_cg/1\n") ;
			
			if(!PL_get_bool_ex(arg1, &opts->get_cg))
				return FALSE ;
			
		} else if( opt == coloring_a && arity == 1) {
			// Sprintf("nauty opt: coloring/1\n") ;
			
			if(!PL_unify(arg1,Coloring))
				return FALSE ;
			
			opts->have_coloring = TRUE ;
			
		} else if(opt == coloring_a && arity > 1) {
			// Sprintf("nauty opt: coloring/2\n") ;
			
			if(!PL_unify(arg1, InLab) || !PL_unify(arg2, InPtn))
				return FALSE ;
			
			opts->have_labptn = TRUE ;
			
		} else if(opt == digraph_a) {
			// Sprintf("nauty opt: digraph/1\n") ;
			
			if(!PL_get_bool_ex(arg1, &opts->digraph))
				return FALSE ;
		}
	}
	
	
	return TRUE ;
}

/*
 * **********************************************
 * parse nauty prolog options to an options struct
 * **********************************************
 */
int
pl_get_isomorphic_opts_ex(term_t Opts, pl_isomorphic_optsblk* opts)
{
	term_t head = PL_new_term_ref() ;
	term_t tail = PL_copy_term_ref(Opts) ;
	
	atom_t opt ;
	int  arity ;
	
	term_t arg1 = PL_new_term_ref() ;
	
	PL_ISOMORPHIC_DEFAULTOPTS_PTR(opts) ;
	
	while(PL_get_list_ex(tail, head, tail)) {
		
		/*
		 * skip all non-options
		 */
		if(!PL_get_compound_name_arity(head, &opt, &arity))
			continue ;
		
		if(arity != 1)
			continue ;
		
		if(opt != fmt1_a   && opt != fmt2_a && opt != get_perm_a &&
		   opt != get_cg_a && opt != cgfmt_a && opt != digraph_a)
			continue ;
		
		/*
		 * get the option's parameters
		 */
		if(!PL_get_arg(1, head, arg1)) {
			PL_representation_error("opt has arity >= 1, arg/3 failed!") ;
			return FALSE ;
		}
		
		
		/*
		 * set each options
		 */
		
		if(opt == fmt1_a) {
			// Sprintf("nauty opt: from/1\n") ;
			
			if(!pl_get_densenauty_fmt_ex(arg1, &opts->fmt1))
				return FALSE ;

		} else if(opt == fmt2_a) {
			// Sprintf("nauty opt: to/1\n") ;
			
			if(!pl_get_densenauty_fmt_ex(arg1, &opts->fmt2))
				return FALSE ;
			
		} else if(opt == get_perm_a) {
			// Sprintf("nauty opt: get_perm/1\n") ;
			
			if(!PL_get_bool_ex(arg1, &opts->get_perm))
				return FALSE ;
			
		} else if(opt == get_cg_a) {
			// Sprintf("nauty opt: get_cg/1\n") ;
			
			if(!PL_get_bool_ex(arg1, &opts->get_cg))
				return FALSE ;
			
		} else if(opt == cgfmt_a) {
			// Sprintf("nauty opt: cgfmt/1\n") ;
			
			if(!pl_get_densenauty_fmt_ex(arg1, &opts->cgfmt))
				return FALSE ;
		} else if(opt == digraph_a) {
			if (!PL_get_bool_ex(arg1, &opts->digraph))
				return FALSE ;
		}
	}
	
	
	return TRUE ;
}

/*
 * **********************************************
 * extract a format identifier from name
 * **********************************************
 */
int
pl_get_densenauty_fmt_ex(term_t Fmt, flag_t* fmt)
{
	atom_t name ;
	if(!PL_get_atom_ex(Fmt, &name))
		return FALSE ;
	
	     if( name == adj_mat_a      ) *fmt = PL_GRAPH_FMT_MATRIX            ;
	else if( name == adj_lists_a    ) *fmt = PL_GRAPH_FMT_ADJ_LIST          ;
	else if( name == edge_list_a    ) *fmt = PL_GRAPH_FMT_EDGE_LIST         ;
	else if( name == upper_tri_a    ) *fmt = PL_GRAPH_FMT_UPPER             ;
	else if( name == upper_flat_a   ) *fmt = PL_GRAPH_FMT_FLAT              ;
	else if( name == upper_char2_a  ) *fmt = PL_GRAPH_FMT_CHAR2             ;
	else if( name == upper_char16_a ) *fmt = PL_GRAPH_FMT_CHAR16            ;
	else if( name == g6_atom_a      ) *fmt = PL_GRAPH_FMT_G6_ATOM           ;
	else if( name == g6_chars_a     ) *fmt = PL_GRAPH_FMT_G6_CHARS          ;
	else if( name == g6_codes_a     ) *fmt = PL_GRAPH_FMT_G6_CODES          ;
	else if( name == g6_string_a    ) *fmt = PL_GRAPH_FMT_G6_STRING         ;
	else if( name == d6_atom_a      ) *fmt = PL_GRAPH_FMT_D6_ATOM           ;
	else if( name == di_edge_list_a ) *fmt = PL_GRAPH_FMT_DI_EDGE_LIST      ;
	else if( name == perm_list_a    ) *fmt = PL_GRAPH_FMT_PERMUTATION_LIST  ;
	else if( name == perm_pairs_a   ) *fmt = PL_GRAPH_FMT_PERMUTATION_PAIRS ;
	else { PL_type_error("legal graph format", Fmt); return FALSE ; }
	
	return TRUE ;
}

#ifdef PL_NAUTY_DEBUG
/*
 * **********************************************
 * a debug predicate implemented for sanity check
 * should not be included in standard builds.
 * **********************************************
 */
foreign_t
pl_densenauty_debug_opts(term_t Opts, term_t InLab, term_t InPtn, term_t Coloring)
{
	PL_DENSENAUTY_DEFAULTOPTS(pl_densenauty_opts) ;
	if(!pl_get_densenauty_opts_ex(Opts, &pl_densenauty_opts, InLab, InPtn, Coloring))
		PL_fail ;
	
	Sprintf("opts->infmt         = %d\n", pl_densenauty_opts.infmt         ) ;
	Sprintf("opts->cgfmt         = %d\n", pl_densenauty_opts.cgfmt         ) ;
	Sprintf("opts->get_lab       = %d\n", pl_densenauty_opts.get_lab       ) ;
	Sprintf("opts->get_ptn       = %d\n", pl_densenauty_opts.get_ptn       ) ;
	Sprintf("opts->get_perm      = %d\n", pl_densenauty_opts.get_perm      ) ;
	Sprintf("opts->get_orbits    = %d\n", pl_densenauty_opts.get_orbits    ) ;
	Sprintf("opts->get_cg        = %d\n", pl_densenauty_opts.get_cg        ) ;
	Sprintf("opts->have_labptn   = %d\n", pl_densenauty_opts.have_labptn   ) ;
	Sprintf("opts->have_coloring = %d\n", pl_densenauty_opts.have_coloring ) ;
	Sprintf("opts->digraph       = %d\n", pl_densenauty_opts.digraph       ) ;
	
	PL_succeed ;
}
#endif
