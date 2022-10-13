#ifndef PL_DENSENAUTY_OPTS_H
#define PL_DENSENAUTY_OPTS_H

/*
 * **********************************************
 * >           densenauty options               <
 * **********************************************
 */

/*
 * **********************************************
 *       densenauty predicate options 
 * **********************************************
 * we have several options:
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
 * - coloring(Coloring)
 * 	initial vertex coloring, omitted by default
 * 
 * - coloring(Lab, Ptn)
 * 	initial vertex coloring, omitted by default
 * 
 * - digraph(true/false)
 * 	is the graph directed? default false
 * **********************************************
 */

/*
 * **********************************************
 *        densenauty in/out formats
 * **********************************************
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
 * - digraph6_atom
 * **********************************************
 */

/*
 * **********************************************
 * the main options structure for '$densenauty'
 * **********************************************
 */
typedef struct {
	flag_t infmt         ;
	flag_t cgfmt         ;
	bool_t get_lab       ;
	bool_t get_ptn       ;
	bool_t get_perm      ; 
	bool_t get_orbits    ;
	bool_t get_cg        ;
	bool_t have_labptn   ;
	bool_t have_coloring ;
	bool_t digraph       ;
} pl_densenauty_optsblk      ;

#define PL_DENSENAUTY_DEFAULTOPTS(opts) \
	do {				\
	opts.infmt         =     1 ;	\
	opts.cgfmt         =     1 ;	\
	opts.get_lab       =     1 ;	\
	opts.get_ptn       =     1 ;	\
	opts.get_perm      =     1 ;	\
	opts.get_orbits    =     1 ;	\
	opts.get_cg        =     1 ;	\
	opts.have_labptn   =     0 ;	\
	opts.have_coloring =     0 ;    \
	opts.digraph       =     0 ;	\
	} while(0) ;

#define PL_DENSENAUTY_DEFAULTOPTS_PTR(opts) 	\
	do {					\
	opts->infmt         =     1 ;	  	\
	opts->cgfmt         =     1 ;		\
	opts->get_lab       =     1 ;		\
	opts->get_ptn       =     1 ;		\
	opts->get_perm      =     1 ;		\
	opts->get_orbits    =     1 ;		\
	opts->get_cg        =     1 ;		\
	opts->have_labptn   =     0 ;		\
	opts->have_coloring =     0 ;    	\
	opts->digraph       =     0 ;		\
	} while(0) ;

#define PL_DENSENAUTY_OPT(opts, opt) ((opts).(opt))

/*
 * **********************************************
 * the main options structure for '$isomorphic'
 * **********************************************
 */
typedef struct {
	flag_t fmt1     ;
	flag_t fmt2     ;
	flag_t cgfmt    ;
	bool_t get_cg   ;
	bool_t get_perm ;
	bool_t digraph  ;
} pl_isomorphic_optsblk ;

#define PL_ISOMORPHIC_DEFAULTOPTS(opts) \
	do {				\
	opts.fmt1     = 1 ;		\
	opts.fmt2     = 1 ;		\
	opts.cgfmt    = 1 ;		\
	opts.get_cg   = 1 ;		\
	opts.get_perm = 1 ;		\
	opts.digraph  = 0 ;             \
	} while(0) ;


#define PL_ISOMORPHIC_DEFAULTOPTS_PTR(opts) 	\
	do {					\
	opts->fmt1     = 1 ;			\
	opts->fmt2     = 1 ;			\
	opts->cgfmt    = 1 ;			\
	opts->get_cg   = 1 ;			\
	opts->get_perm = 1 ;			\
	opts->digraph  = 0 ;                 	\
	} while(0) ;

/*
 * **********************************************
 * cached values defined in pl-densenauty-opts.c
 * 
 * TODO:
 * some of these can be made static and removed 
 * from the header file
 * **********************************************
 */

/*
 * options names
 */
extern atom_t infmt_a        ;
extern atom_t cgfmt_a        ;
extern atom_t get_lab_a      ;
extern atom_t get_ptn_a      ;
extern atom_t get_perm_a     ;
extern atom_t get_orbits_a   ;
extern atom_t get_cg_a       ;
extern atom_t coloring_a     ;
extern atom_t digraph_a      ;
extern atom_t fmt1_a         ;
extern atom_t fmt2_a         ;

/*
 * format names
 */
extern atom_t adj_mat_a      ;
extern atom_t adj_lists_a    ;
extern atom_t edge_list_a    ;
extern atom_t upper_tri_a    ;
extern atom_t upper_flat_a   ;
extern atom_t upper_char2_a  ;
extern atom_t upper_char16_a ;
extern atom_t g6_atom_a      ;
extern atom_t g6_chars_a     ;
extern atom_t g6_codes_a     ;
extern atom_t g6_string_a    ;
extern atom_t d6_atom_a      ;
extern atom_t di_edge_list_a ;
extern atom_t perm_list_a    ;
extern atom_t perm_pairs_a   ;

/*
 * static options struct
 */
extern pl_densenauty_optsblk pl_densenauty_opts ;
extern pl_isomorphic_optsblk pl_isomorphic_opts ;

/**
 * **********************************************
 * initialize densenauty's environment
 * this must be registered at initialization 
 * either by the prolog module or by C
 * 
 * currently done via a prolog module
 * **********************************************
 */
foreign_t pl_densenauty_init() ;

/**
 * **********************************************
 * pl_get_densenauty_opts_ex(
 * 	term_t Opts, 
 * 	pl_densenauty_optsblk *opts, 
 * 	term_t InLab, 
 * 	term_t InPtn,
 * 	term_t Coloring
 * )
 * 
 * extract the options (see above) from a prolog 
 * list and into the proper c structure. 
 * 
 * @param Opts  the prolog option list
 * @param opts  the c option struct (already allocated)
 * @param InLab the prolog coloring [lab] (if specified)
 * @param InPtn the prolog coloring [ptn] (if specified)
 * 
 * @todo Add a term_t coloring so that coloring/1 will 
 *       be a valid option.
 * **********************************************
 */
int pl_get_densenauty_opts_ex(term_t, pl_densenauty_optsblk*, term_t, term_t, term_t) ;

/*
 * **********************************************
 * int pl_get_isomorphic_opts_ex(
 * 	term_t Opts,
 * 	pl_isomorphic_optsblk* opts
 * )
 * **********************************************
 */
int pl_get_isomorphic_opts_ex(term_t, pl_isomorphic_optsblk*) ;

/**
 * **********************************************
 * int pl_get_densenauty_fmt_ex(term_t Fmt, flag_t* fmt)
 * 
 * parse a named graph format (named above)
 * into a c format which is one of PL_GRAPH_FMT_*
 * (see nauty.h for more info)
 *
 * @param Fmt the prolog format which will be parsed
 * @param fmt the c value corresponding to the format
 *            the dereferenced value of fmt will be 
 *            assigned the format.
 * **********************************************
 */
int pl_get_densenauty_fmt_ex(term_t, flag_t*) ;

/**
 * **********************************************
 * foreign_t pl_densenauty_debug_opts(
 * 	term_t Opts, term_t InLab, term_t InPtn, 
 * 	term_t Coloring) 
 * 
 * a debug predicate which will parse a list of 
 * prolog options, and print some info regarding 
 * the c struct.
 * 
 * @param Opts  the prolog option list
 * @param InLab the coloring lab (if specified)
 * @param InPtn the coloring ptn (if specified)
 * **********************************************
 */
#ifdef PL_NAUTY_DEBUG
foreign_t pl_densenauty_debug_opts(term_t, term_t, term_t, term_t) ;
#endif


//
#endif
