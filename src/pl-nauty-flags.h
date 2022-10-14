#ifndef PL_NAUTY_FLAGS_H
#define PL_NAUTY_FLAGS_H

/*
 * **********************************************
 * additional types used throughout the code
 * **********************************************
 */
typedef unsigned long flag_t ;
typedef int           bool_t ;

/*
 * **********************************************
 * commonly used bitwise operations (for flag_t)
 * **********************************************
 */
#define IS_POW2(v)              ( (v) && !((v) & ((v) - 1)) )
#define ISSET_FLAG(flags, flag) ( ( (flags) & (flag) ) != 0 )

/*
 * **********************************************
 *            supported graph formats
 * **********************************************
 * PL_GRAPH_FMT_* 
 * 
 * all the supported formats of graphs in the 
 * pl-nauty modules (not all are implemented,
 * so if you get an instantiation error - that's 
 * why).
 * 
 * - MATRIX   : a list of lists (adjacency matrix)
 * - ADJ_LIST : a list V-AdjV for all U > V
 * - EDGE_LIST: a list of V-U for all U > V
 * - UPPER    : the upper triangle (rows first) 
 * 		of the adjacency matrix
 * - FLAT     : the concatenation of UPPER
 * - CHAR2    : the atom of FLAT's characters
 * - CHAR16   : same as CHAR2 but the 0s and 1s are 
 *           	converted to hex
 * - G6_*     : graph6 format (code list, char list 
 *  		atom and string are supported)
 * 
 * these two are not necessary (i think)
 * because of how nauty works
 * - DI_ADJ_LIST: directed adjacency list
 * 		  V-AdjV for all (V,U) \in E.
 * 
 * - DI_EDGE_LIST: directed edges list V-U
 *                 for all (V,U) \in E.
 * **********************************************
 */
#define PL_GRAPH_FMT_MATRIX                1
#define PL_GRAPH_FMT_ADJ_LIST              2
#define PL_GRAPH_FMT_EDGE_LIST             4
#define PL_GRAPH_FMT_UPPER                 8
#define PL_GRAPH_FMT_FLAT                 16
#define PL_GRAPH_FMT_CHAR2                32
#define PL_GRAPH_FMT_CHAR16               64
#define PL_GRAPH_FMT_G6_CODES            128
#define PL_GRAPH_FMT_G6_CHARS            256
#define PL_GRAPH_FMT_G6_ATOM             512
#define PL_GRAPH_FMT_G6_STRING          1024
#define PL_GRAPH_FMT_D6_ATOM            2048
#define PL_GRAPH_FMT_DI_EDGE_LIST       4096
#define PL_GRAPH_FMT_PERMUTATION_LIST   8192
#define PL_GRAPH_FMT_PERMUTATION_PAIRS 16384
#define PL_GRAPH_FMT_SAT_ADJ_MATRIX    32768

// #define PL_GRAPH_FMT_DI_ADJ_LIST  2048
// #define PL_GRAPH_FMT_DI_EDGE_LIST 4096

#endif
