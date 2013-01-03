// cscids.h -- C/C++ Interface to SciDS-II Sci_entif D_ata S_ets
//
// DM/MCFA  08/04
//

#ifndef __CSCIDS_H__
#define __CSCIDS_H__

#include <db.h>
#include "cforeign_array.h"

struct cscids_item;

extern "C" {
  extern DB *cscids_open(char *filename, int flags, int perms);
  extern void cscids_trunc(DB *pdb);
  extern void cscids_sync(DB *pdb);
  extern void cscids_close(DB *pdb);

  extern cscids_item *cscids_get_keys(DB *pdb);
  extern void cscids_del_keys(DB *pdb, char *key);
};

// ---------------------------------------------------------------------
// SciDS-II Interface from C/C++
//
// Built on top of Berkeley DB, but provides a consistent and portable
// data interchage format for Sci_entific D_ata S_ets.
//
// ---------------------------------------------------------------
// Array storage and retrieval...
//
extern "C" {
  extern void cscids_get_array_description(DB *pdb, char *key,
					   foreign_array_header *phdr);
  extern foreign_array *cscids_get_subarray(DB *pdb, char *key,
					    long *off, long *len);
  extern foreign_array *cscids_get_array(DB *pdb, char *key);
  extern void cscids_put_subarray(DB *pdb, char *key,
				  foreign_array *pdata, long* off);
  extern void cscids_put_array(DB *pdb, char *key, foreign_array *pdata);
  extern void cscids_del_array(DB *pdb, char *key);
};

// ---------------------------------------------------------------
// Item storage and retrieval...
//
// On retreival, one of these item structs is filled in and returned.
// Memory reclamation becomes your responsibility...
// (hint: call cscids_release_item() on them...)
//
enum item_types {
  // atom types
  SCIDS_CHAR,
  SCIDS_INT,
  SCIDS_FLOAT,
  SCIDS_STRING,
  SCIDS_ARRAY,

  // vector types
  SCIDS_CHARS,  // chars, ints, floats, more efficient than vectors of atoms
  SCIDS_INTS,
  SCIDS_FLOATS,
  SCIDS_STRINGS,
  
  SCIDS_ITEMS };  // a vector of items

// basic type of a SciDS item
// use the constructor methods below instead of creating these directly
struct cscids_item
{
  int  type;		/* item type */
  union
  {
    // atom representations
    char           c;	/* SCIDS_CHAR */
    long           i;	/* SCIDS_INT */
    double         f;	/* SCIDS_FLOAT */
    char          *s;	/* SCIDS_STRING (a null terminated C string) */
    foreign_array *a;	/* SCIDS_ARRAY (foreign_array w/ multiple dimensions) */

    // vector representations
    char          *cv;  /* SCIDS_CHARS (any mix of chars vector *) */
    long          *iv;	/* SCIDS_INTS (a vector of longs) */
    double	  *fv;	/* SCIDS_FLOATS (a vector of doubles) */
    char         **sv;  /* SCIDS_STRINGS (a vector of strings) */

    cscids_item **pv;  /* SCIDS_ITEMS (a vector of ptrs to items) */
  } data;
  long nel;		/* nbr elements for vectors */

  virtual void permit_construct() = 0;
  virtual ~cscids_item() {};
};

struct cscids_item_list;

extern "C" {
  extern cscids_item *cscids_get_item(DB *pdb, char *key);
  extern void cscids_release_item(cscids_item *pitem);

  extern void cscids_put_item(DB *pdb, char *key, cscids_item *pdata);
								
  // encoders for items and item vectors
  // when finished with these dynamically allocated items,
  // delete them, e.g., after putting them to the SciDS file.
  extern cscids_item *cscids_encode_char(char ch);
  extern cscids_item *cscids_encode_int(long v);
  extern cscids_item *cscids_encode_float(double v);
  extern cscids_item *cscids_encode_string(char *str,
					   bool copy = true);
  extern cscids_item *cscids_encode_array(foreign_array *fp,
					  bool copy = true);
  extern cscids_item *cscids_encode_chars(char *cp, long nel,
					  bool copy = true);
  extern cscids_item *cscids_encode_ints(long *vp, long nel,
					 bool copy = true);
  extern cscids_item *cscids_encode_floats(double *vp, long nel,
					   bool copy = true);
  extern cscids_item *cscids_encode_strings(char **sv, long nel,
					    bool copy = true);

  // constructors for lists of items
  // open a list, add items to it, then close it to get the aggregate item
  extern cscids_item_list *cscids_open_item_list();
  extern void cscids_add_item_to_list(cscids_item_list *plist,
				      cscids_item *pitem);
  extern cscids_item *cscids_close_item_list(cscids_item_list *plist);

};

#endif // __CSCIDS_H__

// -- end of cscids.h -- //
