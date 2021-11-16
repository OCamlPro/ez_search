/************************************************************************/
/*                                Ironmin                               */
/*                                                                      */
/*  Copyright 2018-2019 OCamlPro                                        */
/*                                                                      */
/*  This file is distributed under the terms of the GNU General Public  */
/*  License as published by the Free Software Foundation; either        */
/*  version 3 of the License, or (at your option) any later version.    */
/*                                                                      */
/*  Ironmin is distributed in the hope that it will be useful,          */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of      */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       */
/*  GNU General Public License for more details.                        */
/*                                                                      */
/************************************************************************/

#ifdef __APPLE__
#define O_LARGEFILE 0
#else
#define _LARGEFILE64_SOURCE
#endif

#define _GNU_SOURCE

#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/alloc.h"
#include "caml/gc.h"
#include "caml/memory.h"
#include <caml/bigarray.h>

#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>


typedef struct {
  int fd ;
  unsigned char* addr;
  uint64_t map_size ;
} mapfile_t ;

value ocp_mapfile_openfile_c (
                           value filename_v
                           )
{
  CAMLparam1( filename_v );
  CAMLlocal1( db_v );
  char *addr ;
  const unsigned char* filename = String_val( filename_v );
  int fd ;
  int map_flags;
  int open_mode = 420; /* 0o644 */
  mapfile_t* db = NULL;
  uint64_t initial_size;

  fd = open( filename, O_LARGEFILE | O_RDWR, 420 /* 0o644 */ );
  if( fd < 0 ) { caml_failwith("Mapfile.opendir: open failed"); }
  initial_size = lseek( fd, 0, SEEK_END );
  if( initial_size < 8 ) { caml_failwith("Mapfile.opendir: bad size"); }

  addr = mmap(NULL,
              initial_size,
              PROT_READ | PROT_WRITE,
              MAP_SHARED,
              fd,
              0);
  if( addr == NULL ){ caml_failwith("Mapfile.openfile: mmap failed"); }

  db = (mapfile_t*)malloc( sizeof(mapfile_t) );
  db->fd = fd;
  db->addr = addr;
  db->map_size = initial_size;
  db_v = caml_alloc( 1 , Abstract_tag );
  Field (db_v, 0) = (value) db;
  CAMLreturn( db_v ) ;
}

value ocp_mapfile_get_string_c ( value db_v )
{
  mapfile_t * db = (mapfile_t *) Field( db_v, 0);
  value result = (value) ( db-> addr + 8 ) ;
  mlsize_t len = db->map_size - 24 ;
  mlsize_t wosize = (len + sizeof (value)) / sizeof (value);
  mlsize_t offset_index ;
  
  Hd_val( result ) = Make_header( wosize, String_tag, Caml_black ) ;
  Field (result, wosize - 1) = 0;
  offset_index = Bsize_wsize (wosize) - 1;
  Byte (result, offset_index) = offset_index - len;
  
  return result ;
}

value memmem_c( value haystack_v,
                value haystack_pos_v,
                value haystack_length_v,
                value needle_v,
                value needle_length_v 
                )
{
  const char* needle = String_val( needle_v );
  const char* haystack = String_val( haystack_v );
  long int haystack_pos = Long_val( haystack_pos_v );
  char* occ = memmem( haystack + haystack_pos,
                      Long_val( haystack_length_v ) - haystack_pos,
                      needle,
                      Long_val( needle_length_v ) );
  if( occ == NULL )
    return Val_long( -1 );
  else
    return Val_long( occ - haystack ) ;
}


