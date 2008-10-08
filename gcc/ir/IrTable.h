/* translates tree gimple into Sun IR.
   Copyright (C) 2007, 2008 by Sun Microsystems, Inc. All rights reserved.  
   File is licensed under the GNU Public License.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _IRTABLE_H_
#define _IRTABLE_H_

#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif
                                                                                
#ifndef BOOLEAN
#define BOOLEAN int
#endif

struct Chunk;
typedef int Index;

struct IrPtrConv;

typedef int (*LaunderFuncType)(char *ptr, const struct IrPtrConv *conv);
typedef char* (*NextFuncType)(const char *);
typedef void (*PrintFuncType)(const char *, FILE *);

typedef struct IrTable {
	struct Chunk *freeChunk;	/* a chunk with a free entry, if any */
	struct Chunk *chunks;	/* list of all chunks in the table */
	struct Chunk *chunkTail;/* tail of the list of chunks */
	unsigned numEntries;	/* number entries (not counting free ones) */
	unsigned entrySize;	/* size in bytes of each entry in the table */
	unsigned dfltEntriesPerChunk;	/* default # of entries in each chunk */
	BOOLEAN forbidDeletion;	/* true means cannot free entries */
	BOOLEAN zeroNew;	/* True if allocated bytes should be zeroed */
} IrTable;


typedef struct {	/* info about the entry just returned */
	struct Chunk *chunk;	/* chunk containing the entry */
	Index entry;	/* 0 first entry in chunk, 1 for second, etc. */
} IrTableContext;

typedef struct {
	const IrTable *table;	/* table being traversed */
	struct Chunk *chunk;	/* current chunk in the table */
	Index entry;		/* current entry in the chunk */
	BOOLEAN reset;		/* true if not yet retrieved first entry */
} IrTableIterator;

int irTableInit(IrTable *table, unsigned entrySize, unsigned entriesPerChunk);

int irTableInitNoDelete(IrTable *table, unsigned entrySize, 
		unsigned entriesPerChunk);

/* Returns the number of entries in the table that are in use--ie, not free. */
unsigned irTableEntries(const IrTable *table);

/* Returns the size of each entry. */
/* LINTED static unused */
inline static int irTableEntrySize(const IrTable *table)
{
	return table->entrySize;
}

/* True if entries cannot be deleted from the table. */
/* LINTED static unused */
inline static BOOLEAN irTableForbidsDeletion(const IrTable *table)
{
	return table->forbidDeletion;
}


/* Allocate an entry from the table. */
char *irTableAlloc(IrTable *table);

char *irTableAllocArray(IrTable *table, unsigned numElem);

/* Deallocate an entry that was previously allocated from this table.
   If the entry pointer is invalid or was previously freed, FALSE is returned.
*/
int irTableDealloc(IrTable *table, char *entryPtr);

/*  Free all memory associated with this table. */
void irTableDestroy(IrTable *table);

IrTableIterator irTableIterator(const IrTable *table);
void *irTableIterNext(IrTableIterator *iterator);
void irTableIterReset(IrTableIterator *iterator);
BOOLEAN irTableIterMore(IrTableIterator *iterator);

char *irTableFirstChunk(const IrTable *table, int *bytes, IrTableContext *ctxt);
char *irTableNextChunk(const IrTable *table, int *bytes, IrTableContext *ctxt);

int irTableRead(IrTable *table, struct IrPtrConv *conv, FILE *inFile, 
		int numEntries, int fileEntrySize, long offsetToTable);
int irTablePostRead(IrTable *table, const struct IrPtrConv *conv, 
		LaunderFuncType launder);

int irTablePreWrite(IrTable *table, struct IrPtrConv *conv, 
		LaunderFuncType launder, int fileEntrySize, long *offset);
int irTableWrite(IrTable *table, const struct IrPtrConv *conv, FILE *outFile,
		LaunderFuncType launder, int fileEntrySize);

/* If the zeroOnAlloc argument is true, then all data returned from 
   irTableAlloc() and irTableAllocArray() will be zeroed before being
   presented to the user.  By default, data is zeroed.
   (Not zeroing data saves time.)
*/
void irTableZeroOnAlloc(IrTable *table, BOOLEAN zeroOnAlloc);

void irTablePrint(const IrTable *table, PrintFuncType print, FILE *outfile);

/* Move the contents from one table to another.  The dest table must already
   be empty.  The source table will be empty (in default initialized state)
   after the move.  Note, this does not make a copy.
   This moves attributes as well as contents, e.g., if the source table is
   read-only, the dest will be as well.
*/
void irTableMove(IrTable *dest, IrTable *source);

#ifdef __cplusplus
}
#endif

#endif
