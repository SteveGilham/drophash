/*
 ***********************************************************************
 ** md5.h -- header file for implementation of MD5                    **
 ** RSA Data Security, Inc. MD5 Message-Digest Algorithm              **
 ** Created: 2/17/90 RLR                                              **
 ** Revised: 12/27/90 SRD,AJ,BSK,JT Reference C version               **
 ** Revised (for MD5): RLR 4/27/91                                    **
 **   -- G modified to have y&~z instead of y&z                       **
 **   -- FF, GG, HH modified to add in last register done             **
 **   -- Access pattern: round 2 works mod 5, round 3 works mod 3     **
 **   -- distinct additive constant for each step                     **
 **   -- round 4 added, working mod 7                                 **
 ***********************************************************************
 */

/*
 * Edited 7 May 93 by CP to change the interface to match that
 * of the MD5 routines in RSAREF.  Due to this alteration, this
 * code is "derived from the RSA Data Security, Inc. MD5 Message-
 * Digest Algorithm".  (See below.)  Also added argument names
 * to the prototypes.
 */

/*
 ***********************************************************************
 ** Copyright (C) 1990, RSA Data Security, Inc. All rights reserved.  **
 **                                                                   **
 ** License to copy and use this software is granted provided that    **
 ** it is identified as the "RSA Data Security, Inc. MD5 Message-     **
 ** Digest Algorithm" in all material mentioning or referencing this  **
 ** software or this function.                                        **
 **                                                                   **
 ** License is also granted to make and use derivative works          **
 ** provided that such works are identified as "derived from the RSA  **
 ** Data Security, Inc. MD5 Message-Digest Algorithm" in all          **
 ** material mentioning or referencing the derived work.              **
 **                                                                   **
 ** RSA Data Security, Inc. makes no representations concerning       **
 ** either the merchantability of this software or the suitability    **
 ** of this software for any particular purpose.  It is provided "as  **
 ** is" without express or implied warranty of any kind.              **
 **                                                                   **
 ** These notices must be retained in any copies of any part of this  **
 ** documentation and/or software.                                    **
 ***********************************************************************
 */

 /*
 ** Interface modified for CTC - Mr. Tines Jun. 96
 ** unit32_t standard 32-bit unsigned type - Mr. Tines 8-Jan-97
 ** More intrface tweaks - Mr. Tines 16-feb-97
 ** Made standalone for drophash - Mr. Tines 26-May-00
 */

#ifndef _md5
#define _md5

#include <stddef.h>
#define MD5HASHSIZE 16
typedef unsigned int    uint32_t;
typedef unsigned char   byte;
/* Data structure for MD5 (Message-Digest) computation */
typedef struct {
  uint32_t buf[4];                                    /* scratch buffer */
  uint32_t i[2];                   /* number of _bits_ handled mod 2^64 */
  unsigned char in[64];                              /* input buffer */
} MD5_CTX;

#ifdef __cplusplus
extern "C" {
#endif

void MD5Init(MD5_CTX **mdContext, size_t *length);
void MD5Update(MD5_CTX *mdContext, byte *buf, uint32_t len);
void MD5Final(MD5_CTX **mdContext, byte digest[MD5HASHSIZE], size_t length);
void Transform(uint32_t *buf, uint32_t *in);
#ifdef __cplusplus
}
#endif

#endif

/* end of file md5.h */
