/* sha.c  */

/* NIST Secure Hash Algorithm */
/* heavily modified by Uwe Hollerbach uh@alumni.caltech edu */
/* from Peter C. Gutmann's implementation as found in */
/* Applied Cryptography by Bruce Schneier */
/* This code is in the public domain */

/* Modified to incorporate into CTC - Mr. Tines, June 1996 */
/* Primarily to flag the endianness at run rather than compile time */
/* Note that if we activate USE_MODIFIED_SHA, then we have SHA-1; the */
/* code then passes these test vectors:-

SHA-1 Test Vectors (from FIPS PUB 180-1)
"abc"
  A9993E36 4706816A BA3E2571 7850C26C 9CD0D89D
"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
  84983E44 1C3BD26E BAAE4AA1 F95129E5 E54670F1

Presumably, though I've not tried, it will also pass

A million repetitions of "a"
  34AA973C D4C4DAA4 F61EEB2B DBAD2731 6534016F

  This and tweaks to the standalone test-harness bit ; also
  adding SHA1Init() code so we can use common code for the two
  flavours - Mr. Tines 1-1-97

  And byte-reverse the output properly rather than only in the stand-alone
  test harness, so that the hash output is in MSB first form - Mr. Tines 5-1-97

  SHA with the same test vectors

  0164b8a9 14cd2a5e 74c4f7ff 082c4d97 f1edf880
  d2516ee1 acfa5baf 33dfc1c4 71e43844 9ef134c8
  3232affa 48628a26 653b5aaa 44541fd9 0d690603

  Localise hash-vector deallocation with
  the allocation.  Mr. Tines 16-Feb-97

*/


#include <stdlib.h>
#include <string.h>
#include "sha.h"
/*#include "utils.h"*/

/* UNRAVEL should be fastest & biggest */
/* UNROLL_LOOPS should be just as big, but slightly slower */
/* both undefined should be smallest and slowest */

#define UNRAVEL
/* #define UNROLL_LOOPS */

/* NIST's proposed modification to SHA of 7/11/94 may be */
/* activated by defining USE_MODIFIED_SHA; leave it off for now */
#undef USE_MODIFIED_SHA

/* SHA f()-functions */

#define f1(x,y,z)	((x & y) | (~x & z))
#define f2(x,y,z)	(x ^ y ^ z)
#define f3(x,y,z)	((x & y) | (x & z) | (y & z))
#define f4(x,y,z)	(x ^ y ^ z)

/* SHA constants */

#define CONST1		0x5a827999L
#define CONST2		0x6ed9eba1L
#define CONST3		0x8f1bbcdcL
#define CONST4		0xca62c1d6L

/* 32-bit rotate */

#define ROT32(x,n)	((x << n) | (x >> (32 - n)))

/* the generic case, for when the overall rotation is not unraveled */

#define FG(n)	\
	 T = ROT32(A,5) + f##n(B,C,D) + E + *WP++ + CONST##n;	\
	 E = D; D = C; C = ROT32(B,30); B = A; A = T

/* specific cases, for when the overall rotation is unraveled */

#define FA(n)	\
	 T = ROT32(A,5) + f##n(B,C,D) + E + *WP++ + CONST##n; B = ROT32(B,30)

#define FB(n)	\
	 E = ROT32(T,5) + f##n(A,B,C) + D + *WP++ + CONST##n; A = ROT32(A,30)

#define FC(n)	\
	 D = ROT32(E,5) + f##n(T,A,B) + C + *WP++ + CONST##n; T = ROT32(T,30)

#define FD(n)	\
	 C = ROT32(D,5) + f##n(E,T,A) + B + *WP++ + CONST##n; E = ROT32(E,30)

#define FE(n)	\
	 B = ROT32(C,5) + f##n(D,E,T) + A + *WP++ + CONST##n; D = ROT32(D,30)

#define FT(n)	\
	 A = ROT32(B,5) + f##n(C,D,E) + T + *WP++ + CONST##n; C = ROT32(C,30)

/* do SHA transformation */

#ifdef __BORLANDC__
#pragma warn -aus
#endif
static void sha_transform(SHA_INFO *sha_info)
{
	 int i;
	 uint32_t T, A, B, C, D, E, W[80], *WP;

	 for (i = 0; i < 16; ++i) {
	W[i] = sha_info->data[i];
	 }
	 for (i = 16; i < 80; ++i) {
	W[i] = W[i-3] ^ W[i-8] ^ W[i-14] ^ W[i-16];
/*#ifdef USE_MODIFIED_SHA*/
	if(sha_info->SHA1) W[i] = ROT32(W[i], 1);
/*#endif*/ /* USE_MODIFIED_SHA */
	 }
	 A = sha_info->digest[0];
	 B = sha_info->digest[1];
	 C = sha_info->digest[2];
	 D = sha_info->digest[3];
	 E = sha_info->digest[4];
	 WP = W;
#ifdef UNRAVEL
	 FA(1); FB(1); FC(1); FD(1); FE(1); FT(1); FA(1); FB(1); FC(1); FD(1);
	 FE(1); FT(1); FA(1); FB(1); FC(1); FD(1); FE(1); FT(1); FA(1); FB(1);
	 FC(2); FD(2); FE(2); FT(2); FA(2); FB(2); FC(2); FD(2); FE(2); FT(2);
	 FA(2); FB(2); FC(2); FD(2); FE(2); FT(2); FA(2); FB(2); FC(2); FD(2);
	 FE(3); FT(3); FA(3); FB(3); FC(3); FD(3); FE(3); FT(3); FA(3); FB(3);
	 FC(3); FD(3); FE(3); FT(3); FA(3); FB(3); FC(3); FD(3); FE(3); FT(3);
	 FA(4); FB(4); FC(4); FD(4); FE(4); FT(4); FA(4); FB(4); FC(4); FD(4);
	 FE(4); FT(4); FA(4); FB(4); FC(4); FD(4); FE(4); FT(4); FA(4); FB(4);
	 sha_info->digest[0] += E;
	 sha_info->digest[1] += T;
	 sha_info->digest[2] += A;
	 sha_info->digest[3] += B;
	 sha_info->digest[4] += C;
#else /* !UNRAVEL */
#ifdef UNROLL_LOOPS
	 FG(1); FG(1); FG(1); FG(1); FG(1); FG(1); FG(1); FG(1); FG(1); FG(1);
	 FG(1); FG(1); FG(1); FG(1); FG(1); FG(1); FG(1); FG(1); FG(1); FG(1);
	 FG(2); FG(2); FG(2); FG(2); FG(2); FG(2); FG(2); FG(2); FG(2); FG(2);
	 FG(2); FG(2); FG(2); FG(2); FG(2); FG(2); FG(2); FG(2); FG(2); FG(2);
	 FG(3); FG(3); FG(3); FG(3); FG(3); FG(3); FG(3); FG(3); FG(3); FG(3);
	 FG(3); FG(3); FG(3); FG(3); FG(3); FG(3); FG(3); FG(3); FG(3); FG(3);
	 FG(4); FG(4); FG(4); FG(4); FG(4); FG(4); FG(4); FG(4); FG(4); FG(4);
	 FG(4); FG(4); FG(4); FG(4); FG(4); FG(4); FG(4); FG(4); FG(4); FG(4);
#else /* !UNROLL_LOOPS */
	 for (i =  0; i < 20; ++i) { FG(1); }
	 for (i = 20; i < 40; ++i) { FG(2); }
	 for (i = 40; i < 60; ++i) { FG(3); }
	 for (i = 60; i < 80; ++i) { FG(4); }
#endif /* !UNROLL_LOOPS */
	 sha_info->digest[0] += A;
	 sha_info->digest[1] += B;
	 sha_info->digest[2] += C;
	 sha_info->digest[3] += D;
	 sha_info->digest[4] += E;
#endif /* !UNRAVEL */
}
#ifdef __BORLANDC__
#pragma warn .aus
#endif

/* change endianness of data - equivalent to the usual dodge (elsewhere in this
*  program) of converting in-line via bit-shifting operations --Tines*/

static void byte_reverse(uint32_t *buffer, int count)
{
    int i;
	 byte ct[4], *cp;

	 count /= sizeof(uint32_t);
	 cp = (byte *) buffer;
    for (i = 0; i < count; ++i) {
	ct[0] = cp[0];
	ct[1] = cp[1];
	ct[2] = cp[2];
	ct[3] = cp[3];
	cp[0] = ct[3];
	cp[1] = ct[2];
	cp[2] = ct[1];
	cp[3] = ct[0];
	cp += sizeof(uint32_t);
    }
}

/* initialize the SHA digest */

void SHAInit(SHA_INFO **sha_info, size_t *length)
{

	*length = sizeof(SHA_INFO);
	*sha_info = malloc(*length);
	if(!(*sha_info)) return;

	 (*sha_info)->digest[0] = 0x67452301L;
	 (*sha_info)->digest[1] = 0xefcdab89L;
	 (*sha_info)->digest[2] = 0x98badcfeL;
	 (*sha_info)->digest[3] = 0x10325476L;
	 (*sha_info)->digest[4] = 0xc3d2e1f0L;
	 (*sha_info)->count_lo = 0L;
	 (*sha_info)->count_hi = 0L;
	 (*sha_info)->local = 0;
	 (*sha_info)->SHA1 = FALSE;
}

void SHA1Init(SHA_INFO **sha_info, size_t *length)
{
	SHAInit(sha_info, length);
	(*sha_info)->SHA1 = TRUE;
}


/* update the SHA digest */

void SHAUpdate(SHA_INFO *sha_info, byte *buffer, uint32_t count)
{
	 uint32_t i;

	 if ((sha_info->count_lo + ((uint32_t) count << 3)) < sha_info->count_lo) {
	++sha_info->count_hi;
    }
	 sha_info->count_lo += (uint32_t) count << 3;
	 sha_info->count_hi += (uint32_t) count >> 29;
    if (sha_info->local) {
	i = SHA_BLOCKSIZE - sha_info->local;
	if (i > count) {
		 i = count;
	}
	if(i > 0) memcpy(((byte *) sha_info->data) + sha_info->local, buffer, (size_t)i);
	count -= i;
	buffer += (size_t)i;
	sha_info->local += (int)i;
	if (sha_info->local == SHA_BLOCKSIZE) {
		 byte_reverse(sha_info->data, SHA_BLOCKSIZE);
	    sha_transform(sha_info);
	} else {
	    return;
	}
    }
    while (count >= SHA_BLOCKSIZE) {
	memcpy(sha_info->data, buffer, SHA_BLOCKSIZE);
	buffer += SHA_BLOCKSIZE;
	count -= SHA_BLOCKSIZE;
	byte_reverse(sha_info->data, SHA_BLOCKSIZE);
	sha_transform(sha_info);
    }
	 if(count > 0) memcpy(sha_info->data, buffer, (size_t) count);
	 sha_info->local = (int) count;
}

/* finish computing the SHA digest */

void SHAFinal(SHA_INFO **psha_info, byte digest[SHAHASHSIZE], size_t length)
{
    int count;
	 uint32_t lo_bit_count, hi_bit_count;
    SHA_INFO *sha_info = *psha_info;

    lo_bit_count = sha_info->count_lo;
    hi_bit_count = sha_info->count_hi;
    count = (int) ((lo_bit_count >> 3) & 0x3f);
	 ((byte *) sha_info->data)[count++] = 0x80;
    if (count > SHA_BLOCKSIZE - 8) {
	memset(((byte *) sha_info->data) + count, 0, SHA_BLOCKSIZE - count);
	byte_reverse(sha_info->data, SHA_BLOCKSIZE);
	sha_transform(sha_info);
	memset((byte *) sha_info->data, 0, SHA_BLOCKSIZE - 8);
    } else {
	memset(((byte *) sha_info->data) + count, 0,
	    SHA_BLOCKSIZE - 8 - count);
    }
    byte_reverse(sha_info->data, SHA_BLOCKSIZE);
    sha_info->data[14] = hi_bit_count;
    sha_info->data[15] = lo_bit_count;
	 sha_transform(sha_info);

	 byte_reverse(sha_info->digest, SHAHASHSIZE);
	 memcpy(digest, sha_info->digest, SHAHASHSIZE);

    if(length > 0) free(*psha_info);
}

#ifdef STANDALONE_SHA
/* compute the SHA digest of a FILE stream */

#define BLOCK_SIZE	8192

void sha_stream(byte digest[SHAHASHSIZE], FILE *fin)
{
	 int i;
	 byte data[BLOCK_SIZE];
	 size_t length;
	 SHA_INFO *sha_info;

	 SHA1Init(&sha_info, &length);
	 while ((i = fread(data, 1, BLOCK_SIZE, fin)) > 0) {
		SHAUpdate(sha_info, data, i);
	 }
	 SHAFinal(&sha_info, digest, length);
}

/* print a SHA digest */

void sha_print(byte digest[SHAHASHSIZE])
{
	int i,j;
	 for (i = 0; i < 5; i++) {
		  for (j = 0; j < 4; j++) {
				printf("%02X", digest[i*4+j]);
		  }
		  putchar(' ');
	 }
	 putchar('\n');
}

#ifdef __BORLANDC__
#pragma warn -pia
#endif
int main(int argc, char** argv)
{
	unsigned char digest[SHAHASHSIZE];
	FILE* file;

	 if (argc < 2)
	 {
		size_t length;
		SHA_INFO *sha_info;
		byte a[8192];
		long count;
		int i;
		for(i=0;i<8192;i++) a[i] = 'a';

		puts("SHA(-1) test harness");
		puts("Produces the SHA-1 hash of a file, or SHA1 and SHA test vectors.");

		SHA1Init(&sha_info, &length);
		SHAUpdate(sha_info, (byte*)"abc", 3);
		SHAFinal(&sha_info, digest, length);
		printf("A9993E36 4706816A BA3E2571 7850C26C 9CD0D89D expected\n");
		sha_print(digest);


		SHA1Init(&sha_info, &length);
		SHAUpdate(sha_info, (byte*)
		"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",56);
		SHAFinal(&sha_info, digest, length);
		printf("84983E44 1C3BD26E BAAE4AA1 F95129E5 E54670F1 expected\n");
		sha_print(digest);

		SHA1Init(&sha_info, &length);
		for(count=1000000L; count > 0; count -=8192)
		{
			int l = (count > 8192) ? 8192 : (int) count;
			SHAUpdate(sha_info, a, l);
			printf(".");
		}
		printf("\n");
		SHAFinal(&sha_info, digest, length);
		printf("34AA973C D4C4DAA4 F61EEB2B DBAD2731 6534016F expected\n");
		sha_print(digest);

		SHAInit(&sha_info, &length);
		SHAUpdate(sha_info, (byte*)"abc", 3);
		SHAFinal(&sha_info, digest, length);
		printf("0164B8A9 14CD2A5E 74C4F7FF 082C4D97 F1EDF880 expected\n");
		sha_print(digest);

		SHAInit(&sha_info, &length);
		SHAUpdate(sha_info, (byte*)
		"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",56);
		SHAFinal(&sha_info, digest, length);
		printf("D2516EE1 ACFA5BAF 33DFC1C4 71E43844 9EF134C8 expected\n");
		sha_print(digest);

		SHAInit(&sha_info, &length);
		for(count=1000000L; count > 0; count -=8192)
		{
			int l = (count > 8192) ? 8192 : (int)count;
			SHAUpdate(sha_info, a, l);
			printf(".");
		}
		printf("\n");
		SHAFinal(&sha_info, digest, length);
		printf("3232AFFA 48628A26 653B5AAA 44541FD9 0D690603 expected\n");
		sha_print(digest);

		return 0;
	 }
	 if (argc < 2) {
        file = stdin;
    }
    else {
		  if (!(file = fopen(argv[1], "rb"))) {
            fputs("Unable to open file.", stderr);
            exit(-1);
        }
	 }

	 sha_stream(digest, file);
	 fclose(file);
	 sha_print(digest);
	 exit(0);
	 return 0;
	 }

#endif

/* end of file sha.c */
