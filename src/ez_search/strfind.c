#define _GNU_SOURCE
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <stdio.h>
#include <stdint.h>

#include <x86intrin.h>
#pragma GCC target("avx2")

#define CAML_NAME_SPACE
#include "caml/mlvalues.h"
#include "caml/memory.h"
#include "caml/bigarray.h"

CAMLprim value ocaml_findfirst(value vneedle, value vhaystack,
                               value vstartpos, value vlength)
{
  CAMLparam4 (vneedle, vhaystack, vstartpos, vlength);
  const void* needle = String_val(vneedle);
  const void* haystack = String_val(vhaystack);
  size_t startpos = Long_val(vstartpos);
  size_t length = Long_val(vlength);

  void* ptr = memmem(haystack + startpos, length, needle, caml_string_length(vneedle));
  if(ptr) {
    CAMLreturn(Val_long(ptr - haystack));
  } else {
    CAMLreturn(Val_long(-1));
  }
}

static
__m256i hsum_epu8_epu64(__m256i v) {
    return _mm256_sad_epu8(v, _mm256_setzero_si256());
}

static
uint64_t hsum_epu64_scalar(__m256i v) {
    __m128i lo = _mm256_castsi256_si128(v);
    __m128i hi = _mm256_extracti128_si256(v, 1);
    __m128i sum2x64 = _mm_add_epi64(lo, hi);   // narrow to 128

    hi = _mm_unpackhi_epi64(sum2x64, sum2x64);
    __m128i sum = _mm_add_epi64(hi, sum2x64);  // narrow to 64
    return _mm_cvtsi128_si64(sum);
}

static __m256i count_one_pack(const char *needle, int needle_len,
                              const void *haystack, uint64_t pos)
{
  __m256i mask = _mm256_set1_epi8 (0xFF);
  for(int i = 0; i < needle_len; i++) {
    __m256i tester = _mm256_set1_epi8 (needle[i]);
    __m256i v = _mm256_lddqu_si256((__m256i const *)(haystack + pos + i));
    __m256i eq_char = _mm256_cmpeq_epi8 (v, tester);
    mask = _mm256_and_si256 (mask, eq_char);
    if(_mm256_testz_si256(mask, mask))
      return _mm256_setzero_si256();
  }
  __m256i ones = _mm256_sub_epi8(_mm256_setzero_si256(),mask);
  __m256i sum_64 = hsum_epu8_epu64(ones);
  return sum_64;
}

static uint64_t count_matches(const void *needle, int needle_len,
                              const void *haystack, uint64_t haystack_len)
{
  __m256i count_64 = _mm256_setzero_si256();
  uint64_t packs = (haystack_len - 32 - needle_len) / 32;
  for(uint64_t i = 0; i < packs; i++) {
    count_64 = _mm256_add_epi64(count_64, count_one_pack(needle, needle_len, haystack, i * 32));
  }
  uint64_t count = hsum_epu64_scalar(count_64);
  uint64_t pos = (packs + 1) * 32;

  while(1) {
    void* ptr = memmem(haystack + pos, haystack_len - pos, needle, needle_len);
    if(ptr == NULL) return count;
    pos = (ptr - haystack) + 1;
    count++;
  }
}

CAMLprim value ocaml_countmatch(value vneedle, value vhaystack,
                                value vstartpos, value vlength)
{
  CAMLparam4 (vneedle, vhaystack, vstartpos, vlength);
  const void* needle = String_val(vneedle);
  const void* haystack = String_val(vhaystack);
  uint64_t startpos = Long_val(vstartpos);
  uint64_t length = Long_val(vlength);

  uint64_t count =
    count_matches(needle, caml_string_length(vneedle),
                  haystack + startpos, length);

  CAMLreturn(Val_long(count));
}
