;;;; -*- mode: lisp; indent-tabs-mode: nil -*-

(in-package :crypto)

#+(and ecl ironclad-assembly)
(progn
  (declaim (inline x-chacha-core))
  (defun x-chacha-core (n-rounds buffer state)
    (ffi:clines "#define ROTL32(v, c) \\
  (((v) << (c)) | ((v) >> (32 - (c))))

#define CHACHA_QUARTER_ROUND(a, b, c, d) \\
  x[a] += x[b]; x[d] = ROTL32(x[d] ^ x[a], 16); \\
  x[c] += x[d]; x[b] = ROTL32(x[b] ^ x[c], 12); \\
  x[a] += x[b]; x[d] = ROTL32(x[d] ^ x[a], 8); \\
  x[c] += x[d]; x[b] = ROTL32(x[b] ^ x[c], 7);")
    (ffi:c-inline (n-rounds buffer state)
                  (:unsigned-int t t)
                  :void
                  "{
unsigned int n_rounds = #0;
uint8_t *buffer = (#1)->array.self.b8;
uint32_t *state = (#2)->array.self.b32;
unsigned int i;
uint32_t x[16];

for(i = 0; i < 16; i++)
  x[i] = state[i];

for(i = 0; i < n_rounds; i++)
{
  CHACHA_QUARTER_ROUND(0, 4, 8, 12);
  CHACHA_QUARTER_ROUND(1, 5, 9, 13);
  CHACHA_QUARTER_ROUND(2, 6, 10, 14);
  CHACHA_QUARTER_ROUND(3, 7, 11, 15);

  CHACHA_QUARTER_ROUND(0, 5, 10, 15);
  CHACHA_QUARTER_ROUND(1, 6, 11, 12);
  CHACHA_QUARTER_ROUND(2, 7, 8, 13);
  CHACHA_QUARTER_ROUND(3, 4, 9, 14);
}

for(i = 0; i < 16; i++)
  x[i] += state[i];

for(i = 0; i < 16; i++)
{
  buffer[4 * i] = x[i] & 0xff;
  buffer[4 * i + 1] = (x[i] >> 8) & 0xff;
  buffer[4 * i + 2] = (x[i] >> 16) & 0xff;
  buffer[4 * i + 3] = (x[i] >> 24) & 0xff;
}
}"))

  (declaim (inline x-salsa-core))
  (defun x-salsa-core (n-rounds buffer state)
    (ffi:clines "#define ROTL32(v, c) \\
  (((v) << (c)) | ((v) >> (32 - (c))))

#define SALSA_QUARTER_ROUND(a, b, c, d) \\
  x[a] ^= ROTL32(x[d] + x[c], 7); \\
  x[b] ^= ROTL32(x[a] + x[d], 9); \\
  x[c] ^= ROTL32(x[b] + x[a], 13); \\
  x[d] ^= ROTL32(x[c] + x[b], 18);")
    (ffi:c-inline (n-rounds buffer state)
                  (:unsigned-int t t)
                  :void
                  "{
unsigned int n_rounds = #0;
uint8_t *buffer = (#1)->array.self.b8;
uint32_t *state = (#2)->array.self.b32;
unsigned int i;
uint32_t x[16];

for(i = 0; i < 16; i++)
  x[i] = state[i];

for(i = 0; i < n_rounds; i++)
{
  SALSA_QUARTER_ROUND(4, 8, 12, 0);
  SALSA_QUARTER_ROUND(9, 13, 1, 5);
  SALSA_QUARTER_ROUND(14, 2, 6, 10);
  SALSA_QUARTER_ROUND(3, 7, 11, 15);

  SALSA_QUARTER_ROUND(1, 2, 3, 0);
  SALSA_QUARTER_ROUND(6, 7, 4, 5);
  SALSA_QUARTER_ROUND(11, 8, 9, 10);
  SALSA_QUARTER_ROUND(12, 13, 14, 15);
}

for(i = 0; i < 16; i++)
  x[i] += state[i];

for(i = 0; i < 16; i++)
{
  buffer[4 * i] = x[i] & 0xff;
  buffer[4 * i + 1] = (x[i] >> 8) & 0xff;
  buffer[4 * i + 2] = (x[i] >> 16) & 0xff;
  buffer[4 * i + 3] = (x[i] >> 24) & 0xff;
}
}"))

  (declaim (inline poly1305-process-block))
  (defun poly1305-process-block (h0 h1 h2 h3 h4 r0 r1 r2 r3 rr0 rr1 rr2 rr3 hibit data start)
    (ffi:c-inline (h0 h1 h2 h3 h4 r0 r1 r2 r3 rr0 rr1 rr2 rr3 hibit data start)
                  (:uint32-t :uint32-t :uint32-t :uint32-t :uint32-t
                   :uint32-t :uint32-t :uint32-t :uint32-t
                   :uint32-t :uint32-t :uint32-t :uint32-t
                   :uint32-t t :unsigned-int)
                  (values :uint32-t :uint32-t :uint32-t :uint32-t :uint32-t)
                  "{
uint32_t h0 = #0;
uint32_t h1 = #1;
uint32_t h2 = #2;
uint32_t h3 = #3;
uint32_t h4 = #4;
uint32_t r0 = #5;
uint32_t r1 = #6;
uint32_t r2 = #7;
uint32_t r3 = #8;
uint32_t rr0 = #9;
uint32_t rr1 = #a;
uint32_t rr2 = #b;
uint32_t rr3 = #c;
uint32_t hibit = #d;
unsigned int start = #f;
uint8_t *data = (#e)->array.self.b8 + start;
uint32_t c0 = data[0] + (data[1] << 8) + (data[2] << 16) + (data[3] << 24);
uint32_t c1 = data[4] + (data[5] << 8) + (data[6] << 16) + (data[7] << 24);
uint32_t c2 = data[8] + (data[9] << 8) + (data[10] << 16) + (data[11] << 24);
uint32_t c3 = data[12] + (data[13] << 8) + (data[14] << 16) + (data[15] << 24);
uint64_t s0 = h0 + (uint64_t) c0;
uint64_t s1 = h1 + (uint64_t) c1;
uint64_t s2 = h2 + (uint64_t) c2;
uint64_t s3 = h3 + (uint64_t) c3;
uint32_t s4 = h4 + hibit;
uint64_t x0 = (s0 * r0) + (s1 * rr3) + (s2 * rr2) + (s3 * rr1) + (s4 * rr0);
uint64_t x1 = (s0 * r1) + (s1 * r0) + (s2 * rr3) + (s3 * rr2) + (s4 * rr1);
uint64_t x2 = (s0 * r2) + (s1 * r1) + (s2 * r0) + (s3 * rr3) + (s4 * rr2);
uint64_t x3 = (s0 * r3) + (s1 * r2) + (s2 * r1) + (s3 * r0) + (s4 * rr3);
uint32_t x4 = s4 * (r0 & 3);
uint32_t u5 = x4 + (x3 >> 32);
uint64_t u0 = ((u5 >> 2) * 5) + (x0 & 0xffffffff);
uint64_t u1 = (u0 >> 32) + (x1 & 0xffffffff) + (x0 >> 32);
uint64_t u2 = (u1 >> 32) + (x2 & 0xffffffff) + (x1 >> 32);
uint64_t u3 = (u2 >> 32) + (x3 & 0xffffffff) + (x2 >> 32);
uint64_t u4 = (u3 >> 32) + (u5 & 3);

@(return 0) = u0 & 0xffffffff;
@(return 1) = u1 & 0xffffffff;
@(return 2) = u2 & 0xffffffff;
@(return 3) = u3 & 0xffffffff;
@(return 4) = u4 & 0xffffffff;
}")))
