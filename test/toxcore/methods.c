#include "methods.h"

#include "util.h"

#include <crypto_core.h>
#include <net_crypto.h>

char const *const pending = "Pending";
char const *const unimplemented = "Unimplemented";


METHOD (array, Box, encrypt)
{
  CHECK (args.size == 3);
  CHECK (args.ptr[0].type == MSGPACK_OBJECT_BIN);
  CHECK (args.ptr[0].via.bin.size == crypto_box_BEFORENMBYTES);

  CHECK (args.ptr[1].type == MSGPACK_OBJECT_BIN);
  CHECK (args.ptr[1].via.bin.size == crypto_box_NONCEBYTES);

  uint8_t cipher_text[args.ptr[2].via.bin.size + crypto_box_MACBYTES];

  int len = encrypt_data_symmetric ((uint8_t *) args.ptr[0].via.bin.ptr,
                                    (uint8_t *) args.ptr[1].via.bin.ptr,
                                    (uint8_t *) args.ptr[2].via.bin.ptr,
                                    args.ptr[2].via.bin.size, cipher_text);
  if (len < 0)
      return "an error occured while encryption process";

  SUCCESS {
    msgpack_pack_bin (res, sizeof cipher_text);
    msgpack_pack_bin_body (res, cipher_text, sizeof cipher_text);
  }
  return 0;
}


METHOD (array, Box, decrypt)
{
  CHECK (args.size == 3);
  CHECK (args.ptr[0].type == MSGPACK_OBJECT_BIN);
  CHECK (args.ptr[0].via.bin.size == crypto_box_BEFORENMBYTES);

  CHECK (args.ptr[1].type == MSGPACK_OBJECT_BIN);
  CHECK (args.ptr[1].via.bin.size == crypto_box_NONCEBYTES);

  uint8_t plain_text[args.ptr[2].via.bin.size - crypto_box_MACBYTES];

  int len = decrypt_data_symmetric ((uint8_t *) args.ptr[0].via.bin.ptr,
                                    (uint8_t *) args.ptr[1].via.bin.ptr,
                                    (uint8_t *) args.ptr[2].via.bin.ptr,
                                    args.ptr[2].via.bin.size, plain_text);
  if (len < 0)
      return "an error occured while decryption process";

  SUCCESS {
    msgpack_pack_bin(res, sizeof plain_text);
    msgpack_pack_bin_body(res, plain_text, sizeof plain_text);
  }
  return 0;
}


METHOD (array, CombinedKey, precompute)
{

    CHECK (args.size == 2);
    CHECK (args.ptr[1].type == MSGPACK_OBJECT_BIN);
    CHECK (args.ptr[1].via.bin.size == crypto_box_SECRETKEYBYTES);
    CHECK (args.ptr[0].type == MSGPACK_OBJECT_BIN);
    CHECK (args.ptr[0].via.bin.size == crypto_box_PUBLICKEYBYTES);

    uint8_t secret_key[crypto_box_SECRETKEYBYTES];
    uint8_t public_key[crypto_box_PUBLICKEYBYTES];
    uint8_t combined_key[crypto_box_BEFORENMBYTES];

    memcpy (secret_key, args.ptr[0].via.bin.ptr, crypto_box_SECRETKEYBYTES);
    memcpy (public_key, args.ptr[1].via.bin.ptr, crypto_box_PUBLICKEYBYTES);

    encrypt_precompute (public_key, secret_key, combined_key);

    SUCCESS {
        msgpack_pack_bin (res, sizeof combined_key);
        msgpack_pack_bin_body (res, combined_key, sizeof combined_key);
    }
    return 0;
}


METHOD (array, KeyPair, newKeyPair)
{
  Net_Crypto c;
  new_keys (&c);

  SUCCESS {
    //init array
    msgpack_pack_array (res, 2);

    msgpack_pack_bin (res, crypto_box_SECRETKEYBYTES);
    msgpack_pack_bin_body (res, c.self_secret_key, crypto_box_SECRETKEYBYTES);

    msgpack_pack_bin (res, crypto_box_PUBLICKEYBYTES);
    msgpack_pack_bin_body (res, c.self_public_key, crypto_box_PUBLICKEYBYTES);
  }
  return 0;
}


METHOD (array, KeyPair, fromSecretKey)
{
  CHECK (args.size == 1);
  CHECK (args.ptr[0].type == MSGPACK_OBJECT_BIN);
  CHECK (args.ptr[0].via.bin.size == crypto_box_SECRETKEYBYTES);

  Net_Crypto c;
  uint8_t secret_key[crypto_box_SECRETKEYBYTES];
  memcpy (secret_key, args.ptr[0].via.bin.ptr, crypto_box_SECRETKEYBYTES);
  load_secret_key (&c, secret_key);

  SUCCESS {
    msgpack_pack_array (res, 2);

    msgpack_pack_bin (res, crypto_box_PUBLICKEYBYTES);
    msgpack_pack_bin_body (res, c.self_secret_key, crypto_box_PUBLICKEYBYTES);
    msgpack_pack_bin (res, crypto_box_SECRETKEYBYTES);
    msgpack_pack_bin_body (res, c.self_public_key, crypto_box_SECRETKEYBYTES);
  }
  return 0;
}


METHOD (array, Nonce, newNonce)
{
  uint8_t nonce[24] = { 0 };
  new_nonce (nonce);

  SUCCESS {
    msgpack_pack_bin (res, sizeof nonce);
    msgpack_pack_bin_body (res, nonce, sizeof nonce);
  }

  return 0;
}


METHOD (array, Nonce, increment)
{
  CHECK (args.size == 1);
  CHECK (args.ptr[0].type == MSGPACK_OBJECT_BIN);
  CHECK (args.ptr[0].via.bin.size == 24);

  uint8_t nonce[24];
  memcpy (nonce, args.ptr[0].via.bin.ptr, 24);
  increment_nonce (nonce);

  SUCCESS {
    msgpack_pack_bin (res, sizeof nonce);
    msgpack_pack_bin_body (res, nonce, sizeof nonce);
  }

  return 0;
}


char const *
call_method (msgpack_object_str name, msgpack_object_array args, msgpack_packer *res)
{
#define DISPATCH(SERVICE, NAME) \
  if (name.size == sizeof #SERVICE"."#NAME - 1 && \
      memcmp (name.ptr, #SERVICE"."#NAME, name.size) == 0) \
    return SERVICE##_##NAME (args, res)
  DISPATCH (Binary, decode);
  DISPATCH (Binary, encode);
  DISPATCH (Box, decrypt);
  DISPATCH (Box, encrypt);
  DISPATCH (CombinedKey, precompute);
  DISPATCH (KeyPair, fromSecretKey);
  DISPATCH (KeyPair, newKeyPair);
  DISPATCH (Nonce, increment);
  DISPATCH (Nonce, newNonce);
#undef DISPATCH

  // Default action: "Unimplemented" exception. New tests should be added here
  // returning "Pending" until they are properly implemented.
  return unimplemented;
}

