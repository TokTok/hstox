#include "errors.h"
#include "driver.h"

#include <stdio.h>
#include <stdlib.h>


// The msgpack-rpc test port expected by the test runner.
#define PORT 1234


static char const *
error_desc (int code)
{
  switch (code)
    {
    case E_OK:     return "Success";
    case E_NOMEM:  return "Error: Out of memory";
    case E_SOCKET: return "Error: socket creation failed";
    case E_BIND:   return "Error: bind failed";
    case E_LISTEN: return "Error: listen failed";
    case E_ACCEPT: return "Error: accept failed";
    case E_PARSE:  return "Error: unable to parse msgpack input";
    case E_OPEN:   return "Error: open failed";
    case E_READ:   return "Error: read failed";
    case E_WRITE:  return "Error: write failed";
    case E_SODIUM: return "Error: libsodium initialisation failed";
    }

  return "Unknown error code";
}


int
main (void)
{
  struct settings cfg = { true, true };
  uint32_t result = network_main (cfg, PORT);
  int line  =  result >> 16;
  int error = (result >>  8) & 0xff;
  int code  =  result        & 0xff;

  if (code != E_OK)
    {
      printf ("%s, errno=%d, line=%d\n", error_desc (code), error, line);
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
