#pragma once

#include <stdbool.h>
#include <stdint.h>

struct settings
{
  bool debug;
  bool collect_samples;
};

int communicate (struct settings cfg, int read_fd, int write_fd);
uint32_t test_main (bool debug, bool collect_samples, uint16_t port);
