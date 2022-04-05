#pragma once

void run_command_impl(int ignore, ...);

#define RUN_COMMAND(...) run_command_impl(0, __VA_ARGS__, NULL)
