#pragma once

typedef enum {
    FILE_REGULAR,
    FILE_DIRECTORY,
    FILE_NONEXISTENT,
    FILE_OTHER,
} FileType;

FileType get_file_type(char const* path);

#ifdef _WIN32
struct dirent {
    char d_name[MAX_PATH + 1];
};
typedef struct DIR DIR;
DIR* opendir(char const* dirpath);
struct dirent* readdir(DIR* dirp);
int closedir(DIR* dirp);
#else // _WIN32
#include <dirent.h>
#endif // !_WIN32
