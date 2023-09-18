#include "fileutil.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else // _WIN32
#include <sys/stat.h>
#endif // !_WIN32

FileType get_file_type(char const* path) {
#ifdef _WIN32
    DWORD attr = GetFileAttributesA(path);
    if (attr == INVALID_FILE_ATTRIBUTES) {
        // FIXME: this is not always the case
        return FILE_NONEXISTENT;
    }

    if (attr & FILE_ATTRIBUTE_DIRECTORY)
        return FILE_DIRECTORY;
    return FILE_REGULAR;
#else // _WIN32
    struct stat statbuf;
    if (stat(path, &statbuf) < 0) {
        if (errno == ENOENT) {
            return FILE_NONEXISTENT;
        }
        fprintf(stderr, "Could not get file mode of '%s': %s", path, strerror(errno));
        exit(1);
    }

    switch (statbuf.st_mode & S_IFMT) {
        case S_IFDIR:
            return FILE_DIRECTORY;
        case S_IFREG:
            return FILE_REGULAR;
        default:
            return FILE_OTHER;
    }
#endif // !_WIN32
}

#ifdef _WIN32
struct DIR {
    HANDLE hFind;
    WIN32_FIND_DATA data;
    struct dirent* dirent;
};

DIR* opendir(char const* dirpath) {
    COOL_ASSERT(dirpath, "NULL path");
    char buffer[_MAX_PATH];
    snprintf(buffer, _MAX_PATH, "%s\\*", dirpath);

    DIR* dir = COOL_REALLOC(NULL, sizeof(DIR));
    memset(dir, 0, sizeof(DIR));

    dir->hFind = FindFirstFile(buffer, &dir->data);
    if (dir->hFind == INVALID_HANDLE_VALUE) {
        errno = ENOSYS;
        goto fail;
    }

    return dir;

fail:
    if (dir)
        COOL_FREE(dir);
    return NULL;
}

struct dirent* readdir(DIR* dirp) {
    COOL_ASSERT(dirp, "NULL dirp");
    if (dirp->dirent == NULL) {
        dirp->dirent = COOL_REALLOC(NULL, sizeof(struct dirent));
        memset(dirp->dirent, 0, sizeof(struct dirent));
    } else {
        if (!FindNextFile(dirp->hFind, &dirp->data)) {
            if (GetLastError() != ERROR_NO_MORE_FILES) {
                errno = ENOSYS;
            }

            return NULL;
        }
    }

    memset(dirp->dirent->d_name, 0, sizeof(dirp->dirent->d_name));
    strncpy_s(dirp->dirent->d_name, sizeof(dirp->dirent->d_name), dirp->data.cFileName,
            sizeof(dirp->dirent->d_name) - 1);
    return dirp->dirent;
}

int closedir(DIR* dirp) {
    COOL_ASSERT(dirp, "NULL dirp");

    if (!FindClose(dirp->hFind)) {
        errno = ENOSYS;
        return -1;
    }

    if (dirp->dirent) {
        COOL_FREE(dirp->dirent);
    }
    COOL_FREE(dirp);

    return 0;
}
#else // _WIN32
// already defined in dirent.h
#endif // !_WIN32
