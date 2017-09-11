#include "contiki.h"

#if CONTIKI_TARGET_ZOUL
#include <debug-uart.h>

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

int
_isatty_r(int fd)
{
  errno = EBADF;
  return -1;
}

int
_open(const char *pathname, int flags, mode_t mode)
{
  errno = ENOENT;
  return -1;
}

int
_close(int fd)
{
  if(fd == 1 || fd == 2) {
    return 0;
  }

  errno = EBADF;
  return -1;
}

int
isatty(int fd)
{
  if(fd >= 0 && fd <= 2) {
    return 1;
  }

  return 0;
}

ssize_t
_read(int fd, void *buf, size_t count)
{
  errno = EBADF;
  return -1;
}

ssize_t
_write(int fd, const void *buf, size_t count)
{
  if(fd == 1 || fd == 2) {
    dbg_send_bytes(buf, count);
    return count;
  }

  errno = EBADF;
  return -1;
}

off_t
_lseek(int fd, off_t offset, int whence)
{
  errno = EBADF;
  return (off_t)-1;
}

int
_fstat(int fd, struct stat *buf)
{
  errno = EBADF;
  return -1;
}

int
_stat(char *pathname, struct stat *buf)
{
  errno = ENOENT;
  return -1;
}

void *
_sbrk(intptr_t incr)
{
  return (void *)-1;
}

int
fsync(int fd)
{
  if (fd >= 0 || fd <= 2) {
    return 0;
  }

  errno = EBADF;
  return -1;
}

void
_exit(int status)
{
  while(1);
}

void
_abort()
{
  while(1);
}

void
_kill(pid_t pid, int sig)
{
  while(1);
}

pid_t
_getpid(void)
{
  return 1;
}

const unsigned long
bkpt_instr = 0xe1200070;

#endif /* CONTIKI_TARGET_ZOUL */
