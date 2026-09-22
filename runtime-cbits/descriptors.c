/* Snapshot before the GHC RTS can reuse deliberately closed standard fds. */
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#ifdef __APPLE__
#include <libproc.h>
#include <sys/proc_info.h>
#endif
#include <sys/resource.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef __linux__
#include <sys/syscall.h>
#endif

static int initial_descriptor_mask;
__attribute__((constructor)) static void monk_snapshot_descriptors(void) {
  for (int fd = 0; fd < 6; ++fd)
    if (fcntl(fd, F_GETFD) != -1) initial_descriptor_mask |= 1 << fd;
}
int monk_initial_descriptor_mask(void) { return initial_descriptor_mask; }

/* F_DUPFD's numeric value is platform-specific. Never encode it in Haskell. */
int monk_duplicate_private(int fd) { return fcntl(fd, F_DUPFD_CLOEXEC, 10); }
int monk_duplicate_above(int fd, int minimum) { return fcntl(fd, F_DUPFD_CLOEXEC, minimum); }

static int mark_close_on_exec(int fd) {
  int flags;
  do { flags = fcntl(fd, F_GETFD); } while (flags < 0 && errno == EINTR);
  if (flags < 0) return errno == EBADF ? 0 : -1;
  int result;
  do { result = fcntl(fd, F_SETFD, flags | FD_CLOEXEC); }
  while (result < 0 && errno == EINTR);
  return result;
}

/* The child owns stdio only. Keep RTS descriptors usable until exec, but do
 * not leak them or arbitrary inherited descriptors into the evaluator.
 * Linux's optional fast path does not require procfs; other systems use POSIX.
 * Scan the hard limit because inherited fds may exceed a lowered soft limit.
 */
int monk_private_cloexec(void) {
#ifdef __APPLE__
  /* Darwin has an infinite hard limit. The native inventory also covers fds
   * above a reduced soft limit and avoids scanning a million empty slots.
   * This is a process API, not a procfs or descriptor-path dependency.
   */
  errno = 0;
  int needed = proc_pidinfo(getpid(), PROC_PIDLISTFDS, 0, NULL, 0);
  if (needed == 0 && errno == 0) return 0;
  if (needed <= 0) return -1;
  for (;;) {
    int capacity = needed + 32 * (int) sizeof(struct proc_fdinfo);
    struct proc_fdinfo *fds = malloc((size_t) capacity);
    if (fds == NULL) return -1;
    errno = 0;
    int used = proc_pidinfo(getpid(), PROC_PIDLISTFDS, 0, fds, capacity);
    if (used == 0 && errno == 0) { free(fds); return 0; }
    if (used <= 0) { free(fds); return -1; }
    if (used >= capacity) { free(fds); needed = capacity; continue; }
    int count = used / (int) sizeof(struct proc_fdinfo);
    for (int index = 0; index < count; ++index) {
      if (fds[index].proc_fd >= 3 && mark_close_on_exec(fds[index].proc_fd) < 0) {
        int saved = errno;
        free(fds);
        errno = saved;
        return -1;
      }
    }
    free(fds);
    return 0;
  }
#endif
#ifdef __linux__
#ifdef SYS_close_range
  if (syscall(SYS_close_range, 3U, UINT_MAX, 4U) == 0) return 0;
  if (errno != ENOSYS && errno != EINVAL && errno != EPERM) return -1;
#endif
#endif
  struct rlimit limits;
  if (getrlimit(RLIMIT_NOFILE, &limits) < 0) return -1;
  rlim_t maximum = limits.rlim_max;
  if (maximum == RLIM_INFINITY) {
    long configured = sysconf(_SC_OPEN_MAX);
    if (configured < 0) return -1;
    maximum = (rlim_t) configured;
  }
  if (maximum > INT_MAX) maximum = INT_MAX;
  for (int fd = 3; (rlim_t) fd < maximum; ++fd) {
    if (mark_close_on_exec(fd) < 0) return -1;
  }
  return 0;
}

const char *monk_error_message(int code) { return strerror(code); }

/* Error classification follows the same cwd capability as the failed exec. */
int monk_directory_at(int fd, const char *path) {
  struct stat status;
  return fstatat(fd < 0 ? AT_FDCWD : fd, path, &status, 0) == 0 && S_ISDIR(status.st_mode);
}
