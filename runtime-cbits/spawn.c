#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <pthread.h>
#include <spawn.h>
#include <stddef.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

static int initial_ignored_signals;
__attribute__((constructor)) static void monk_snapshot_signals(void) {
  struct sigaction disposition;
  if (sigaction(SIGINT, NULL, &disposition) == 0 && disposition.sa_handler == SIG_IGN)
    initial_ignored_signals |= 1;
  if (sigaction(SIGQUIT, NULL, &disposition) == 0 && disposition.sa_handler == SIG_IGN)
    initial_ignored_signals |= 2;
}
int monk_initial_signal_ignored(int signal) {
  return signal == SIGINT ? (initial_ignored_signals & 1) != 0 :
         signal == SIGQUIT ? (initial_ignored_signals & 2) != 0 : 0;
}

static const int managed_signals[] = {
  SIGPIPE, SIGINT, SIGQUIT, SIGTERM, SIGHUP, SIGCHLD, SIGTSTP, SIGTTIN, SIGTTOU
};
#define MANAGED_COUNT (sizeof(managed_signals) / sizeof(managed_signals[0]))

static int install_child_signals(int asynchronous) {
  struct sigaction disposition;
  sigemptyset(&disposition.sa_mask);
  disposition.sa_flags = 0;
  for (size_t i = 0; i < MANAGED_COUNT; ++i) {
    int signal = managed_signals[i];
    disposition.sa_handler = ((asynchronous && (signal == SIGINT || signal == SIGQUIT)) ||
                             monk_initial_signal_ignored(signal)) ? SIG_IGN : SIG_DFL;
    if (sigaction(signal, &disposition, NULL) < 0) return errno;
  }
  sigset_t mask;
  sigemptyset(&mask);
  if (sigprocmask(SIG_SETMASK, &mask, NULL) < 0) return errno;
  return 0;
}

/* POSIX spawn cannot request SIG_IGN. This bounded alternate path prepares
 * everything in the parent and runs only async-signal-safe C in the child.
 * The CLOEXEC acknowledgement distinguishes exec success from setup failure.
 */
static int spawn_ignored(const char *path, char *const argv[], char *const env[],
                         const char *cwd, int cwd_fd, const int *targets,
                         const int *sources, int count, int closed_mask,
                         int asynchronous, int *result_pid) {
  int minimum = 10;
  for (int i = 0; i < count; ++i) {
    if (targets[i] >= minimum) minimum = targets[i] + 1;
    if (sources[i] >= minimum) minimum = sources[i] + 1;
  }
  if (cwd_fd >= minimum) minimum = cwd_fd + 1;
  int raw[2], reply[2] = {-1, -1};
  if (pipe(raw) < 0) return errno;
  reply[0] = fcntl(raw[0], F_DUPFD_CLOEXEC, minimum);
  int error = errno;
  if (reply[0] >= 0) { reply[1] = fcntl(raw[1], F_DUPFD_CLOEXEC, minimum); error = errno; }
  close(raw[0]); close(raw[1]);
  if (reply[0] < 0 || reply[1] < 0) {
    if (reply[0] >= 0) close(reply[0]);
    if (reply[1] >= 0) close(reply[1]);
    return error;
  }
  sigset_t blocked, previous_mask;
  sigfillset(&blocked);
  error = pthread_sigmask(SIG_SETMASK, &blocked, &previous_mask);
  if (error) { close(reply[0]); close(reply[1]); return error; }
  pid_t pid = fork();
  if (pid != 0) {
    error = errno;
    (void)pthread_sigmask(SIG_SETMASK, &previous_mask, NULL);
    if (pid < 0) { close(reply[0]); close(reply[1]); return error; }
  }
  if (pid == 0) {
    close(reply[0]);
    error = 0;
    if (cwd_fd >= 0) { if (fchdir(cwd_fd) < 0) error = errno; }
    else if (cwd && chdir(cwd) < 0) error = errno;
    for (int fd = 0; fd < 3; ++fd)
      if (closed_mask & (1 << fd)) close(fd);
    for (int i = 0; !error && i < count; ++i)
      if (dup2(sources[i], targets[i]) < 0) error = errno;
    if (!error) error = install_child_signals(asynchronous);
    if (!error) { execve(path, argv, env); error = errno; }
    const char *bytes = (const char *)&error;
    size_t remaining = sizeof(error);
    while (remaining) {
      ssize_t written = write(reply[1], bytes, remaining);
      if (written < 0 && errno == EINTR) continue;
      if (written <= 0) break;
      bytes += written; remaining -= (size_t)written;
    }
    _exit(127);
  }
  close(reply[1]);
  size_t received = 0;
  error = 0;
  while (received < sizeof(error)) {
    ssize_t amount = read(reply[0], (char *)&error + received, sizeof(error) - received);
    if (amount < 0 && errno == EINTR) continue;
    if (amount < 0) { error = errno; received = sizeof(error); break; }
    if (amount == 0) break;
    received += (size_t)amount;
  }
  close(reply[0]);
  if (received) {
    if (received != sizeof(error)) error = EIO;
    while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {}
    return error;
  }
  *result_pid = (int)pid;
  return 0;
}

/* Every byte, descriptor duplicate and action is prepared before spawning.
 * No Haskell/RTS code runs between process creation and exec. */
int monk_spawn(const char *path, char *const argv[], char *const env[],
               const char *cwd, int cwd_fd, const int *targets, const int *sources,
               int count, int closed_mask, int asynchronous, int *result_pid) {
  if (asynchronous || initial_ignored_signals)
    return spawn_ignored(path, argv, env, cwd, cwd_fd, targets, sources, count, closed_mask, asynchronous, result_pid);
  posix_spawn_file_actions_t actions;
  posix_spawnattr_t attributes;
  int error = posix_spawn_file_actions_init(&actions);
  if (error) return error;
  error = posix_spawnattr_init(&attributes);
  if (error) { posix_spawn_file_actions_destroy(&actions); return error; }
  /* Retain the 10.15+ Darwin entry point for supported older deployment
   * targets; the standardized spelling requires macOS 26. */
#ifdef __APPLE__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif
  if (cwd_fd >= 0) {
    if ((error = posix_spawn_file_actions_addfchdir_np(&actions, cwd_fd))) goto done;
  } else if (cwd && (error = posix_spawn_file_actions_addchdir_np(&actions, cwd))) goto done;
#ifdef __APPLE__
#pragma clang diagnostic pop
#endif
  for (int fd = 0; fd < 3; ++fd)
    if ((closed_mask & (1 << fd)) && fcntl(fd, F_GETFD) >= 0 && (error = posix_spawn_file_actions_addclose(&actions, fd))) goto done;
  for (int i = 0; i < count; ++i)
    if ((error = posix_spawn_file_actions_adddup2(&actions, sources[i], targets[i]))) goto done;
  sigset_t defaults, mask;
  sigemptyset(&defaults);
  int signals[] = {SIGPIPE, SIGINT, SIGQUIT, SIGTERM, SIGHUP, SIGCHLD, SIGTSTP, SIGTTIN, SIGTTOU};
  for (size_t i = 0; i < sizeof(signals)/sizeof(signals[0]); ++i) sigaddset(&defaults, signals[i]);
  sigemptyset(&mask);
  if ((error = posix_spawnattr_setsigdefault(&attributes, &defaults))) goto done;
  if ((error = posix_spawnattr_setsigmask(&attributes, &mask))) goto done;
  if ((error = posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETSIGDEF | POSIX_SPAWN_SETSIGMASK))) goto done;
  pid_t pid;
  error = posix_spawn(&pid, path, &actions, &attributes, argv, env);
  if (!error) *result_pid = (int) pid;
done:
  posix_spawnattr_destroy(&attributes);
  posix_spawn_file_actions_destroy(&actions);
  return error;
}

/* Failed exec still has a waitable source child identity. This child performs
 * only async-signal-safe _exit, and never enters the Haskell runtime. */
int monk_spawn_status(int status) {
  pid_t pid = fork();
  if (pid == 0) _exit(status);
  return (int) pid;
}

/* Search-only access does not require permission to enumerate cwd. Keep the
 * capability above stdio so opening it never changes an initially closed fd. */
int monk_open_working_directory(void) {
#ifdef O_SEARCH
  int flags = O_SEARCH;
#elif defined(O_PATH)
  int flags = O_PATH | O_DIRECTORY;
#else
  int flags = O_RDONLY | O_DIRECTORY;
#endif
  int fd;
  do { fd = open(".", flags | O_CLOEXEC); } while (fd < 0 && errno == EINTR);
  if (fd < 0) return -1;
  int owned;
  do { owned = fcntl(fd, F_DUPFD_CLOEXEC, 10); } while (owned < 0 && errno == EINTR);
  int error = errno;
  close(fd);
  errno = error;
  return owned;
}

/* Source opens occur once, relative to the transferred directory capability.
 * Interrupted blocking opens return to the interruptible Haskell boundary. */
int monk_open_at(int directory, const char *path, int mode) {
  int flags;
  switch (mode) {
    case 0: flags = O_RDONLY; break;
    case 1: flags = O_WRONLY | O_CREAT | O_TRUNC; break;
    case 2: flags = O_WRONLY | O_CREAT | O_APPEND; break;
    case 3: flags = O_RDWR | O_CREAT; break;
    default: errno = EINVAL; return -1;
  }
  int fd = openat(directory, path, flags | O_CLOEXEC, 0666);
  if (fd < 0) return -1;
  int owned;
  do { owned = fcntl(fd, F_DUPFD_CLOEXEC, 10); } while (owned < 0 && errno == EINTR);
  int error = errno;
  close(fd);
  errno = error;
  return owned;
}

/* A bounded external-site primitive replaces itself on success. This is not
 * a source interpreter; ENOEXEC is returned without a shell fallback. */
extern int monk_initial_descriptor_mask(void);
int monk_exec(const char *path, char *const argv[], char *const env[]) {
  struct sigaction saved_signals[MANAGED_COUNT];
  sigset_t saved_mask;
  for (size_t i = 0; i < MANAGED_COUNT; ++i)
    if (sigaction(managed_signals[i], NULL, &saved_signals[i]) < 0) return errno;
  if (sigprocmask(SIG_SETMASK, NULL, &saved_mask) < 0) return errno;
  int initial = monk_initial_descriptor_mask();
  int protected[3] = {-1, -1, -1};
  int original_flags[3] = {0, 0, 0};
  for (int fd = 0; fd < 3; ++fd) {
    if (initial & (1 << fd)) continue;
    int flags = fcntl(fd, F_GETFD);
    if (flags < 0) continue;
    original_flags[fd] = flags;
    protected[fd] = fcntl(fd, F_DUPFD_CLOEXEC, 10);
    if (protected[fd] < 0) {
      int error = errno;
      for (int saved = 0; saved < 3; ++saved)
        if (protected[saved] >= 0) close(protected[saved]);
      return error;
    }
  }
  for (int fd = 0; fd < 3; ++fd)
    if (!(initial & (1 << fd))) close(fd);
  int error = install_child_signals(0);
  if (!error) { execve(path, argv, env); error = errno; }
  /* Failed exec returns to the RTS for source diagnostics or another PATH
   * candidate. Restore any internal fd that occupied initially closed stdio. */
  for (int fd = 0; fd < 3; ++fd) {
    if (protected[fd] >= 0) {
      int restored;
      do { restored = dup2(protected[fd], fd); } while (restored < 0 && errno == EINTR);
      do { restored = fcntl(fd, F_SETFD, original_flags[fd]); } while (restored < 0 && errno == EINTR);
      close(protected[fd]);
    }
  }
  for (size_t i = 0; i < MANAGED_COUNT; ++i)
    (void)sigaction(managed_signals[i], &saved_signals[i], NULL);
  (void)sigprocmask(SIG_SETMASK, &saved_mask, NULL);
  return error;
}
