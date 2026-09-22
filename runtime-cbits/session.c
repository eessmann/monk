#include <errno.h>
#include <fcntl.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

static int socket_address(const char *path, struct sockaddr_un *address) {
  memset(address, 0, sizeof(*address));
  address->sun_family = AF_UNIX;
  size_t length = strlen(path);
  if (length >= sizeof(address->sun_path)) { errno = ENAMETOOLONG; return -1; }
  memcpy(address->sun_path, path, length + 1);
  return 0;
}
static int owned_socket(void) {
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  if (fd < 0) return -1;
  if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0) { int error = errno; close(fd); errno = error; return -1; }
#ifdef SO_NOSIGPIPE
  int yes = 1;
  setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &yes, sizeof(yes));
#endif
  return fd;
}
int monk_session_listen(const char *path) {
  struct sockaddr_un address;
  if (socket_address(path, &address) < 0) return -1;
  int fd = owned_socket();
  if (fd < 0) return -1;
  if (bind(fd, (struct sockaddr *) &address, sizeof(address)) < 0 || listen(fd, 64) < 0 || fcntl(fd, F_SETFL, O_NONBLOCK) < 0) {
    int error = errno; close(fd); errno = error; return -1;
  }
  return fd;
}
int monk_session_connect(const char *path) {
  struct sockaddr_un address;
  if (socket_address(path, &address) < 0) return -1;
  int fd = owned_socket();
  if (fd < 0) return -1;
  int result;
  do { result = connect(fd, (struct sockaddr *) &address, sizeof(address)); } while (result < 0 && errno == EINTR);
  if (result < 0) { int error = errno; close(fd); errno = error; return -1; }
  return fd;
}
int monk_session_accept(int listener) {
  int fd = accept(listener, NULL, NULL);
  if (fd < 0) return -1;
  if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0 || fcntl(fd, F_SETFL, 0) < 0) {
    int error = errno; close(fd); errno = error; return -1;
  }
#ifdef SO_NOSIGPIPE
  int yes = 1;
  setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &yes, sizeof(yes));
#endif
  return fd;
}
int monk_session_send_fds(int socket_fd, const int *fds, int count) {
  if (count < 0 || count > 4) { errno = EINVAL; return -1; }
  char byte = 'M';
  struct iovec buffer = { .iov_base = &byte, .iov_len = 1 };
  char control[CMSG_SPACE(4 * sizeof(int))];
  memset(control, 0, sizeof(control));
  struct msghdr message;
  memset(&message, 0, sizeof(message));
  message.msg_iov = &buffer;
  message.msg_iovlen = 1;
  if (count) {
    message.msg_control = control;
    message.msg_controllen = CMSG_SPACE(count * sizeof(int));
    struct cmsghdr *header = CMSG_FIRSTHDR(&message);
    header->cmsg_level = SOL_SOCKET;
    header->cmsg_type = SCM_RIGHTS;
    header->cmsg_len = CMSG_LEN(count * sizeof(int));
    memcpy(CMSG_DATA(header), fds, count * sizeof(int));
  }
  ssize_t result;
  do { result = sendmsg(socket_fd, &message, 0); } while (result < 0 && errno == EINTR);
  return result == 1 ? 0 : -1;
}
int monk_session_receive_fds(int socket_fd, int *fds) {
  char byte = 0;
  struct iovec buffer = { .iov_base = &byte, .iov_len = 1 };
  char control[CMSG_SPACE(4 * sizeof(int))];
  memset(control, 0, sizeof(control));
  struct msghdr message;
  memset(&message, 0, sizeof(message));
  message.msg_iov = &buffer;
  message.msg_iovlen = 1;
  message.msg_control = control;
  message.msg_controllen = sizeof(control);
  ssize_t received;
  do { received = recvmsg(socket_fd, &message, MSG_DONTWAIT); } while (received < 0 && errno == EINTR);
  if (received != 1) { if (received == 0) errno = EPIPE; return -1; }
  int count = 0;
  int excess = 0;
  for (struct cmsghdr *header = CMSG_FIRSTHDR(&message); header; header = CMSG_NXTHDR(&message, header)) {
    if (header->cmsg_level == SOL_SOCKET && header->cmsg_type == SCM_RIGHTS) {
      int found = (int) ((header->cmsg_len - CMSG_LEN(0)) / sizeof(int));
      int *received_fds = (int *) CMSG_DATA(header);
      for (int index = 0; index < found; ++index) {
        if (count < 4) fds[count++] = received_fds[index];
        else { close(received_fds[index]); excess = 1; }
      }
    }
  }
  if ((message.msg_flags & MSG_CTRUNC) || excess || byte != 'M') {
    for (int i = 0; i < count; ++i) close(fds[i]);
    errno = EPROTO;
    return -1;
  }
  return count;
}
int monk_session_shutdown_write(int fd) { return shutdown(fd, SHUT_WR); }
