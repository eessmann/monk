/* Linux admission profile: snapshot before the GHC RTS can reuse closed fds. */
#include <fcntl.h>
static int initial_descriptor_mask;
__attribute__((constructor)) static void monk_snapshot_descriptors(void) {
  for (int fd = 0; fd < 4; ++fd)
    if (fcntl(fd, F_GETFD) != -1) initial_descriptor_mask |= 1 << fd;
}
int monk_initial_descriptor_mask(void) { return initial_descriptor_mask; }
