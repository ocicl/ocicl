/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * echo-server.c --- Simple echo server for testing purposes.
 *
 */

/* TODO: IPv6 support, port to Winsock */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <ctype.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#include <Winsock2.h>
#include <Ws2tcpip.h>
#else
#include <unistd.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/select.h>
#endif

#define DEFAULT_PORT 7
#define BACKLOG 10
#define MAXCONS FD_SETSIZE

static int
sendallto(int s, char *buf, int len, struct sockaddr *to, socklen_t tolen)
{
    int sent = 0;
    int left = len;

    while (sent < len) {
        int n = sendto(s, buf+sent, left, 0, to, tolen);
        if (n == -1) { return -1; }
        sent += n;
        left -= n;
    }

    return 0;
}

static char time_str[9];

static const char* timestamp()
{
    time_t now = time(NULL);
    struct tm *tms = localtime(&now);
    strftime(time_str, (sizeof time_str)-1, "%T", tms);
    return time_str;
}

int main(int argc, char *argv[])
{
    int fds[MAXCONS];
    struct sockaddr_in addrs[MAXCONS];
    int conns = 0;
    int local_port, listenfd, dgramfd;
    struct sockaddr_in local;
    socklen_t sin_size = sizeof(struct sockaddr_in);

    switch (argc) {
    case 1: local_port = DEFAULT_PORT; break;
    case 2: local_port = strtol(argv[1], NULL, 10); break;
    default:
        fprintf(stderr, "Usage: %s [port number]\n", argv[0]);
        fprintf(stderr, "The default port is: %d.\n", DEFAULT_PORT);
        exit(1);
    }

    if (local_port <= 0 || local_port > 65535) {
        fprintf(stderr, "Error: invalid port number.\n");
        exit(1);
    }

    printf("[%s] listening on port %d\n", timestamp(), local_port);

    if ((listenfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        perror("socket");
        exit(1);
    }

    fds[conns++] = listenfd;

    if ((dgramfd = socket(AF_INET, SOCK_DGRAM, 0)) == -1) {
        perror("dgram socket");
        exit(1);
    }

    fds[conns++] = dgramfd;

    local.sin_family = AF_INET;
    local.sin_port = htons(local_port);
    local.sin_addr.s_addr = INADDR_ANY;
    memset(local.sin_zero, 0, sizeof local.sin_zero);

    if (bind(listenfd, (struct sockaddr *) &local, sin_size) == -1) {
        perror("bind");
        exit(1);
    }

    if (bind(dgramfd, (struct sockaddr *) &local, sin_size) == -1) {
        perror("dgram bind");
        exit(1);
    }

    if (listen(listenfd, BACKLOG) == -1) {
        perror("listen");
        exit(1);
    }

    for (;;) {
        int fd, max_fd, i, j, n;
        fd_set readfds;
        char buf[4096];

        FD_ZERO(&readfds);
        for (i = 0, max_fd = -1; i < conns; i++) {
            if (fds[i] > max_fd)
                max_fd = fds[i];
            FD_SET(fds[i], &readfds);
        }

        if (select(max_fd+1, &readfds, NULL, NULL, NULL) == -1) {
            perror("select");
            exit(1);
        }

        if (FD_ISSET(listenfd, &readfds)) {
            fd = accept(listenfd, (struct sockaddr *) &addrs[conns], &sin_size);

            if (fd == -1) {
                perror("accept");
                continue;
            } else {
                printf("[%s] accepted connection from %s (fd: %d)\n",
                       timestamp(), inet_ntoa(addrs[conns].sin_addr), fd);
                fds[conns++] = fd;
            }
        }

        for (i = 1; i < conns; i++) {
            if (FD_ISSET(fds[i], &readfds)) {
                sin_size = sizeof(struct sockaddr_in);
                n = recvfrom(fds[i], buf, (sizeof buf)-1, 0,
                             (struct sockaddr *) &addrs[i], &sin_size);
                if (n == -1) {
                    perror("recvfrom");
                    goto error;
                }

                // n == 0? huh hmm..
                if (n == 0) goto error;

                printf("[%s] got %d bytes from %s (%s) ", timestamp(),
                       n, inet_ntoa(addrs[i].sin_addr),
                       i == 1 ? "udp" : "tcp");

                for (j = 0; j < n; j++) {
                    if (j % 75 == 0)
                        printf("\n  ");

                    if (isprint(buf[j]))
                        putchar(buf[j]);
                    else
                        printf("[0x%x]", buf[j]);

                    //putchar(isprint(buf[j]) ? buf[j] : '?');
                }

                if (sendallto(fds[i], buf, n, (struct sockaddr *) &addrs[i],
                              sizeof(struct sockaddr_in)) == -1) {
                    perror("\nsendto");
                    goto error;
                }

                printf("\n  (echoed)\n");
                continue;

            error:
                if (i == 1) continue;
                printf("[%s] removing fd %d from set\n", timestamp(), fds[i]);
                close(fds[i]);
                for (j = i+1; j < conns; j++) {
                    fds[j-1] = fds[j];
                    addrs[j-1] = addrs[j];
                }
                conns--;
            }
        }
    }

    return 0;
}
