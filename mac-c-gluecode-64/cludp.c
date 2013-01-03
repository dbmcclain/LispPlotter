/* Copyright (c) 2004, Nils Goesche. All rights reserved.

 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:

 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.

 *   * Redistributions in binary form must reproduce the above
 *     copyright notice, this list of conditions and the following
 *     disclaimer in the documentation and/or other materials
 *     provided with the distribution.

 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 * GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/poll.h>
#include <netinet/in.h>
#include <netdb.h>
#include <errno.h>
#include "cludp.h"

int make_udp_socket(const char **fun, int *err)
{
	struct protoent *ent;
	int proto = 0;
	int ret;
#if 0
	int opt;
#endif
	if ((ent = getprotobyname("udp")))
		proto = ent->p_proto;
	ret = socket(PF_INET, SOCK_DGRAM, proto);
	if (ret < 0) {
		*fun = "socket";
		*err = errno;
		return -1;
	}
	if (fcntl(ret, F_SETFL, O_NONBLOCK) < 0) {
		*fun = "fcntl";
		*err = errno;
		close(ret);
		return -1;
	}
#if 0
	opt = 0xFFFF;
	setsockopt(ret, SOL_SOCKET, SO_SNDBUF, &opt, sizeof opt);
	setsockopt(ret, SOL_SOCKET, SO_RCVBUF, &opt, sizeof opt);
#endif
	return ret;
}

int bind_udp_socket(int sock, unsigned long address, unsigned short port,
		    const char **fun, int *err)
{
	struct sockaddr_in addr;

	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(address);
	if (bind(sock, (struct sockaddr *) &addr, sizeof addr) < 0) {
		*fun = "bind";
		*err = errno;
		return -1;
	}
	return 0;
}

int close_udp_socket(int sock, const char **fun, int *err)
{
	if (close(sock)) {
		*fun = "close";
		*err = errno;
		return -1;
	}
	return 0;
}

int send_udp_socket(int sock, unsigned long address, unsigned short port,
		    const unsigned char *buf, unsigned long len,
		    const char **fun, int *err)
{
	struct sockaddr_in addr;
	int ret;

	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(address);
	ret = sendto(sock, buf, len, 0,
		     (struct sockaddr *) &addr, sizeof addr);
	if (ret < 0) {
		if (errno == EAGAIN) {
			ret = -2;
		} else {
			ret = -1;
			*fun = "sendto";
			*err = errno;
		}
	}
	return ret;
}

int recv_udp_socket(int sock, unsigned char *buf, unsigned long len,
		    unsigned long *addr, unsigned short *port,
		    int *valid,
		    const char **fun, int *err)
{
	int ret;
	struct sockaddr_in sockaddr;
	socklen_t siz = sizeof sockaddr;

	ret = recvfrom(sock, buf, len, 0,
		       (struct sockaddr *) &sockaddr, &siz);
	if (ret < 0) {
		if (errno == EAGAIN) {
			ret = -2;
		} else {
			ret = -1;
			*fun = "recv";
			*err = errno;
		}
	} else if (siz < sizeof sockaddr || sockaddr.sin_family != AF_INET) {
		*valid = 0;
	} else {
		*valid = 1;
		*addr = ntohl(sockaddr.sin_addr.s_addr);
		*port = ntohs(sockaddr.sin_port);
	}
	return ret;
}

int peek_udp_socket(int sock, const char **fun, int *err)
{
	struct pollfd pollfd;
	int ret;

	pollfd.fd = sock;
	pollfd.events = POLLIN;
	ret = poll(&pollfd, 1, 0);
	if (ret < 0) {
		*fun = "poll";
		*err = errno;
		ret = -1;
	}
	return ret;
}

int poke_udp_socket(int sock, const char **fun, int *err)
{
	struct pollfd pollfd;
	int ret;

	pollfd.fd = sock;
	pollfd.events = POLLOUT;
	ret = poll(&pollfd, 1, 0);
	if (ret < 0) {
		*fun = "poll";
		*err = errno;
		ret = -1;
	}
	return ret;
}
