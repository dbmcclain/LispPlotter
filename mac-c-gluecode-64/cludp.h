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

#ifndef CLUDP_H_INCLUDED__
#define CLUDP_H_INCLUDED__

int make_udp_socket(const char **fun, int *err);

int close_udp_socket(int sock, const char **fun, int *err);

int bind_udp_socket(int sock, unsigned long address, unsigned short port,
		    const char **fun, int *err);

int send_udp_socket(int sock, unsigned long address, unsigned short port,
		    const unsigned char *buf, unsigned long len,
		    const char **fun, int *err);

int recv_udp_socket(int sock, unsigned char *buf, unsigned long len,
		    unsigned long *addr, unsigned short *port,
		    int *valid,
		    const char **fun, int *err);

int peek_udp_socket(int sock, const char **fun, int *err);

int poke_udp_socket(int sock, const char **fun, int *err);

#endif /* CLUDP_H_INCLUDED__ */
