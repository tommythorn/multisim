/*
 * Multisim: a microprocessor architecture exploration framework
 * Copyright (C) 2014 Tommy Thorn
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>
#include <netinet/in.h>
#include <signal.h>
#include <getopt.h>
#include <ctype.h>
#include "sim.h"
#include "run_vliw.h"

static int run = '1';


static struct option long_options[] = {
    {"simple",         0, NULL, '1'},
    {"sscalar",        0, NULL, '2'},
    {"ooo-sscalar",    0, NULL, '3'},
    {"vliw",           0, NULL, '4'},
    {"help",           0, NULL, '?'},
    {0, 0, 0, 0}
};


void usage(char *program)
{
    int i;

    if (strchr(program, '/')) {
        program = strrchr(program, '/') + 1;
    }

    fprintf(stderr,
            "Multi ISA Simulator\n"
            "\n"
            "Usage: %s [options] <elf-files ...>\n"
            "\n"
            "  where options can be one or more of\n"
            "\n"
            "  -i<diskimage>\n"
            "  -d   disassemble executed instructions\n"
            "  -t   trace executed instructions\n"
            "  -c   compliance\n",
            program);

    for (i = 0; long_options[i].name; ++i) {
        fprintf(stderr, "  --%s\n", long_options[i].name);
    }

    fprintf(stderr,
            "\n"
            "Comments to <tommy-git@thorn.ws>\n");

    exit(1);
}

int main(int argc, char **argv)
{
    char **images;
    int num_images;
    verbosity_t verbosity = VERBOSE_CONSOLE;

    for (;;) {
        int c;
        int option_index = 0;

        c = getopt_long(argc, argv, "ctdf:i:o:1234",
                        long_options, &option_index);
        if (c == -1)
            break;

        switch (c) {
        case 0:
            break;

	case 'i':
	    disk_image = optarg;
	    break;

        case 'd':
            verbosity = verbosity*2 + 1;
            break;

        case 'c':
            verbosity = VERBOSE_COMPLIANCE;
            break;

        case 't':
            verbosity |= VERBOSE_TRACE;
            break;

        case '?':
            usage(argv[0]);
            break;

        default:
            if (isdigit(c))
                run = c;
            else
                printf ("?? getopt returned character code 0%o ??\n", c);
        }
    }

    if (optind >= argc) {
        usage(argv[0]);
    }

    if (verbosity & VERBOSE_DISASS)
        verbosity &= ~VERBOSE_TRACE;

    images = argv + optind;
    num_images = argc - optind;

    signal(SIGINT, exit);

    switch (run) {
    case '1':
        run_simple(num_images, images, verbosity);
        break;

#if 0
    case '2':
        run_sscalar(num_images, images, verbosity);
        break;

    case '3':
        run_sscalar_oooe(num_images, images, verbosity);
        break;
#endif

    case '4':
        run_vliw(num_images, images, verbosity);
        break;

    default:
        printf("No XX-run option given\n");
        exit(1);
    }

    exit(0);
}

// Local Variables:
// mode: C
// c-style-variables-are-local-p: t
// c-file-style: "stroustrup"
// End:
