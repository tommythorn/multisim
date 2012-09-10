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
#include "alpha_opcode.h"
#include "sim.h"

static int run = '3';


static struct option long_options[] = {
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
                "\n",
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

        for (;;) {
                int c;
                int option_index = 0;

                c = getopt_long(argc, argv, "tf:i:o:",
                                long_options, &option_index);
                if (c == -1)
                        break;

                switch (c) {
                case 0:
                        break;

                case '?':
                        usage(argv[0]);
                        break;

                default:
                        printf ("?? getopt returned character code 0%o ??\n", c);
                }
        }

        if (optind >= argc) {
                usage(argv[0]);
        }

        images = argv + optind;
        num_images = argc - optind;

        signal(SIGINT, exit);

        switch (run) {
        case '1':
                run_simple(num_images, images);
                break;

        case '2':
                run_sscalar_io(num_images, images);
                break;

        case '3':
                run_sscalar_oooe(num_images, images);
                break;

        default:
                printf("No XX-run option given\n");
                exit(1);
        }

        exit(0);
}
