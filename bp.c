/*
 *
 * Very simple branch predictor
 * API:
 * - bool bp_predict_next(pc, &pc_next, &token)
 *   False means no prediction made (and token is irrevant)
 *   True means prediction was made and token is necessary for correction.
 * - void bp_was_correct(token)
 * - void bp_was_wrong(token, &actual_pc_next)
 *
 * by Tommy Thorn (C) 2020
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>
#include <riscv.h>

int branches, correct, mispredicts;

#define BTB_SIZE_LG2 10
#define BTB_PC_BITS  16 // check 16 KiB
#define BTB_TAG_BITS  (BTB_PC_BITS - 2 - BTB_SIZE_LG2)
#define BTB_TAG(pc) (((pc) >> (BTB_SIZE_LG2 + 2)) & (BTB_TAG_BITS - 1))
#define BTB_TARGET_BITS 10  // Inside page
#define BTB_SIZE (1 << BTB_SIZE_LG2)
#define BHT_SIZE 256

// tag 15:10 index 9:2 ignore 1:0    target 13:2  = 18 bits
struct btb_e {
    uint32_t tag, target; // Obviously you'd not store all the bits
} btb_array[BTB_SIZE];

static int global_hist = 0;

static inline struct btb_e *btb(uint32_t pc)
{
    int index = (pc >> 2) ^ global_hist;
    return btb_array + (index & (BTB_SIZE - 1));
}

// enum prediction { SNT, WNT, WT, ST }; // Strongly Not Taken, Weakly Not Taken, ...
enum prediction bht_array[BHT_SIZE];

static inline enum prediction *bht(uint32_t pc)
{
    int index = (pc >> 2) ^ global_hist;
    return bht_array + (index & (BTB_SIZE - 1));
}

bool bp_predict(uint32_t pc, uint32_t *pc_next, xxx

int main(int argc, char **argv)
{
    uint32_t pc_prev = 0x80000000-4, inst_prev = 0;

    system("/bin/pwd");

    for (;;) {
        int op_prev = (inst_prev >> 2) & 31;

        /* Predict */
        uint32_t pc_prediction = pc_prev + 4;
        bool redirected = false;
        if (/*WT <= *bht(pc_prev) && */
            btb(pc_prev)->tag == BTB_TAG(pc_prev)) {
            pc_prediction = btb(pc_prev)->target;
            redirected = true;
        }

        uint32_t pc, inst;
        if (!get_next(&pc, &inst))
            break;

        if (op_prev == JAL || op_prev == JALR || op_prev == BRANCH) {

            char *op_prev_s = op_prev == JAL ? "jal" : op_prev == JALR ? "jalr" : "br";
            int actually_taken  = pc != pc_prev + 4;
            int predicted_taken = pc_prediction != pc_prev + 4;

            ++branches;

            printf("%08x %-4s", pc_prev, op_prev_s);
            if (pc == pc_prediction)
                printf(" correct prediction %d\n", redirected);
            else
                printf(" MIS prediction %d\n", redirected);

            if (0)
            printf("%08x %sprediction, %staken %08x\n",
                   pc_prev,
                   (actually_taken != predicted_taken) ? "mis-" : "    ",
                   actually_taken ? "non-" : "    ",
                   pc_prediction);

            if (pc != pc_prediction) {
                ++mispredicts;
                btb(pc_prev)->tag    = BTB_TAG(pc_prev);
                btb(pc_prev)->target = pc;

#if 0
                if (pc == pc_prev + 4)
                    // Not taken
                    switch () {
                    case SNT:
                    case WNT: *bht(pc_prev) = SNT; break;
                    case WT:
                    case ST:  *bht(pc_prev) = WNT; break;
                    }
                else
                    // taken
                    switch (*bht(pc_prev)) {
                    case WNT:
                    case SNT: *bht(pc_prev) = WT; break;
                    case ST:
                    case WT:  *bht(pc_prev) = ST; break;
                    }
#endif
            }

            global_hist = (global_hist << 2) ^ (3 & ((pc_prev >> 2) ^ (pc_prev >> 4)));
        } else if (pc != pc_prediction)
            printf("%08x %08x (%d %d)\n", pc, inst, inst_prev >> 25, op_prev);

        inst_prev = inst;
        pc_prev = pc;
        //printf("%08x %08x\n", pc, inst);
    }

    printf("Prediction Accuracy: %d/%d (%5.2f%%)\n", branches - mispredicts, branches,
           100.0*(branches - mispredicts) / branches);

    printf("BTB %d entries, tags %d b, target %d b\n", BTB_SIZE, BTB_TAG_BITS, 0);

    return 0;
}
