#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdarg.h>

#define SAMPLE_RATE 44100

static FILE *output_file;
static int datalen;

void die(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt); 
    fprintf(stderr, "\nERROR: ");
	vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

static void die_perror(const char *msg) {
    fprintf(stderr, "\nERROR: ");
    perror(msg);
    exit(1);
}

void output_write(void *ptr, int len) {
    fwrite(ptr, len, 1, output_file);
}

static void write_le32(int i)
{
	unsigned u = (unsigned)i;
	output_write((void*)&u, 4);
}

static void write_le16(int i)
{
	unsigned short u = (unsigned short)i;
	output_write((void*)&u, 2);
}

void output_sample(short left, short right) {
    output_write(&left,  2);
    output_write(&right, 2);
    datalen += 4;
}

void write_pulse(int one_or_zero, int cycles, int freq) {
    short val = one_or_zero ? (short)0x7fff : (short)0x0;

    double time_seconds = (double)cycles / (double)freq;
    int cycles_at_sample_rate = (int)(time_seconds * SAMPLE_RATE);

    int i;
    printf("write %d samples of %d\n", cycles_at_sample_rate, val);
    for (i = 0; i < cycles_at_sample_rate; i++) {
        output_sample(val, val);
    }
}

int main() {
    output_file = fopen("output.wav", "wb");
    if (!output_file)
        die_perror("failed to open file");

    int samplerate    = SAMPLE_RATE;
    int channels      = 2;
    int bitspersample = 16;

    int datarate = samplerate * channels * (bitspersample / 8);
    int blockalign = channels * (bitspersample / 8);

    // write wav header
    output_write("RIFF", 4);
    long riff_len_pos = ftell(output_file);     // need to write 36+datalen to riff_len_pos
    write_le32(0);      
    output_write("WAVE", 4);
    output_write("fmt ", 4);
    write_le32(16);
    write_le16(1);
    write_le16(channels);
    write_le32(samplerate);
    write_le32(datarate);
    write_le16(blockalign);
    write_le16(bitspersample);
    output_write("data", 4);
    long wav_data_pos = ftell(output_file);     // need to write datalen to wav_data_pos
    write_le32(0);      

    int i;
    for (i = 0; i < 22050; i++) output_sample(0, 0);    // write 500ms of silence

    write_pulse(1, 4909, 1000000);         // command start
    write_pulse(0, 4320, 1000000);

    i = 0;
    unsigned code = 0xe0e040bf;
    for (i = 0; i < 32; i++) {
        int bit = (code >> (31 - i)) & 0x1;

        if (bit) {
            write_pulse(1, 818, 1000000);         // 1 bit
            write_pulse(0, 1425, 1000000);
        } else {
            write_pulse(1, 818, 1000000);         // 0 bit
            write_pulse(0, 325, 1000000);
        }
    }

    write_pulse(1, 717, 1000000);          // command stop
    write_pulse(0, 717, 1000000);

    for (i = 0; i < 22050; i++) output_sample(0, 0);    // write 500ms of silence

    fseek(output_file, riff_len_pos, SEEK_SET);
    write_le32(36 + datalen);

    fseek(output_file, wav_data_pos, SEEK_SET);
    write_le32(datalen);

    fclose(output_file);

    return 0;
}

