/*****************************************************************************
 *  Copyright (c) 2021, Lev Kujawski.
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a
 *  copy of this software and associated documentation files (the "Software")
 *  to deal in the Software without restriction, including without limitation
 *  the rights to use, copy, modify, merge, publish, distribute, sublicense,
 *  and sell copies of the Software, and to permit persons to whom the
 *  Software is furnished to do so.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 *  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 *  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 *  DEALINGS IN THE SOFTWARE.
 *
 *  SPDX-License-Identifier: MIT-0
 *
 *  File:          cio.c (C Source File)
 *  Language:      C (1990)
 *  Author:        Lev Kujawski
 *  Description:   C Standard Input/Output (stdio.h) interface
 *
 ****************************************************************************/

#include <stdio.h>

FILE *
get_stdin(void)
{
    return stdin;
}

FILE *
get_stdout(void)
{
    return stdout;
}

FILE *
get_stderr(void)
{
    return stderr;
}

int
fprintf_d(FILE *stream, char *format, double arg)
{
    return fprintf(stream, format, arg);
}

int
sprintf_d(char *s, char *format, double arg)
{
    return sprintf(s, format, arg);
}

int
fprintf_i(FILE *stream, char *format, int arg)
{
    return fprintf(stream, format, arg);
}

int
sprintf_i(char *s, char *format, int arg)
{
    return sprintf(s, format, arg);
}

int
fprintf_ld(FILE *stream, char *format, long double arg)
{
    return fprintf(stream, format, arg);
}

int
sprintf_ld(char *s, char *format, long double arg)
{
    return sprintf(s, format, arg);
}

int
fprintf_li(FILE *stream, char *format, long int arg)
{
    return fprintf(stream, format, arg);
}

int
sprintf_li(char *s, char *format, long int arg)
{
    return sprintf(s, format, arg);
}

int
fprintf_p(FILE *stream, char *format, void *arg)
{
    return fprintf(stream, format, arg);
}

int
sprintf_p(char *s, char *format, void *arg)
{
    return sprintf(s, format, arg);
}
