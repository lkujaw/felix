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
 *  File:          nls.c (C Source File)
 *  Language:      C (1990)
 *  Author:        Lev Kujawski
 *  Description:   X/Open Native Language System interface
 *
 ****************************************************************************/

#include <locale.h>
#include <nl_types.h>

const int lc_all_c      = LC_ALL;
const int lc_collate_c  = LC_COLLATE;
const int lc_ctype_c    = LC_CTYPE;
const int lc_messages_c = LC_MESSAGES;
const int lc_monetary_c = LC_MONETARY;
const int lc_numeric_c  = LC_NUMERIC;
const int lc_time_c     = LC_TIME;

const int nl_cat_locale_c = NL_CAT_LOCALE;
