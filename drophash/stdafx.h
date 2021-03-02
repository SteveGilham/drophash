// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once
#pragma warning (disable : 4514 4710 4711) // inlining
#pragma warning (disable: 5045) // TODO Spectre

// #define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#pragma warning (push, 1)
#pragma warning (disable : 4996 26814 4548 4355 4917 4702 26400 4987 4820 4365 4623 4625 4626 5026 5027 4571 4774 26412 26461 26426 26432 26447 26472 26446 26473 26440 26429 26496 26472 26482 26486 26487 26434)
#include <gsl.h>
#pragma warning (pop) // unbalanced push in span.h
#include <windows.h>
#include <Shlobj.h>
#include <Strsafe.h>

#include <algorithm>
#include <fstream>
#include <iostream>
#include <iomanip>
#include <sstream>
#include <memory>
#include <string>
#include <vector>
#include <array>
#include <ranges>
#include <span>

#include "Resource.h"
#pragma warning (pop)
