// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once
#pragma warning (disable : 4514 4710 4711) // inlining

// #define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#pragma warning (push, 1)
#pragma warning (disable : 4244 4355 4917 4702 26400 4987 4820 4365 4623 4625 4626 5026 5027 4571 4774 26412 26461 26481 26493 26495 26496 26499 26444 26434)
#include <gsl.h>
#pragma warning (pop) // unbalanced push in span.h
#include <range/v3/all.hpp>
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

#include "Resource.h"
#pragma warning (pop)
