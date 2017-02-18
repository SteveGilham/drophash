// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once
#pragma warning (disable : 4710 4711)

// #define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#pragma warning (push, 1)
#pragma warning (disable : 4355 4917)
#include <gsl.h>
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
