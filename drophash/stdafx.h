// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//

#pragma once


// #define WIN32_LEAN_AND_MEAN             // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#pragma warning (disable : 4995)
#include <windows.h>
#include <Strsafe.h>

#include <algorithm>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <vector>

#include "Resource.h"

#include "boost/cast.hpp"
#include "boost/format.hpp"
#include "boost/lexical_cast.hpp"
#include "boost/noncopyable.hpp"
#include "boost/range/algorithm.hpp"
#include "boost/tuple/tuple.hpp"
#pragma warning (default : 4995)
