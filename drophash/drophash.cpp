#include "stdafx.h"

//---------------------------------------------------------------------------
#define ID_EDIT 1

LRESULT CALLBACK MainWndProc(HWND, UINT, WPARAM, LPARAM);
#define MAINCLASS TEXT("DropHashClass")
#define APPNAME   TEXT("DropHash2020")

//---------------------------------------------------------------------------
static std::wstring text(L"Drop files to hash");

static HCURSOR Wait() noexcept
{
  auto cursor = SetCursor(LoadCursor(nullptr, IDC_WAIT));
  ShowCursor(TRUE);
  return cursor;
}

static void Unwait(HCURSOR cursor) noexcept
{
  ShowCursor(FALSE);
  SetCursor(cursor);
  ShowCursor(TRUE);
}

// POD-ptr to POD-ptr
template <typename U, typename T>
U* data_cast(T* input) noexcept
{
  static_assert(std::is_trivially_copyable<U>::value || std::is_void<U>::value,
    "The output must be a trivial type.");
  static_assert(std::is_trivially_copyable<T>::value,
    "The input must be a trivial type.");

#pragma warning (suppress : 26490) // done in controlled fashion
  return reinterpret_cast<U*>(input);
}

template <typename U>
U* data_cast(void* input) noexcept
{
  static_assert(std::is_trivially_copyable<U>::value || std::is_void<U>::value,
    "The output must be a trivial type.");
  return static_cast<U*>(input);
}

// POD-ptrlike to POD-ptrlike
template <typename U, typename T>
U ptr_cast(T input) noexcept
{
  static_assert((std::is_pointer<U>::value && std::is_trivially_copyable<std::remove_pointer<U>>::value)
    || (std::is_integral<U>::value && sizeof(U) >= sizeof(int*)),
    "The output must be a POD-pointer-like type.");
  static_assert((std::is_pointer<T>::value && std::is_trivially_copyable<std::remove_pointer<T>>::value)
    || (std::is_integral<T>::value), // int can be expanded
    "The input must be a POD-pointer-like type.");

#pragma warning (suppress : 26490 26471) // done in controlled fashion, some casts from void* can't be static e.g to WPARAM
  return reinterpret_cast<U>(input);
}

// Retrieve the system error message for the last-error code
static DWORD get_error_message(std::wstring& message)
{
#pragma warning(suppress: 26429) //What is this smoking? Symbol 'lpMsgBuf' is never tested for nullness, it can be marked as not_null (f.23).
  gsl::wzstring<> lpMsgBuf{ nullptr };
  const DWORD dw{ GetLastError() };

  if (FormatMessageW(
    FORMAT_MESSAGE_ALLOCATE_BUFFER |
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS,
    nullptr,
    dw,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    data_cast<wchar_t>(&lpMsgBuf),
    0, NULL))
  {
    auto free_buffer = gsl::finally([&lpMsgBuf]() noexcept { LocalFree(lpMsgBuf); });
    message += *lpMsgBuf;
  }
  else
  {
    std::array<wchar_t, 128> buffer{ 0 };
    swprintf_s(&gsl::at(buffer, 0), buffer.size(), L"Error %x -- not expanded because %x\n", gsl::narrow<unsigned int>(dw), gsl::narrow<unsigned int>(GetLastError()));
    message += &gsl::at(buffer, 0);
  }

  return dw;
}

static void raise_error_message(std::wstring& message)
{
  const DWORD dw{ get_error_message(message) };
  MessageBoxW(nullptr, message.c_str(), APPNAME, MB_ICONERROR);
  ExitProcess(dw);
}

static void format_hex_string(std::vector<BYTE>& buffer, std::wstring& sink)
{
  std::wstringstream stream{};
  std::ranges::for_each(buffer, [&stream](BYTE x) {
    static int count = 1;
    stream << std::setfill(L'0') << std::setw(2) << std::hex << x;
    if ((++count) % 2) stream << L" ";
    });

  sink += stream.str();
}

//---------------------------------------------------------------------------
int WINAPI WinMain(_In_ HINSTANCE instance,
  _In_opt_ HINSTANCE,
  _In_ LPSTR,
  _In_ int show)
{
  WNDCLASS wndclass{};
  wndclass.style = CS_HREDRAW | CS_VREDRAW;
  wndclass.lpfnWndProc = MainWndProc;
  wndclass.cbClsExtra =
    wndclass.cbWndExtra = 0;
  wndclass.hInstance = instance;
  wndclass.hIcon = LoadIconW(instance, MAKEINTRESOURCE(IDI_DROPHASH));
  wndclass.hCursor = LoadCursorW(nullptr, IDC_ARROW);
  wndclass.hbrBackground = ptr_cast<HBRUSH>(GetStockObject(WHITE_BRUSH));
  wndclass.lpszMenuName = nullptr;
  wndclass.lpszClassName = MAINCLASS;

  if (!RegisterClass(&wndclass))
  {
    std::wstring message(L"Could not register main window - ");
    raise_error_message(message);
    return 0;
  }

  auto hmain = CreateWindowExW(WS_EX_ACCEPTFILES, MAINCLASS, APPNAME,
    WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT,
    CW_USEDEFAULT,
    CW_USEDEFAULT,
    CW_USEDEFAULT,
    nullptr, nullptr, instance, nullptr);
  ShowWindow(hmain, show);
  UpdateWindow(hmain);

  MSG msg{};
  while (GetMessageW(&msg, nullptr, 0, 0))
  {
    TranslateMessage(&msg);
    DispatchMessageW(&msg);
  }
  return msg.wParam != 0;
}

int CALLBACK EnumFontFamiliesExProc(CONST LOGFONTW* lpelfe, CONST TEXTMETRICW*, DWORD, LPARAM lParam) noexcept
{
  auto back_channel{ ptr_cast<LOGFONTW*>(lParam) };
  const auto* const extended{ data_cast<const ENUMLOGFONTEXW>(lpelfe) };

  if (!wcscmp(&extended->elfStyle[0], L"Regular"))
  {
    *back_channel = *lpelfe;
  }

  return !(back_channel->lfFaceName[0]);
}

LRESULT CALLBACK MainWndProc(HWND hwnd, UINT message,
  WPARAM wParam, LPARAM lParam)
{
  static HWND client;
  constexpr LRESULT handled{ 0L };

#pragma warning (suppress : 26818) //do-nothing implied default is enough
  switch (message)
  {
  case WM_CREATE:
  {
    client = CreateWindowW(TEXT("edit"), nullptr,
      WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL | WS_BORDER | ES_LEFT |
      ES_MULTILINE | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_READONLY,
      0, 0, 0, 0, hwnd,
      ptr_cast<HMENU>(gsl::narrow<size_t>(ID_EDIT)),
      ptr_cast<LPCREATESTRUCT>(lParam)->hInstance,
      nullptr);

    {
      // System font for size
      NONCLIENTMETRICSW metrics{ sizeof(NONCLIENTMETRICSW) };
      const auto status = SystemParametersInfoW(
        SPI_GETNONCLIENTMETRICS,
        metrics.cbSize,
        &metrics,
        0);
      if (!status)
      {
        std::wstring info(L"Could not get system parameter info - ");
        raise_error_message(info);
        return handled;
      }

      auto context = GetDC(nullptr);
      const auto resetDC = gsl::finally([&context]() noexcept {ReleaseDC(nullptr, context); });

      LOGFONTW probe{};
      probe.lfFaceName[0] = L'\0';
      probe.lfCharSet = ANSI_CHARSET;

      LOGFONTW result{};
      result.lfFaceName[0] = L'\0';

      // Find a monospace font
      std::array<std::wstring, 4> faces{ L"Inconsolata", L"Consolas", L"Lucida Console", L"Courier New" };

      const auto ignored =
        std::ranges::find_if(faces, [&result, &probe, &context](std::wstring face) noexcept -> bool {
        const auto hr = wcscpy_s(&probe.lfFaceName[0], LF_FACESIZE, face.c_str());
        if (!FAILED(hr))
        {
          EnumFontFamiliesExW(
            context,
            &probe,
            EnumFontFamiliesExProc,
            ptr_cast<LPARAM>(&result),
            0);
          return !!result.lfFaceName[0];
        }

        return false;
          });

      // copy the sizes over
      if (result.lfFaceName[0])
      {
        result.lfHeight = metrics.lfMessageFont.lfHeight;
        result.lfWidth = metrics.lfMessageFont.lfWidth;
      }

      // And set the monospaced font
      auto hfont = CreateFontIndirectW(
        result.lfFaceName[0] ?
        &result :
        &metrics.lfMessageFont);

      SendMessageW(client, WM_SETFONT,
        ptr_cast<WPARAM>(hfont),
        0);
    }

    SetWindowText(client, text.c_str());
    text.clear();

    // Synthetic drop on activation
    {
      int nArgs{ 0 };

      auto szArglist = CommandLineToArgvW(GetCommandLineW(), &nArgs);
      if (szArglist && nArgs > 1)
      {
        auto releaseArgs = gsl::finally([&szArglist]() noexcept {LocalFree(szArglist); });

        // Skip the executable name
#pragma warning (suppress : 26821) // only used as a range
        auto args = std::ranges::views::counted(std::next(szArglist), static_cast<ptrdiff_t>(nArgs) - 1);

        // pointer arithmetic, yuck!!
        constexpr SIZE_T wideheader{
        (sizeof(DROPFILES) + sizeof(wchar_t) - 1) / sizeof(wchar_t)
        };
        constexpr SIZE_T header{ wideheader * sizeof(wchar_t) };
        SIZE_T buffersize{ header + sizeof(wchar_t) };

        std::ranges::for_each(args, [&buffersize](gsl::wzstring<> in) noexcept {
          buffersize += sizeof(wchar_t) * (wcslen(in) + 1);
          });

        auto hGlobal = GlobalAlloc(GHND | GMEM_SHARE, buffersize);
        if (hGlobal)
        {
          void* memory = GlobalLock(hGlobal);
          auto unlock = gsl::finally([&hGlobal]() noexcept {GlobalUnlock(hGlobal); });

          const gsl::span<wchar_t> characters{
              data_cast<wchar_t>(memory),
              gsl::narrow<int>(buffersize / sizeof(wchar_t))
          };

          auto pDropFiles = data_cast<DROPFILES>(memory);
          pDropFiles->pFiles = gsl::narrow<DWORD>(header);
          pDropFiles->fWide = TRUE;
          pDropFiles->pt.x = pDropFiles->pt.y = 0;
          pDropFiles->fNC = FALSE;

          auto cursor{ characters.begin() }; // mutable write-to point
          std::advance(cursor, wideheader);
          const auto end{ characters.end() }; // fixed buffer end

          std::ranges::for_each(args, [&cursor, &end](gsl::wzstring<> in) {
            wcscpy_s(std::to_address(cursor), gsl::narrow<rsize_t>(end - cursor), in);
            std::advance(cursor, wcslen(in) + 1);
            });

          *cursor = L'\0';

          // send DnD event
          PostMessage(hwnd, WM_DROPFILES, ptr_cast<WPARAM>(hGlobal), 0);
        }
      }
    }

    return handled;
  }

  case WM_SETFOCUS:
  {
    SetFocus(client);
    return handled;
  }

  case WM_SIZE:
  {
    MoveWindow(client, 0, 0, LOWORD(lParam),
      HIWORD(lParam), TRUE);
    return handled;
  }

  case WM_DROPFILES:
  {
    auto cursor = Wait();
    auto unwait = gsl::finally([&cursor]() noexcept {Unwait(cursor); });
    auto render = gsl::finally([]() {
      text += L"\r\n";
      SetWindowText(client, text.c_str());
      });

    auto drop = ptr_cast<HDROP>(wParam);
    auto finishDrag = gsl::finally([&drop]() noexcept { DragFinish(drop); });

    const UINT nfiles = DragQueryFileW(drop, 0xFFFFFFFF, nullptr, 0);

    text += L"Dropped " + std::to_wstring(nfiles) +
      L" files\r\n";

    // Get handle to the crypto provider
    HCRYPTPROV hProv = 0;
    if (!CryptAcquireContextW(&hProv,
      nullptr,
      nullptr,
      PROV_RSA_AES,
      CRYPT_VERIFYCONTEXT))
    {
      text += L"CryptAcquireContext failed: " + std::to_wstring(GetLastError());
      return handled;
    }

    // and manage its lifetime
    auto releaseContext = gsl::finally([&hProv]() noexcept {CryptReleaseContext(hProv, 0); });

    // Now hash each file in turn
    for (UINT i = 0; i < nfiles; ++i)
    {
      const UINT length = DragQueryFileW(drop, i, nullptr, 0) + 1;
      std::vector<wchar_t> fn(length);
      DragQueryFileW(drop, i, &gsl::at(fn, 0), gsl::narrow<UINT>(fn.size()));

      typedef std::tuple<std::wstring, DWORD, DWORD> Recipe;
      typedef std::tuple<std::wstring, HCRYPTHASH, DWORD> Record;
      bool is_good = true;

      std::vector<Recipe> inputs;
      inputs.push_back(std::make_tuple(L"MD5     ", CALG_MD5, 16));
      inputs.push_back(std::make_tuple(L"SHA     ", CALG_SHA1, 20));
      inputs.push_back(std::make_tuple(L"SHA2-256", CALG_SHA_256, 32));
      inputs.push_back(std::make_tuple(L"SHA2-512", CALG_SHA_512, 64));
      // ... add more hash algorithms here e.g. CALG_SHA_384 or CALG_SHA_512

      std::vector<Record> results;
      auto releaseResults = gsl::finally([&results]() noexcept {for (auto& item : results)
      {
        const auto hash = std::get<1>(item);
        if (hash) { CryptDestroyHash(hash); }
      }});
      results.resize(inputs.size());
      std::ranges::transform(inputs, results.begin(), [&hProv](Recipe in) -> Record {
        HCRYPTHASH hHash = 0;

        if (!CryptCreateHash(hProv, std::get<1>(in), 0, 0, &hHash))
        {
          std::wstring message = L"CryptCreateHash " + std::get<0>(in) + L" failed: ";
          get_error_message(message);
          text += message;
          hHash = 0;
        }

        return std::make_tuple(
          std::get<0>(in),
          hHash,
          std::get<2>(in));
        });

      text += &gsl::at(fn, 0);
      text += L"\r\n";

      std::ifstream file{ &gsl::at(fn, 0), std::ios::in | std::ios::binary };
      std::array<char, 4096> chunk{};
      if (file.is_open())
      {
        for (;;)
        {
          file.read(&gsl::at(chunk, 0), gsl::narrow<std::streamsize>(chunk.size()));

          DWORD got = gsl::narrow<DWORD>(file.gcount());
          auto chunkBYTEs{ gsl::as_span<BYTE>(gsl::as_bytes(gsl::as_span(chunk))) };

          const auto check =
            std::ranges::find_if(results, [&chunkBYTEs, &got](Record hash) -> bool {
            const auto handle = std::get<1>(hash);
            if (!handle || CryptHashData(handle, &chunkBYTEs[0], got, 0))
            {
              return false;
            }

            std::wstring message = L"CryptHashData " + std::get<0>(hash) + L" failed: ";
            get_error_message(message);
            text += message;
            text += L"\r\n";
            return true;
              });

          if (check != results.end() || file.rdstate())
          {
            is_good = check == results.end();
            break;
          }
        }

        file.close();
        if (!is_good)
        {
          continue;
        }

        // Format the outputs
        std::ranges::for_each(results, [](Record hash) {
          DWORD hash_size = std::get<2>(hash);
          std::vector<BYTE> buffer(hash_size);
          const auto handle = std::get<1>(hash);
          if (handle)
          {
            if (CryptGetHashParam(handle, HP_HASHVAL, &gsl::at(buffer, 0), &hash_size, 0))
            {
              text += std::get<0>(hash) + L": ";
              format_hex_string(buffer, text);
            }
            else
            {
              std::wstring message = L"CryptGetHashParam " + std::get<0>(hash) + L" failed: ";
              get_error_message(message);
              text += message;
            }
            text += L"\r\n";
          }
          });
      } // file opened

      text += L"\r\n";
    }

    return handled;
  }

  case WM_DESTROY:
  {
    PostQuitMessage(0);
    return handled;
  }
  }

  return DefWindowProc(hwnd, message, wParam, lParam);
}