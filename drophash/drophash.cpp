#include "stdafx.h"

//---------------------------------------------------------------------------
#define ID_EDIT 1

LRESULT CALLBACK MainWndProc(HWND, UINT, WPARAM, LPARAM);
#define MAINCLASS TEXT("DropHashClass")
#define APPNAME   TEXT("DropHash2017")

//---------------------------------------------------------------------------
static std::wstring text(L"Drop files to hash");

static HCURSOR Wait()
{
    auto cursor = SetCursor(LoadCursor(NULL, IDC_WAIT));
    ShowCursor(TRUE);
    return cursor;
}

static void Unwait(HCURSOR cursor) 
{
    ShowCursor(FALSE);
    SetCursor(cursor);
    ShowCursor(TRUE);
}

// POD-ptr to POD-ptr
template <typename U, typename T>
U* data_cast(T* input) noexcept
{
    static_assert(std::is_pod<U>::value || std::is_void<U>::value,
        "The output must be a trivial type.");
    static_assert(std::is_pod<T>::value,
        "The input must be a trivial type.");


#pragma warning (suppress : 26490) // done in controlled fashion
    return reinterpret_cast<U*>(input);
}

template <typename U>
U* data_cast(void* input) noexcept 
{
    static_assert(std::is_pod<U>::value || std::is_void<U>::value,
        "The output must be a trivial type.");
    return static_cast<U*>(input);
}


// POD-ptrlike to POD-ptrlike
template <typename U, typename T>
U ptr_cast(T input) noexcept
{
    static_assert((std::is_pointer<U>::value && std::is_pod<std::remove_pointer<U>>::value)
        || (std::is_integral<U>::value && sizeof(U) >= sizeof(int*)),
        "The output must be a POD-pointer-like type.");
    static_assert((std::is_pointer<T>::value && std::is_pod<std::remove_pointer<T>>::value)
        || (std::is_integral<T>::value), // int can be expanded
        "The input must be a POD-pointer-like type.");

#pragma warning (suppress : 26490 26471) // done in controlled fashion, some casts from void* can't be static e.g to WPARAM
    return reinterpret_cast<U>(input);
}

// Retrieve the system error message for the last-error code
static DWORD get_error_message(std::wstring & message)
{
    gsl::wzstring<> lpMsgBuf{ nullptr };
    DWORD dw{ GetLastError() };

    if (FormatMessageW(
        FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
#pragma warning (suppress : 26412) // no useful mitigation -- it *is* initialised
        data_cast<wchar_t>(&lpMsgBuf),
        0, NULL))
    {
#pragma warning (suppress : 26412 26499) // no useful mitigation
        auto free_buffer = gsl::finally([&lpMsgBuf]() { LocalFree(lpMsgBuf);});
#pragma warning (suppress : 26401 26413) // can we convince the analyser that this is set?
        message += *lpMsgBuf;
    }
    else
    {
        std::array<wchar_t, 128> buffer{ 0 };
        swprintf_s(&buffer[0], buffer.size(), L"Error %x -- not expanded because %x\n", gsl::narrow<unsigned int>(dw), gsl::narrow<unsigned int>(GetLastError()));
        message += &buffer[0];
    }

    return dw; 
}

static void raise_error_message(std::wstring & message)
{
    DWORD dw{ get_error_message(message) };
    MessageBoxW(NULL, message.c_str(), APPNAME, MB_ICONERROR);
    ExitProcess(dw); 
}

static void format_hex_string(std::vector<BYTE> & buffer, std::wstring & sink)
{
    std::wstringstream stream{};
#pragma warning (suppress : 26444) // Avoid unnamed objects with custom construction and destruction (es.84).
	ranges::v3::for_each(buffer, [&stream] (BYTE x) {
        static int count = 1;
#pragma warning (suppress : 26499) // **this no useful mitigation
        stream << std::setfill(L'0') << std::setw(2) << std::hex << x;
#pragma warning (suppress : 26499) // **this no useful mitigation
        if ((++count)%2) stream << L" ";
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
    wndclass.hCursor = LoadCursorW(NULL, IDC_ARROW);
    wndclass.hbrBackground = ptr_cast<HBRUSH>(GetStockObject(WHITE_BRUSH));
    wndclass.lpszMenuName = nullptr;
    wndclass.lpszClassName = MAINCLASS;

    if(!RegisterClass(&wndclass))
    {
        std::wstring message(L"Could not register main window - ");
        raise_error_message(message);
        return 0;
    }

    auto hmain = CreateWindowExW(WS_EX_ACCEPTFILES,MAINCLASS, APPNAME,
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        NULL, NULL, instance, NULL);
    ShowWindow(hmain, show);
    UpdateWindow(hmain);

    MSG msg{};
    while(GetMessageW(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }
    return msg.wParam != 0;
}

int CALLBACK EnumFontFamiliesExProc(CONST LOGFONTW *lpelfe, CONST TEXTMETRICW *, DWORD, LPARAM lParam )
{
    auto back_channel{ ptr_cast<LOGFONTW*>(lParam) };
#pragma warning (suppress : 26496) // it is marked as const!!
    const auto * const extended{ data_cast<const ENUMLOGFONTEXW>(lpelfe) };

#pragma warning (suppress : 26499) // **extended no useful mitigation
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
#pragma warning (suppress : 26496) // it is marked as const!!
    const LRESULT handled{ 0 };

    switch(message)
    {
        case WM_CREATE:
        {
#pragma warning (suppress : 26499) // no useful mitigation
            client = CreateWindowW(TEXT("edit"), NULL,
                WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL | WS_BORDER | ES_LEFT |
                ES_MULTILINE | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_READONLY,
                0, 0, 0, 0, hwnd,
                ptr_cast<HMENU>(gsl::narrow<size_t>(ID_EDIT)),
                ptr_cast<LPCREATESTRUCT>(lParam)->hInstance,
                NULL);


            {
                // System font for size
                NONCLIENTMETRICSW metrics{ sizeof(NONCLIENTMETRICSW) };
#pragma warning (suppress : 26496) // it is marked as const!!
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

                auto context = GetDC(NULL);
#pragma warning (suppress : 26499) // no useful mitigation
                const auto resetDC = gsl::finally([&context]() {ReleaseDC(NULL, context);});

                LOGFONTW probe{};
                probe.lfFaceName[0] = L'\0';
                probe.lfCharSet = ANSI_CHARSET;

                LOGFONTW result{};
                result.lfFaceName[0] = L'\0';

                // Find a monospace font
                std::array<std::wstring, 4> faces{ L"Inconsolata", L"Consolas", L"Lucida Console", L"Courier New" };

                ranges::v3::find_if(faces, [&result, &probe, &context](std::wstring face) -> bool {
#pragma warning (suppress : 26499) // no useful mitigation
#pragma warning (suppress : 26496) // it is marked as const!!
                    const auto hr = wcscpy_s(&probe.lfFaceName[0], LF_FACESIZE, face.c_str());
                    if (!FAILED(hr))
                    {
#pragma warning (suppress : 26499) // **this
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
#pragma warning (suppress : 26499) // no useful mitigation
                    auto releaseArgs = gsl::finally([&szArglist](){LocalFree(szArglist);});

                    // Skip the executable name
                    auto args = ranges::v3::view::counted(std::next(szArglist), static_cast<ptrdiff_t>(nArgs));

                    // pointer arithmetic, yuck!!
                    SIZE_T wideheader{ (sizeof(DROPFILES) + sizeof(wchar_t) - 1) / sizeof(wchar_t) };
                    SIZE_T header{ wideheader * sizeof(wchar_t) };
                    SIZE_T buffersize{ header };

                    ranges::v3::for_each(args, [&buffersize](gsl::wzstring<> in) {
                        buffersize += sizeof(wchar_t) * (wcslen(in) + 1);
                    });

                    auto hGlobal = GlobalAlloc(GHND | GMEM_SHARE, buffersize);
                    if (hGlobal)
                    {
                        void * memory = GlobalLock(hGlobal);
#pragma warning (suppress : 26499) // no useful mitigation
                        auto unlock = gsl::finally([&hGlobal]() {GlobalUnlock(hGlobal);});

                        gsl::span<wchar_t> characters{
                            data_cast<wchar_t>(memory),
                            gsl::narrow<int>(buffersize / sizeof(wchar_t))
                        };

                        auto pDropFiles = data_cast<DROPFILES>(memory);
                        pDropFiles->pFiles = gsl::narrow<DWORD>(header);
                        pDropFiles->fWide = TRUE;
                        pDropFiles->pt.x = pDropFiles->pt.y = 0;
                        pDropFiles->fNC = FALSE;

                        auto buffer{ characters.begin() };
                        std::advance(buffer, wideheader);
                        auto end{ characters.end() };

                        ranges::v3::for_each(args, [&buffer, &end](gsl::wzstring<> in) {
                            wcscpy_s(&buffer[0], gsl::narrow<rsize_t>(end - buffer), in);
                            std::advance(buffer, (wcslen(in) + 1));
                        });

                        *buffer = L'\0';

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
#pragma warning (suppress : 26493) // standard casting macros
            MoveWindow(client, 0, 0, LOWORD(lParam),
                HIWORD(lParam), TRUE);
            return handled;
        }

        case WM_DROPFILES:
        {
            auto cursor = Wait();
#pragma warning (suppress : 26499) // no useful mitigation
            auto unwait = gsl::finally([&cursor]() {Unwait(cursor);});
            auto render = gsl::finally([] () { 
                        text += L"\r\n";
                        SetWindowText(client, text.c_str()); 
            });

            auto drop = ptr_cast<HDROP>(wParam);
#pragma warning (suppress : 26499) // no useful mitigation
            auto finishDrag = gsl::finally([&drop]() { DragFinish(drop); });

            UINT nfiles = DragQueryFileW(drop, 0xFFFFFFFF, nullptr, 0);

            text += L"Dropped "+ std::to_wstring(nfiles) + 
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
            auto releaseContext = gsl::finally([&hProv]() {CryptReleaseContext(hProv, 0);});

            // Now hash each file in turn
            for(UINT i = 0; i < nfiles; ++i)
            {
                UINT length = DragQueryFileW(drop, i, nullptr, 0);
                std::vector<wchar_t> fn(length + 1);
                DragQueryFileW(drop, i, &fn[0], gsl::narrow<UINT>(fn.size()));

                typedef std::tuple<std::wstring, DWORD, DWORD> Recipe;
                typedef std::tuple<std::wstring, HCRYPTHASH, DWORD> Record;
                bool is_good = true;

                std::vector<Recipe> inputs;
                inputs.push_back( std::make_tuple(L"MD5     ", CALG_MD5, 16 ));
                inputs.push_back( std::make_tuple(L"SHA     ", CALG_SHA1, 20 ));
                inputs.push_back( std::make_tuple(L"SHA2-256", CALG_SHA_256, 32 ));
                // ... add more hash algorithms here e.g. CALG_SHA_384 or CALG_SHA_512

                auto results = ranges::v3::action::transform(inputs, [&hProv] (const Recipe &in) -> Record {
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

#pragma warning (suppress : 26493) // implicit 'C'-style cast
                auto releaseResults = gsl::finally([&results]() {for (auto item : results)
                {
                    auto hash = std::get<1>(item);
                    if (hash) { CryptDestroyHash(hash); }
                }});

                text += &fn[0];
                text += L"\r\n";

#pragma warning (suppress : 26493) // implicit 'C'-style cast
                std::ifstream file{ &fn[0], std::ios::in | std::ios::binary };
                std::array<char, 4096> chunk{};
                if (file.is_open())
                {

                    for(;;)
                    {
                        file.read(&chunk[0], gsl::narrow<std::streamsize>(chunk.size()));

                        DWORD got = gsl::narrow<DWORD>(file.gcount());
                        auto chunkBYTEs{ gsl::as_span<BYTE>(gsl::as_bytes(gsl::as_span(chunk))) };

#pragma warning (suppress : 26496) // it is marked as const!!
                        const auto check =
                            ranges::v3::find_if(results, [&chunkBYTEs, &got] (Record hash) -> bool {
                            auto handle = std::get<1>(hash);
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
#pragma warning (suppress : 26444) // Avoid unnamed objects with custom construction and destruction (es.84).
					ranges::v3::for_each(results, [] (Record hash) {
                        DWORD hash_size = std::get<2>(hash);
                        std::vector<BYTE> buffer(hash_size);
                        auto handle = std::get<1>(hash);
                        if (handle)
                        {
                            if(CryptGetHashParam(handle, HP_HASHVAL, &buffer[0], &hash_size, 0))
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
