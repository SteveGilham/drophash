#include "stdafx.h"

//---------------------------------------------------------------------------
#define ID_EDIT 1

LRESULT CALLBACK MainWndProc(HWND, UINT, WPARAM, LPARAM);
#define MAINCLASS TEXT("DropHashClass")
#define APPNAME   TEXT("DropHash2012")

//---------------------------------------------------------------------------
template<class _Ty> class disposable {
private:
    template<class _Uy> class disposalbase
    {
        disposalbase(const disposalbase &) = delete;
        disposalbase & operator= (const disposalbase &) = delete;
    public:
        _Uy object;
        disposalbase(_Uy _Ut) : object{ _Ut } {}
        virtual ~disposalbase() {}
    };
    template<class _Uy,
        class _Dy> class disposer  : public disposalbase<_Uy> {
        _Dy deleter;
        public:
            disposer(_Uy _Ut, _Dy _Dt) : disposalbase{ _Ut }, deleter{ _Dt } {}
            ~disposer() { deleter(object); }
#pragma warning(suppress : 4820)
    };
    std::unique_ptr<disposalbase<_Ty>> disposal;

    disposable(const disposable &) = delete;
    disposable & operator= (const disposable &) = delete;

public:
    template<class _Ty, class _Dx>
#pragma warning (suppress : 4868 26424) // evaluation of arguments is not order dependent; ownership is immediate
    disposable(_Ty _X, _Dx _Dt) : disposal{ new disposer<_Ty, _Dx>{ _X, _Dt } } {}
    _Ty operator()(void) const { return disposal->object; }
    ~disposable() { }
};

class WaitCursor {
private:
    HCURSOR cursor;
    WaitCursor(const WaitCursor &) = delete;
    WaitCursor & operator= (const WaitCursor &) = delete;
public:
    WaitCursor() { 
        cursor = SetCursor(LoadCursor(NULL, IDC_WAIT));
        ShowCursor(TRUE); 
    }
    ~WaitCursor() { 
        ShowCursor(FALSE); 
        SetCursor(cursor);
        ShowCursor(TRUE); 
    }
};

class GlobalLocked {
private:
    HGLOBAL hGlobal;
    void * value;
    GlobalLocked(const GlobalLocked &) = delete;
    GlobalLocked & operator= (const GlobalLocked &) = delete;
public:
    GlobalLocked(HGLOBAL _hGlobal) : hGlobal(_hGlobal) {
        value = GlobalLock(hGlobal);
    }
    void * get(void) const { return value; }
    ~GlobalLocked() {
        GlobalUnlock(hGlobal);
    }
};

// simplifies unique_ptr declaration -- can we get rid of it?
#pragma warning (suppress : 26490 26499) // safe reinterpret_cast to void*
template<typename T> void local_free(T * value) { LocalFree(reinterpret_cast<HLOCAL>(value)); value = nullptr; }

//---------------------------------------------------------------------------

static DWORD get_error_message(std::wstring & message)
{
    // Retrieve the system error message for the last-error code

    LPVOID lpMsgBuf{ nullptr };
    DWORD dw = GetLastError(); 

#pragma warning (suppress : 26490) // safe reinterpret_cast
    FormatMessageW(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | 
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        reinterpret_cast<LPTSTR>(&lpMsgBuf),
        0, NULL );

#pragma warning (suppress : 26490) // safe reinterpret_cast
    std::unique_ptr<wchar_t, void(__cdecl*)(wchar_t*)> buffer(
        reinterpret_cast<wchar_t *>(lpMsgBuf), local_free<wchar_t>);

    message += buffer.get();
    return dw; 
}

static void raise_error_message(std::wstring & message)
{
    DWORD dw = get_error_message(message);
    MessageBoxW(NULL, message.c_str(), APPNAME, MB_ICONERROR);
    ExitProcess(dw); 
}

static void format_hex_string(std::vector<BYTE> & buffer, std::wstring & sink)
{
    std::wstringstream stream;
    std::for_each(buffer.begin(), buffer.end(), [&stream] (BYTE x) {
        static int count = 1;
        stream << std::setfill(L'0') << std::setw(2) << std::hex << x;
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
    WNDCLASS wndclass;
    wndclass.style = CS_HREDRAW | CS_VREDRAW;
    wndclass.lpfnWndProc = reinterpret_cast<WNDPROC>(MainWndProc);
    wndclass.cbClsExtra =
    wndclass.cbWndExtra = 0;
    wndclass.hInstance = instance;
    wndclass.hIcon = LoadIconW(instance, MAKEINTRESOURCE(IDI_DROPHASH));
    wndclass.hCursor = LoadCursorW(NULL, IDC_ARROW);
#pragma warning (suppress : 26490) // safe reinterpret_cast
    wndclass.hbrBackground = reinterpret_cast<HBRUSH>(GetStockObject(WHITE_BRUSH));
    wndclass.lpszMenuName = nullptr;
    wndclass.lpszClassName = MAINCLASS;

    if(!RegisterClass(&wndclass))
    {
        std::wstring message(L"Could not register main window - ");
        raise_error_message(message);
        return 0;
    }

    HWND hmain = CreateWindowExW(WS_EX_ACCEPTFILES,MAINCLASS, APPNAME,
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        NULL, NULL, instance, NULL);
    ShowWindow(hmain, show);
    UpdateWindow(hmain);

    MSG msg;
    while(GetMessageW(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessageW(&msg);
    }
    return msg.wParam;
}

int CALLBACK EnumFontFamiliesExProc( ENUMLOGFONTEXW *lpelfe, NEWTEXTMETRICEXW *lpntme, int FontType, LPARAM lParam )
{
#pragma warning (suppress : 26490) // safe reinterpret_cast
    auto back_channel = reinterpret_cast<LOGFONTW*>(lParam);
#pragma warning (suppress : 26490) // safe reinterpret_cast
    auto found = reinterpret_cast<LOGFONTW*>(lpelfe);
#pragma warning (suppress : 26499)
        if (!wcscmp(&lpelfe->elfStyle[0], L"Regular"))
        {
            *back_channel = *found;
        }

        return !(back_channel->lfFaceName[0]);
}

LRESULT CALLBACK MainWndProc(HWND hwnd, UINT message,
                WPARAM wParam, LPARAM lParam)
{
    static HWND client;
    static std::wstring text(L"Drop files to hash");
    const LRESULT handled{ 0 };

    switch(message)
    {
        case WM_CREATE:
        {
#pragma warning (suppress : 26490 26425 26499) // safe reinterpret_cast; deliberate assignment to static
            client = CreateWindowW(TEXT("edit"), NULL,
                WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL | WS_BORDER | ES_LEFT |
                ES_MULTILINE | ES_AUTOHSCROLL | ES_AUTOVSCROLL | ES_READONLY,
                0, 0, 0, 0, hwnd,
                reinterpret_cast<HMENU>(ID_EDIT),
                reinterpret_cast<LPCREATESTRUCT>(lParam)->hInstance,
                NULL);


            {
                // System font for size
                NONCLIENTMETRICSW metrics = { sizeof(NONCLIENTMETRICSW) };
#pragma warning (suppress : 26490) // safe reinterpret_cast
                BOOL status = SystemParametersInfoW(
                    SPI_GETNONCLIENTMETRICS,
                    metrics.cbSize,
                    reinterpret_cast<void*>(&metrics),
                    0);
                if (!status)
                {
                    std::wstring message(L"Could not get system parameter info - ");
                    raise_error_message(message);
                    return handled;
                }

                disposable<HDC> context{ GetDC(NULL), [](HDC dc) {ReleaseDC(NULL, dc);} };

                LOGFONTW probe;
                probe.lfFaceName[0] = L'\0';
                probe.lfCharSet = ANSI_CHARSET;

                LOGFONTW result;
                result.lfFaceName[0] = L'\0';

                // Find a monospace font
                std::array<std::wstring, 4> faces{ L"Inconsolata", L"Consolas", L"Lucida Console", L"Courier New" };

                std::find_if(faces.begin(), faces.end(), [&result, &probe, &context](std::wstring face) -> bool {
#pragma warning (suppress : 26499)
                    HRESULT hr = wcscpy_s(&probe.lfFaceName[0], LF_FACESIZE, face.c_str());
                    if (!FAILED(hr))
                    {
#pragma warning (suppress : 26490) // safe reinterpret_cast
                        EnumFontFamiliesExW(context(), &probe, reinterpret_cast<FONTENUMPROC>(EnumFontFamiliesExProc), reinterpret_cast<LPARAM>(&result), 0);
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
                HFONT hfont = CreateFontIndirectW(
                    result.lfFaceName[0] ?
                    &result :
                    &metrics.lfMessageFont);

#pragma warning (suppress : 26490) // safe reinterpret_cast
                SendMessageW(client, WM_SETFONT,
                    reinterpret_cast<WPARAM>(hfont),
                    0);
            }


            SetWindowText(client, text.c_str());
            text.clear();

            // Synthetic drop on activation
            {
                int nArgs;

                LPWSTR *szArglist = CommandLineToArgvW(GetCommandLineW(), &nArgs);
                if (szArglist && nArgs > 1)
                {
                    std::unique_ptr<LPWSTR, void(__cdecl*)(LPWSTR*)> buffer(
                        szArglist, local_free<LPWSTR>);

                    // Skip the executable name
                    gsl::span<LPWSTR> args{ std::next(szArglist), std::next(szArglist, nArgs) };

                    // pointer arithmetic, yuck!!
                    SIZE_T wideheader = (sizeof(DROPFILES) + sizeof(wchar_t) - 1) / sizeof(wchar_t);
                    SIZE_T header = wideheader * sizeof(wchar_t);
                    SIZE_T buffersize = header;

                    std::for_each(args.begin(), args.end(), [&buffersize](LPWSTR in) {
                        buffersize += sizeof(wchar_t) * (wcslen(in) + 1);
                    });

                    HGLOBAL hGlobal = GlobalAlloc(GHND | GMEM_SHARE, buffersize);
                    if (hGlobal)
                    {
                        {
                            GlobalLocked memory(hGlobal);

#pragma warning (suppress : 26490) // safe reinterpret_cast
                            LPDROPFILES pDropFiles = reinterpret_cast<LPDROPFILES>(memory.get());
                            pDropFiles->pFiles = header;
                            pDropFiles->fWide = TRUE;
                            pDropFiles->pt.x = pDropFiles->pt.y = 0;
                            pDropFiles->fNC = FALSE;

#pragma warning (suppress : 26490) // safe reinterpret_cast
                            wchar_t * buffer{ std::next(reinterpret_cast<wchar_t *>(memory.get()), wideheader) };
#pragma warning (suppress : 26490) // safe reinterpret_cast
                            wchar_t * end{ std::next(reinterpret_cast<wchar_t *>(memory.get()), (buffersize / sizeof(wchar_t))) };

                            std::for_each(args.begin(), args.end(), [&buffer, &end](LPWSTR in) {
#pragma warning (suppress : 26499)
                                wcscpy_s(buffer, end - buffer, in);
                                std::advance(buffer, (wcslen(in) + 1));
                            });

#pragma warning (suppress : 26401) // buffer is not set to nullptr above despite what the rule claims
                            *buffer = L'\0';
                        }

                        // send DnD event
#pragma warning (suppress : 26490) // safe reinterpret_cast
                        PostMessage(hwnd, WM_DROPFILES, reinterpret_cast<WPARAM>(hGlobal), 0);
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
            WaitCursor waiter{};

            HWND canvas = client;
            auto lambda = [canvas] (std::wstring * str) { 
                        *str += L"\r\n";
                        SetWindowText(canvas, str->c_str()); 
                        };
            std::unique_ptr<std::wstring, decltype(lambda)> data(&text, lambda);

#pragma warning (suppress : 26490) // safe reinterpret_cast
            disposable<HDROP> drop{ reinterpret_cast<HDROP>(wParam), &DragFinish };

            UINT nfiles = DragQueryFileW(drop(), 0xFFFFFFFF, nullptr, 0);

            *data += L"Dropped "+ std::to_wstring(nfiles) + 
                   L" files\r\n";

            // Get handle to the crypto provider
            HCRYPTPROV hProv = 0;
            if (!CryptAcquireContextW(&hProv,
                    nullptr,
                    nullptr,
                    PROV_RSA_AES,
                    CRYPT_VERIFYCONTEXT))
            {
                *data += L"CryptAcquireContext failed: " + std::to_wstring(GetLastError());
                return handled;
            }

            // and manage its lifetime
            disposable<HCRYPTPROV> provider{ hProv, [](HCRYPTPROV prov) {CryptReleaseContext(prov, 0);} };

            // Now hash each file in turn
            for(UINT i = 0; i < nfiles; ++i)
            {
                UINT length = DragQueryFileW(drop(), i, nullptr, 0);
                std::vector<wchar_t> fn(length + 1);
                DragQueryFileW(drop(), i, &fn[0], gsl::narrow<UINT>(fn.size()));

                typedef std::tuple<std::wstring, DWORD, DWORD> Recipe;
                typedef std::tuple<std::wstring, HCRYPTHASH, DWORD> Record;
                bool is_good = true;

                std::vector<Recipe> inputs;
                inputs.push_back( std::make_tuple(L"MD5     ", CALG_MD5, 16 ));
                inputs.push_back( std::make_tuple(L"SHA     ", CALG_SHA1, 20 ));
                inputs.push_back( std::make_tuple(L"SHA2-256", CALG_SHA_256, 32 ));
                // ... add more hash algorithms here e.g. CALG_SHA_384 or CALG_SHA_512

                std::vector<Record> results;
                results.resize(inputs.size());
                auto sink = results.begin();

                std::transform(inputs.begin(), inputs.end(), results.begin(), [&hProv, &data] (Recipe in) -> Record {
                    HCRYPTHASH hHash = 0;

                    if (!CryptCreateHash(hProv, std::get<1>(in), 0, 0, &hHash))
                    {
                        std::wstring message = L"CryptCreateHash " + std::get<0>(in) + L" failed: ";
                        get_error_message(message);
                        *data += message;
                        hHash = 0;
                    }

                    return std::make_tuple(
                        std::get<0>(in), 
                        hHash,
                        std::get<2>(in));
                });

                disposable<std::vector<Record>*> hashes{ &results, [](std::vector<Record> * vec) {
                    for (auto item : *vec)
                    {
                        auto hash = std::get<1>(item);
                        if (hash) { CryptDestroyHash(hash); }
                    }} };

                *data += &fn[0];
                *data += L"\r\n";

                std::ifstream file(&fn[0], std::ios::in|std::ios::binary);
                std::vector<char> chunk(4096);
                if (file.is_open())
                {

                    for(;;)
                    {
                        file.read(&chunk[0], gsl::narrow<std::streamsize>(chunk.size()));

                        DWORD got = gsl::narrow<DWORD>(file.gcount());

                        auto check = 
                            std::find_if(results.begin(), results.end(), [&data, &chunk, &got] (Record hash) -> bool {
                            auto handle = std::get<1>(hash);
#pragma warning (suppress : 26490) // safe reinterpret_cast
                            if (!handle || CryptHashData(handle, reinterpret_cast<BYTE*>(&chunk[0]), got, 0))
                            {
                                return false;
                            }

                            std::wstring message = L"CryptHashData " + std::get<0>(hash) + L" failed: ";
                            get_error_message(message);
                            *data += message;
                            *data += L"\r\n";
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
                    std::for_each(results.begin(), results.end(), [&data] (Record hash) {
                        DWORD hash_size = std::get<2>(hash);
                        std::vector<BYTE> buffer(hash_size);
                        auto handle = std::get<1>(hash);
                        if (handle)
                        {
                            if(CryptGetHashParam(handle, HP_HASHVAL, &buffer[0], &hash_size, 0))
                            {
                                *data += std::get<0>(hash) + L": ";
                                format_hex_string(buffer, *data.get());
                            }
                            else
                            {
                                std::wstring message = L"CryptGetHashParam " + std::get<0>(hash) + L" failed: ";
                                get_error_message(message);
                                *data += message;
                            }
                            *data += L"\r\n";
                        }
                    });
                } // file opened

                *data += L"\r\n";

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
