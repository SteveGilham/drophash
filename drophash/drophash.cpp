#include "stdafx.h"

//---------------------------------------------------------------------------
#define ID_EDIT 1

LRESULT CALLBACK MainWndProc(HWND, UINT, WPARAM, LPARAM);
#define MAINCLASS TEXT("DropHashClass")
#define APPNAME   TEXT("DropHash2012")

//---------------------------------------------------------------------------
class DropFinisher : private boost::noncopyable {
private:
    HDROP drop;
public:
    DropFinisher(HDROP _drop) : drop(_drop) {}
    HDROP get(void) const { return drop; }
    ~DropFinisher() { DragFinish(drop); }
};

class StringFinisher : private boost::noncopyable {
private:
    HWND client;
    std::wstring & text;
public:
    StringFinisher(HWND _client, std::wstring & _text) : client(_client), text(_text) {}
    std::wstring & get(void) const { return text; }
    ~StringFinisher() { 
        text += L"\r\n";
        SetWindowText(client, text.c_str()); 
    }
};

class ScopedCrypto : private boost::noncopyable {
private:
    HCRYPTPROV prov;
public:
    ScopedCrypto(HCRYPTPROV _prov) : prov(_prov) {}
    HCRYPTPROV get(void) const { return prov; }
    ~ScopedCrypto() { CryptReleaseContext(prov, 0); }
};

class ScopedHash : private boost::noncopyable {
private:
    HCRYPTHASH hash;
public:
    ScopedHash(HCRYPTHASH _hash) : hash(_hash) {}
    HCRYPTHASH get(void) const { return hash; }
    ~ScopedHash() { 
        if (hash)
        {
            CryptDestroyHash(hash);
        } 
    }
};

class WaitCursor : private boost::noncopyable {
private:
    HCURSOR cursor;
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

class ScopedDc : private boost::noncopyable {
private:
    HDC context;
public:
    ScopedDc() { 
        context = GetDC( NULL ); 
    }
    HDC get(void) const { return context; }
    ~ScopedDc() { ReleaseDC( NULL, context ); }
};

template<typename T> class LocalFreed : private boost::noncopyable {
private:
    T * value;
public: 
    LocalFreed(T * _value) : value(_value) {}
    T * get(void) const { return value; }
    ~LocalFreed() {
        if (value) 
        {
            LocalFree(value);
        }
    }
};

class GlobalLocked : private boost::noncopyable {
private:
    HGLOBAL hGlobal;
    void * value;
public:
    GlobalLocked(HGLOBAL _hGlobal) : hGlobal(_hGlobal) {
        value = GlobalLock(hGlobal);
    }
    void * get(void) const { return value; }
    ~GlobalLocked() {
        GlobalUnlock(hGlobal);
    }
};

//---------------------------------------------------------------------------

static DWORD get_error_message(std::wstring & message)
{
    // Retrieve the system error message for the last-error code

    LPVOID lpMsgBuf;
    DWORD dw = GetLastError(); 

    FormatMessageW(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | 
        FORMAT_MESSAGE_FROM_SYSTEM |
        FORMAT_MESSAGE_IGNORE_INSERTS,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) &lpMsgBuf,
        0, NULL );

    LocalFreed<void> buffer(lpMsgBuf);
    message += reinterpret_cast<wchar_t *>(buffer.get());
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
    boost::wformat hex(L"%02x");
    boost::for_each(buffer, [&hex, &sink] (BYTE x) {
        static int count = 1;
        hex % x;
        sink += hex.str();  
        if ((++count)%2) sink += L" ";
    });
}

//---------------------------------------------------------------------------
int WINAPI WinMain(HINSTANCE instance, HINSTANCE, LPSTR, int show)
{
    WNDCLASS wndclass;
    wndclass.style = CS_HREDRAW | CS_VREDRAW;
    wndclass.lpfnWndProc = reinterpret_cast<WNDPROC>(MainWndProc);
    wndclass.cbClsExtra =
    wndclass.cbWndExtra = 0;
    wndclass.hInstance = instance;
    wndclass.hIcon = LoadIconW(instance, MAKEINTRESOURCE(IDI_DROPHASH));
    wndclass.hCursor = LoadCursorW(NULL, IDC_ARROW);
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
        auto back_channel = reinterpret_cast<LOGFONTW*>(lParam);
        auto found = reinterpret_cast<LOGFONTW*>(lpelfe);

        if (!wcscmp(lpelfe->elfStyle, L"Regular"))
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

    switch(message)
    {
        case WM_CREATE:
        client = CreateWindowW(TEXT("edit"), NULL,
            WS_CHILD|WS_VISIBLE|WS_HSCROLL|WS_VSCROLL|WS_BORDER|ES_LEFT|
            ES_MULTILINE|ES_AUTOHSCROLL|ES_AUTOVSCROLL|ES_READONLY,
            0,0,0,0, hwnd, 
            reinterpret_cast<HMENU>(ID_EDIT),
            reinterpret_cast<LPCREATESTRUCT>(lParam)->hInstance,
            NULL);

            
        {
            // System font for size
            NONCLIENTMETRICSW metrics = { sizeof(NONCLIENTMETRICSW) };
            BOOL status = SystemParametersInfoW(
                SPI_GETNONCLIENTMETRICS,
                metrics.cbSize,
                reinterpret_cast<void*>(&metrics),
                0);
            if (!status)
            {
                std::wstring message(L"Could not get system parameter info - ");
                raise_error_message(message);
                return 0;
            }
            
            ScopedDc context;

            LOGFONTW probe;
            probe.lfFaceName[0] = L'\0';
            probe.lfCharSet = ANSI_CHARSET;

            LOGFONTW result;
            result.lfFaceName[0] = L'\0';

            // Find a monospace font
            std::wstring faces[4] = { L"Inconsolata", L"Consolas", L"Lucida Console", L"Courier New" };

            boost::find_if(faces, [&result, &probe, &context] (std::wstring face) -> bool {
                HRESULT hr = StringCchCopyW(probe.lfFaceName, LF_FACESIZE, face.c_str() );
                if (!FAILED(hr))
                {
                    EnumFontFamiliesExW( context.get(), &probe, reinterpret_cast<FONTENUMPROC>(EnumFontFamiliesExProc), reinterpret_cast<LPARAM>(&result), 0 );
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
                &result:                
                &metrics.lfMessageFont); 

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
                LocalFreed<LPWSTR> buffer(szArglist);

                // Skip the executable name
                std::vector<LPWSTR> args(szArglist+1, szArglist + nArgs);

                // pointer arithmetic, yuck!!
                SIZE_T wideheader = (sizeof(DROPFILES) + sizeof(wchar_t) - 1)/sizeof(wchar_t);
                SIZE_T header = wideheader *sizeof(wchar_t);
                SIZE_T buffersize = header;

                boost::for_each(args, [&buffersize] (LPWSTR in) {
                    buffersize += sizeof(wchar_t) * (wcslen(in) + 1);
                });

                HGLOBAL hGlobal = GlobalAlloc(GHND | GMEM_SHARE, buffersize);
                if (hGlobal)
                {
                    {
                        GlobalLocked memory(hGlobal);

                        LPDROPFILES pDropFiles = reinterpret_cast<LPDROPFILES>(memory.get());
                        pDropFiles->pFiles = header;
                        pDropFiles->fWide = TRUE;
                        pDropFiles->pt.x = pDropFiles->pt.y = 0;
                        pDropFiles->fNC = FALSE;

                        wchar_t * buffer = reinterpret_cast<wchar_t *>(memory.get()) + wideheader;
                        wchar_t * end = reinterpret_cast<wchar_t *>(memory.get()) + (buffersize / sizeof(wchar_t));

                        boost::for_each(args, [&buffer, &end] (LPWSTR in) {
                            wcscpy_s(buffer, end-buffer, in);
                            buffer += (wcslen(in) + 1);
                        });

                        *buffer = L'\0';
                    }

                    // send DnD event
                    PostMessage(hwnd, WM_DROPFILES, (WPARAM)hGlobal, 0);
                }
            }
        }

        return 0;

        case WM_SETFOCUS:
        SetFocus(client);
        return 0;

        case WM_SIZE:
        MoveWindow(client, 0,0, LOWORD(lParam),
            HIWORD(lParam), TRUE);
        return 0;

        case WM_DROPFILES:
        {
            WaitCursor waiter;
            waiter;
            StringFinisher data(client, text);
            DropFinisher drop(reinterpret_cast<HDROP>(wParam));
            UINT nfiles = DragQueryFileW(drop.get(), 0xFFFFFFFF, nullptr, 0);

            data.get() += L"Dropped "+ boost::lexical_cast<std::wstring>(nfiles) + 
                   L" files\r\n";


            // Get handle to the crypto provider
            HCRYPTPROV hProv = 0;
            if (!CryptAcquireContextW(&hProv,
                    nullptr,
                    nullptr,
                    PROV_RSA_AES,
                    CRYPT_VERIFYCONTEXT))
            {
                data.get() += L"CryptAcquireContext failed: " + boost::lexical_cast<std::wstring>(GetLastError());
                return 0;
            }

            // and manage its lifetime
            ScopedCrypto provider(hProv);

            // Now hash each file in turn
            for(UINT i = 0; i < nfiles; ++i)
            {
                UINT length = DragQueryFileW(drop.get(), i, nullptr, 0);
                std::vector<wchar_t> fn(length + 1);
                DragQueryFileW(drop.get(), i, &fn[0], fn.size());

                typedef boost::tuple<std::wstring, DWORD, DWORD> Recipe;
                typedef boost::tuple<std::wstring, std::shared_ptr<ScopedHash>, DWORD> Record;
                bool is_good = true;

                std::vector<Recipe> inputs;
                inputs.push_back( boost::make_tuple(L"MD5     ", CALG_MD5, 16 ));
                inputs.push_back( boost::make_tuple(L"SHA     ", CALG_SHA1, 20 ));
                inputs.push_back( boost::make_tuple(L"SHA2-256", CALG_SHA_256, 32 ));
                // ... add more hash algorithms here e.g. CALG_SHA_384 or CALG_SHA_512

                std::vector<Record> results;
                results.resize(inputs.size());  

                boost::transform(inputs, results.begin(), [&provider, &data] (Recipe in) -> Record {
                    HCRYPTHASH hHash = 0;

                    if (!CryptCreateHash(provider.get(), in.get<1>(), 0, 0, &hHash))
                    {
                        std::wstring message = L"CryptCreateHash " + in.get<0>() + L" failed: ";
                        get_error_message(message);
                        data.get() += message;
                        hHash = 0;
                    }

                    ScopedHash * ptr = new ScopedHash(hHash);
                    return boost::make_tuple(in.get<0>(), std::tr1::shared_ptr<ScopedHash>(ptr), in.get<2>());
                });

                data.get() += &fn[0];
                data.get() += L"\r\n";

                std::ifstream file(&fn[0], std::ios::in|std::ios::binary);
                std::vector<char> chunk(4096);
                if (file.is_open())
                {

                    for(;;)
                    {
                        file.read(&chunk[0], chunk.size());

                        DWORD got = boost::numeric_cast<DWORD>(file.gcount());

                        auto check = 
                            boost::find_if(results, [&data, &chunk, &got] (Record hash) -> bool {
                            if (!hash.get<1>()->get() || CryptHashData(hash.get<1>()->get(), reinterpret_cast<BYTE*>(&chunk[0]), got, 0))
                            {
                                return false;
                            }

                            std::wstring message = L"CryptHashData " + hash.get<0>() + L" failed: ";
                            get_error_message(message);
                            data.get() += message;
                            data.get() += L"\r\n";
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
                    boost::for_each(results, [&data] (Record hash) {
                        DWORD hash_size = hash.get<2>();
                        std::vector<BYTE> buffer(hash_size);
                        if (hash.get<1>()->get())
                        {
                            if(CryptGetHashParam(hash.get<1>()->get(), HP_HASHVAL, &buffer[0], &hash_size, 0))
                            {
                                data.get() += hash.get<0>() + L": ";
                                format_hex_string(buffer, data.get());
                            }
                            else
                            {
                                std::wstring message = L"CryptGetHashParam " + hash.get<0>() + L" failed: ";
                                get_error_message(message);
                                data.get() += message;
                            }
                            data.get() += L"\r\n";
                        }
                    });
                } // file opened

                data.get() += L"\r\n";

            }

            return 0;
        }


        case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProc(hwnd, message, wParam, lParam);
}
