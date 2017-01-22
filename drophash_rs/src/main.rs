// MSVC build cargo rustc -- -Clink-args="/SUBSYSTEM:WINDOWS /ENTRY:mainCRTStartup src\hello.res"
// the link args are "-Wl,--subsystem,windows" on GCC

extern crate kernel32;
extern crate user32;
extern crate gdi32;
extern crate shell32;
extern crate winapi;
extern crate libc;

use std::io::Write;
use std::iter;

use winapi::windef::HDC;
use winapi::windef::HWND;
use winapi::windef::HMENU;
use winapi::windef::HBRUSH;
use winapi::windef::HCURSOR;
use winapi::minwindef::HINSTANCE;

use winapi::minwindef::BOOL;
use winapi::minwindef::UINT;
use winapi::minwindef::DWORD;
use winapi::minwindef::WPARAM;
use winapi::minwindef::LPARAM;
use winapi::minwindef::LRESULT;
use winapi::minwindef::HGLOBAL;

use winapi::winnt::LPWSTR;
use winapi::winnt::LPCWSTR;

extern "system" {
    pub fn CommandLineToArgvW(lp_cmd_line: LPWSTR, p_num_args: *mut libc::c_int) -> *const LPWSTR;
}

const APPNAME: &'static str = "DropHash2016";

struct Hdc {
    dc: HDC,
}
impl Drop for Hdc {
    fn drop(&mut self) {
        unsafe {
            user32::ReleaseDC(std::ptr::null_mut(), self.dc);
        }
    }
}

struct WaitCursor {
    cursor: HCURSOR,
}

impl WaitCursor {
    pub fn new() -> WaitCursor {
        unsafe {
            let w = WaitCursor {
                cursor: user32::SetCursor(user32::LoadCursorW(std::ptr::null_mut(),
                                                              winapi::winuser::IDC_WAIT)),
            };
            user32::ShowCursor(1);
            return w;
        }
    }
}

impl Drop for WaitCursor {
    fn drop(&mut self) {
        unsafe {
            user32::ShowCursor(0);
            user32::SetCursor(self.cursor);
            user32::ShowCursor(1);
        }
    }
}

struct GlobalLocked<T> {
    h_global: HGLOBAL,
    value: *const T,
}

impl<T> GlobalLocked<T> {
    pub fn new(global: HGLOBAL) -> GlobalLocked<T> {
        unsafe {
            return GlobalLocked {
                h_global: global,
                value: kernel32::GlobalLock(global) as *const T,
            };
        }
    }
}

impl<T> Drop for GlobalLocked<T> {
    fn drop(&mut self) {
        unsafe {
            kernel32::GlobalUnlock(self.h_global);
        }
    }
}

struct LocalFreeable<T> {
    value: *mut T,
}

impl<T> LocalFreeable<T> {
    pub fn new(local: *mut T) -> LocalFreeable<T> {
        return LocalFreeable { value: local };
    }
}

impl<T> Drop for LocalFreeable<T> {
    fn drop(&mut self) {
        unsafe {
            kernel32::LocalFree(self.value as *mut _);
        }
    }
}

// //---------------------------------------------------------------------------

unsafe fn get_error_message(message: &Vec<u16>) -> (DWORD, Vec<u16>) {
    // Retrieve the system error message for the last-error code

    let dw = kernel32::GetLastError();
    let message_buffer: winapi::LPWSTR = std::ptr::null_mut();

    kernel32::FormatMessageW(winapi::FORMAT_MESSAGE_ALLOCATE_BUFFER |
                             winapi::FORMAT_MESSAGE_FROM_SYSTEM |
                             winapi::FORMAT_MESSAGE_IGNORE_INSERTS,
                             std::ptr::null_mut(),
                             dw,
                             winapi::winnt::MAKELANGID(winapi::LANG_NEUTRAL,
                                                       winapi::SUBLANG_DEFAULT) as
                             DWORD,
                             message_buffer,
                             0,
                             std::ptr::null_mut());

    let buffer = LocalFreeable::new(message_buffer);

    // Drop trailing nulls
    let affix = (0..).map(|i| *buffer.value.offset(i)).take_while(|c: &u16| *c != 0);
    let prefix = message.iter().take_while(|c: &&u16| **c != 0).map(|c| *c);

    // concat and null terminate
    let tmp = prefix.chain(affix).chain(iter::once(0)).collect();
    return (dw, tmp);
}

fn widen(in_text: &str) -> Vec<u16> {
    return in_text.encode_utf16().chain(iter::once(0)).collect();
}

unsafe fn raise_error_message(message: &Vec<u16>) {
    let (dw, message_x) = get_error_message(message);
    user32::MessageBoxW(std::ptr::null_mut(),
                        message_x.as_ptr(),
                        widen(APPNAME).as_ptr(),
                        winapi::MB_ICONERROR);
    std::process::exit(dw as i32);
}

fn format_hex_string(buffer: Vec<u8>) -> Vec<u8> {
    let mut w = Vec::new();
    for chunk in buffer.chunks(2) {
        let _sink = write!(&mut w, "{:02x}{:02x} ", chunk[0], chunk[1]);
    }

    return w;
}

pub unsafe extern "system" fn enum_font_families_x_proc(lpelfe: *const winapi::wingdi::LOGFONTW,
                                                        _lpntme: *const winapi::winnt::VOID,
                                                        _font_type: DWORD,
                                                        l_param: LPARAM)
                                                        -> std::os::raw::c_int {
    let back_channel = l_param as *mut winapi::wingdi::LOGFONTW;
    let elfstyle = (*(lpelfe as *const winapi::wingdi::ENUMLOGFONTEXW)).elfStyle;

    if widen("Regular").iter().zip(elfstyle.iter()).all(|(l, r)| l == r) {
        *back_channel = *lpelfe;
    }

    return if (*back_channel).lfFaceName[0] == 0 {
        1
    } else {
        0
    };
}

pub static mut TEXT: &'static str = "Drop files to hash";
pub static mut CLIENT: HWND = 0 as HWND;

//---------------------------------------------------------------------------
pub unsafe extern "system" fn window_proc(h_wnd: HWND,
                                          msg: UINT,
                                          w_param: WPARAM,
                                          l_param: LPARAM)
                                          -> LRESULT {
    match msg {
        winapi::winuser::WM_CREATE => {
            let pcs = ::std::mem::transmute::<::winapi::LPARAM,
                                                *const ::winapi::CREATESTRUCTW>(l_param);
            CLIENT = user32::CreateWindowExW(0,
                                             widen("edit").as_ptr(),
                                             std::ptr::null_mut(),
                                             winapi::WS_CHILD | winapi::WS_VISIBLE |
                                             winapi::WS_HSCROLL |
                                             winapi::WS_VSCROLL |
                                             winapi::WS_BORDER |
                                             winapi::ES_LEFT |
                                             winapi::ES_MULTILINE |
                                             winapi::ES_AUTOHSCROLL |
                                             winapi::ES_AUTOVSCROLL |
                                             winapi::ES_READONLY,
                                             0,
                                             0,
                                             0,
                                             0,
                                             h_wnd,
                                             1 as HMENU,
                                             (*pcs).hInstance,
                                             std::ptr::null_mut());

            {
                // System font for size
                let mut metrics = winapi::NONCLIENTMETRICSW {
                    cbSize: std::mem::size_of::<winapi::NONCLIENTMETRICSW>() as UINT,
                    iBorderWidth: 0 as i32,
                    iScrollWidth: 0 as i32,
                    iScrollHeight: 0 as i32,
                    iCaptionWidth: 0 as i32,
                    iCaptionHeight: 0 as i32,
                    lfCaptionFont: std::mem::uninitialized(),
                    iSmCaptionWidth: 0 as i32,
                    iSmCaptionHeight: 0 as i32,
                    lfSmCaptionFont: std::mem::uninitialized(),
                    iMenuWidth: 0 as i32,
                    iMenuHeight: 0 as i32,
                    lfMenuFont: std::mem::uninitialized(),
                    lfStatusFont: std::mem::uninitialized(),
                    lfMessageFont: std::mem::uninitialized(),
                    iPaddedBorderWidth: 0 as i32,
                };
                let status = user32::SystemParametersInfoW(winapi::SPI_GETNONCLIENTMETRICS,
                                                           metrics.cbSize,
                                                           &mut metrics as *mut _ as
                                                           *mut std::os::raw::c_void,
                                                           0);
                if status == 0 {
                    raise_error_message(&widen("Could not get system parameter info - "));
                    return 0;
                }

                let context = Hdc { dc: user32::GetDC(std::ptr::null_mut()) };

                let mut probe: winapi::wingdi::LOGFONTW = std::mem::uninitialized();
                probe.lfFaceName[0] = 0;
                probe.lfCharSet = winapi::ANSI_CHARSET as u8;

                let mut result: winapi::wingdi::LOGFONTW = std::mem::uninitialized();
                result.lfFaceName[0] = 0;

                // Find a monospace font
                let faces = [widen("Inconsolata"),
                             widen("Consolas"),
                             widen("Lucida Console"),
                             widen("Courier New")];

                faces.iter().find(|face: &&Vec<u16>| -> bool {
                    if face.len() > winapi::LF_FACESIZE {
                        return false;
                    }
                    for pair in face.iter().enumerate() {
                        let (i, val) = pair;
                        probe.lfFaceName[i] = *val;
                    }
                    gdi32::EnumFontFamiliesExW(context.dc,
                                               &mut probe as *mut _,
                                               Some(enum_font_families_x_proc),
                                               &mut result as *mut _ as LPARAM,
                                               0);
                    return result.lfFaceName[0] != 0;
                });

                // copy the sizes over
                if result.lfFaceName[0] != 0 {
                    result.lfHeight = metrics.lfMessageFont.lfHeight;
                    result.lfWidth = metrics.lfMessageFont.lfWidth;
                }

                // And set the monospaced font
                let hfont = gdi32::CreateFontIndirectW(if result.lfFaceName[0] != 0 {
                    &result
                } else {
                    &metrics.lfMessageFont
                });

                user32::SendMessageW(CLIENT, winapi::WM_SETFONT, hfont as WPARAM, 0);
            }

            user32::SetWindowTextW(CLIENT, widen(TEXT).as_ptr() as *mut _);
            TEXT = "";

            // Synthetic drop on activation
            {
                let command_line = kernel32::GetCommandLineW();
                let mut n_args: libc::c_int = 0;
                let arg_list = CommandLineToArgvW(command_line, &mut n_args as *mut _);

                if n_args > 1 {
                    //     if (szArglist && nArgs > 1)
                    //     {
                    //         std::unique_ptr<LPWSTR, void(__cdecl*)(LPWSTR*)> buffer(
                    //             szArglist, local_free<LPWSTR>);

                    //         // Skip the executable name
                    //         std::vector<LPWSTR> args(szArglist+1, szArglist + nArgs);

                    //         // pointer arithmetic, yuck!!
                    //         SIZE_T wideheader = (sizeof(DROPFILES) + sizeof(wchar_t) - 1)/sizeof(wchar_t);
                    //         SIZE_T header = wideheader *sizeof(wchar_t);
                    //         SIZE_T buffersize = header;

                    //         boost::for_each(args, [&buffersize] (LPWSTR in) {
                    //             buffersize += sizeof(wchar_t) * (wcslen(in) + 1);
                    //         });

                    //         HGLOBAL hGlobal = GlobalAlloc(GHND | GMEM_SHARE, buffersize);
                    //         if (hGlobal)
                    //         {
                    //             {
                    //                 GlobalLocked memory(hGlobal);

                    //                 LPDROPFILES pDropFiles = reinterpret_cast<LPDROPFILES>(memory.get());
                    //                 pDropFiles->pFiles = header;
                    //                 pDropFiles->fWide = TRUE;
                    //                 pDropFiles->pt.x = pDropFiles->pt.y = 0;
                    //                 pDropFiles->fNC = FALSE;

                    //                 wchar_t * buffer = reinterpret_cast<wchar_t *>(memory.get()) + wideheader;
                    //                 wchar_t * end = reinterpret_cast<wchar_t *>(memory.get()) + (buffersize / sizeof(wchar_t));

                    //                 boost::for_each(args, [&buffer, &end] (LPWSTR in) {
                    //                     wcscpy_s(buffer, end-buffer, in);
                    //                     buffer += (wcslen(in) + 1);
                    //                 });

                    //                 *buffer = L'\0';
                    //             }

                    //             // send DnD event
                    //             PostMessage(hwnd, WM_DROPFILES, (WPARAM)hGlobal, 0);
                    //         }
                }
            }
            return 0;
        }

        winapi::winuser::WM_SETFOCUS => {
            user32::SetFocus(CLIENT);
            return 0;
        }

        winapi::winuser::WM_SIZE => {
            user32::MoveWindow(CLIENT,
                               0,
                               0,
                               winapi::minwindef::LOWORD(l_param as DWORD) as i32,
                               winapi::minwindef::HIWORD(l_param as DWORD) as i32,
                               1 as BOOL);
            return 0;
        }

        winapi::winuser::WM_DROPFILES => {
            let _waiter = WaitCursor::new(); // leading _ for "unused by design"

            // auto lambda = [canvas] (std::wstring * str) {
            //             *str += L"\r\n";
            //             SetWindowText(canvas, str->c_str());
            //             };
            // std::unique_ptr<std::wstring, decltype(lambda)> data(&text, lambda);

            // disposeable<HDROP> drop(reinterpret_cast<HDROP>(wParam), &DragFinish);

            // UINT nfiles = DragQueryFileW(drop(), 0xFFFFFFFF, nullptr, 0);

            // *data += L"Dropped "+ boost::lexical_cast<std::wstring>(nfiles) +
            //        L" files\r\n";

            // // Get handle to the crypto provider
            // HCRYPTPROV hProv = 0;
            // if (!CryptAcquireContextW(&hProv,
            //         nullptr,
            //         nullptr,
            //         PROV_RSA_AES,
            //         CRYPT_VERIFYCONTEXT))
            // {
            //     *data += L"CryptAcquireContext failed: " + boost::lexical_cast<std::wstring>(GetLastError());
            //     return 0;
            // }

            // // and manage its lifetime
            // disposeable<HCRYPTPROV> provider(hProv, [] (HCRYPTPROV prov) {CryptReleaseContext(prov, 0);});

            // // Now hash each file in turn
            // for(UINT i = 0; i < nfiles; ++i)
            // {
            //     UINT length = DragQueryFileW(drop(), i, nullptr, 0);
            //     std::vector<wchar_t> fn(length + 1);
            //     DragQueryFileW(drop(), i, &fn[0], fn.size());

            //     typedef boost::tuple<std::wstring, DWORD, DWORD> Recipe;
            //     typedef boost::tuple<std::wstring, std::shared_ptr<disposeable<HCRYPTHASH>>, DWORD> Record;
            //     bool is_good = true;

            //     std::vector<Recipe> inputs;
            //     inputs.push_back( boost::make_tuple(L"MD5     ", CALG_MD5, 16 ));
            //     inputs.push_back( boost::make_tuple(L"SHA     ", CALG_SHA1, 20 ));
            //     inputs.push_back( boost::make_tuple(L"SHA2-256", CALG_SHA_256, 32 ));
            //     // ... add more hash algorithms here e.g. CALG_SHA_384 or CALG_SHA_512

            //     std::vector<Record> results;
            //     results.resize(inputs.size());

            //     boost::transform(inputs, results.begin(), [&provider, &data] (Recipe in) -> Record {
            //         HCRYPTHASH hHash = 0;

            //         if (!CryptCreateHash(provider(), in.get<1>(), 0, 0, &hHash))
            //         {
            //             std::wstring message = L"CryptCreateHash " + in.get<0>() + L" failed: ";
            //             get_error_message(message);
            //             *data += message;
            //             hHash = 0;
            //         }

            //         auto ptr = new disposeable<HCRYPTHASH>(hHash, [] (HCRYPTHASH hash){if(hash){CryptDestroyHash(hash);}});
            //         return boost::make_tuple(in.get<0>(), std::tr1::shared_ptr<disposeable<HCRYPTHASH>>(ptr), in.get<2>());
            //     });

            //     *data += &fn[0];
            //     *data += L"\r\n";

            //     std::ifstream file(&fn[0], std::ios::in|std::ios::binary);
            //     std::vector<char> chunk(4096);
            //     if (file.is_open())
            //     {

            //         for(;;)
            //         {
            //             file.read(&chunk[0], chunk.size());

            //             DWORD got = boost::numeric_cast<DWORD>(file.gcount());

            //             auto check =
            //                 boost::find_if(results, [&data, &chunk, &got] (Record hash) -> bool {
            //                 auto handle = (*hash.get<1>())();
            //                 if (!handle || CryptHashData(handle, reinterpret_cast<BYTE*>(&chunk[0]), got, 0))
            //                 {
            //                     return false;
            //                 }

            //                 std::wstring message = L"CryptHashData " + hash.get<0>() + L" failed: ";
            //                 get_error_message(message);
            //                 *data += message;
            //                 *data += L"\r\n";
            //                 return true;
            //             });

            //             if (check != results.end() || file.rdstate())
            //             {
            //                 is_good = check == results.end();
            //                 break;
            //             }
            //         }

            //         file.close();
            //         if (!is_good)
            //         {
            //             continue;
            //         }

            //         // Format the outputs
            //         boost::for_each(results, [&data] (Record hash) {
            //             DWORD hash_size = hash.get<2>();
            //             std::vector<BYTE> buffer(hash_size);
            //             auto handle = (*hash.get<1>())();
            //             if (handle)
            //             {
            //                 if(CryptGetHashParam(handle, HP_HASHVAL, &buffer[0], &hash_size, 0))
            //                 {
            //                     *data += hash.get<0>() + L": ";
            //                     format_hex_string(buffer, *data.get());
            //                 }
            //                 else
            //                 {
            //                     std::wstring message = L"CryptGetHashParam " + hash.get<0>() + L" failed: ";
            //                     get_error_message(message);
            //                     *data += message;
            //                 }
            //                 *data += L"\r\n";
            //             }
            //         });
            //     } // file opened

            //     *data += L"\r\n";

            // }

            return 0;
        }

        winapi::winuser::WM_DESTROY => {
            user32::PostQuitMessage(0);
            return 0;
        }

        _ => {
            return user32::DefWindowProcW(h_wnd, msg, w_param, l_param);
        }
    }
}

fn winmain(class_name: &str,
           title: &str,
           wndproc: unsafe extern "system" fn(HWND, UINT, WPARAM, LPARAM) -> LRESULT,
           icon_index: Option<u16>)
           -> i32 {
    let class_name_w = widen(class_name);

    let mut msg = winapi::winuser::MSG {
        hwnd: 0 as HWND,
        message: 0 as UINT,
        wParam: 0 as WPARAM,
        lParam: 0 as LPARAM,
        time: 0 as DWORD,
        pt: winapi::windef::POINT { x: 0, y: 0 },
    };

    unsafe {
        let (h_instance, index) = match icon_index {
            None => (0 as HINSTANCE, winapi::winuser::IDI_APPLICATION),
            Some(x) => {
                (kernel32::GetModuleHandleW(std::ptr::null_mut()) as HINSTANCE, x as *const u16)
            }
        };

        // Register a new window class with Windows
        let wnd = winapi::winuser::WNDCLASSW {
            style: winapi::CS_HREDRAW | winapi::CS_VREDRAW,
            lpfnWndProc: Some(wndproc),
            cbClsExtra: 0,
            cbWndExtra: 0,
            hInstance: h_instance,
            hIcon: user32::LoadIconW(h_instance, index),
            hCursor: user32::LoadCursorW(0 as HINSTANCE, winapi::winuser::IDC_ARROW),
            hbrBackground: gdi32::GetStockObject(winapi::WHITE_BRUSH) as HBRUSH,
            lpszMenuName: 0 as LPCWSTR,
            lpszClassName: class_name_w.as_ptr(),
        };
        // let w_class =
        user32::RegisterClassW(&wnd);

        // Create a window based on our new class
        let h_wnd = user32::CreateWindowExW(0,
                                            // w_class as *mut _, // alternative
                                            class_name_w.as_ptr(),
                                            widen(title).as_ptr(),
                                            winapi::WS_OVERLAPPEDWINDOW | winapi::WS_VISIBLE,
                                            winapi::CW_USEDEFAULT,
                                            winapi::CW_USEDEFAULT,
                                            winapi::CW_USEDEFAULT,
                                            winapi::CW_USEDEFAULT,
                                            std::ptr::null_mut(),
                                            0 as HMENU,
                                            0 as HINSTANCE,
                                            std::ptr::null_mut());

        if h_wnd == 0 as HWND {
            let err = kernel32::GetLastError();
            return err as i32;
        }

        // Show and update our window
        user32::ShowWindow(h_wnd, winapi::SW_SHOWNORMAL);
        user32::UpdateWindow(h_wnd);

        // Retrieve and process messages until we get WM_QUIT
        loop {
            if user32::GetMessageW(&mut msg, 0 as HWND, 0, 0) == 0 {
                break;
            }

            user32::TranslateMessage(&mut msg);
            user32::DispatchMessageW(&mut msg);
        }

        return msg.wParam as i32;
    }
}

fn main() {
    std::process::exit(winmain("DropHashClass", APPNAME, window_proc, Some(1)));
}