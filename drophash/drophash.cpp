
#include <windows.h>
#pragma hdrstop
#if __BORLANDC__ >=	0x0530
#include <condefs.h>
#endif
#include <stdio.h>
#include <string>
#include "md5.h"
#include "sha.h"
#include "drophash.rh"

#if __BORLANDC__ >=	0x0530
USEUNIT("sha.c");
USEUNIT("md5.c");
USERC("drophash.rc");
USEOBJ("E:\C++Builder3\CBuilder3\Lib\c0w32.obj");
USELIB("E:\C++Builder3\CBuilder3\Lib\import32.lib");
USELIB("E:\C++Builder3\CBuilder3\Lib\cw32mt.lib");
//---------------------------------------------------------------------------
#endif
//---------------------------------------------------------------------------
#define ID_EDIT 1

LRESULT CALLBACK MainWndProc(HWND, UINT, WPARAM, LPARAM);
#define MAINCLASS TEXT("DropHashClass")
#define APPNAME   TEXT("DropHash")

//---------------------------------------------------------------------------
#pragma argsused
int WINAPI WinMain(HINSTANCE instance, HINSTANCE, LPSTR, int show)
{
    WNDCLASS wndclass;
    wndclass.style = CS_HREDRAW | CS_VREDRAW;
    wndclass.lpfnWndProc = reinterpret_cast<WNDPROC>(MainWndProc);
    wndclass.cbClsExtra =
    wndclass.cbWndExtra = 0;
    wndclass.hInstance = instance;
    wndclass.hIcon = LoadIcon(instance, MAKEINTRESOURCE(IDI_DROPHASH));
    wndclass.hCursor = LoadCursor(NULL, IDC_ARROW);
    wndclass.hbrBackground = (HBRUSH) GetStockObject(WHITE_BRUSH);
    wndclass.lpszMenuName = NULL;
    wndclass.lpszClassName = MAINCLASS;

    if(!RegisterClass(&wndclass))
    {
    	  char buffer[256];
        sprintf(buffer, "Could not register main window - %d\n", GetLastError());
        MessageBox(NULL, buffer, APPNAME, MB_ICONERROR);
        return 0;
    }

    HWND hmain = CreateWindowEx(WS_EX_ACCEPTFILES,MAINCLASS, APPNAME,
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        CW_USEDEFAULT,
        NULL, NULL, instance, NULL);
    ShowWindow(hmain, show);
    UpdateWindow(hmain);

    MSG msg;
    while(GetMessage(&msg, NULL, 0, 0))
    {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }
    return msg.wParam;
}

LRESULT CALLBACK MainWndProc(HWND hwnd, UINT message,
                WPARAM wParam, LPARAM lParam)
{
    static HWND client;
    static std::string text("Drop files to hash");

    switch(message)
    {
        case WM_CREATE:
        client = CreateWindow(TEXT("edit"), NULL,
        WS_CHILD|WS_VISIBLE|WS_HSCROLL|WS_VSCROLL|WS_BORDER|ES_LEFT|
        ES_MULTILINE|ES_AUTOHSCROLL|ES_AUTOVSCROLL|ES_READONLY,
        0,0,0,0, hwnd, (HMENU) ID_EDIT,
        reinterpret_cast<LPCREATESTRUCT>(lParam)->hInstance,
        NULL);

        SendMessage(client, WM_SETFONT,
            reinterpret_cast<WPARAM>(GetStockObject(SYSTEM_FIXED_FONT)), 0);

        SetWindowText(client, text.c_str());
        text = "";
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
            char buff[1024];
            HDROP hDrop = reinterpret_cast<HDROP>(wParam);
            UINT nfiles = DragQueryFile(hDrop, 0xFFFFFFFF, buff, sizeof buff);
            sprintf(buff, "Dropped %d files\r\n", nfiles);
            text+=buff;

            for(UINT i=0; i<nfiles; ++i)
            {
                char fn[512];
                DragQueryFile(hDrop, i, fn, sizeof fn);
                FILE *	fp = fopen(fn, "rb");
                byte chunk[256], md5[MD5HASHSIZE], sha[SHAHASHSIZE];
                long got;
                MD5_CTX *md5ctx; size_t md5len; MD5Init(&md5ctx, &md5len);
                SHA_INFO *shactx; size_t shalen;  SHA1Init(&shactx, &shalen);

                while( (got=fread(chunk, 1, sizeof chunk, fp)) > 0)
                {
		            MD5Update(md5ctx, chunk, got);
		            SHAUpdate(shactx, chunk, got);
                }
                fclose(fp);

                SHAFinal(&shactx, sha, shalen);
	            MD5Final(&md5ctx, md5, md5len);

                int x = sprintf(buff, "MD5: ");
                int k;
                for(k=0; k<MD5HASHSIZE; ++k)
                {
     	            x += sprintf(buff+x, "%02x", md5[k]);
                    if(k%2) x += sprintf(buff+x, " ");
                }
                x += sprintf(buff+x, "SHA: ");
                for(k=0; k<SHAHASHSIZE; ++k)
                {
     	            x += sprintf(buff+x, "%02x", sha[k]);
                    if(k%2) x += sprintf(buff+x, " ");
                }

                sprintf(buff+x, " %s\r\n", fn);
                text+=buff;
            }
            text+="\r\n";
            SetWindowText(client, text.c_str());
            DragFinish(hDrop);
            return 0;
        }


        case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProc(hwnd, message, wParam, lParam);

}
