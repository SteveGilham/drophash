#!/usr/bin/env python
# -*- coding: ISO-8859-1 -*-

#MD5 test suite:
#MD5 ("") = d41d8cd98f00b204e9800998ecf8427e
#MD5 ("a") = 0cc175b9c0f1b6a831c399e269772661
#MD5 ("abc") = 900150983cd24fb0d6963f7d28e17f72
#MD5 ("message digest") = f96b697d7cb7938d525a2f31aaf161d0
#MD5 ("abcdefghijklmnopqrstuvwxyz") = c3fcd3d76192e4007dfb496cca67e13b
#MD5 ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") = d174ab98d277d9f5a5611c2c9f419d9f
#MD5 ("12345678901234567890123456789012345678901234567890123456789012345678901234567890") = 57edf4a22be3c955ac49da2e2107b67a

#SHA test suite:
#SHA ("abc") = A9993E36 4706816A BA3E2571 7850C26C 9CD0D89D
#SHA ("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq") = 84983E44 1C3BD26E BAAE4AA1 F95129E5 E54670F1
#SHA (A million repetitions of "a") = 34AA973C D4C4DAA4 F61EEB2B DBAD2731 6534016F


import wx
import wx.html as html
from md5 import *
from sha import *

def getHash(file, hashOp, delimiter) :

    try :
        reader = open(file, 'rb')
    except :
        return ""
    
    try :
        digest = hashOp()
        while True :
            chunk = reader.read(1024)
            if not chunk :
                break
            digest.update(chunk)
    finally:
        reader.close()
        
    raw = digest.hexdigest()
    work = []
    i = 0
    while i < len(raw) :
        work.append(raw[i])
        i += 1
        if 0 == (i%8) :
            work = work + delimiter

    return ''.join(work)

class FileDropTarget(wx.FileDropTarget):
    def __init__(self, window):
        wx.FileDropTarget.__init__(self)
        self.window = window

    hashes = (md5, sha)
    nbsp = list("&nbsp;")

    def OnDropFiles(self, x, y, filenames) :
        self.window.AppendToPage("<table border=1><tr><th>File</th><th>MD5</th><th>SHA-1</th></tr>")
        for file in filenames :
            self.window.AppendToPage("<tr><td>%s</td>" % file)

            for hashOp in self.hashes :
                anHash = getHash(file, hashOp, self.nbsp)
                self.window.AppendToPage("<td><pre>%s</pre></td>" % anHash)

            self.window.AppendToPage("</tr>")
        self.window.AppendToPage("</table>")

def decorate(frame):
    frame.SetTitle("wxPython - Drophash")
    _icon = wx.EmptyIcon()
    _icon.CopyFromBitmap(wx.Bitmap("drophash.ico", wx.BITMAP_TYPE_ANY))
    frame.SetIcon(_icon)
    frame.style = wx.DEFAULT_FRAME_STYLE
    pane = html.HtmlWindow(frame, -1)
    dt = FileDropTarget(pane)
    pane.SetDropTarget(dt)


if __name__ == "__main__":
    theApp = wx.App(False)
    frame  = wx.Frame(None, -1)
    decorate(frame)
    theApp.SetTopWindow(frame)
    frame.Show()
    theApp.MainLoop()