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

import clr
clr.AddReferenceByPartialName("System.Windows.Forms")
clr.AddReferenceByPartialName("System.Drawing")

from System.Windows.Forms import *
from System.Drawing import *

from md5 import *
from sha import *

pane = None
hashes = [md5, sha]
nbsp = list(" ")


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

def dragOver(sender, evt):
  evt.Effect = DragDropEffects.Link
 
def dragDrop(sender, evt):
  global pane
  data = evt.Data
  data = data.GetData(DataFormats.FileDrop)
  num = pane.Items.Count
 
  for file in data:
    pane.Items.Add(file)
    for hashOp in hashes :
      anHash = getHash(file, hashOp, nbsp)
      pane.Items[num].SubItems.Add(anHash)
    num += 1
      
    


def decorate(frame):
    global pane
    frame.Text = "IronPython - Drophash"
    ico = Image.FromFile("drophash.ico")
    frame.Icon = Icon.FromHandle(ico.GetHicon());
    pane = ListView()
    pane.Dock = DockStyle.Fill;
    frame.Controls.Add(pane)
    pane.AllowDrop = True
    pane.DragOver += dragOver
    pane.DragEnter += dragOver
    pane.DragDrop += dragDrop
    pane.View = View.Details
    pane.Columns.Add("File name", 100, HorizontalAlignment.Left)
    pane.Columns.Add("MD5", 200, HorizontalAlignment.Left)
    pane.Columns.Add("SHA-1", 200, HorizontalAlignment.Left)


if __name__ == "__main__":
    frame = Form()
    decorate(frame)
    Application.Run(frame)