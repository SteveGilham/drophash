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


import sys
import os.path
sys.packageManager.makeJavaPackage("javax.swing", "JWindow", None)
sys.packageManager.makeJavaPackage("java.awt", "Window", None)
sys.packageManager.makeJavaPackage("java.awt.dnd", "DropTargetListener", None)
sys.packageManager.makeJavaPackage("java.awt.datatransfer", "DataFlavor", None)
import javax.swing
import java.lang
import java.awt.dnd
import java.awt.datatransfer

from md5 import *
from sha import *

pane = None
hashes = [md5, sha]
nbsp = list("&nbsp;")
target = None

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
  
def OnDropFiles(filenames) :
  global pane, nbsp, hashes
  pane.text += "<table border=1><tr><th>File</th><th>MD5</th><th>SHA-1</th></tr>"
  for file in filenames :
    pane.text += "<tr><td>%s</td>" % file

    for hashOp in hashes :
      anHash = getHash(str(file), hashOp, nbsp)
      pane.text += "<td><pre>%s</pre></td>" % anHash

  pane.text +="</tr>"
  pane.text +="</table>"

  

class dropHandler(java.awt.dnd.DropTargetAdapter):
  def __init__(self):
    java.awt.dnd.DropTargetAdapter.__init__(self)

  def drop(self, evt):
    if not evt.transferable.isDataFlavorSupported(java.awt.datatransfer.DataFlavor.javaFileListFlavor):
      return
    evt.acceptDrop(-1)
    names = evt.transferable.getTransferData(java.awt.datatransfer.DataFlavor.javaFileListFlavor)
    OnDropFiles(names)

def decorate(frame):
  global pane
  frame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
  frame.title = "Jython - Drophash"
  frame.layout = java.awt.BorderLayout()

  pane = javax.swing.JLabel()
  pane.border = javax.swing.BorderFactory.createEmptyBorder(5,5,5,5)
  frame.contentPane.add(javax.swing.JScrollPane(pane), java.awt.BorderLayout.CENTER)
  pane.text = "<html>"
  icon = java.awt.Toolkit.getDefaultToolkit().createImage("drophash.png")
  frame.setIconImage(icon)
  target = java.awt.dnd.DropTarget(pane, dropHandler())
  
  
  width = 800
  height = 600
  frame.setSize(width, height);
  screenDim = java.awt.Toolkit.getDefaultToolkit().getScreenSize();
  frame.setLocation(
        (screenDim.width - width) / 2,
        (screenDim.height - height) / 2
        )
  



if __name__ == "__main__":
  frame = javax.swing.JFrame()
  decorate(frame)
  frame.visible = True